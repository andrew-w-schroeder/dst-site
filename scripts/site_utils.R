# ==============================================================================
# site_utils.R — helpers shared by the website refresh scripts (45_dst_refresh.R, 55_k_refresh.R) and the
# reports (44, 54). Base R + dplyr/tibble/purrr only (runs on the GitHub runner).
#   * weather: Open-Meteo forecasts (temp, wind, gusts, precipitation) for this week's outdoor venues,
#     logged to data/lines/weather_history.csv (one fetch per run, shared by D/ST and kickers)
#   * tiers:   natural-break tiers of the projections (optimal 1-D grouping)
#   * trend:   projection history (data/lines/proj_history.csv) and the dotted sparkline
#   * drivers: what moves each team's projection away from an average team this week (Vegas excluded)
# ==============================================================================

## ---- Weather ----
STADIUMS_SITE <- tibble::tribble(               # outdoor / retractable venues (domes need no forecast)
  ~stadium_id, ~lat, ~lon,
  "GNB00", 44.5013, -88.0622, "BUF00", 42.7738, -78.7870, "CLE00", 41.5061, -81.6995, "IND00", 39.7601, -86.1639,
  "JAX00", 30.3239, -81.6373, "MIA00", 25.9580, -80.2389, "NYC01", 40.8135, -74.0745, "PIT00", 40.4468, -80.0158,
  "WAS00", 38.9078, -76.8645, "SFO01", 37.4030, -121.9700, "TAM00", 27.9759, -82.5033, "DAL00", 32.7473, -97.0945,
  "DEN00", 39.7439, -105.0201, "CHI98", 41.8623, -87.6167, "KAN00", 39.0489, -94.4839, "BAL00", 39.2780, -76.6227,
  "CAR00", 35.2258, -80.8528, "CIN00", 39.0955, -84.5161, "NAS00", 36.1665, -86.7713, "BOS00", 42.0909, -71.2643,
  "PHI00", 39.9008, -75.1675, "SEA00", 47.5952, -122.3316, "PHO00", 33.5276, -112.2626, "HOU00", 29.6847, -95.4107,
  "LON00", 51.5560, -0.2796, "LON02", 51.6043, -0.0664, "MEX00", 19.3029, -99.1505, "RIO00", -22.9121, -43.2302,
  "MAD01", 40.4531, -3.6883, "SAO00", -23.5453, -46.4742, "DUB00", 53.3607, -6.2512, "BER00", 52.5147, 13.2395)
utc_time <- function(x) as.POSIXct(sub("Z$", "", x), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
iso_utc  <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
WX_COLS  <- c("pulled_at", "game_id", "kickoff", "temp", "wind", "gust", "precip_prob", "precip_in", "precip_max")   # precip_max added 2026-09-25 (older rows NA)

# this week's games from the nflverse schedule (roof, stadium, kickoff in UTC)
week_games <- function(season, week) {
  tmp <- tempfile(fileext = ".rds")
  ok <- tryCatch({ utils::download.file("https://github.com/nflverse/nflverse-data/releases/download/schedules/games.rds", tmp, mode = "wb", quiet = TRUE); TRUE },
                 error = function(e) FALSE)
  if (!ok) return(NULL)
  g <- readRDS(tmp)
  g <- g[g$season == season & g$week == week & g$game_type == "REG", ]
  tibble::tibble(game_id = g$game_id, stadium_id = g$stadium_id, roof = ifelse(is.na(g$roof), "", g$roof),
                 kickoff = as.POSIXct(format(as.POSIXct(paste(g$gameday, g$gametime), tz = "America/New_York"), tz = "UTC"), tz = "UTC"),
                 home_team = g$home_team, away_team = g$away_team,          # projected starters (starters.R)
                 home_qb_id = g$home_qb_id, home_qb_name = g$home_qb_name, away_qb_id = g$away_qb_id, away_qb_name = g$away_qb_name)
}

wx_fetch1 <- function(lat, lon, ko) {          # temp / wind: mean over kickoff hour + 2 h; gust: max. Rain: 1 h before to 3 h after kickoff
  d <- format(ko, "%Y-%m-%d", tz = "America/New_York"); hr <- as.integer(format(ko, "%H", tz = "America/New_York"))
  url <- sprintf(paste0("https://api.open-meteo.com/v1/forecast?latitude=%.4f&longitude=%.4f",
                        "&hourly=temperature_2m,wind_speed_10m,wind_gusts_10m,precipitation_probability,precipitation",
                        "&temperature_unit=fahrenheit&wind_speed_unit=mph&precipitation_unit=inch&timezone=America%%2FNew_York&start_date=%s&end_date=%s"),
                 lat, lon, d, d)
  js <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
  if (is.null(js)) return(NULL)
  h <- js$hourly; hh <- as.integer(substr(h$time, 12, 13))
  idx <- which(hh %in% hr:(hr + 2)); ir <- which(hh %in% (hr - 1):(hr + 3))     # game window / rain window (a wet field matters too)
  if (!length(idx)) return(NULL)
  mx <- function(v, i = idx) if (is.null(v) || all(is.na(v[i]))) NA_real_ else max(v[i], na.rm = TRUE)
  tibble::tibble(temp = round(mean(h$temperature_2m[idx])), wind = round(mean(h$wind_speed_10m[idx])), gust = round(mx(h$wind_gusts_10m)),
                 precip_prob = round(mx(h$precipitation_probability, ir)), precip_in = round(sum(h$precipitation[ir], na.rm = TRUE), 2),
                 precip_max = round(mx(h$precipitation, ir), 3))
}

read_wx_hist <- function(file) {
  if (!file.exists(file)) return(tibble::tibble(pulled_at = character(), game_id = character(), kickoff = character(), temp = numeric(),
                                                wind = numeric(), gust = numeric(), precip_prob = numeric(), precip_in = numeric(), precip_max = numeric()))
  h <- tibble::as_tibble(utils::read.csv(file, stringsAsFactors = FALSE, colClasses = c(pulled_at = "character", game_id = "character", kickoff = "character")))
  for (v in setdiff(WX_COLS, names(h))) h[[v]] <- NA_real_
  h[WX_COLS]
}

# fetch forecasts for games not yet started (skips games pulled < min_gap minutes ago, so D/ST and
# kickers share one fetch per run); returns the history
wx_update <- function(file, games, now, min_gap = 30, skip = nzchar(Sys.getenv("NO_WEATHER"))) {
  hist <- read_wx_hist(file)
  if (is.null(games) || skip) return(hist)
  recent <- hist$game_id[!is.na(utc_time(hist$pulled_at)) & as.numeric(difftime(now, utc_time(hist$pulled_at), units = "mins")) < min_gap]
  todo <- dplyr::inner_join(games, STADIUMS_SITE, by = "stadium_id")
  todo <- todo[todo$roof != "dome" & todo$kickoff > now & !todo$game_id %in% recent, ]
  new <- purrr::map_dfr(seq_len(nrow(todo)), function(i) {
    w <- wx_fetch1(todo$lat[i], todo$lon[i], todo$kickoff[i]); if (is.null(w)) return(NULL)
    dplyr::bind_cols(tibble::tibble(pulled_at = iso_utc(now), game_id = todo$game_id[i], kickoff = iso_utc(todo$kickoff[i])), w)
  })
  if (nrow(new)) { hist <- dplyr::bind_rows(hist, new); dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
                   utils::write.csv(hist, file, row.names = FALSE); message("weather: ", nrow(new), " forecasts from Open-Meteo") }
  else if (nrow(todo)) message("weather: no new forecasts (fetch failed) — using stored values")
  hist
}
# newest forecast pulled before kickoff, per game (started games stay frozen at their last pre-kickoff forecast)
wx_latest <- function(hist) {
  if (!nrow(hist)) return(tibble::tibble(game_id = character(), temp = numeric(), wind = numeric(), gust = numeric(), precip_prob = numeric(), precip_in = numeric(), precip_max = numeric(), wx_time = as.POSIXct(character())))
  h <- dplyr::mutate(hist, t = utc_time(pulled_at), ko = utc_time(kickoff))
  h <- h[!is.na(h$t) & h$t < h$ko, ]
  h <- dplyr::ungroup(dplyr::slice_max(dplyr::group_by(h, game_id), t, n = 1, with_ties = FALSE))
  dplyr::transmute(h, game_id, temp, wind, gust, precip_prob, precip_in, precip_max = dplyr::coalesce(precip_max, precip_in / 3), wx_time = t)   # old rows: 3-h total / 3
}
# Rain intensity from the peak hourly rate in the rain window (NWS: light < 0.10 in/h, moderate 0.10–0.30, heavy > 0.30).
# The forecast amount is one best-guess run, so "60% chance, light" is common; the chance says whether it rains at all.
rain_level <- function(mx) ifelse(is.na(mx), NA_character_, ifelse(mx < 0.01, "", ifelse(mx < 0.10, "light", ifelse(mx < 0.30, "moderate", "heavy"))))
RAIN_HIGH <- 50                                   # % chance that counts as "likely"
rain_cls <- function(prob, mx) { lv <- rain_level(mx)
  ifelse(!is.na(prob) & prob >= RAIN_HIGH & lv %in% "heavy", "wx2", ifelse(!is.na(prob) & prob >= RAIN_HIGH & lv %in% "moderate", "wx1", "")) }
wind_cls <- function(x) ifelse(is.na(x), "", ifelse(x > 25, "wx2", ifelse(x > 15, "wx1", "")))     # sustained wind or gust, mph
rain_txt <- function(prob, mx) ifelse(is.na(prob), "", paste0(round(prob), "% rain", ifelse(is.na(rain_level(mx)) | !nzchar(coalesce(rain_level(mx), "")), "", paste0(" (", rain_level(mx), ")"))))
wspan <- function(txt, cls) ifelse(nzchar(cls), sprintf('<span class="%s">%s</span>', cls, txt), txt)
# compact label: "64° · 8 mph (g 17) · 60% rain (moderate)"; html = TRUE colours wind / gust > 15 / 25 mph and likely moderate / heavy rain
wx_label <- function(indoor, temp, wind, gust, precip_prob, precip_max = NA, html = FALSE) {
  w <- paste0(round(wind), " mph"); g <- ifelse(is.na(gust), "", paste0("(g ", round(gust), ")")); r <- rain_txt(precip_prob, precip_max)
  if (html) { w <- wspan(w, wind_cls(wind)); g <- ifelse(nzchar(g), wspan(g, wind_cls(gust)), g); r <- ifelse(nzchar(r), wspan(r, rain_cls(precip_prob, precip_max)), r) }
  ifelse(indoor %in% 1, "indoor", ifelse(is.na(wind), "—",
    paste0(ifelse(is.na(temp), "", paste0(round(temp), "° · ")), w, ifelse(nzchar(g), paste0(" ", g), ""), ifelse(nzchar(r), paste0(" · ", r), ""))))
}

## ---- Tiers: optimal 1-D grouping (Jenks natural breaks by dynamic programming) ----
# Returns tier (1 = best) and whether the drop into each tier is a clear break (gap > `clear`).
tiers <- function(x, k = 6, clear = NULL) {
  n <- length(x); k <- min(k, n); o <- order(-x); xs <- x[o]
  cs <- c(0, cumsum(xs)); cs2 <- c(0, cumsum(xs^2))
  sse <- function(i, j) { m <- j - i + 1; s <- cs[j + 1] - cs[i]; cs2[j + 1] - cs2[i] - s^2 / m }
  D <- matrix(Inf, k, n); B <- matrix(0L, k, n); for (j in 1:n) D[1, j] <- sse(1, j)
  if (k >= 2) for (kk in 2:k) for (j in kk:n) for (i in kk:j) { v <- D[kk - 1, i - 1] + sse(i, j); if (v < D[kk, j]) { D[kk, j] <- v; B[kk, j] <- i } }
  cl <- integer(n); j <- n; for (kk in k:1) { i <- if (kk == 1) 1 else B[kk, j]; cl[i:j] <- kk; j <- i - 1 }
  tier <- integer(n); tier[o] <- cl
  gap_in <- sapply(seq_len(k), function(t) if (t == 1) NA else min(x[tier == t - 1]) - max(x[tier == t]))
  list(tier = tier, gap_in = gap_in, clear = if (is.null(clear)) rep(NA, k) else gap_in > clear)
}

# Cell colour by tier: 1 = dark green, 2 = light green, second-to-last = light red, last = dark red
tier_cls <- function(tier, k = max(tier, na.rm = TRUE)) ifelse(tier == 1, "t1", ifelse(tier == 2, "t2", ifelse(tier == k, "t6", ifelse(tier == k - 1, "t5", ""))))

## ---- Projection history + dotted trend sparkline ----
PH_COLS <- c("model", "season", "week", "system", "team", "time", "kind", "proj", "implied")   # implied added 2026-09-25 (older rows NA)
read_ph <- function(file) {
  if (!file.exists(file)) return(tibble::tibble(model = character(), season = integer(), week = integer(), system = character(), team = character(),
                                                time = character(), kind = character(), proj = numeric(), implied = numeric()))
  h <- tibble::as_tibble(utils::read.csv(file, stringsAsFactors = FALSE, colClasses = c(model = "character", system = "character", team = "character", time = "character", kind = "character")))
  if (!"implied" %in% names(h)) h$implied <- NA_real_
  h
}
# base = weekly-run projections (added ONCE per week, at the first weekly run's fit time: a mid-week rerun of the model
# does not replace it, so Δ columns and the trend's open dot always refer to the first (Tuesday) run), cur = this refresh
ph_update <- function(file, model, season, week, fit_time, base, cur, now) {   # base / cur: system, team, proj [, implied]
  ph <- read_ph(file)
  has_base <- any(ph$model == model & ph$season == season & ph$week == week & ph$kind == "weekly")
  add <- dplyr::bind_rows(if (!has_base) dplyr::mutate(base, model = model, season = season, week = week, time = iso_utc(fit_time), kind = "weekly"),
                          dplyr::mutate(cur, model = model, season = season, week = week, time = iso_utc(now), kind = "refresh"))
  if (!"implied" %in% names(add)) add$implied <- NA_real_
  ph <- dplyr::bind_rows(ph, add[PH_COLS]); dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(ph, file, row.names = FALSE)
  ph[ph$model == model & ph$season == season & ph$week == week, ]
}
# the week's baseline = the first weekly run: proj / implied per system × team + its time (for "Δ since Tue")
ph_base <- function(ph) {
  w <- ph[ph$kind == "weekly", ]; w$t <- utc_time(w$time)
  w <- dplyr::ungroup(dplyr::slice_min(dplyr::group_by(w, system, team), t, n = 1, with_ties = FALSE))
  list(tbl = dplyr::transmute(w, system, team, proj_first = proj, implied_first = implied), time = if (nrow(w)) min(w$t) else as.POSIXct(NA))
}
# one point for the weekly run + one per refresh
trend_points <- function(ph_team) {
  ph_team$t <- utc_time(ph_team$time); ph_team <- ph_team[order(ph_team$t), ]
  wk <- ph_team[ph_team$kind == "weekly", ][1, ]
  rf <- ph_team[ph_team$kind == "refresh", ]
  dplyr::bind_rows(if (nrow(wk) && !is.na(wk$proj)) dplyr::mutate(wk, lab = paste0("weekly run ", format(wk$t, "%a %b %d", tz = "America/New_York"))),
                   dplyr::mutate(rf, lab = sub(" 0", " ", format(t, "%a %b %d %I:%M %p", tz = "America/New_York"))))
}
sparkline <- function(pts, w = NULL, h = 24, min_span = 1) {
  if (is.null(pts) || nrow(pts) < 1) return("")
  v <- pts$proj; n <- length(v); lo <- min(v); hi <- max(v)
  if (is.null(w)) w <- max(96, min(180, 10 + 7 * (n - 1)))            # grows with the number of refreshes
  if (hi - lo < min_span) { mid <- (hi + lo) / 2; lo <- mid - min_span / 2; hi <- mid + min_span / 2 }
  xs <- if (n == 1) w / 2 else 5 + (seq_len(n) - 1) * (w - 10) / (n - 1)
  ys <- h - 4 - (v - lo) / (hi - lo) * (h - 8)
  d <- v[n] - v[1]; col <- if (abs(d) < 0.05) "#888" else if (d > 0) "#1a9850" else "#d73027"
  line <- if (n > 1) sprintf('<polyline points="%s" fill="none" stroke="%s" stroke-width="1.5" stroke-dasharray="3,2"/>',
                             paste(sprintf("%.1f,%.1f", xs, ys), collapse = " "), col) else ""
  dots <- paste(sprintf('<circle cx="%.1f" cy="%.1f" r="%s" fill="%s"%s><title>%s: %.2f</title></circle>', xs, ys,
                        ifelse(pts$kind == "weekly", "2.6", if (n > 12) "1.7" else "2.2"), ifelse(pts$kind == "weekly", "var(--bg)", col),
                        ifelse(pts$kind == "weekly", sprintf(' stroke="%s" stroke-width="1.3"', col), ""), pts$lab, v), collapse = "")
  sprintf('<svg class="spark" width="%d" height="%d" viewBox="0 0 %d %d" role="img"><title>%s</title>%s%s</svg>', w, h, w, h,
          paste(sprintf("%s: %.2f", pts$lab, v), collapse = "; "), line, dots)
}

## ---- Drivers: occlusion attribution ----
# For each feature group: projection minus the projection with that group set to this week's league average
# (all 32 teams). Positive = the group raises this team's projection. Exact for any model; Vegas groups are excluded.
drivers <- function(te, pred_fun, groups, top = 4, min_abs = 0.05) {
  full <- pred_fun(te)
  M <- sapply(names(groups), function(g) {
    v <- intersect(groups[[g]], names(te)); if (!length(v)) return(rep(0, nrow(te)))
    t2 <- te; for (cc in v) t2[[cc]] <- mean(te[[cc]], na.rm = TRUE); full - pred_fun(t2) })
  M <- matrix(M, nrow = nrow(te), dimnames = list(NULL, names(groups)))
  txt <- apply(M, 1, function(r) { r <- r[abs(r) >= min_abs]; if (!length(r)) return("No feature group moves this projection by 0.05+ from an average team.")
    r <- r[order(-abs(r))][seq_len(min(top, length(r)))]; paste(sprintf("%+.2f  %s", r, names(r)), collapse = "\n") })
  list(text = txt, matrix = M)
}
# D/ST feature groups: model family × side (mirrors family_of() in 40_dst_model.R)
dst_groups <- function(vars) {
  b <- sub("^(o_|d_|t_)", "", vars)
  fam <- dplyr::case_when(
    vars %in% c("spread", "total_line", "implied_opp", "implied_own", "home") ~ "vegas",
    vars %in% c("indoor", "grass", "temp", "wind", "wind_hi", "cold", "rest_diff", "div_game", "week") ~ "Venue, weather & rest",
    startsWith(vars, "qb_") ~ "Opp. QB career rates",
    b %in% c("qb_cont", "pc_cont", "n_hist") ~ "Opp. QB / play-caller continuity",
    vars %in% c("press_x", "sack_x") ~ "Pass-rush vs pass-pro matchup",
    vars == "to_x" ~ "Turnover matchup",
    b %in% c("epa_play", "epa_pass", "epa_rush", "succ", "succ_pass", "succ_rush", "ypp", "third_conv", "td_drive", "pts_g", "yds_g") ~ "efficiency",
    b %in% c("expl", "expl_pass", "expl_rush") ~ "explosive plays",
    b %in% c("sack_rate", "hit_rate", "sacked_g") ~ "sacks & QB hits",
    b %in% c("press_rate", "hurry_rate", "blitz_rate", "bad_throw") ~ "pressure & blitz",
    b %in% c("int_rate", "fum_rate", "fuml_rate", "to_rate", "to_given_g") ~ "turnovers",
    b %in% c("neg_rate", "runloss_rate") ~ "negative plays",
    b %in% c("pass_rate", "pass_oe", "plays_g", "punt_drive") ~ "play volume & pass rate",
    b %in% c("sec_play", "sec_play_neu") ~ "pace (sec / play)",
    b %in% c("sr_succ", "sr_fail") ~ "success / failure rate (PFR)",
    b %in% c("pen_play", "penyds_g") ~ "penalties",
    startsWith(b, "dst_") | b %in% c("fp_g", "fpc_g") ~ "D/ST points history",
    b %in% c("ngs_press", "ngs_ttp", "ngs_p2s", "ngs_blitz", "ngs_getoff") ~ "pass rush (NGS)",
    b %in% c("ngs_ttt", "ngs_sep", "ngs_yacoe", "ngs_pa") ~ "passing (NGS)",
    b %in% c("ngs_ryoe", "ngs_ybco", "ngs_stuff", "ngs_light", "ngs_stacked") ~ "run game (NGS)",
    startsWith(b, "ftn_") ~ "charting (FTN)",
    TRUE ~ "other")
  side <- ifelse(grepl("^d_", vars) & !fam %in% c("vegas") & !grepl("^(Venue|Opp\\.|Pass-rush|Turnover)", fam), "This D: ",
                 ifelse(grepl("^o_", vars) & !grepl("^(Venue|Opp\\.|Pass-rush|Turnover)", fam), "Opp. offense: ",
                        ifelse(grepl("^t_", vars), "Own offense: ", "")))
  lab <- paste0(side, fam)
  keep <- fam != "vegas"
  split(vars[keep], lab[keep])
}
# kicker feature groups (Vegas and the league-level trend terms excluded: the latter are equal for every team)
k_groups <- function(vars) {
  lab <- dplyr::case_when(
    vars %in% c("indoor", "wind_o", "wind_hi", "temp", "cold", "altitude", "grass", "wind_x_long") ~ "Weather & venue",
    vars %in% c("k_fg_pct", "k_fg50_pct", "k_xp_pct") ~ "Kicker career accuracy",
    vars %in% c("k_fgoe", "k_fgoe2", "k_fgoe2r", "k_xpoe", "k_log_fga", "k_new", "k_drafted") ~ "Kicker recent skill & experience",
    vars %in% c("k_long_share", "k_avg_dist") ~ "Kicker range (long attempts)",
    startsWith(vars, "c_") ~ "Coach 4th-down tendency",
    grepl("sec_play", vars) ~ "Game pace (sec / play)",
    startsWith(vars, "o_") ~ "Own offense (scoring / drives)",
    startsWith(vars, "d_") ~ "Opp. defense (points / drives allowed)",
    TRUE ~ NA_character_)
  keep <- !is.na(lab)
  split(vars[keep], lab[keep])
}

## ---- HTML bits shared by the reports ----
tip_span <- function(text, tip) ifelse(is.na(tip) | !nzchar(tip), text,
  sprintf('<span class="tt" tabindex="0">%s<span class="tip">%s</span></span>', text, gsub("\n", "<br>", tip)))
SITE_CSS <- '
.tt{position:relative;cursor:help;border-bottom:1px dotted var(--muted,#888)}
.tt .tip{display:none;position:absolute;left:0;top:1.4em;z-index:20;min-width:260px;max-width:340px;white-space:normal;text-align:left;
  background:var(--bg,#fff);color:var(--fg,#222);border:1px solid var(--line,#ccc);border-radius:6px;padding:6px 8px;font-size:12.5px;
  box-shadow:0 2px 10px rgba(0,0,0,.18);font-weight:400;line-height:1.35}
.tt:hover .tip,.tt:focus .tip{display:block}
tr.tb-clear td{border-top:3px solid var(--fg,#222)}tr.tb-soft td{border-top:2px dashed var(--muted,#888)}
tr.tier-odd td:not(.top):not(.bot):not(.t1):not(.t2):not(.t5):not(.t6):not(.wx1):not(.wx2){background:rgba(127,127,127,.08)}
svg.spark{vertical-align:middle;overflow:visible}
:root{--t1:#9fd8b0;--t2:#e0f3e6;--t5:#fbe3e3;--t6:#f2b6b6;--wx1:#fbd5d5;--wx2:#f19a9a}
@media (prefers-color-scheme: dark){:root{--t1:#1f6b3a;--t2:#173a24;--t5:#3d1c1c;--t6:#6e2727;--wx1:#5a2a2a;--wx2:#8f2f2f}}
td.t1{background:var(--t1);font-weight:700}td.t2{background:var(--t2);font-weight:600}td.t5{background:var(--t5)}td.t6{background:var(--t6)}
span.wx1,span.wx2{border-radius:3px;padding:0 3px}span.wx1{background:var(--wx1)}span.wx2{background:var(--wx2);font-weight:600}
td.wx1{background:var(--wx1)}td.wx2{background:var(--wx2);font-weight:600}
td.stk,th.stk{position:sticky;z-index:2}td.stk{background:var(--bg,#fff)}th.stk{z-index:4}
td.stk:hover,td.stk:focus-within{z-index:6}
tr.tier-odd td.stk.stk:not(.top):not(.bot):not(.t1):not(.t2):not(.t5):not(.t6){background:linear-gradient(rgba(127,127,127,.08),rgba(127,127,127,.08)),var(--bg,#fff)}
td.stk.t1{background:var(--t1)}td.stk.t2{background:var(--t2)}td.stk.t5{background:var(--t5)}td.stk.t6{background:var(--t6)}
td.stk-last,th.stk-last{box-shadow:2px 0 3px -1px rgba(0,0,0,.25)}
td.stk:hover,td.stk:focus-within,td:hover,td:focus-within{z-index:30}td:hover,td:focus-within{position:relative}td.stk:hover,td.stk:focus-within{position:sticky}
.tt .tip{position:fixed}'
# Sticky first columns: tables with data-stick="n" keep their first n columns in view when scrolled sideways.
# Offsets are measured in the browser (column widths vary), and again when a tab is shown or the window resized.
SITE_JS <- 'function stickCols(){document.querySelectorAll("table[data-stick]").forEach(t=>{if(!t.offsetParent)return;
const n=+t.dataset.stick,hr=t.tHead.rows[0];let left=0;for(let c=0;c<n&&c<hr.cells.length;c++){const w=hr.cells[c].getBoundingClientRect().width;
[...t.rows].forEach(r=>{const x=r.cells[c];if(!x)return;x.classList.add("stk");x.classList.toggle("stk-last",c==n-1);x.style.left=left+"px"});left+=w}})}
window.addEventListener("load",stickCols);window.addEventListener("resize",stickCols);
function placeTip(t){const tip=t.querySelector(".tip");if(!tip)return;requestAnimationFrame(()=>{const r=t.getBoundingClientRect(),w=tip.offsetWidth||300,h=tip.offsetHeight||120;
let x=Math.max(8,Math.min(r.left,window.innerWidth-w-8)),y=r.bottom+4;if(y+h>window.innerHeight-8)y=Math.max(8,r.top-h-4);tip.style.left=x+"px";tip.style.top=y+"px"})}
document.addEventListener("mouseover",e=>{const t=e.target.closest&&e.target.closest(".tt");if(t)placeTip(t)});
document.addEventListener("focusin",e=>{const t=e.target.closest&&e.target.closest(".tt");if(t)placeTip(t)});'

## ---- Track record tabs (62_track_record.R → output/track/track_<season>.rds; rendered by 44 and 54) ----
track_file <- function(proj_dir, season) file.path(Sys.getenv("TRACK_DIR", file.path(proj_dir, "output/track")), sprintf("track_%d.rds", season))
.tr_esc <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); gsub(">", "&gt;", x) }
.f  <- function(x, d = 2) ifelse(is.na(x), "—", formatC(x, format = "f", digits = d))
.sg <- function(x, d = 2) ifelse(is.na(x), "—", sprintf(paste0("%+.", d, "f"), x))
.pc <- function(x) ifelse(is.na(x), "—", paste0(round(100 * x), "%"))
.cls <- function(x) ifelse(is.na(x) | abs(x) < 0.005, "", ifelse(x > 0, "gain", "loss"))
# plain table: cols = list of (header, values, tooltip, class vector or NULL); first `left` columns left-aligned
tr_table <- function(cols, left = 1, foot = NULL) {
  th <- vapply(cols, function(c) sprintf('<th title="%s">%s</th>', .tr_esc(c$tip %||% ""), .tr_esc(c$h)), "")
  n <- length(cols[[1]]$v)
  rows <- vapply(seq_len(n), function(i) paste0(if (!is.null(foot) && foot[i]) '<tr class="tr-season">' else "<tr>",
    paste0(vapply(seq_along(cols), function(k) { cl <- c(if (k <= left) "ltxt", if (!is.null(cols[[k]]$cls)) cols[[k]]$cls[i])
      cl <- cl[nzchar(cl)]; sprintf("<td%s>%s</td>", if (length(cl)) sprintf(' class="%s"', paste(cl, collapse = " ")) else "", .tr_esc(cols[[k]]$v[i])) }, ""), collapse = ""), "</tr>"), "")
  sprintf('<div class="tw"><table class="track"><thead><tr>%s</tr></thead><tbody>%s</tbody></table></div>', paste(th, collapse = ""), paste(rows, collapse = ""))
}
`%||%` <- function(a, b) if (is.null(a)) b else a
# cumulative "points gained per starting slot" line chart: one line per comparison (palette slots 1-3; direct labels + legend)
TRACK_SERIES <- c(vegas = "vs Vegas-only", sleeper = "vs Sleeper", espn = "vs ESPN")
track_chart <- function(d) {                     # d: week, key (vegas / sleeper / espn), cum
  d <- d[!is.na(d$cum), ]; if (!nrow(d)) return("")
  W <- 620; H <- 210; L <- 48; R <- 170; T <- 14; B <- 30
  wks <- sort(unique(d$week)); xr <- if (length(wks) > 1) range(wks) else wks + c(-0.5, 0.5)
  yr <- range(c(0, d$cum)); pad <- max(0.5, diff(yr) * 0.12); yr <- yr + c(-pad, pad)
  X <- function(w) L + (w - xr[1]) / diff(xr) * (W - L - R); Y <- function(v) T + (yr[2] - v) / diff(yr) * (H - T - B)
  ticks <- pretty(yr, 4); ticks <- ticks[ticks >= yr[1] & ticks <= yr[2]]
  grid <- paste0(sprintf('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" class="tc-grid%s"/><text x="%d" y="%.1f" class="tc-ax" text-anchor="end">%s</text>',
                         L, W - R, Y(ticks), Y(ticks), ifelse(ticks == 0, " tc-zero", ""), L - 6, Y(ticks) + 4, sprintf("%+g", ticks)), collapse = "")
  xl <- paste0(sprintf('<text x="%.1f" y="%d" class="tc-ax" text-anchor="middle">Wk %d</text>', X(wks), H - 10, wks), collapse = "")
  keys <- intersect(names(TRACK_SERIES), unique(d$key)); slot <- setNames(paste0("tc-s", seq_along(names(TRACK_SERIES))), names(TRACK_SERIES))
  # end labels: nudge apart so they never overlap (>= 14 px between them)
  ends <- do.call(rbind, lapply(keys, function(k) { e <- d[d$key == k, ]; e <- e[order(e$week), ]; data.frame(k = k, y = Y(e$cum[nrow(e)])) }))
  ends <- ends[order(ends$y), ]; if (nrow(ends) > 1) for (i in 2:nrow(ends)) ends$y[i] <- max(ends$y[i], ends$y[i - 1] + 14)
  lab_y <- setNames(ends$y, ends$k)
  lines <- paste0(vapply(keys, function(k) { e <- d[d$key == k, ]; e <- e[order(e$week), ]
    pl <- if (nrow(e) > 1) sprintf('<polyline class="tc-line %s" points="%s"/>', slot[k], paste(sprintf("%.1f,%.1f", X(e$week), Y(e$cum)), collapse = " ")) else ""
    dots <- paste0(sprintf('<circle class="tc-dot %s" cx="%.1f" cy="%.1f" r="4"><title>Through week %d: %+.2f pts per starting slot %s (season total)</title></circle>',
                           slot[k], X(e$week), Y(e$cum), e$week, e$cum, TRACK_SERIES[k]), collapse = "")
    lab <- sprintf('<text x="%.1f" y="%.1f" class="tc-lab">%s %+.1f</text>', X(max(e$week)) + 8, lab_y[k] + 4, TRACK_SERIES[k], e$cum[nrow(e)])
    paste0(pl, dots, lab) }, ""), collapse = "")
  legend <- paste0(vapply(keys, function(k) sprintf('<span class="tc-key"><i class="%s"></i>%s</span>', slot[k], TRACK_SERIES[k]), ""), collapse = "")
  sprintf('<div class="tc"><div class="tc-legend">%s</div><svg viewBox="0 0 %d %d" width="%d" height="%d" role="img" aria-label="Cumulative points gained per starting slot">%s%s%s</svg></div>',
          legend, W, H, W, H, grid, xl, lines)
}
# HTML for the two tabs of one position ("DEF" / "K"); systems = named labels in page order; ext_system = format the
# Sleeper / ESPN comparison uses (ESPN standard)
track_tabs <- function(TR, pos, systems, ext_system = "espn", unit = "D/ST") {
  wk <- TR$weekly[TR$weekly$pos == pos, ]; tot <- TR$total[TR$total$pos == pos, ]
  if (!nrow(wk)) return(NULL)
  wlab <- function(w) if (length(w) > 1) sprintf("weeks %d–%d", min(w), max(w)) else sprintf("week %d", w)
  bf <- sort(unique(wk$week[wk$kind == "backfilled"]))
  bf_note <- if (length(bf)) sprintf(" %s %s backfilled: the current model re-run as of that week, trained only on earlier games, with the closing lines (the site started in week %d).",
                                     tools::toTitleCase(wlab(bf)), if (length(bf) > 1) "are" else "is", max(bf) + 1) else ""
  ## Track record (vs Vegas-only), one section per scoring format
  sec <- vapply(names(systems), function(sy) {
    w <- wk[wk$system == sy, ]; t <- tot[tot$system == sy, ]; if (!nrow(w)) return("")
    w <- w[order(w$week), ]
    rows <- dplyr::bind_rows(w, dplyr::mutate(t, week = NA_integer_, kind = "season"))
    lab <- c(ifelse(is.na(rows$week[-nrow(rows)]), "", paste("Week", rows$week[-nrow(rows)])), "Season")
    kind_lab <- ifelse(rows$kind == "season", "", rows$kind)
    d12 <- rows$top12_ours - rows$top12_vegas
    cols <- list(list(h = "Week", v = lab), list(h = "Type", v = kind_lab, tip = "live = the Tuesday projection the site showed; backfilled = re-run afterwards as of that week"),
                 list(h = "Our top 12", v = .f(rows$top12_ours), tip = "average actual points of our weekly top 12 (Tuesday projection)"),
                 list(h = "Vegas-only top 12", v = .f(rows$top12_vegas), tip = "average actual points of the top 12 by the Vegas-only baseline (same lines)"),
                 list(h = "Δ per start", v = .sg(d12), cls = .cls(d12), tip = "points per starting slot gained by using our top 12 instead of Vegas-only's"),
                 list(h = "Start/sit calls", v = ifelse(is.na(rows$ss_vegas_calls), "—", rows$ss_vegas_calls), tip = "pairs of teams we and Vegas-only ranked in opposite order"),
                 list(h = "Ours right", v = .pc(rows$ss_vegas_right), tip = "share of those calls where our pick scored more"),
                 list(h = "Pts per call", v = .sg(rows$ss_vegas_gain), cls = .cls(rows$ss_vegas_gain), tip = "average points gained per call by following ours"),
                 list(h = "RMSE ours / Vegas", v = paste(.f(rows$rmse_ours), "/", .f(rows$rmse_vegas)), tip = "projection error (lower = better)"),
                 list(h = "In our 80% range", v = .pc(rows$cov80), tip = "share of actual scores inside our 80% range (should be about 80%)"),
                 list(h = "P(boom): said / happened", v = paste(.pc(rows$p_boom), "/", .pc(rows$boom)), tip = "our average chance of a boom week vs how often it happened"),
                 list(h = "P(bust): said / happened", v = paste(.pc(rows$p_bust), "/", .pc(rows$bust)), tip = "our average chance of a bust week vs how often it happened"))
    if ("top12_ours_pk" %in% names(rows) && any(!is.na(rows$top12_ours_pk)))
      cols <- append(cols, list(list(h = "Our top 12 at kickoff", v = .f(rows$top12_ours_pk), tip = "same, with the site's last projection before each kickoff (live weeks only)")), after = 2)
    head <- sprintf("<p class='s'><b>%s, %s:</b> our top 12 averaged <b>%s</b> points vs <b>%s</b> for Vegas-only (<b>%s per start</b>). In %s start/sit calls where we disagreed with Vegas-only, our pick scored more %s of the time (%s pts per call).</p>",
                    systems[sy], wlab(sort(w$week)), .f(t$top12_ours), .f(t$top12_vegas), .sg(t$top12_ours - t$top12_vegas),
                    format(t$ss_vegas_calls, big.mark = ","), .pc(t$ss_vegas_right), .sg(t$ss_vegas_gain))
    paste0(sprintf("<h3>%s</h3>", systems[sy]), head, tr_table(cols, left = 2, foot = rows$kind == "season"))
  }, "")
  th <- TR$thresholds[[pos]]
  track <- paste0("<p class='s'>How our projections did once the games were played, week by week and season to date, against the Vegas-only baseline (a regression on the betting lines alone, refit each week on earlier games and scored on the same lines). ",
                  sprintf("Boom = %s, bust = under %s points.", if (pos == "DEF") paste0(th["boom"], "+") else paste("over", th["boom"]), th["bust"]),
                  bf_note, " One season is a small sample: the long-run edge is in the Back-test tab.</p>", paste(sec, collapse = ""))
  ## vs Sleeper / ESPN (ranks, ESPN standard)
  w <- wk[wk$system == ext_system, ]; t <- tot[tot$system == ext_system, ]
  srcs <- c(sleeper = "Sleeper", espn = "ESPN")[c("top12_sleeper", "top12_espn") %in% names(w)]
  ext <- if (!length(srcs) || !nrow(w)) "<p class='s'>No Sleeper / ESPN rankings stored yet: the daily refresh saves them before each kickoff from this week on.</p>" else {
    w <- w[order(w$week), ]; rows <- dplyr::bind_rows(w, dplyr::mutate(t, week = NA_integer_, kind = "season"))
    lab <- c(paste("Week", rows$week[-nrow(rows)]), "Season")
    how <- TR$ext_how[TR$ext_how$pos == pos, ]
    how_lab <- vapply(rows$week, function(x) if (is.na(x)) "" else { h <- how[how$week == x, ]; if (!nrow(h)) "—" else
      if (all(h$how == "pre-kickoff snapshot")) "before kickoff" else if (all(h$how != "pre-kickoff snapshot")) "pulled after" else "mixed" }, "")
    cols <- list(list(h = "Week", v = lab), list(h = "Their ranks", v = how_lab, tip = "before kickoff = the last snapshot before each game; pulled after = their final pre-game projections downloaded after the week (backfilled weeks)"),
                 list(h = "Our top 12", v = .f(rows$top12_ours)))
    for (s in names(srcs)) {
      d <- rows[[paste0("top12_ours_vs_", s)]] - rows[[paste0("top12_", s)]]
      cols <- c(cols, list(list(h = paste(srcs[s], "top 12"), v = .f(rows[[paste0("top12_", s)]]), tip = paste("average actual points of", srcs[s], "'s top 12")),
                           list(h = paste("Δ per start vs", srcs[s]), v = .sg(d), cls = .cls(d), tip = paste("points per starting slot gained by using our top 12 instead of", srcs[s], "'s (same teams)")),
                           list(h = "Calls", v = ifelse(is.na(rows[[paste0("ss_", s, "_calls")]]), "—", rows[[paste0("ss_", s, "_calls")]]), tip = paste("pairs of teams we and", srcs[s], "ranked in opposite order")),
                           list(h = "Ours right", v = .pc(rows[[paste0("ss_", s, "_right")]])),
                           list(h = "Pts per call", v = .sg(rows[[paste0("ss_", s, "_gain")]]), cls = .cls(rows[[paste0("ss_", s, "_gain")]]))))
    }
    cols <- c(cols, list(list(h = "Vegas-only top 12", v = .f(rows$top12_vegas))))
    heads <- vapply(names(srcs), function(s) sprintf("Using our rankings instead of <b>%s</b>'s gained <b>%s points per starting slot per week</b>; when the two disagreed on which of two %s to start (%s calls), our pick scored more <b>%s</b> of the time (%s pts per call).",
                                                     srcs[s], .sg(t[[paste0("top12_ours_vs_", s)]] - t[[paste0("top12_", s)]]), if (pos == "DEF") "D/STs" else "kickers",
                                                     format(t[[paste0("ss_", s, "_calls")]], big.mark = ","), .pc(t[[paste0("ss_", s, "_right")]]), .sg(t[[paste0("ss_", s, "_gain")]])), "")
    cd <- dplyr::bind_rows(lapply(c(vegas = "vegas", names(srcs)), function(s) {
      o <- if (s == "vegas") w$top12_ours else w[[paste0("top12_ours_vs_", s)]]
      g <- o - w[[paste0("top12_", s)]]; k <- !is.na(g)                    # a source's line starts at its first week with data
      tibble::tibble(week = w$week[k], key = rep(s, sum(k)), cum = cumsum(g[k])) }))
    paste0(sprintf("<p class='s'>Most players set their lineups from Sleeper's or ESPN's rankings. This compares <b>rankings</b> (who to start), in ESPN standard scoring, %s: their projections re-scored into ESPN standard (Sleeper's %s from its projected stats), ranked, and only the rank kept.%s</p>",
                   wlab(sort(w$week)), if (pos == "DEF") "D/STs" else "kickers", bf_note),
           "<p class='s'>", paste(heads, collapse = "<br>"), "</p>",
           "<h3>Points gained per starting slot, season total</h3>", track_chart(cd),
           tr_table(cols, left = 2, foot = rows$kind == "season"))
  }
  list(track = track, ext = ext)
}
TRACK_CSS <- '
.track td.ltxt{text-align:left}.track tr.tr-season td{font-weight:700;border-top:2px solid var(--fg,#222)}
.track td.gain{background:var(--t2,#e0f3e6)}.track td.loss{background:var(--t5,#fbe3e3)}
.tc{margin:.4rem 0 1rem;max-width:100%;overflow-x:auto}.tc-legend{display:flex;gap:14px;font-size:12.5px;margin:0 0 4px;color:var(--fg,#222)}
.tc-key i{display:inline-block;width:14px;height:3px;border-radius:2px;vertical-align:middle;margin-right:5px}
:root{--tc1:#2a78d6;--tc2:#eb6834;--tc3:#1baf7a}
@media (prefers-color-scheme: dark){:root{--tc1:#3987e5;--tc2:#d95926;--tc3:#199e70}}
.tc-s1{stroke:var(--tc1);fill:var(--tc1);background:var(--tc1)}.tc-s2{stroke:var(--tc2);fill:var(--tc2);background:var(--tc2)}.tc-s3{stroke:var(--tc3);fill:var(--tc3);background:var(--tc3)}
.tc-line{fill:none;stroke-width:2;stroke-linejoin:round}.tc-dot{stroke:var(--bg,#fff);stroke-width:2}
.tc-grid{stroke:var(--line,#ddd);stroke-width:1}.tc-zero{stroke:var(--muted,#888)}.tc-ax{font-size:11px;fill:var(--muted,#666)}
.tc-lab{font-size:12px;fill:var(--fg,#222)}'
SITE_CSS <- paste0(SITE_CSS, TRACK_CSS)
