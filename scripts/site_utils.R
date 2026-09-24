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
WX_COLS  <- c("pulled_at", "game_id", "kickoff", "temp", "wind", "gust", "precip_prob", "precip_in")

# this week's games from the nflverse schedule (roof, stadium, kickoff in UTC)
week_games <- function(season, week) {
  tmp <- tempfile(fileext = ".rds")
  ok <- tryCatch({ utils::download.file("https://github.com/nflverse/nflverse-data/releases/download/schedules/games.rds", tmp, mode = "wb", quiet = TRUE); TRUE },
                 error = function(e) FALSE)
  if (!ok) return(NULL)
  g <- readRDS(tmp)
  g <- g[g$season == season & g$week == week & g$game_type == "REG", ]
  tibble::tibble(game_id = g$game_id, stadium_id = g$stadium_id, roof = ifelse(is.na(g$roof), "", g$roof),
                 kickoff = as.POSIXct(format(as.POSIXct(paste(g$gameday, g$gametime), tz = "America/New_York"), tz = "UTC"), tz = "UTC"))
}

wx_fetch1 <- function(lat, lon, ko) {          # mean over kickoff hour + 2 h; max gust / rain chance; total rain
  d <- format(ko, "%Y-%m-%d", tz = "America/New_York"); hr <- as.integer(format(ko, "%H", tz = "America/New_York"))
  url <- sprintf(paste0("https://api.open-meteo.com/v1/forecast?latitude=%.4f&longitude=%.4f",
                        "&hourly=temperature_2m,wind_speed_10m,wind_gusts_10m,precipitation_probability,precipitation",
                        "&temperature_unit=fahrenheit&wind_speed_unit=mph&precipitation_unit=inch&timezone=America%%2FNew_York&start_date=%s&end_date=%s"),
                 lat, lon, d, d)
  js <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
  if (is.null(js)) return(NULL)
  h <- js$hourly; idx <- which(as.integer(substr(h$time, 12, 13)) %in% hr:(hr + 2))
  if (!length(idx)) return(NULL)
  mx <- function(v) if (is.null(v) || all(is.na(v[idx]))) NA_real_ else max(v[idx], na.rm = TRUE)
  tibble::tibble(temp = round(mean(h$temperature_2m[idx])), wind = round(mean(h$wind_speed_10m[idx])), gust = round(mx(h$wind_gusts_10m)),
                 precip_prob = round(mx(h$precipitation_probability)), precip_in = round(sum(h$precipitation[idx], na.rm = TRUE), 2))
}

read_wx_hist <- function(file) {
  if (!file.exists(file)) return(tibble::tibble(pulled_at = character(), game_id = character(), kickoff = character(), temp = numeric(),
                                                wind = numeric(), gust = numeric(), precip_prob = numeric(), precip_in = numeric()))
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
  if (!nrow(hist)) return(tibble::tibble(game_id = character(), temp = numeric(), wind = numeric(), gust = numeric(), precip_prob = numeric(), precip_in = numeric(), wx_time = as.POSIXct(character())))
  h <- dplyr::mutate(hist, t = utc_time(pulled_at), ko = utc_time(kickoff))
  h <- h[!is.na(h$t) & h$t < h$ko, ]
  h <- dplyr::ungroup(dplyr::slice_max(dplyr::group_by(h, game_id), t, n = 1, with_ties = FALSE))
  dplyr::transmute(h, game_id, temp, wind, gust, precip_prob, precip_in, wx_time = t)
}
# compact label: "64° · 8 mph (g 17) · 20% rain, 0.05 in"
rain_in <- function(x) ifelse(is.na(x), "", ifelse(x > 0 & x < 0.01, "<0.01 in", sprintf("%.2f in", x)))
wx_label <- function(indoor, temp, wind, gust, precip_prob, precip_in = NA) {
  ifelse(indoor %in% 1, "indoor", ifelse(is.na(wind), "—",
    paste0(ifelse(is.na(temp), "", paste0(round(temp), "° · ")), round(wind), " mph",
           ifelse(is.na(gust), "", paste0(" (g ", round(gust), ")")),
           ifelse(is.na(precip_prob), "", paste0(" · ", round(precip_prob), "% rain")),
           ifelse(is.na(precip_in), "", paste0(ifelse(is.na(precip_prob), " · ", ", "), rain_in(precip_in))))))
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

## ---- Projection history + dotted trend sparkline ----
PH_COLS <- c("model", "season", "week", "system", "team", "time", "kind", "proj")
read_ph <- function(file) {
  if (!file.exists(file)) return(tibble::tibble(model = character(), season = integer(), week = integer(), system = character(), team = character(),
                                                time = character(), kind = character(), proj = numeric()))
  tibble::as_tibble(utils::read.csv(file, stringsAsFactors = FALSE, colClasses = c(model = "character", system = "character", team = "character", time = "character", kind = "character")))
}
# base = weekly-run projections (added once per week at the model's fit time), cur = this refresh
ph_update <- function(file, model, season, week, fit_time, base, cur, now) {
  ph <- read_ph(file)
  has_base <- any(ph$model == model & ph$season == season & ph$week == week & ph$kind == "weekly")
  add <- dplyr::bind_rows(if (!has_base) dplyr::mutate(base, model = model, season = season, week = week, time = iso_utc(fit_time), kind = "weekly"),
                          dplyr::mutate(cur, model = model, season = season, week = week, time = iso_utc(now), kind = "refresh"))
  ph <- dplyr::bind_rows(ph, add[PH_COLS]); dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(ph, file, row.names = FALSE)
  ph[ph$model == model & ph$season == season & ph$week == week, ]
}
# one point for the weekly run + one per (Eastern) day the page was refreshed (that day's last value)
trend_points <- function(ph_team) {
  ph_team$t <- utc_time(ph_team$time); ph_team <- ph_team[order(ph_team$t), ]
  wk <- ph_team[ph_team$kind == "weekly", ][1, ]
  rf <- ph_team[ph_team$kind == "refresh", ]
  rf$day <- format(rf$t, "%Y-%m-%d", tz = "America/New_York")
  rf <- dplyr::ungroup(dplyr::slice_max(dplyr::group_by(rf, day), t, n = 1, with_ties = FALSE))
  dplyr::bind_rows(if (nrow(wk) && !is.na(wk$proj)) dplyr::mutate(wk, lab = paste0("weekly run ", format(wk$t, "%a %b %d", tz = "America/New_York"))),
                   dplyr::mutate(rf, lab = format(t, "%a %b %d", tz = "America/New_York")))
}
sparkline <- function(pts, w = 96, h = 24, min_span = 1) {
  if (is.null(pts) || nrow(pts) < 1) return("")
  v <- pts$proj; n <- length(v); lo <- min(v); hi <- max(v)
  if (hi - lo < min_span) { mid <- (hi + lo) / 2; lo <- mid - min_span / 2; hi <- mid + min_span / 2 }
  xs <- if (n == 1) w / 2 else 5 + (seq_len(n) - 1) * (w - 10) / (n - 1)
  ys <- h - 4 - (v - lo) / (hi - lo) * (h - 8)
  d <- v[n] - v[1]; col <- if (abs(d) < 0.05) "#888" else if (d > 0) "#1a9850" else "#d73027"
  line <- if (n > 1) sprintf('<polyline points="%s" fill="none" stroke="%s" stroke-width="1.5" stroke-dasharray="3,2"/>',
                             paste(sprintf("%.1f,%.1f", xs, ys), collapse = " "), col) else ""
  dots <- paste(sprintf('<circle cx="%.1f" cy="%.1f" r="%s" fill="%s"%s><title>%s: %.2f</title></circle>', xs, ys,
                        ifelse(pts$kind == "weekly", "2.6", "2.2"), ifelse(pts$kind == "weekly", "var(--bg)", col),
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
  b <- sub("^(o_|d_)", "", vars)
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
    b %in% c("pass_rate", "pass_oe", "plays_g", "punt_drive") ~ "pace & pass rate",
    b %in% c("pen_play", "penyds_g") ~ "penalties",
    startsWith(b, "dst_") | b %in% c("fp_g", "fpc_g") ~ "D/ST points history",
    b %in% c("ngs_press", "ngs_ttp", "ngs_p2s", "ngs_blitz", "ngs_getoff") ~ "pass rush (NGS)",
    b %in% c("ngs_ttt", "ngs_sep", "ngs_yacoe", "ngs_pa") ~ "passing (NGS)",
    b %in% c("ngs_ryoe", "ngs_ybco", "ngs_stuff", "ngs_light", "ngs_stacked") ~ "run game (NGS)",
    startsWith(b, "ftn_") ~ "charting (FTN)",
    TRUE ~ "other")
  side <- ifelse(grepl("^d_", vars) & !fam %in% c("vegas") & !grepl("^(Venue|Opp\\.|Pass-rush|Turnover)", fam), "This D: ",
                 ifelse(grepl("^o_", vars) & !grepl("^(Venue|Opp\\.|Pass-rush|Turnover)", fam), "Opp. offense: ", ""))
  lab <- paste0(side, fam)
  keep <- fam != "vegas"
  split(vars[keep], lab[keep])
}
# kicker feature groups (Vegas and the league-level trend terms excluded: the latter are equal for every team)
k_groups <- function(vars) {
  lab <- dplyr::case_when(
    vars %in% c("indoor", "wind_o", "wind_hi", "temp", "cold", "altitude", "grass", "wind_x_long") ~ "Weather & venue",
    vars %in% c("k_fg_pct", "k_fg50_pct", "k_xp_pct") ~ "Kicker career accuracy",
    vars %in% c("k_fgoe", "k_xpoe", "k_log_fga", "k_new", "k_drafted") ~ "Kicker recent skill & experience",
    vars %in% c("k_long_share", "k_avg_dist") ~ "Kicker range (long attempts)",
    startsWith(vars, "c_") ~ "Coach 4th-down tendency",
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
tr.tier-odd td:not(.top):not(.bot){background:rgba(127,127,127,.08)}
svg.spark{vertical-align:middle;overflow:visible}'
