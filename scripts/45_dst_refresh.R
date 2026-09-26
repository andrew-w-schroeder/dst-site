# ==============================================================================
# 45_dst_refresh.R — daily re-score of this week's D/ST projections with the latest Vegas lines
#
# Runs on GitHub Actions (see .github/workflows/dst_refresh.yml in the dst-site repo) or locally.
# No data downloads and no refitting: it reads the weekly scoring bundles written by 40_dst_model.R
# (output/dst/<system>/bundle_<season>_wk<ww>.rds), which hold the fitted blend as coefficients.
#
#   1. Pull current NFL spreads + totals from The Odds API (env var ODDS_API_KEY; 2 credits per run).
#      Consensus line = median across US sportsbooks. Every pull is appended to data/lines/line_history.csv.
#   2. For each game this week use the newest line pulled BEFORE kickoff. Games that have kicked off are
#      locked at their last pre-kickoff line; games the books have taken off the board keep their last line;
#      games never priced keep the line from the weekly run (nflverse schedule).
#   3. Re-score every scoring system: projection, components, 80%/50% ranges, P(10+), P(<3), P(top 8),
#      bootstrap 90% CI. Also keeps the weekly-run values as the "since Tuesday" baseline.
#   4. Write re-scored report parts to work/ (the published weekly parts are never modified), run
#      44_dst_report.R on them and copy the HTML to site/index.html (+ weekly archive).
#   Starting QBs (starters.R): each refresh re-checks every offense's starter (injury report, Sleeper + Ourlads depth
#   charts, nflverse schedule, data/lines/qb_override.csv) and re-scores a changed starter exactly with the bundle's
#   QB scenarios (40_dst_model.R 9d); games that have kicked off keep their last pre-kickoff starter.
#   Choices are logged to data/lines/starter_history.csv.
#   Also (site_utils.R): Open-Meteo forecasts incl. gusts / rain for this week's outdoor games (display only for
#   D/ST; logged to data/lines/weather_history.csv and reused by the kicker refresh), the projection history
#   behind the trend sparklines (data/lines/proj_history.csv) and the per-team "drivers" hover text.
#
# Usage:  Rscript scripts/45_dst_refresh.R            (newest bundle week)
#         ODDS_JSON_FILE=odds.json Rscript …          (test with a saved API response, no credits used)
#         ODDS_API_KEY unset and no file → re-score with stored lines only
# ==============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(purrr); library(tibble) })
PROJ_DIR <- Sys.getenv("FF_PROJ_DIR", getwd())
DST_DIR  <- file.path(PROJ_DIR, "output/dst")
LINE_CSV <- file.path(PROJ_DIR, "data/lines/line_history.csv")
SITE_DIR <- file.path(PROJ_DIR, "site")
WORK_DIR <- file.path(PROJ_DIR, "work")               # re-scored report parts + report (scratch, .gitignored)
SYSTEMS  <- c("espn", "yahoo", "ffpc")
NOW      <- as.POSIXct(Sys.getenv("REFRESH_NOW", format(Sys.time(), tz = "UTC")), tz = "UTC")   # REFRESH_NOW for testing
source(file.path(PROJ_DIR, "scripts/dst_blend_utils.R"))
source(file.path(PROJ_DIR, "scripts/site_utils.R"))
source(file.path(PROJ_DIR, "scripts/starters.R"))
QBO_CSV <- file.path(PROJ_DIR, "data/lines/qb_override.csv"); ST_CSV <- file.path(PROJ_DIR, "data/lines/starter_history.csv")
WX_CSV <- file.path(PROJ_DIR, "data/lines/weather_history.csv"); PH_CSV <- file.path(PROJ_DIR, "data/lines/proj_history.csv")

## ---- 1. Which week? newest bundle across systems ----
bf <- list.files(file.path(DST_DIR, SYSTEMS), pattern = "^bundle_\\d{4}_wk\\d{2}\\.rds$", full.names = TRUE)
if (!length(bf)) stop("no scoring bundles found under ", DST_DIR, ": run the weekly model (43_dst_run_all.R) and publish first")
key <- max(sub(".*bundle_(\\d{4})_wk(\\d{2}).*", "\\1\\2", bf))
SEASON <- as.integer(substr(key, 1, 4)); WEEK <- as.integer(substr(key, 5, 6))
bundles <- set_names(SYSTEMS) %>% map(~ { f <- file.path(DST_DIR, .x, sprintf("bundle_%d_wk%02d.rds", SEASON, WEEK)); if (file.exists(f)) readRDS(f) }) %>% compact()
message(sprintf("refreshing %d week %d: %s", SEASON, WEEK, paste(names(bundles), collapse = ", ")))

## ---- 2. Pull odds ----
TEAM_ABBR <- c(
  "Arizona Cardinals" = "ARI", "Atlanta Falcons" = "ATL", "Baltimore Ravens" = "BAL", "Buffalo Bills" = "BUF",
  "Carolina Panthers" = "CAR", "Chicago Bears" = "CHI", "Cincinnati Bengals" = "CIN", "Cleveland Browns" = "CLE",
  "Dallas Cowboys" = "DAL", "Denver Broncos" = "DEN", "Detroit Lions" = "DET", "Green Bay Packers" = "GB",
  "Houston Texans" = "HOU", "Indianapolis Colts" = "IND", "Jacksonville Jaguars" = "JAX", "Kansas City Chiefs" = "KC",
  "Las Vegas Raiders" = "LV", "Los Angeles Chargers" = "LAC", "Los Angeles Rams" = "LA", "Miami Dolphins" = "MIA",
  "Minnesota Vikings" = "MIN", "New England Patriots" = "NE", "New Orleans Saints" = "NO", "New York Giants" = "NYG",
  "New York Jets" = "NYJ", "Philadelphia Eagles" = "PHI", "Pittsburgh Steelers" = "PIT", "San Francisco 49ers" = "SF",
  "Seattle Seahawks" = "SEA", "Tampa Bay Buccaneers" = "TB", "Tennessee Titans" = "TEN", "Washington Commanders" = "WAS")

fetch_odds <- function() {
  f <- Sys.getenv("ODDS_JSON_FILE")
  key <- gsub("[[:space:]\"']", "", Sys.getenv("ODDS_API_KEY"))     # strip stray spaces / line breaks / quotes from the pasted secret
  if (nzchar(f)) { message("odds: reading ", f); return(jsonlite::fromJSON(f, simplifyVector = FALSE)) }
  if (!nzchar(key)) { message("odds: ODDS_API_KEY not set — using stored lines only"); return(NULL) }
  redact <- function(x) gsub(key, "<key>", x, fixed = TRUE)                  # never print the key
  message(sprintf("odds: key found (%d characters)", nchar(key)))
  url <- paste0("https://api.the-odds-api.com/v4/sports/americanfootball_nfl/odds/?regions=us&markets=spreads,totals",
                "&oddsFormat=american&dateFormat=iso&apiKey=", utils::URLencode(key, reserved = TRUE))
  r <- NULL
  for (k in 1:3) {                                                           # retry transient network errors
    r <- tryCatch(curl::curl_fetch_memory(url), error = function(e) { message("odds: attempt ", k, " error: ", redact(conditionMessage(e))); NULL })
    if (!is.null(r)) break; Sys.sleep(5 * k)
  }
  if (is.null(r)) { warning("odds: no response from The Odds API — using stored lines only"); return(NULL) }
  if (r$status_code != 200) {
    warning(sprintf("odds: HTTP %d from The Odds API: %s — using stored lines only", r$status_code,
                    redact(substr(rawToChar(r$content), 1, 300)))); return(NULL) }
  h <- curl::parse_headers_list(r$headers)
  message(sprintf("odds: OK · credits used this call %s · remaining this month %s", h[["x-requests-last"]], h[["x-requests-remaining"]]))
  jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE)
}
# one row per game: consensus (median across books) home spread in nflverse sign (+ = home favored) and total
parse_odds <- function(ev) map_dfr(ev, function(e) {
  home <- TEAM_ABBR[e$home_team]; away <- TEAM_ABBR[e$away_team]
  if (is.na(home) || is.na(away)) { warning("odds: unknown team name ", e$home_team, " / ", e$away_team); return(NULL) }
  sp <- tot <- numeric()
  for (b in e$bookmakers) for (m in b$markets) {
    if (m$key == "spreads") for (o in m$outcomes) if (identical(o$name, e$home_team) && !is.null(o$point)) sp <- c(sp, -o$point)
    if (m$key == "totals")  for (o in m$outcomes) if (identical(o$name, "Over") && !is.null(o$point)) tot <- c(tot, o$point)
  }
  if (!length(sp) || !length(tot)) return(NULL)
  tibble(pulled_at = format(NOW, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), event_id = e$id, commence_time = e$commence_time,
         home = unname(home), away = unname(away), n_books = min(length(sp), length(tot)),
         home_spread = median(sp), total = median(tot))
})
hist_cols <- c("pulled_at", "event_id", "commence_time", "home", "away", "n_books", "home_spread", "total")
hist <- if (file.exists(LINE_CSV)) read.csv(LINE_CSV, stringsAsFactors = FALSE) %>% as_tibble() else
  tibble(pulled_at = character(), event_id = character(), commence_time = character(), home = character(), away = character(),
         n_books = integer(), home_spread = numeric(), total = numeric())
ev <- fetch_odds()
new <- if (length(ev)) parse_odds(ev) else NULL
if (!is.null(new) && nrow(new)) {
  hist <- bind_rows(hist, new %>% select(all_of(hist_cols)))
  dir.create(dirname(LINE_CSV), recursive = TRUE, showWarnings = FALSE)
  write.csv(hist, LINE_CSV, row.names = FALSE)
  message("odds: ", nrow(new), " games priced; history now ", nrow(hist), " rows")
}

## ---- 3. Line to use for each game this week ----
utc <- function(x) as.POSIXct(sub("Z$", "", x), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
games <- bundles[[1]]$te %>% filter(home == 1) %>% transmute(game_id, home = team, away = opp, gameday)
pre <- hist %>% mutate(t = utc(pulled_at), ko = utc(commence_time)) %>% filter(t < ko) %>%   # never use in-game (live) lines
  # match on teams AND kickoff within 2 days of the scheduled gameday (a rematch later in the season has another date)
  inner_join(games %>% select(home, away, game_id, gameday), by = c("home", "away")) %>%
  filter(abs(as.numeric(difftime(ko, as.POSIXct(gameday, tz = "UTC"), units = "days"))) <= 2)
latest <- pre %>% group_by(game_id) %>% slice_max(t, n = 1, with_ties = FALSE) %>% ungroup() %>%
  select(game_id, ko, line_time = t, n_books, home_spread, total)
lines <- games %>% left_join(latest, by = "game_id") %>%
  mutate(locked = !is.na(ko) & NOW >= ko, src = ifelse(is.na(home_spread), "weekly run", "sportsbooks"))
message(sprintf("lines: %d games · %d from sportsbooks · %d locked", nrow(lines), sum(lines$src == "sportsbooks"), sum(lines$locked)))

## ---- 3b. Weather forecasts (display only for D/ST; shared with the kicker refresh) ----
wk_games <- tryCatch(week_games(SEASON, WEEK), error = function(e) NULL)
wx <- tryCatch(wx_latest(wx_update(WX_CSV, wk_games, NOW)), error = function(e) { message("weather: ", conditionMessage(e)); wx_latest(read_wx_hist(WX_CSV)) })

## ---- 3b2. Sleeper / ESPN weekly ranks for the track record (rank only; teams not yet kicked off; ext_utils.R) ----
EXT_CSV <- file.path(PROJ_DIR, "data/lines/ext_rank_history.csv")
if (!nzchar(Sys.getenv("NO_EXT")) && file.exists(file.path(PROJ_DIR, "scripts/ext_utils.R"))) tryCatch({
  source(file.path(PROJ_DIR, "scripts/ext_utils.R"))
  kt <- if (!is.null(wk_games)) bind_rows(wk_games %>% transmute(team = home_team, ko = kickoff), wk_games %>% transmute(team = away_team, ko = kickoff)) else
    bind_rows(lines %>% transmute(team = home, ko), lines %>% transmute(team = away, ko))
  ext_snapshot(EXT_CSV, SEASON, WEEK, NOW, kt)
}, error = function(e) message("Sleeper / ESPN ranks: ", conditionMessage(e)))
# this week's latest Sleeper / ESPN rank per team (frozen at kickoff) → shown next to ours in the ESPN-format table
ext_now <- tryCatch({ kt2 <- if (exists("kt")) kt else NULL
  if (is.null(kt2) || !file.exists(EXT_CSV)) NULL else
    ext_latest(read_ext_hist(EXT_CSV) %>% filter(season == SEASON, week == WEEK), kt2 %>% mutate(season = as.integer(SEASON), week = as.integer(WEEK))) %>%
      filter(pos == "DEF") %>% select(team, source, rank, pts) },
  error = function(e) NULL)

## ---- 3c. Projected starting QBs ----
b1 <- bundles[[1]]
p1 <- readRDS(file.path(DST_DIR, names(bundles)[1], sprintf("report_parts_%d_wk%02d.rds", SEASON, WEEK)))$pred
weekly_qb <- p1 %>% distinct(team = opp, qb_name = opp_qb_name) %>%
  mutate(qb_id = if (!is.null(b1$qb)) b1$qb$raw$opp_qb_id[match(team, b1$qb$raw$opp)] else NA_character_)
can_swap <- all(map_lgl(bundles, ~ !is.null(.x$qb) && isTRUE(.x$qb$max_diff < 1e-6)))
if (!can_swap) message("starters: this week's bundles have no QB scenarios (weekly model predates them) — starters shown, not re-scored")
starters <- tryCatch({
  S <- starter_sources(SEASON, WEEK, weekly_qb$team, cache = file.path(WORK_DIR, "starter_sources.rds"), now = NOW,
                       snap_dir = file.path(PROJ_DIR, "data/lines/sources"))
  st <- pick_qbs(S, weekly_qb, wk_games, if (!is.null(b1$qb)) b1$qb$pool else NULL, QBO_CSV, SEASON, WEEK)
  # games that have kicked off keep the last starter chosen before kickoff (else the weekly QB)
  ko <- if (!is.null(wk_games)) c(setNames(wk_games$kickoff, wk_games$home_team), setNames(wk_games$kickoff, wk_games$away_team)) else NULL
  started <- if (is.null(ko)) character() else st$team[!is.na(ko[st$team]) & NOW >= ko[st$team]]
  if (length(started)) {
    prev <- last_pre_kickoff(ST_CSV, SEASON, WEEK, ko)
    for (tm in started) {
      i <- which(st$team == tm); h <- if (!is.null(prev)) prev[prev$team == tm, ] else NULL
      if (!is.null(h) && nrow(h)) { st$qb_id[i] <- h$player_id; st$qb_name[i] <- h$player; st$status[i] <- na_if(h$status, ""); st$rule[i] <- paste("locked at kickoff:", h$rule) }
      else { w <- weekly_qb[weekly_qb$team == tm, ]; st$qb_id[i] <- w$qb_id; st$qb_name[i] <- w$qb_name; st$rule[i] <- "locked at kickoff: weekly run" }
    }
  }
  log_starters(ST_CSV, NOW, SEASON, WEEK, st %>% filter(!team %in% started) %>%
                 transmute(team, pos = "QB", player = qb_name, player_id = qb_id, status = coalesce(status, ""), rule, note = disagree))
  attr(st, "sources") <- list(ok = S$ok, fail = S$fail, stale = S$stale)
  st
}, error = function(e) { message("starters: failed — ", conditionMessage(e), " (weekly QBs kept)"); NULL })
if (!is.null(starters)) {
  chg <- starters %>% left_join(weekly_qb %>% select(team, w_name = qb_name), by = "team") %>% filter(st_norm(qb_name) != st_norm(w_name))
  message(sprintf("starters: %d offenses · sources OK: %s%s · changed since weekly run: %s", nrow(starters), paste(attr(starters, "sources")$ok, collapse = ", "),
                  if (length(attr(starters, "sources")$fail)) paste0(" · FAILED: ", paste(attr(starters, "sources")$fail, collapse = ", ")) else "",
                  if (nrow(chg)) paste(sprintf("%s %s → %s", chg$team, chg$w_name, chg$qb_name), collapse = "; ") else "none"))
  flag <- starters %>% filter(!is.na(status) | nzchar(disagree) | rule != "nflverse schedule")
  if (nrow(flag)) for (i in seq_len(nrow(flag))) message(sprintf("  %-3s %-22s %-12s %s%s", flag$team[i], flag$qb_name[i], coalesce(flag$status[i], ""), flag$rule[i],
                                                           if (nzchar(flag$disagree[i])) paste0(" | other sources: ", flag$disagree[i]) else ""))
}

## ---- 4. Re-score each system ----
apply_lines <- function(te, lines) {
  l <- bind_rows(lines %>% transmute(game_id, team = home, sp = home_spread, tot = total),
                 lines %>% transmute(game_id, team = away, sp = -home_spread, tot = total))
  te %>% left_join(l, by = c("game_id", "team")) %>%
    mutate(spread = coalesce(sp, spread), total_line = coalesce(tot, total_line),
           implied_opp = (total_line - spread) / 2, implied_own = (total_line + spread) / 2) %>% select(-sp, -tot)
}
score <- function(b, te) {
  M <- b$main
  out <- tibble(team = te$team, spread = te$spread, total_line = te$total_line, implied_opp = te$implied_opp,
                enet = lin_pred(M$enet, te), ridge = lin_pred(M$ridge, te), components = comp_pred(M$comp, b$specs, te, b$SC))
  out$proj <- rowMeans(as.matrix(out[b$final_models]))
  boot <- sapply(b$boot, blend_pred, te = te, sc = b$SC, specs = b$specs, models = b$final_models)
  od <- outcome_dist(out$proj, b$unc$proj, b$unc$resid, b$unc$bw, b$unc$n_sim, b$unc$seed)
  sp <- dst_sim_probs(b$sim, te, out$proj, b$SC)       # component simulation (bundles from 2026-09-25 on): P(10+), P(<3), P(15+)
  if (!is.null(sp)) { od$p_boom <- sp$p_boom; od$p_bust <- sp$p_bust; od$p_ceiling <- sp$p_ceiling; od$sim_sd <- sp$sim_sd }
  bind_cols(out, component_cols(M$comp, b$specs, te, b$SC), od, boot_summary(boot))
}
dyn_cols <- c("spread", "total_line", "implied_opp", "enet", "ridge", "components", "proj", "e_sacks", "e_to", "e_pa", "e_ya", "p_td",
              "q10", "q25", "q75", "q90", "p_boom", "p_bust", "p_ceiling", "sim_sd", "p_top8", "proj_se", "ci_lo", "ci_hi")
out_parts <- list()
for (s in names(bundles)) {
  b <- bundles[[s]]
  pf <- file.path(DST_DIR, s, sprintf("report_parts_%d_wk%02d.rds", SEASON, WEEK))
  if (!file.exists(pf)) { warning("no report parts for ", s, " — skipped"); next }
  parts <- readRDS(pf)
  base <- score(b, b$te) %>% transmute(team, spread_base = spread, total_base = total_line, implied_opp_base = implied_opp, proj_base = proj,
              rank_base = rank(-proj, ties.method = "first"))
  sw <- swap_qbs(b, if (can_swap && !is.null(starters)) starters %>% select(team, qb_id, qb_name) else NULL)
  te_now <- apply_lines(sw$te, lines)
  now  <- score(b, te_now)
  # what drives each team's projection (vs an average team this week, Vegas excluded)
  dr <- drivers(te_now, function(t) blend_pred(b$main, t, b$SC, b$specs, b$final_models), dst_groups(setdiff(names(b$te), c("game_id", "team", "opp", "gameday"))))
  now$why <- dr$text[match(now$team, te_now$team)]
  info <- bind_rows(lines %>% transmute(team = home, ko, line_time, n_books, locked, src), lines %>% transmute(team = away, ko, line_time, n_books, locked, src))
  parts$pred <- parts$pred %>% select(-any_of(c(dyn_cols, names(base)[-1], names(info)[-1], "rank"))) %>%
    left_join(now, by = "team") %>% left_join(base, by = "team") %>% left_join(info, by = "team") %>%
    left_join(wx %>% select(game_id, wx_temp = temp, wx_wind = wind, wx_gust = gust, wx_precip_prob = precip_prob, wx_precip_in = precip_in, wx_precip_max = precip_max), by = "game_id") %>%
    arrange(desc(proj)) %>% mutate(rank = row_number(), .before = 1)
  if (!is.null(starters)) {                                 # opponent QB shown on the page + hover text
    pq <- starters[match(parts$pred$opp, starters$team), ]
    w_name <- parts$pred$opp_qb_name
    changed <- !is.na(pq$qb_name) & st_norm(pq$qb_name) != st_norm(w_name)
    rescored <- changed & can_swap
    if (!is.null(sw$info)) parts$pred$o_qb_cont <- sw$info$o_qb_cont[match(parts$pred$team, sw$info$team)]
    parts$pred <- parts$pred %>% mutate(opp_qb_weekly = w_name, opp_qb_name = coalesce(pq$qb_name, w_name), opp_qb_status = pq$status,
                                        opp_qb_changed = changed, opp_qb_rescored = rescored, opp_qb_disagree = coalesce(pq$disagree, "") != "",
                                        opp_qb_tip = ifelse(is.na(pq$qb_name), NA_character_, paste0(qb_tip(pq, w_name, rescored),
                                          ifelse(changed & !rescored, "\n\u26A0 Changed since the weekly run but NOT re-scored: rerun the weekly model (this week's bundle predates QB scenarios)", ""))))
  }
  if (!is.null(b$vegas)) parts$pred$vegas_proj <- vegas_proj(b$vegas, te_now)[match(parts$pred$team, te_now$team)]   # Vegas-only, same lines
  if (s == "espn" && !is.null(ext_now) && nrow(ext_now)) for (src in c("ESPN", "Sleeper")) {
    e <- ext_now[ext_now$source == src, ]; i <- match(parts$pred$team, e$team)
    parts$pred[[paste0(tolower(src), "_rank")]] <- e$rank[i]; parts$pred[[paste0(tolower(src), "_pts")]] <- e$pts[i]
  }
  parts$refresh <- list(time = NOW, n_priced = sum(lines$src == "sportsbooks"), n_locked = sum(lines$locked),
                        books = if (any(!is.na(lines$n_books))) median(lines$n_books, na.rm = TRUE) else NA, model_fit = b$created,
                        qb_sources = if (!is.null(starters)) attr(starters, "sources") else NULL, qb_rescore = can_swap)
  dir.create(file.path(WORK_DIR, "output/dst", s), recursive = TRUE, showWarnings = FALSE)
  out_parts[[s]] <- list(parts = parts, file = file.path(WORK_DIR, "output/dst", s, basename(pf)), fit = b$created)
  message(sprintf("%s: re-scored · biggest move %s", b$SC$label,
                  with(parts$pred[which.max(abs(parts$pred$proj - parts$pred$proj_base)), ], sprintf("%s %+.2f", team, proj - proj_base))))
}

## ---- 4b. Projection history → dotted trend line per team (weekly run + one point per refresh) ----
ph <- ph_update(PH_CSV, "dst", SEASON, WEEK, fit_time = out_parts[[1]]$fit,
                base = bind_rows(imap(out_parts, ~ tibble(system = .y, team = .x$parts$pred$team, proj = .x$parts$pred$proj_base, implied = .x$parts$pred$implied_opp_base))),
                cur  = bind_rows(imap(out_parts, ~ tibble(system = .y, team = .x$parts$pred$team, proj = .x$parts$pred$proj, implied = .x$parts$pred$implied_opp))), now = NOW)
# Δ columns: always vs the week's FIRST weekly run (normally Tuesday); a mid-week model rerun does not reset them
fb <- ph_base(ph)
for (s in names(out_parts)) {
  p <- out_parts[[s]]$parts
  f <- fb$tbl[fb$tbl$system == s, ]; i <- match(p$pred$team, f$team)
  p$pred$proj_base <- coalesce(f$proj_first[i], p$pred$proj_base)
  p$pred$implied_opp_base <- coalesce(f$implied_first[i], p$pred$implied_opp_base)
  p$refresh$base_time <- if (is.na(fb$time)) p$refresh$model_fit else fb$time
  p$pred$trend_svg <- map_chr(p$pred$team, ~ sparkline(trend_points(ph[ph$system == s & ph$team == .x, ])))
  saveRDS(p, out_parts[[s]]$file)                                          # the published weekly parts stay untouched
}

## ---- 5. Report + site ----
Sys.setenv(FF_PROJ_DIR = WORK_DIR, TRACK_DIR = file.path(PROJ_DIR, "output/track"))       # track record = the published file
st <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(file.path(PROJ_DIR, "scripts/44_dst_report.R")), SEASON, WEEK))
Sys.setenv(FF_PROJ_DIR = PROJ_DIR)
if (!identical(as.integer(st), 0L)) stop("44_dst_report.R failed")
html <- file.path(WORK_DIR, "output/dst", sprintf("dst_proj_%d_wk%02d_all.html", SEASON, WEEK))
dir.create(file.path(SITE_DIR, "archive"), recursive = TRUE, showWarnings = FALSE)
invisible(file.copy(html, file.path(SITE_DIR, c("index.html", file.path("archive", basename(html)))), overwrite = TRUE))
arch <- sort(list.files(file.path(SITE_DIR, "archive"), pattern = "^dst_proj_.*\\.html$"), decreasing = TRUE)   # not the archive index itself
writeLines(c('<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>D/ST archive</title>',
             '<style>body{font-family:system-ui,sans-serif;max-width:700px;margin:2rem auto;padding:0 16px}</style></head><body><h1>Past weeks</h1><ul>',
             sprintf('<li><a href="%s">%s</a></li>', arch, sub("dst_proj_(\\d{4})_wk(\\d{2})_all\\.html", "\\1 week \\2", arch)),
             '</ul><p><a href="../index.html">Current week</a></p></body></html>'), file.path(SITE_DIR, "archive", "index.html"))
message("site updated: ", file.path(SITE_DIR, "index.html"))
