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
  f <- Sys.getenv("ODDS_JSON_FILE"); key <- Sys.getenv("ODDS_API_KEY")
  if (nzchar(f)) { message("odds: reading ", f); return(jsonlite::fromJSON(f, simplifyVector = FALSE)) }
  if (!nzchar(key)) { message("odds: ODDS_API_KEY not set — using stored lines only"); return(NULL) }
  url <- paste0("https://api.the-odds-api.com/v4/sports/americanfootball_nfl/odds/?regions=us&markets=spreads,totals",
                "&oddsFormat=american&dateFormat=iso&apiKey=", key)
  r <- tryCatch(curl::curl_fetch_memory(url), error = function(e) NULL)          # never print `url`: it holds the key
  if (is.null(r) || r$status_code != 200) {
    warning("odds: request failed (HTTP ", if (is.null(r)) "no response" else r$status_code, ") — using stored lines only"); return(NULL) }
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
  bind_cols(out, component_cols(M$comp, b$specs, te, b$SC),
            outcome_dist(out$proj, b$unc$proj, b$unc$resid, b$unc$bw, b$unc$n_sim, b$unc$seed), boot_summary(boot))
}
dyn_cols <- c("spread", "total_line", "implied_opp", "enet", "ridge", "components", "proj", "e_sacks", "e_to", "e_pa", "e_ya", "p_td",
              "q10", "q25", "q75", "q90", "p_boom", "p_bust", "p_top8", "proj_se", "ci_lo", "ci_hi")
for (s in names(bundles)) {
  b <- bundles[[s]]
  pf <- file.path(DST_DIR, s, sprintf("report_parts_%d_wk%02d.rds", SEASON, WEEK))
  if (!file.exists(pf)) { warning("no report parts for ", s, " — skipped"); next }
  parts <- readRDS(pf)
  base <- score(b, b$te) %>% transmute(team, spread_base = spread, total_base = total_line, implied_opp_base = implied_opp, proj_base = proj,
              rank_base = rank(-proj, ties.method = "first"))
  now  <- score(b, apply_lines(b$te, lines))
  info <- bind_rows(lines %>% transmute(team = home, ko, line_time, n_books, locked, src), lines %>% transmute(team = away, ko, line_time, n_books, locked, src))
  parts$pred <- parts$pred %>% select(-any_of(c(dyn_cols, names(base)[-1], names(info)[-1], "rank"))) %>%
    left_join(now, by = "team") %>% left_join(base, by = "team") %>% left_join(info, by = "team") %>%
    arrange(desc(proj)) %>% mutate(rank = row_number(), .before = 1)
  parts$refresh <- list(time = NOW, n_priced = sum(lines$src == "sportsbooks"), n_locked = sum(lines$locked),
                        books = if (any(!is.na(lines$n_books))) median(lines$n_books, na.rm = TRUE) else NA, model_fit = b$created)
  dir.create(file.path(WORK_DIR, "output/dst", s), recursive = TRUE, showWarnings = FALSE)
  saveRDS(parts, file.path(WORK_DIR, "output/dst", s, basename(pf)))       # the published weekly parts stay untouched
  message(sprintf("%s: re-scored · biggest move %s", b$SC$label,
                  with(parts$pred[which.max(abs(parts$pred$proj - parts$pred$proj_base)), ], sprintf("%s %+.2f", team, proj - proj_base))))
}

## ---- 5. Report + site ----
Sys.setenv(FF_PROJ_DIR = WORK_DIR)
st <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(file.path(PROJ_DIR, "scripts/44_dst_report.R")), SEASON, WEEK))
Sys.setenv(FF_PROJ_DIR = PROJ_DIR)
if (!identical(as.integer(st), 0L)) stop("44_dst_report.R failed")
html <- file.path(WORK_DIR, "output/dst", sprintf("dst_proj_%d_wk%02d_all.html", SEASON, WEEK))
dir.create(file.path(SITE_DIR, "archive"), recursive = TRUE, showWarnings = FALSE)
invisible(file.copy(html, file.path(SITE_DIR, c("index.html", file.path("archive", basename(html)))), overwrite = TRUE))
arch <- sort(list.files(file.path(SITE_DIR, "archive"), pattern = "\\.html$"), decreasing = TRUE)
writeLines(c('<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>D/ST archive</title>',
             '<style>body{font-family:system-ui,sans-serif;max-width:700px;margin:2rem auto;padding:0 16px}</style></head><body><h1>Past weeks</h1><ul>',
             sprintf('<li><a href="%s">%s</a></li>', arch, sub("dst_proj_(\\d{4})_wk(\\d{2})_all\\.html", "\\1 week \\2", arch)),
             '</ul><p><a href="../index.html">Current week</a></p></body></html>'), file.path(SITE_DIR, "archive", "index.html"))
message("site updated: ", file.path(SITE_DIR, "index.html"))
