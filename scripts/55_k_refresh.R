# ==============================================================================
# 55_k_refresh.R — daily re-score of this week's kicker projections with the latest Vegas lines AND
# weather forecasts. Runs on GitHub Actions right after 45_dst_refresh.R (same job), or locally.
#
# No data downloads (besides the forecast) and no refitting: reads the weekly scoring bundle written by
# 50_k_model.R (output/k/bundle_<season>_wk<ww>.rds: the fitted blend as coefficients).
#   1. Lines: reuses data/lines/line_history.csv that 45_dst_refresh.R just updated (no extra Odds API
#      credits). Per game: newest consensus line pulled before kickoff; unpriced games keep the weekly line.
#   2. Weather: Open-Meteo hourly forecast (free, no key) at kickoff +2 h for outdoor / retractable venues,
#      appended to data/lines/weather_history.csv. Per game: newest forecast pulled before kickoff, so started
#      games are frozen. Fetch failure → last stored forecast, else the weekly-run value.
#   3. Re-score (projection, components, ranges, boom/bust, P(top N), bootstrap CI) for ESPN + decimal; keeps
#      the weekly-run values as the "since Tuesday" baseline.
#   4. Writes re-scored report parts to work/, runs 54_k_report.R there, copies the page to site/k/index.html
#      (+ site/k/archive/). The D/ST page (site/index.html) is not touched.
# Usage: Rscript scripts/55_k_refresh.R          (REFRESH_NOW=2026-09-27T16:00:00Z for testing;
#        NO_WEATHER=1 skips the forecast fetch)
# ==============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(purrr); library(tibble) })
PROJ_DIR <- Sys.getenv("FF_PROJ_DIR", getwd())
K_DIR    <- file.path(PROJ_DIR, "output/k")
LINE_CSV <- file.path(PROJ_DIR, "data/lines/line_history.csv")
WX_CSV   <- file.path(PROJ_DIR, "data/lines/weather_history.csv")
SITE_DIR <- file.path(PROJ_DIR, "site")
WORK_DIR <- file.path(PROJ_DIR, "work")
SITE_BASE <- Sys.getenv("SITE_BASE", "/dst-site/")        # GitHub Pages project path (for the D/ST ↔ kicker links)
source(file.path(PROJ_DIR, "scripts/k_blend_utils.R"))
source(file.path(PROJ_DIR, "scripts/site_utils.R"))
PH_CSV <- file.path(PROJ_DIR, "data/lines/proj_history.csv")
utc <- function(x) as.POSIXct(sub("Z$", "", x), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
NOW <- if (nzchar(Sys.getenv("REFRESH_NOW"))) utc(Sys.getenv("REFRESH_NOW")) else as.POSIXct(format(Sys.time(), tz = "UTC"), tz = "UTC")

## ---- 1. Newest bundle ----
bf <- list.files(K_DIR, pattern = "^bundle_\\d{4}_wk\\d{2}\\.rds$")
if (!length(bf)) {                                  # no kicker model published yet: placeholder page instead of a 404
  dir.create(file.path(SITE_DIR, "k"), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(file.path(SITE_DIR, "k", "index.html")))
    writeLines(c('<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Kicker projections</title>',
                 '<style>body{font-family:system-ui,sans-serif;max-width:700px;margin:2rem auto;padding:0 16px}</style></head><body>',
                 '<h1>Kicker projections</h1><p>Not published yet: run 50_k_model.R and 46_dst_publish.R (or 43_dst_run_all.R).</p>',
                 sprintf('<p><a href="%s">D/ST projections</a></p></body></html>', SITE_BASE)), file.path(SITE_DIR, "k", "index.html"))
  message("no kicker bundle under ", K_DIR, ": placeholder page only"); quit(save = "no")
}
key <- max(sub("bundle_(\\d{4})_wk(\\d{2}).*", "\\1\\2", bf)); SEASON <- as.integer(substr(key, 1, 4)); WEEK <- as.integer(substr(key, 5, 6))
B <- readRDS(file.path(K_DIR, sprintf("bundle_%d_wk%02d.rds", SEASON, WEEK)))
parts_file <- file.path(K_DIR, sprintf("report_parts_%d_wk%02d.rds", SEASON, WEEK))
if (!file.exists(parts_file)) stop("no kicker report parts for ", key)
P <- readRDS(parts_file)
message(sprintf("kickers: refreshing %d week %d", SEASON, WEEK))
games <- B$te %>% filter(home == 1 | !duplicated(game_id)) %>% distinct(game_id, .keep_all = TRUE) %>%
  transmute(game_id, home_team = ifelse(home == 1, team, opp), away_team = ifelse(home == 1, opp, team), gameday, gametime, stadium_id, indoor,
            ko_sched = as.POSIXct(paste(gameday, gametime), tz = "America/New_York"))

## ---- 2. Lines (from the D/ST refresh's history; no extra API calls) ----
hist <- if (file.exists(LINE_CSV)) read.csv(LINE_CSV, stringsAsFactors = FALSE) %>% as_tibble() else tibble()
if (nrow(hist)) {
  # nflverse schedule home/away can be "neutral" for international games: match either orientation
  pre <- hist %>% mutate(t = utc(pulled_at), ko = utc(commence_time)) %>% filter(t < ko)
  m1 <- pre %>% inner_join(games %>% select(game_id, home = home_team, away = away_team, gameday), by = c("home", "away")) %>% mutate(flip = FALSE)
  m2 <- pre %>% inner_join(games %>% select(game_id, home = away_team, away = home_team, gameday), by = c("home", "away")) %>% mutate(flip = TRUE)
  latest <- bind_rows(m1, m2) %>% filter(abs(as.numeric(difftime(ko, as.POSIXct(gameday, tz = "UTC"), units = "days"))) <= 2) %>%
    group_by(game_id) %>% slice_max(t, n = 1, with_ties = FALSE) %>% ungroup() %>%
    transmute(game_id, ko, n_books, home_spread = ifelse(flip, -home_spread, home_spread), total)
} else latest <- tibble(game_id = character(), ko = as.POSIXct(character()), n_books = integer(), home_spread = numeric(), total = numeric())
lines <- games %>% left_join(latest, by = "game_id") %>% mutate(ko = coalesce(ko, ko_sched), locked = NOW >= ko, src = ifelse(is.na(home_spread), "weekly run", "sportsbooks"))
message(sprintf("kickers: lines %d games · %d from sportsbooks · %d started", nrow(lines), sum(lines$src == "sportsbooks"), sum(lines$locked)))

## ---- 3. Weather forecasts (site_utils.R; the D/ST refresh usually fetched them minutes ago → reused) ----
wk_games <- tryCatch(week_games(SEASON, WEEK), error = function(e) NULL)
wx_hist <- tryCatch(wx_update(WX_CSV, wk_games, NOW), error = function(e) { message("kickers: weather ", conditionMessage(e)); read_wx_hist(WX_CSV) })
wx_all <- wx_latest(wx_hist) %>% filter(game_id %in% games$game_id)
wx_use <- wx_all %>% select(game_id, temp_new = temp, wind_new = wind)

## ---- 4. Re-score ----
l2 <- bind_rows(lines %>% transmute(game_id, team = home_team, sp = home_spread, tot = total),
                lines %>% transmute(game_id, team = away_team, sp = -home_spread, tot = total))
te_now <- B$te %>% left_join(l2, by = c("game_id", "team")) %>% left_join(wx_use, by = "game_id") %>%
  mutate(spread = coalesce(sp, spread), total_line = coalesce(tot, total_line),
         wind_known = ifelse(indoor == 0 & !is.na(wind_new), 1L, wind_known),
         temp = coalesce(temp_new, temp), wind = coalesce(wind_new, wind)) %>%
  select(-sp, -tot, -temp_new, -wind_new) %>% derive_vegas() %>% derive_env()
base <- score_bundle(B, B$te); now <- score_bundle(B, te_now)
# what drives each kicker's projection vs an average kicker this week (Vegas + league-level terms excluded)
grp <- k_groups(setdiff(names(te_now), c("game_id", "team", "opp", "gameday", "gametime", "stadium_id", "k_long_share_raw", "wind", "wind_known")))
for (sy in names(B$SCORING)) now[[paste0("why_", sy)]] <- drivers(te_now, function(t) blend_score(B, t, sy), grp)$text
dyn <- setdiff(names(now), c("game_id", "team"))
base_cols <- base %>% select(team, starts_with("proj_")) %>% rename_with(~ sub("^proj_", "proj_base_", .x), starts_with("proj_"))
info <- bind_rows(lines %>% transmute(team = home_team, ko, locked, src), lines %>% transmute(team = away_team, ko, locked, src))
P$pred <- P$pred %>% select(-any_of(c(dyn, "spread", "total_line", "implied_own", "implied_opp", "wind", "temp", "wind_known", "ko", "locked", "src",
                                      names(base_cols)[-1], "implied_own_base", "wx_gust", "wx_precip_prob", "wx_precip_in", "wx_precip_max"))) %>%
  left_join(now %>% select(-game_id), by = "team") %>%
  left_join(te_now %>% select(team, spread, total_line, implied_own, implied_opp, wind, temp, wind_known), by = "team") %>%
  left_join(base_cols, by = "team") %>% left_join(B$te %>% transmute(team, implied_own_base = (total_line + spread) / 2), by = "team") %>%
  left_join(info, by = "team") %>%
  left_join(wx_all %>% select(game_id, wx_gust = gust, wx_precip_prob = precip_prob, wx_precip_in = precip_in, wx_precip_max = precip_max), by = "game_id")
# kicker injury status (starters.R; the sources were fetched by the D/ST refresh minutes ago and are reused).
# Display only: an Out kicker is flagged with the likely replacement; the projection keeps the listed kicker.
source(file.path(PROJ_DIR, "scripts/starters.R"))
ks <- tryCatch({
  S <- starter_sources(SEASON, WEEK, sort(unique(P$pred$team)), cache = file.path(WORK_DIR, "starter_sources.rds"), now = NOW,
                       snap_dir = file.path(PROJ_DIR, "data/lines/sources"))
  k <- kicker_status(S, P$pred %>% transmute(team, kicker_id = if ("kicker_id" %in% names(P$pred)) kicker_id else NA_character_, kicker))
  started <- P$pred$team[P$pred$locked %in% TRUE]
  log_starters(file.path(PROJ_DIR, "data/lines/starter_history.csv"), NOW, SEASON, WEEK,
               k %>% filter(!team %in% started) %>% left_join(P$pred %>% select(team, kicker, any_of("kicker_id")), by = "team") %>%
                 transmute(team, pos = "K", player = kicker, player_id = if ("kicker_id" %in% names(.)) kicker_id else "", status = coalesce(k_status, ""),
                           rule = "weekly run (status check only)", note = ifelse(is.na(k_alt), "", paste("replacement:", k_alt))))
  message(sprintf("kickers: status checked (%s) · flagged: %s", paste(S$ok, collapse = ", "),
                  if (any(!is.na(k$k_status))) paste(sprintf("%s %s", k$team, k$k_status)[!is.na(k$k_status)], collapse = ", ") else "none"))
  k }, error = function(e) { message("kickers: status check failed — ", conditionMessage(e)); NULL })
if (!is.null(ks)) P$pred <- P$pred %>% select(-any_of(c("k_status", "k_out", "k_status_detail", "k_alt"))) %>% left_join(ks, by = "team")
# projection history → dotted trend line (weekly run + one point per refresh)
ph <- ph_update(PH_CSV, "k", SEASON, WEEK, fit_time = B$created,
                base = bind_rows(map(names(B$SCORING), ~ tibble(system = .x, team = P$pred$team, proj = P$pred[[paste0("proj_base_", .x)]], implied = P$pred$implied_own_base))),
                cur  = bind_rows(map(names(B$SCORING), ~ tibble(system = .x, team = P$pred$team, proj = P$pred[[paste0("proj_", .x)]], implied = P$pred$implied_own))), now = NOW)
# Δ columns: always vs the week's FIRST weekly run (normally Tuesday); a mid-week model rerun does not reset them
fb <- ph_base(ph)
for (sy in names(B$SCORING)) {
  f <- fb$tbl[fb$tbl$system == sy, ]; i <- match(P$pred$team, f$team)
  P$pred[[paste0("proj_base_", sy)]] <- coalesce(f$proj_first[i], P$pred[[paste0("proj_base_", sy)]])
  if (sy == names(B$SCORING)[1]) P$pred$implied_own_base <- coalesce(f$implied_first[i], P$pred$implied_own_base)
  P$pred[[paste0("trend_svg_", sy)]] <- map_chr(P$pred$team, ~ sparkline(trend_points(ph[ph$system == sy & ph$team == .x, ])))
}
# this week's latest Sleeper / ESPN kicker rank per team (snapshot taken by the D/ST refresh; frozen at kickoff)
ext_f <- file.path(PROJ_DIR, "data/lines/ext_rank_history.csv")
if (file.exists(ext_f) && file.exists(file.path(PROJ_DIR, "scripts/ext_utils.R"))) tryCatch({
  source(file.path(PROJ_DIR, "scripts/ext_utils.R"))
  kt <- bind_rows(lines %>% transmute(team = home_team, ko), lines %>% transmute(team = away_team, ko)) %>% mutate(season = as.integer(SEASON), week = as.integer(WEEK))
  en <- ext_latest(read_ext_hist(ext_f) %>% filter(season == SEASON, week == WEEK), kt) %>% filter(pos == "K") %>% select(team, source, rank)
  for (src in c("ESPN", "Sleeper")) P$pred[[paste0(tolower(src), "_rank")]] <- en$rank[en$source == src][match(P$pred$team, en$team[en$source == src])]
}, error = function(e) message("kickers: Sleeper / ESPN ranks — ", conditionMessage(e)))
P$refresh <- list(time = NOW, n_priced = sum(lines$src == "sportsbooks"), n_locked = sum(lines$locked), n_games = nrow(lines),
                  books = if (any(!is.na(lines$n_books))) median(lines$n_books, na.rm = TRUE) else NA,
                  weather = if (nrow(wx_use)) sprintf("Open-Meteo forecasts for %d outdoor games", nrow(wx_use)) else "weekly-run values",
                  model_fit = B$created, base_time = if (is.na(fb$time)) B$created else fb$time)
P$nav <- sprintf("<p class='s'><a href='%s'>D/ST projections</a> · <b>Kickers</b> · <a href='%sk/archive/'>past weeks</a></p>", SITE_BASE, SITE_BASE)
mv <- P$pred[which.max(abs(P$pred$proj_espn - P$pred$proj_base_espn)), ]
message(sprintf("kickers: re-scored · biggest ESPN move %s %+.2f", mv$team, mv$proj_espn - mv$proj_base_espn))

## ---- 5. Report + site ----
dir.create(file.path(WORK_DIR, "output/k"), recursive = TRUE, showWarnings = FALSE)
saveRDS(P, file.path(WORK_DIR, "output/k", basename(parts_file)))
Sys.setenv(FF_PROJ_DIR = WORK_DIR, TRACK_DIR = file.path(PROJ_DIR, "output/track"))       # track record = the published file
st <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(file.path(PROJ_DIR, "scripts/54_k_report.R")), SEASON, WEEK))
Sys.setenv(FF_PROJ_DIR = PROJ_DIR)
if (!identical(as.integer(st), 0L)) stop("54_k_report.R failed")
html <- file.path(WORK_DIR, "output/k", sprintf("k_proj_%d_wk%02d.html", SEASON, WEEK))
dir.create(file.path(SITE_DIR, "k", "archive"), recursive = TRUE, showWarnings = FALSE)
invisible(file.copy(html, file.path(SITE_DIR, "k", c("index.html", file.path("archive", basename(html)))), overwrite = TRUE))
arch <- sort(list.files(file.path(SITE_DIR, "k", "archive"), pattern = "^k_proj.*\\.html$"), decreasing = TRUE)
writeLines(c('<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Kicker archive</title>',
             '<style>body{font-family:system-ui,sans-serif;max-width:700px;margin:2rem auto;padding:0 16px}</style></head><body><h1>Kickers: past weeks</h1><ul>',
             sprintf('<li><a href="%s">%s</a></li>', arch, sub("k_proj_(\\d{4})_wk(\\d{2})\\.html", "\\1 week \\2", arch)),
             sprintf('</ul><p><a href="%sk/">Current week</a> · <a href="%s">D/ST projections</a></p></body></html>', SITE_BASE, SITE_BASE)),
           file.path(SITE_DIR, "k", "archive", "index.html"))
message("kickers: site updated: ", file.path(SITE_DIR, "k", "index.html"))
