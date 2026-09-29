# ==============================================================================
# 65_player_refresh.R — Vegas-only QB / RB / WR / TE projections from live player props
#
# Runs on GitHub Actions (.github/workflows/player_refresh.yml in the dst-site repo) or locally.
# Andrew 2026-09-29: the page shows the Vegas-only projection. Back-tests (85 / 87) found no model or
# feature set that beats the sportsbook props at kickoff at any position.
#   1. Reads the newest weekly bundle (output/players_site/bundle_<season>_wk<ww>.rds, 63_player_site_bundle.R).
#   2. Pulls player props for this week's games that haven't kicked off (The Odds API, secret PROPS_API_KEY;
#      9 markets, US books; about 9 credits per game with props posted, 0 for the free events list).
#      Consensus per player x market = median across books (props_utils.R); every pull is appended to
#      data/players/props_live_<season>_wk<ww>.csv.
#   3. Per game: the newest pull before kickoff. Started games are locked at their last pre-kickoff props.
#   4. Converts props -> expected stats -> points in Standard / Half / PPR / FFPC (player_site_utils.R =
#      83_vegas_baseline.R's conversion); stats without a prop use the player's recency-weighted career
#      average; P(boom) / P(bust) from the bundle's calibrations. Shows players with a yardage / receptions
#      prop, plus TD-only players whose anytime-TD price is at or above the position's median.
#   5. Writes site/players/index.html (+ archive). Trend = projection at every pull since props posted.
# Usage: Rscript scripts/65_player_refresh.R
#   env PROPS_API_KEY (no key: re-renders from stored pulls), PROPS_MIN_REMAINING (default 300: stop pulling
#   below this many credits), REFRESH_NOW=2026-09-27T16:00:00Z and PLAYER_PROPS_MOCK=<rds> for testing
# ==============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(purrr); library(tibble) })
if (!isTRUE(l10n_info()$`UTF-8`)) invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")))
PROJ_DIR <- Sys.getenv("FF_PROJ_DIR", getwd())
SITE_DIR <- file.path(PROJ_DIR, "site"); SITE_BASE <- Sys.getenv("SITE_BASE", "/dst-site/")
source(file.path(PROJ_DIR, "scripts/props_utils.R"))
source(file.path(PROJ_DIR, "scripts/player_site_utils.R"))
source(file.path(PROJ_DIR, "scripts/site_utils.R"))
source(file.path(PROJ_DIR, "scripts/64_player_page.R"))
source(file.path(PROJ_DIR, "scripts/ext_player_utils.R"))
utc <- function(x) as.POSIXct(sub("Z$", "", x), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
isoz <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
NOW <- if (nzchar(Sys.getenv("REFRESH_NOW"))) utc(Sys.getenv("REFRESH_NOW")) else as.POSIXct(format(Sys.time(), tz = "UTC"), tz = "UTC")

## ---- 1. Bundle ----
bf <- list.files(file.path(PROJ_DIR, "output/players_site"), pattern = "^bundle_\\d{4}_wk\\d{2}\\.rds$")
if (!length(bf)) {
  dir.create(file.path(SITE_DIR, "players"), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(file.path(SITE_DIR, "players", "index.html")))
    writeLines(sprintf('<!doctype html><html><head><meta charset="utf-8"><title>Player projections</title></head><body><h1>Player projections</h1><p>Not published yet: run 63 and 66 (or 89).</p><p><a href="%s">D/ST projections</a></p></body></html>', SITE_BASE),
               file.path(SITE_DIR, "players", "index.html"))
  message("players: no bundle yet, placeholder page"); quit(save = "no")
}
key <- max(sub("bundle_(\\d{4})_wk(\\d{2}).*", "\\1\\2", bf)); SEASON <- as.integer(substr(key, 1, 4)); WEEK <- as.integer(substr(key, 5, 6))
B <- readRDS(file.path(PROJ_DIR, "output/players_site", sprintf("bundle_%d_wk%02d.rds", SEASON, WEEK)))
games <- B$games |> mutate(ko = utc(kickoff_utc))
LIVE_CSV <- file.path(PROJ_DIR, sprintf("data/players/props_live_%d_wk%02d.csv", SEASON, WEEK))
LIVE_COLS <- c("pulled_at", "game_id", "event_id", "commence_time", "market", "player", "n_books", "line", "p_over", "med_est",
               "line_min", "line_max", "p_td_raw")
message(sprintf("players: %d week %d, %d games, %d started", SEASON, WEEK, nrow(games), sum(NOW >= games$ko)))

## ---- 2. Pull props for games not started ----
API <- Sys.getenv("ODDS_API_BASE", "https://api.the-odds-api.com/v4/sports/americanfootball_nfl")
KEY <- Sys.getenv("PROPS_API_KEY"); MOCK <- Sys.getenv("PLAYER_PROPS_MOCK")
MIN_LEFT <- as.numeric(Sys.getenv("PROPS_MIN_REMAINING", "300"))
get_json <- function(path, query) {
  if (nzchar(MOCK)) { M <- readRDS(MOCK); return(list(body = if (path == "/events") M$events else M$odds[[sub("^/events/([^/]+)/odds$", "\\1", path)]],
                                                     remaining = 99999, used = 0)) }
  url <- paste0(API, path, "?", paste(names(query), vapply(query, utils::URLencode, "", reserved = TRUE), sep = "=", collapse = "&"))
  r <- curl::curl_fetch_memory(url)
  if (r$status_code != 200) stop(sprintf("HTTP %d for %s", r$status_code, path))
  h <- curl::parse_headers_list(r$headers)
  list(body = jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE),
       remaining = suppressWarnings(as.numeric(h[["x-requests-remaining"]] %||% NA)), used = suppressWarnings(as.numeric(h[["x-requests-last"]] %||% NA)))
}
pulled <- 0; credits <- 0; left <- NA
if (nzchar(KEY) || nzchar(MOCK)) tryCatch({
  ev <- get_json("/events", list(apiKey = KEY, dateFormat = "iso"))$body
  evt <- tibble(event_id = map_chr(ev, "id"), commence_time = map_chr(ev, "commence_time"),
                home = unname(ODDS_TEAMS[map_chr(ev, "home_team")]), away = unname(ODDS_TEAMS[map_chr(ev, "away_team")]))
  ## match events to this week's games (either orientation: neutral-site games), kickoff within 2 days
  m <- bind_rows(evt |> inner_join(games |> select(game_id, home = home_team, away = away_team, ko), by = c("home", "away")),
                 evt |> inner_join(games |> select(game_id, home = away_team, away = home_team, ko), by = c("home", "away"))) |>
    mutate(ct = utc(commence_time)) |> filter(abs(as.numeric(difftime(ct, ko, units = "days"))) <= 2) |>
    distinct(game_id, .keep_all = TRUE) |> filter(ct > NOW)
  message(sprintf("players: %d events listed, %d of this week's games not started", nrow(evt), nrow(m)))
  rows <- list()
  for (i in seq_len(nrow(m))) {
    if (!is.na(left) && left < MIN_LEFT) { message(sprintf("players: stopping, %s credits left (< %s)", left, MIN_LEFT)); break }
    r <- tryCatch(get_json(sprintf("/events/%s/odds", m$event_id[i]),
                           list(apiKey = KEY, regions = "us", markets = paste(PROP_MARKETS, collapse = ","), oddsFormat = "american")),
                  error = function(e) { message("players: ", m$game_id[i], " — ", conditionMessage(e)); NULL })
    if (is.null(r)) next
    left <- r$remaining; credits <- credits + coalesce(r$used, 0)
    long <- flatten_event_odds(r$body)
    if (!nrow(long)) next
    long$game_id <- m$game_id[i]
    ou <- props_consensus(book_lines(long))
    td <- props_anytime(long)
    rows[[length(rows) + 1]] <- bind_rows(
      ou |> transmute(game_id, market, player, n_books, line, p_over, med_est, line_min, line_max),
      td |> transmute(game_id, market, player, n_books, p_td_raw)) |>
      mutate(pulled_at = isoz(NOW), event_id = m$event_id[i], commence_time = m$commence_time[i])
    pulled <- pulled + 1
  }
  if (length(rows)) {
    new <- bind_rows(rows)
    for (c in setdiff(LIVE_COLS, names(new))) new[[c]] <- NA
    dir.create(dirname(LIVE_CSV), recursive = TRUE, showWarnings = FALSE)
    write.table(new[LIVE_COLS], LIVE_CSV, sep = ",", row.names = FALSE, col.names = !file.exists(LIVE_CSV), append = file.exists(LIVE_CSV), qmethod = "double")
  }
  message(sprintf("players: pulled props for %d games (%s credits used, %s left)", pulled, credits, left))
}, error = function(e) message("players: props pull failed — ", conditionMessage(e), " (using stored pulls)"))
if (!nzchar(KEY) && !nzchar(MOCK)) message("players: PROPS_API_KEY not set — using stored pulls only")

## ---- 3. Every stored pull before kickoff -> projections ----
live <- if (file.exists(LIVE_CSV)) as_tibble(read.csv(LIVE_CSV, stringsAsFactors = FALSE)) else tibble()
if (nrow(live)) {
  live <- live |> mutate(t = utc(pulled_at)) |>
    left_join(games |> select(game_id, ko), by = "game_id") |> filter(t < ko)          # never in-game props
}
project_pull <- function(x) {                     # x: consensus rows of one pull time (several games)
  mp <- match_players(x |> distinct(game_id, player), games |> select(game_id, season, week, home_team, away_team), B$roster)
  x <- x |> inner_join(mp |> filter(!is.na(gsis_id)) |> select(game_id, player, gsis_id), by = c("game_id", "player"))
  d <- ps_inputs(x |> filter(market != "player_anytime_td"), x |> filter(market == "player_anytime_td"))
  info <- B$players |> select(gsis_id, player_name, pos, team)
  d <- d |> inner_join(info, by = "gsis_id") |> left_join(B$career, by = "gsis_id") |> ps_flags(B$td_bar)
  if (!nrow(d)) return(tibble())
  e <- ps_expect(d, B)
  pts <- ps_points(e, d$pos)
  bind_cols(d |> select(game_id, gsis_id, player_name, pos, team, core, td_only, td_keep, p_td_raw,
                        starts_with("line."), starts_with("p_over."), starts_with("n_books.")),
            e |> select(-game_id, -gsis_id), pts, ps_bb(pts, d$p_td_raw, d$pos, B$bb))
}
if (nrow(live)) {
  pulls <- sort(unique(live$t))
  hist <- map_dfr(pulls, \(tt) { r <- project_pull(live |> filter(t == tt)); if (nrow(r)) mutate(r, t = tt) else NULL })
  ## current = per game, its newest pre-kickoff pull
  last_t <- live |> group_by(game_id) |> summarise(t = max(t), .groups = "drop")
  cur <- hist |> inner_join(last_t, by = c("game_id", "t"))
} else { hist <- tibble(); cur <- tibble() }
unm <- if (nrow(live)) { lt <- live |> group_by(game_id) |> filter(t == max(t)) |> ungroup()
  match_players(lt |> distinct(game_id, player), games |> select(game_id, season, week, home_team, away_team), B$roster) |>
    filter(method == "unmatched") } else tibble()
if (nrow(unm)) message("players: unmatched names (not shown): ", paste(head(unique(unm$player), 15), collapse = ", "))

## ---- 4. Context: opponent, kickoff, lock, implied points (the D/ST refresh's line history) ----
gl <- bind_rows(games |> transmute(game_id, team = home_team, opp = away_team, home = 1L, ko),
                games |> transmute(game_id, team = away_team, opp = home_team, home = 0L, ko))
imp <- tryCatch({
  lh <- as_tibble(read.csv(file.path(PROJ_DIR, "data/lines/line_history.csv"), stringsAsFactors = FALSE)) |>
    mutate(t = utc(pulled_at), ct = utc(commence_time)) |> filter(t < ct)
  lx <- bind_rows(lh |> inner_join(games |> select(game_id, home = home_team, away = away_team, ko), by = c("home", "away")) |> mutate(sp = home_spread),
                  lh |> inner_join(games |> select(game_id, home = away_team, away = home_team, ko), by = c("home", "away")) |> mutate(sp = -home_spread)) |>
    filter(abs(as.numeric(difftime(ct, ko, units = "days"))) <= 2) |>
    group_by(game_id) |> slice_max(t, n = 1, with_ties = FALSE) |> ungroup()
  g <- games |> select(game_id, home_team, away_team) |> inner_join(lx |> select(game_id, sp, total), by = "game_id")
  ## line_history's home_spread is + when the home team is favored (as nflverse spread_line; see 45)
  bind_rows(g |> transmute(game_id, team = home_team, implied = (total + sp) / 2),
            g |> transmute(game_id, team = away_team, implied = (total - sp) / 2))
}, error = function(e) tibble(game_id = character(), team = character(), implied = numeric()))
if (nrow(cur)) {
  cur <- cur |> left_join(gl, by = c("game_id", "team")) |> left_join(imp, by = c("game_id", "team")) |>
    mutate(locked = NOW >= ko, include = core | td_keep)
}

## ---- 4b. ESPN / Sleeper projections and FantasyPros ECR (ext_player_utils.R): snapshot, then each player's
##          last pull before his kickoff (NO_EXT=1 skips the pulls) ----
XP_CSV <- file.path(PROJ_DIR, sprintf("data/players/ext_players_%d_wk%02d.csv", SEASON, WEEK))
if (!nzchar(Sys.getenv("NO_EXT")) && (any(games$ko > NOW) || !file.exists(XP_CSV)))   # after the week: one pull, for the fallback
  tryCatch(xp_snapshot(XP_CSV, SEASON, WEEK, NOW, games$ko), error = function(e) message("players: ESPN / Sleeper / ECR — ", conditionMessage(e)))
## games that kicked off with no ECR pulled before kickoff (a late publish, a missed run): the DynastyProcess
## file's version from just before kickoff, from its git history
if (!nzchar(Sys.getenv("NO_EXT")))
  tryCatch(xp_ecr_backfill(XP_CSV, gl |> distinct(team, ko), NOW, games$ko), error = function(e) message("players: ECR history — ", conditionMessage(e)))
if (nrow(cur)) {
  xa <- tryCatch(xp_attach(XP_CSV, cur |> distinct(gsis_id, player_name, pos, team), gl |> distinct(team, ko)),
                 error = function(e) { message("players: ESPN / Sleeper / ECR attach — ", conditionMessage(e)); NULL })
  if (!is.null(xa) && nrow(xa)) {
    for (src in c("ESPN", "Sleeper")) {
      e <- xa |> filter(source == src); i <- match(cur$gsis_id, e$gsis_id); lo <- tolower(src)
      for (f in names(PS_FORMATS)) { cur[[paste0(lo, "_rk_", f)]] <- e[[paste0("rk_pts_", f)]][i]; cur[[paste0(lo, "_pts_", f)]] <- e[[paste0("pts_", f)]][i] }
    }
    e <- xa |> filter(source == "ECR"); i <- match(cur$gsis_id, e$gsis_id)
    for (c in c("ecr_rank", "ecr_avg", "ecr_sd", "ecr_best", "ecr_worst", "ecr_date")) cur[[c]] <- e[[c]][i]
    cur$ecr_pts <- e$pts_ppr[i]
    message(sprintf("players: matched ESPN %d, Sleeper %d, ECR %d of %d players shown", sum(!is.na(cur$espn_rk_half[cur$include])),
                    sum(!is.na(cur$sleeper_rk_half[cur$include])), sum(!is.na(cur$ecr_rank[cur$include])), sum(cur$include)))
  }
}

## ---- 5. Page ----
P <- list(season = SEASON, week = WEEK, now = NOW, cur = cur, hist = hist, games = games, bundle_time = B$created,
          n_games = nrow(games), n_priced = if (nrow(cur)) n_distinct(cur$game_id) else 0L,
          n_locked = sum(NOW >= games$ko), books = if (nrow(live)) median(live$n_books[live$t == max(live$t)], na.rm = TRUE) else NA,
          last_pull = if (nrow(live)) max(live$t) else as.POSIXct(NA), bb = B$bb, site_base = SITE_BASE, unmatched = nrow(unm))
html <- player_page(P)
dir.create(file.path(SITE_DIR, "players", "archive"), recursive = TRUE, showWarnings = FALSE)
arch_name <- sprintf("players_%d_wk%02d.html", SEASON, WEEK)
writeLines(html, file.path(SITE_DIR, "players", "index.html")); writeLines(html, file.path(SITE_DIR, "players", "archive", arch_name))
arch <- sort(list.files(file.path(SITE_DIR, "players", "archive"), pattern = "^players_.*\\.html$"), decreasing = TRUE)
writeLines(c('<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Player archive</title>',
             '<style>body{font-family:system-ui,sans-serif;max-width:700px;margin:2rem auto;padding:0 16px}</style></head><body><h1>Players: past weeks</h1><ul>',
             sprintf('<li><a href="%s">%s</a></li>', arch, sub("players_(\\d{4})_wk(\\d{2})\\.html", "\\1 week \\2", arch)),
             sprintf('</ul><p><a href="%splayers/">Current week</a> · <a href="%s">D/ST projections</a></p></body></html>', SITE_BASE, SITE_BASE)),
           file.path(SITE_DIR, "players", "archive", "index.html"))
message(sprintf("players: page written (%d players shown, %d games with props, %d locked)",
                if (nrow(cur)) sum(cur$include) else 0L, P$n_priced, P$n_locked))
