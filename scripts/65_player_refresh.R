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
CI_CSV <- file.path(PROJ_DIR, sprintf("data/players/props_ci_%d_wk%02d.csv", SEASON, WEEK))
LIVE_COLS <- c("pulled_at", "game_id", "event_id", "commence_time", "market", "player", "n_books", "line", "p_over", "med_est",
               "line_min", "line_max", "p_td_raw")
message(sprintf("players: %d week %d, %d games, %d started", SEASON, WEEK, nrow(games), sum(NOW >= games$ko)))

## ---- 2. Pull props for games not started ----
API <- Sys.getenv("ODDS_API_BASE", "https://api.the-odds-api.com/v4/sports/americanfootball_nfl")
KEY <- gsub("^[\"' ]+|[\"' ]+$", "", trimws(Sys.getenv("PROPS_API_KEY"))); MOCK <- Sys.getenv("PLAYER_PROPS_MOCK")   # tolerate a pasted newline / quotes
if (nzchar(KEY)) message(sprintf("players: PROPS_API_KEY is %d characters (Odds API keys are 32)", nchar(KEY)))
MIN_LEFT <- as.numeric(Sys.getenv("PROPS_MIN_REMAINING", "300"))
get_json <- function(path, query) {
  if (nzchar(MOCK)) { M <- readRDS(MOCK); return(list(body = if (path == "/events") M$events else M$odds[[sub("^/events/([^/]+)/odds$", "\\1", path)]],
                                                     remaining = 99999, used = 0)) }
  url <- paste0(API, path, "?", paste(names(query), vapply(query, utils::URLencode, "", reserved = TRUE), sep = "=", collapse = "&"))
  r <- curl::curl_fetch_memory(url)
  if (r$status_code != 200) {
    msg <- tryCatch({ j <- jsonlite::fromJSON(rawToChar(r$content)); paste(c(j$error_code, j$message), collapse = ": ") }, error = function(e) substr(rawToChar(r$content), 1, 200))
    stop(sprintf("HTTP %d for %s (%s)%s", r$status_code, path, msg,
                 if (r$status_code == 401) " -- 401 = the PROPS_API_KEY secret is wrong / expired, or the plan's credits are used up" else ""))
  }
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
  ## ladders (*_alternate markets): "auto" = only for games whose last stored pull has fewer than LADDER_MIN_MAIN
  ## players with a main yardage / receptions line (early in the week); "always" / "never". Credits are charged
  ## only for markets a book actually returns.
  LADDERS <- Sys.getenv("PROPS_LADDERS", "auto"); LADDER_MIN_MAIN <- 12
  prev <- if (file.exists(LIVE_CSV)) read.csv(LIVE_CSV, stringsAsFactors = FALSE) |> filter(market %in% c("player_pass_yds", "player_rush_yds", "player_reception_yds", "player_receptions")) |>
    group_by(game_id) |> filter(pulled_at == max(pulled_at)) |> summarise(n = n_distinct(player), .groups = "drop") else tibble(game_id = character(), n = integer())
  lad_games <- switch(LADDERS, always = m$game_id, never = character(), setdiff(m$game_id, prev$game_id[prev$n >= LADDER_MIN_MAIN]))
  longs <- list()
  for (i in seq_len(nrow(m))) {
    if (!is.na(left) && left < MIN_LEFT) { message(sprintf("players: stopping, %s credits left (< %s)", left, MIN_LEFT)); break }
    mk <- c(PROP_MARKETS, if (m$game_id[i] %in% lad_games) PROP_ALT_MARKETS)
    r <- tryCatch(get_json(sprintf("/events/%s/odds", m$event_id[i]),
                           list(apiKey = KEY, regions = "us", markets = paste(mk, collapse = ","), oddsFormat = "american")),
                  error = function(e) { message("players: ", m$game_id[i], " — ", conditionMessage(e)); NULL })
    if (is.null(r)) next
    left <- r$remaining; credits <- credits + coalesce(r$used, 0)
    long <- flatten_event_odds(r$body)
    if (!nrow(long)) next
    long$game_id <- m$game_id[i]
    longs[[length(longs) + 1]] <- long
    pulled <- pulled + 1
  }
  if (length(longs)) {
    L <- ladder_merge(bind_rows(longs))
    if (attr(L, "ladder_n") > 0) message(sprintf("players: ladders used for %d book x player x stat lines without a main line (Over-only de-vig k = %.3f, %s)",
      attr(L, "ladder_n"), attr(L, "ladder_k"), if (attr(L, "ladder_k_n") >= 5) sprintf("from %d players with both, middle half %s", attr(L, "ladder_k_n"), attr(L, "ladder_k_iqr")) else "default"))
    ou <- props_consensus(book_lines(L)); td <- props_anytime(L)
    new <- bind_rows(ou |> transmute(game_id, market, player, n_books, line, p_over, med_est, line_min, line_max),
                     td |> transmute(game_id, market, player, n_books, p_td_raw)) |>
      left_join(m |> select(game_id, event_id, commence_time), by = "game_id") |> mutate(pulled_at = isoz(NOW))
    for (c in setdiff(LIVE_COLS, names(new))) new[[c]] <- NA
    dir.create(dirname(LIVE_CSV), recursive = TRUE, showWarnings = FALSE)
    write.table(new[LIVE_COLS], LIVE_CSV, sep = ",", row.names = FALSE, col.names = !file.exists(LIVE_CSV), append = file.exists(LIVE_CSV), qmethod = "double")
    longs <- list(L)
  }
  message(sprintf("players: pulled props for %d games (%s credits used, %s left)", pulled, credits, left))
  ## "±": resample the books and the calibration refits (player_site_utils.R ps_ci), stored per pull
  if (length(longs)) tryCatch({
    t1 <- Sys.time(); ci <- ps_ci(bind_rows(longs), B, nboot = as.integer(Sys.getenv("CI_BOOT", "60")))
    if (!is.null(ci) && nrow(ci)) {
      ci <- ci |> mutate(pulled_at = isoz(NOW)) |> relocate(pulled_at)
      write.table(ci, CI_CSV, sep = ",", row.names = FALSE, col.names = !file.exists(CI_CSV), append = file.exists(CI_CSV), qmethod = "double")
      message(sprintf("players: 90%% intervals for %d players (%.0f s)", nrow(ci), as.numeric(difftime(Sys.time(), t1, units = "secs"))))
    } }, error = function(e) message("players: intervals — ", conditionMessage(e)))
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
  bind_rows(g |> transmute(game_id, team = home_team, implied = (total + sp) / 2, spread = sp),
            g |> transmute(game_id, team = away_team, implied = (total - sp) / 2, spread = -sp))
}, error = function(e) tibble(game_id = character(), team = character(), implied = numeric(), spread = numeric()))
## ---- 4a. Games without (enough) props yet: history x implied total and spread (ps_fallback, spec from 79;
##          Andrew 2026-09-29). A game counts as priced once its newest pre-kickoff pull has FB_MIN_CORE core
##          players; until then its pool players without a yardage / receptions prop get the fallback
##          (players with one keep their props). Shown in italics, without ± or love / fade flags. ----
FB_MIN_CORE <- as.integer(Sys.getenv("FB_MIN_CORE", "6"))
if (nrow(cur)) cur$fallback <- FALSE
n_core <- if (nrow(cur)) cur |> group_by(game_id) |> summarise(n = sum(core), .groups = "drop") else tibble(game_id = character(), n = integer())
thin <- setdiff(games$game_id, n_core$game_id[n_core$n >= FB_MIN_CORE])
n_fb <- 0L
if (length(thin)) {
  fbr <- tryCatch(ps_fallback(B, gl |> filter(game_id %in% thin) |> select(game_id, team), imp),
                  error = function(e) { message("players: fallback — ", conditionMessage(e)); tibble() })
  if (nrow(fbr)) {
    has_core <- if (nrow(cur)) cur |> filter(core) |> select(game_id, gsis_id) else tibble(game_id = character(), gsis_id = character())
    fbr <- fbr |> anti_join(has_core, by = c("game_id", "gsis_id")) |> mutate(t = as.POSIXct(NA, tz = "UTC"), td_prop = FALSE)
    ## an anytime-TD price already posted (often the first prop up): use it for his TDs instead of his history
    tdp <- if (nrow(cur)) cur |> filter(!core, !is.na(p_td_raw)) |> select(game_id, gsis_id, p_td_new = p_td_raw, e_td_new = e.tds, n_td = n_books.tds) else tibble()
    if (nrow(tdp)) {
      fbr <- fbr |> left_join(tdp, by = c("game_id", "gsis_id"))
      i <- !is.na(fbr$p_td_new)
      for (f in names(PS_FORMATS)) fbr[[paste0("vfp_", f)]][i] <- pmax(fbr[[paste0("vfp_", f)]][i] + 6 * (fbr$e_td_new[i] - fbr$e.tds[i]), 0)
      fbr$e.tds[i] <- fbr$e_td_new[i]; fbr$src.tds[i] <- "prop"; fbr$p_td_raw[i] <- fbr$p_td_new[i]; fbr$td_prop[i] <- TRUE
      fbr$n_books.tds <- fbr$n_td
      bbn <- ps_bb(fbr |> select(starts_with("vfp_")), fbr$p_td_raw, fbr$pos, B$bb)
      fbr[names(bbn)] <- bbn
      fbr <- fbr |> select(-p_td_new, -e_td_new, -n_td)
      message(sprintf("players: %d of them with a posted anytime-TD price", sum(i)))
    }
    if (nrow(cur)) cur <- cur |> anti_join(fbr |> select(game_id, gsis_id), by = c("game_id", "gsis_id"))   # TD-only rows give way
    cur <- bind_rows(cur, fbr); n_fb <- nrow(fbr)
  }
  message(sprintf("players: %d games without enough props yet: %d players projected from history x implied total", length(thin), n_fb))
}
if (nrow(cur)) {
  cur <- cur |> left_join(gl, by = c("game_id", "team")) |> left_join(imp, by = c("game_id", "team")) |>
    mutate(locked = NOW >= ko, include = core | td_keep | fallback)
  ## "±" from the same pull as each game's projection
  if (file.exists(CI_CSV)) {
    ci <- read.csv(CI_CSV, stringsAsFactors = FALSE) |> mutate(t = utc(pulled_at)) |> select(-pulled_at) |>
      distinct(game_id, gsis_id, t, .keep_all = TRUE)
    cur <- cur |> select(-any_of(grep("^ci_", names(cur), value = TRUE))) |> left_join(ci |> select(-any_of("draws")), by = c("game_id", "gsis_id", "t"))
  }
  rg <- tryCatch(ps_ranges(cur |> select(starts_with("vfp_")), cur$pos, cur$p_td_raw, cur$implied, cur$spread, B$ranges, B$bb),
                 error = function(e) { message("players: ranges — ", conditionMessage(e)); NULL })
  if (!is.null(rg)) cur <- bind_cols(cur |> select(-any_of(names(rg))), rg)
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
INJ_CSV <- file.path(PROJ_DIR, sprintf("data/players/injury_%d_wk%02d.csv", SEASON, WEEK))
if (!nzchar(Sys.getenv("NO_EXT")) && (any(games$ko > NOW) || !file.exists(INJ_CSV)))
  tryCatch(inj_snapshot(INJ_CSV, SEASON, WEEK, NOW), error = function(e) message("players: injuries — ", conditionMessage(e)))
if (nrow(cur)) {
  ij <- tryCatch(inj_attach(INJ_CSV, cur |> distinct(gsis_id, player_name, team, ko)), error = function(e) { message("players: injury attach — ", conditionMessage(e)); NULL })
  if (!is.null(ij) && nrow(ij)) {
    cur <- cur |> select(-any_of(c("inj_status", "inj_out", "inj_badge", "inj_detail"))) |>
      left_join(ij |> select(gsis_id, inj_status, inj_out, inj_badge, inj_detail), by = "gsis_id")
    cur <- cur |> mutate(include = include & !(fallback & inj_out %in% TRUE))   # no props yet and ruled out: not shown
    message(sprintf("players: injury status for %d shown players (%s)", sum(!is.na(cur$inj_status[cur$include])),
                    paste(names(table(cur$inj_badge[cur$include & nzchar(coalesce(cur$inj_badge, ""))])), table(cur$inj_badge[cur$include & nzchar(coalesce(cur$inj_badge, ""))]), collapse = ", ")))
  }
}
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

## no-props QBs: the team's starter; his backup (fb_qb 2) only when the starter is ruled out or doubtful
if (nrow(cur) && "fb_qb" %in% names(cur)) {
  out1 <- if ("inj_out" %in% names(cur)) cur |> filter(fallback, fb_qb %in% 1, inj_out %in% TRUE) |> distinct(game_id, team) else tibble(game_id = character(), team = character())
  cur <- cur |> mutate(include = include & !(fallback & fb_qb %in% 2 & !paste(game_id, team) %in% paste(out1$game_id, out1$team)))
}

## ---- 5. Page ----
P <- list(season = SEASON, week = WEEK, now = NOW, cur = cur, hist = hist, games = games, bundle_time = B$created,
          n_games = nrow(games), n_priced = nrow(games) - length(thin), n_fallback = length(thin), fb_skill = B$fallback$skill,
          n_locked = sum(NOW >= games$ko), books = if (nrow(live)) median(live$n_books[live$t == max(live$t)], na.rm = TRUE) else NA,
          last_pull = if (nrow(live)) max(live$t) else as.POSIXct(NA), bb = B$bb, site_base = SITE_BASE, unmatched = nrow(unm),
          track = { tf <- file.path(PROJ_DIR, "output/players_site/track_players.rds"); if (file.exists(tf)) readRDS(tf) })
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
