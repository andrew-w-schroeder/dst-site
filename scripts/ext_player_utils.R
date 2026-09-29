# ==============================================================================
# ext_player_utils.R — ESPN, Sleeper and FantasyPros ECR weekly QB / RB / WR / TE projections for the
# Players page (65_player_refresh.R). Same sources and conventions as ext_utils.R (D/ST and kickers).
#   Sleeper : api.sleeper.com/projections/nfl/<season>/<week> (public; Rotowire's numbers). Projected stats
#             re-scored into our four formats with our scoring (player_site_utils.R ps_score).
#   ESPN    : lm-api-reads.fantasy.espn.com kona_player_info (league defaults), ESPN's projected stats
#             re-scored the same way (stat ids: 3 pass yds, 4 pass TD, 20 INT, 24 rush yds, 25 rush TD,
#             53 receptions, 42 rec yds, 43 rec TD, 72 fumbles lost, 19 / 26 / 44 two-point conversions).
#   ECR     : FantasyPros expert consensus rankings, as scraped twice a day (about 10 am / 10 pm ET) by
#             DynastyProcess (github.com/dynastyprocess/data, files/fp_latest_weekly.csv). Weekly pages
#             qb, ppr-rb, ppr-wr, ppr-te: RB / WR / TE ranks are PPR ranks in every format. Only rows whose
#             listed game kicks off within 2 days of one of this week's games are used (stale-week guard).
# Snapshots go to data/players/ext_players_<season>_wk<ww>.csv; the page uses each player's last pull
# before his game's kickoff. Base R + dplyr / tidyr / purrr + jsonlite / curl (runs on the GitHub runner).
# EXT_MOCK_DIR=<dir> reads sleeper_<pos>.json, espn.json and ecr.csv from a folder instead (tests).
# ==============================================================================

XP_COLS <- c("pulled_at", "source", "name", "team", "pos", "pts_std", "pts_half", "pts_ppr", "pts_ffpc",
             "ecr_rank", "ecr_avg", "ecr_sd", "ecr_best", "ecr_worst", "ecr_date")
XP_KEEP <- c(QB = 40, RB = 80, WR = 110, TE = 50)               # keep each source's top N per position
xp_get <- function(url, headers = NULL, mock = NULL) {
  md <- Sys.getenv("EXT_MOCK_DIR")
  if (nzchar(md)) { f <- file.path(md, mock); return(if (file.exists(f)) paste(readLines(f, warn = FALSE), collapse = "\n") else NULL) }
  h <- curl::new_handle(timeout = 45, useragent = "Mozilla/5.0 (personal projections page)")
  if (length(headers)) curl::handle_setheaders(h, .list = headers)
  r <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) { message("  ", sub("\\?.*", "", url), ": ", conditionMessage(e)); NULL })
  if (is.null(r) || r$status_code != 200) { if (!is.null(r)) message(sprintf("  %s: HTTP %d", sub("\\?.*", "", url), r$status_code)); return(NULL) }
  txt <- rawToChar(r$content); Encoding(txt) <- "UTF-8"; txt
}
.xn <- function(x) if (is.null(x) || !length(x)) NA_real_ else suppressWarnings(as.numeric(x[[1]]))
.xc <- function(x) if (is.null(x) || !length(x)) NA_character_ else as.character(x[[1]])
xp_team <- function(x) dplyr::recode(x, OAK = "LV", SD = "LAC", STL = "LA", LAR = "LA", JAC = "JAX", WSH = "WAS", ARZ = "ARI")
## projected stats -> points in the four formats (our scoring)
xp_points <- function(d) {
  proj <- tibble::tibble(pos = d$pos, pass_yds = d$pass_yds, pass_td = d$pass_td, pass_int = d$pass_int, rush_yds = d$rush_yds,
                         rec = d$rec, rec_yds = d$rec_yds, rush_td = d$rush_td, rec_td = d$rec_td, fum_lost = d$fum_lost, two_pt = d$two_pt)
  proj[is.na(proj)] <- 0
  for (f in c("std", "half", "ppr", "ffpc")) d[[paste0("pts_", f)]] <- ps_score(proj, f)
  d
}

xp_sleeper <- function(season, week) {
  purrr::map_dfr(c("QB", "RB", "WR", "TE"), function(pos) {
    txt <- xp_get(sprintf("https://api.sleeper.com/projections/nfl/%d/%d?season_type=regular&position[]=%s", season, week, pos),
                  mock = paste0("sleeper_", pos, ".json"))
    if (is.null(txt)) return(NULL)
    js <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
    if (!length(js)) return(NULL)
    purrr::map_dfr(js, function(e) { st <- e$stats; p <- e$player
      if (is.null(st) || is.null(p)) return(NULL)
      if (!is.na(.xn(st$gp)) && .xn(st$gp) == 0) return(NULL)
      g <- function(k) .xn(st[[k]])
      tibble::tibble(source = "Sleeper", name = trimws(paste(.xc(p$first_name), .xc(p$last_name))), team = .xc(e$team %||% p$team), pos = pos,
                     pass_yds = g("pass_yd"), pass_td = g("pass_td"), pass_int = g("pass_int"), rush_yds = g("rush_yd"), rush_td = g("rush_td"),
                     rec = g("rec"), rec_yds = g("rec_yd"), rec_td = g("rec_td"), fum_lost = g("fum_lost"),
                     two_pt = sum(c(g("pass_2pt"), g("rush_2pt"), g("rec_2pt")), na.rm = TRUE)) })
  })
}

XP_ESPN_POS <- c(`1` = "QB", `2` = "RB", `3` = "WR", `4` = "TE")
XP_ESPN_TEAM <- c(`1` = "ATL", `2` = "BUF", `3` = "CHI", `4` = "CIN", `5` = "CLE", `6` = "DAL", `7` = "DEN", `8` = "DET", `9` = "GB", `10` = "TEN",
                  `11` = "IND", `12` = "KC", `13` = "LV", `14` = "LA", `15` = "MIA", `16` = "MIN", `17` = "NE", `18` = "NO", `19` = "NYG", `20` = "NYJ",
                  `21` = "PHI", `22` = "ARI", `23` = "PIT", `24` = "LAC", `25` = "SF", `26` = "SEA", `27` = "TB", `28` = "WAS", `29` = "CAR", `30` = "JAX",
                  `33` = "BAL", `34` = "HOU")
xp_espn <- function(season, week) {
  flt <- list(filterSlotIds = list(value = c(0L, 2L, 4L, 6L)), filterStatsForSourceIds = list(value = c(0L, 1L)),
              filterStatsForSplitTypeIds = list(value = I(1L)), limit = 1000L, sortPercOwned = list(sortPriority = 1L, sortAsc = FALSE),
              filterStatsForTopScoringPeriodIds = list(value = 25L, additionalValue = list(sprintf("11%d%d", season, week), sprintf("01%d%d", season, week))))
  url <- sprintf("https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/%d/segments/0/leaguedefaults/3?scoringPeriodId=%d&view=kona_player_info", season, week)
  hdr <- c(`X-Fantasy-Filter` = as.character(jsonlite::toJSON(list(players = flt), auto_unbox = TRUE)), Accept = "application/json")
  txt <- xp_get(url, hdr, mock = "espn.json"); if (is.null(txt)) return(NULL)
  js <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL); if (is.null(js)) return(NULL)
  tid <- XP_ESPN_TEAM
  purrr::map_dfr(js$players, function(pe) {
    p <- if (!is.null(pe$player)) pe$player else pe
    pos <- unname(XP_ESPN_POS[as.character(.xn(p$defaultPositionId))]); if (!length(pos) || is.na(pos)) return(NULL)
    for (s in p$stats) if (identical(.xn(s$seasonId), as.numeric(season)) && identical(.xn(s$scoringPeriodId), as.numeric(week)) &&
                           identical(.xn(s$statSourceId), 1) && identical(.xn(s$statSplitTypeId), 1)) {
      g <- function(k) .xn(s$stats[[k]])
      tm <- unname(tid[as.character(.xn(s$proTeamId))]); if (is.na(tm)) tm <- unname(tid[as.character(.xn(p$proTeamId))])
      return(tibble::tibble(source = "ESPN", name = .xc(p$fullName), team = tm, pos = pos,
                            pass_yds = g("3"), pass_td = g("4"), pass_int = g("20"), rush_yds = g("24"), rush_td = g("25"),
                            rec = g("53"), rec_yds = g("42"), rec_td = g("43"), fum_lost = g("72"),
                            two_pt = sum(c(g("19"), g("26"), g("44")), na.rm = TRUE)))
    }
    NULL
  })
}

## FantasyPros ECR (DynastyProcess scrape); ko = this week's kickoffs (POSIXct, UTC) for the stale-week guard
xp_ecr_parse <- function(txt, ko) {
  x <- tryCatch(utils::read.csv(text = txt, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(x) || !nrow(x)) return(NULL)
  x <- x[x$page %in% c("qb", "ppr-rb", "ppr-wr", "ppr-te") & x$pos %in% c("QB", "RB", "WR", "TE"), ]
  kt <- as.POSIXct(as.numeric(x$player_game_kickoff_ts), origin = "1970-01-01", tz = "UTC")
  near <- vapply(kt, function(t) !is.na(t) && any(abs(as.numeric(difftime(ko, t, units = "days"))) <= 2), TRUE)
  x <- x[near, ]
  if (!nrow(x)) return(NULL)
  tibble::tibble(source = "ECR", name = x$player_name, team = x$team, pos = x$pos,
                 ecr_rank = as.numeric(x$rank), ecr_avg = as.numeric(x$ecr), ecr_sd = as.numeric(x$sd),
                 ecr_best = as.numeric(x$best), ecr_worst = as.numeric(x$worst), ecr_date = as.character(x$scrape_date),
                 pts_ppr = suppressWarnings(as.numeric(x$r2p_pts)))
}
xp_ecr <- function(ko) {
  txt <- xp_get("https://raw.githubusercontent.com/dynastyprocess/data/master/files/fp_latest_weekly.csv", mock = "ecr.csv")
  if (is.null(txt)) return(NULL)
  x <- xp_ecr_parse(txt, ko)
  if (is.null(x)) message("  ECR: no rows for this week's games yet")
  x
}
## the ECR file as it was just before `before` (POSIXct UTC): newest commit touching it (GitHub API; uses
## GITHUB_TOKEN when set, as in the workflow) -> that version. Returns list(rows, time) or NULL.
xp_ecr_asof <- function(before, ko) {
  api <- sprintf("https://api.github.com/repos/dynastyprocess/data/commits?path=files/fp_latest_weekly.csv&until=%s&per_page=1",
                 format(before, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  tok <- Sys.getenv("GITHUB_TOKEN")
  hdr <- c(Accept = "application/vnd.github+json", if (nzchar(tok)) c(Authorization = paste("Bearer", tok)))
  js <- xp_get(api, hdr, mock = "ecr_commits.json"); if (is.null(js)) return(NULL)
  cm <- tryCatch(jsonlite::fromJSON(js, simplifyVector = FALSE), error = function(e) NULL)
  if (!length(cm)) return(NULL)
  sha <- cm[[1]]$sha; tm <- as.POSIXct(sub("Z$", "", cm[[1]]$commit$committer$date), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  if (is.na(tm) || tm >= before) return(NULL)
  txt <- xp_get(sprintf("https://raw.githubusercontent.com/dynastyprocess/data/%s/files/fp_latest_weekly.csv", sha), mock = "ecr_asof.csv")
  if (is.null(txt)) return(NULL)
  rows <- xp_ecr_parse(txt, ko); if (is.null(rows)) return(NULL)
  list(rows = rows, time = tm)
}
## add a pre-kickoff ECR snapshot for every started team that has none in the weekly file
xp_ecr_backfill <- function(file, team_ko, now, ko_all) {
  have <- if (file.exists(file)) {
    h <- utils::read.csv(file, stringsAsFactors = FALSE, colClasses = c(pulled_at = "character", team = "character"))
    h <- h[h$source == "ECR", ]; h$t <- as.POSIXct(sub("Z$", "", h$pulled_at), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    h <- merge(h, team_ko, by = "team"); unique(h$team[h$t < h$ko]) } else character()
  need <- team_ko[team_ko$ko <= now & !team_ko$team %in% have, ]
  if (!nrow(need)) return(invisible(NULL))
  added <- 0L
  for (k in sort(unique(need$ko))) {
    k <- as.POSIXct(k, origin = "1970-01-01", tz = "UTC")
    r <- xp_ecr_asof(k, ko_all); if (is.null(r)) next
    x <- r$rows; x$team <- xp_team(x$team)
    x <- x[x$team %in% need$team[need$ko == k], ]
    if (!nrow(x)) next
    x$pulled_at <- format(r$time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    for (c in setdiff(XP_COLS, names(x))) x[[c]] <- NA
    utils::write.table(x[XP_COLS], file, sep = ",", row.names = FALSE, col.names = !file.exists(file), append = file.exists(file), qmethod = "double")
    added <- added + nrow(x)
  }
  if (added) message(sprintf("players: ECR from before kickoff (DynastyProcess history) added for %d players", added))
  invisible(added)
}

## pull all three, keep each source's top N per position, append to the weekly snapshot file
xp_snapshot <- function(file, season, week, now, ko) {
  x <- dplyr::bind_rows(
    tryCatch(xp_points(xp_sleeper(season, week)), error = function(e) { message("  Sleeper: ", conditionMessage(e)); NULL }),
    tryCatch(xp_points(xp_espn(season, week)), error = function(e) { message("  ESPN: ", conditionMessage(e)); NULL }),
    tryCatch(xp_ecr(ko), error = function(e) { message("  ECR: ", conditionMessage(e)); NULL }))
  if (!nrow(x)) { message("players: ESPN / Sleeper / ECR: nothing pulled"); return(invisible(NULL)) }
  x$team <- xp_team(x$team)
  for (c in c("ecr_rank", "pts_half")) if (!c %in% names(x)) x[[c]] <- NA_real_   # e.g. no ECR rows yet on Tuesday
  x <- x |> dplyr::filter(!is.na(name)) |>
    dplyr::mutate(ord = dplyr::if_else(source == "ECR", ecr_rank, -pts_half)) |>
    dplyr::group_by(source, pos) |> dplyr::arrange(ord, .by_group = TRUE) |>
    dplyr::filter(dplyr::row_number() <= XP_KEEP[pos[1]]) |> dplyr::ungroup()
  x$pulled_at <- format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  for (c in setdiff(XP_COLS, names(x))) x[[c]] <- NA
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(x[XP_COLS], file, sep = ",", row.names = FALSE, col.names = !file.exists(file), append = file.exists(file), qmethod = "double")
  tb <- table(x$source); message("players: ESPN / Sleeper / ECR snapshot: ", paste(names(tb), tb, collapse = ", "))
  invisible(x)
}

## per source x player: the last snapshot before his team's kickoff (games already started keep it), ranked
## within the source's own position list, then matched to our gsis ids by name + team (else name + position
## if unique). players: gsis_id, player_name, pos, team ; team_ko: team, ko (POSIXct UTC)
xp_attach <- function(file, players, team_ko) {
  if (!file.exists(file) || !nrow(players)) return(NULL)
  h <- tibble::as_tibble(utils::read.csv(file, stringsAsFactors = FALSE, colClasses = c(pulled_at = "character", name = "character", team = "character", ecr_date = "character")))
  h$t <- as.POSIXct(sub("Z$", "", h$pulled_at), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  ## ESPN / Sleeper keep a week's projections after it is played, so when no pull came before kickoff (a page
  ## built after the games) their first pull after kickoff is used; ECR has no such fallback (its file moves on)
  h <- h |> dplyr::inner_join(team_ko, by = "team") |> dplyr::mutate(pre = t < ko) |>
    dplyr::filter(pre | source != "ECR") |> dplyr::mutate(key = name_key(name))
  if (!nrow(h)) return(NULL)
  h <- h |> dplyr::group_by(source, key, team) |> dplyr::mutate(ord = ifelse(pre, -as.numeric(t), 1e12 + as.numeric(t))) |>
    dplyr::slice_min(ord, n = 1, with_ties = FALSE) |> dplyr::ungroup() |>
    dplyr::group_by(source, pos) |>
    dplyr::mutate(dplyr::across(c(pts_std, pts_half, pts_ppr, pts_ffpc), \(v) rank(-v, ties.method = "first", na.last = "keep"), .names = "rk_{.col}")) |>
    dplyr::ungroup()
  pl <- players |> dplyr::mutate(key = name_key(player_name))
  m1 <- h |> dplyr::inner_join(pl |> dplyr::select(gsis_id, key, team), by = c("key", "team"))
  uniq <- pl |> dplyr::group_by(key, pos) |> dplyr::filter(dplyr::n() == 1) |> dplyr::ungroup()
  m2 <- h |> dplyr::anti_join(m1, by = c("source", "key", "team")) |> dplyr::inner_join(uniq |> dplyr::select(gsis_id, key, pos), by = c("key", "pos"))
  dplyr::bind_rows(m1, m2) |> dplyr::distinct(source, gsis_id, .keep_all = TRUE)
}

# ==============================================================================
# Injury report for QB / RB / WR / TE (Andrew 2026-09-29), same sources and rule as starters.R (QBs, kickers):
#   report  : nflverse injuries_<season>.rds, the official NFL injury report (practice Wed-Fri, game status Fri/Sat)
#   sleeper : api.sleeper.app/v1/players/nfl, injury status for every player (one call)
# Status: IR / PUP / NFI / suspended from any source; otherwise the official game status once it exists this
# week; before that Out / Doubtful from Sleeper; otherwise Questionable if either says so. Each refresh appends a
# snapshot to data/players/injury_<season>_wk<ww>.csv; each player keeps his last status before kickoff (a game
# with no pre-kickoff snapshot, e.g. a week published late, uses the official report only).
# ==============================================================================
INJ_COLS <- c("pulled_at", "source", "gsis_id", "name", "team", "pos", "status", "practice", "injury")
INJ_HARD <- c("IR", "PUP", "NFI", "Suspended")
inj_canon <- function(x) {
  u <- toupper(trimws(as.character(x)))
  dplyr::case_when(is.na(u) | u %in% c("", "NA", "P", "PROBABLE", "ACTIVE", "HEALTHY", "NULL") ~ NA_character_,
                   u %in% c("O", "OUT", "COV", "DNR", "INACTIVE") ~ "Out", u %in% c("D", "DOUBTFUL") ~ "Doubtful",
                   u %in% c("Q", "QUESTIONABLE", "GTD") ~ "Questionable",
                   u %in% c("IR", "IR-R", "INJURED RESERVE", "INJURED_RESERVE", "RESERVE/INJURED") ~ "IR",
                   u %in% c("PUP", "PHYSICALLY UNABLE TO PERFORM") ~ "PUP", u %in% c("NFI", "NON-FOOTBALL INJURY") ~ "NFI",
                   u %in% c("SUS", "SUSP", "SUSPENDED", "SUSPENSION") ~ "Suspended", TRUE ~ NA_character_)
}
INJ_ABBR <- c(Out = "O", Doubtful = "D", Questionable = "Q", Suspended = "SUS", IR = "IR", PUP = "PUP", NFI = "NFI")
inj_report <- function(season, week) {
  md <- Sys.getenv("EXT_MOCK_DIR")
  d <- if (nzchar(md)) { f <- file.path(md, "injuries.rds"); if (file.exists(f)) readRDS(f) else NULL } else {
    tmp <- tempfile(fileext = ".rds")
    ok <- tryCatch(utils::download.file(sprintf("https://github.com/nflverse/nflverse-data/releases/download/injuries/injuries_%d.rds", season),
                                        tmp, mode = "wb", quiet = TRUE) == 0, error = function(e) FALSE)
    if (ok) readRDS(tmp) else NULL }
  if (is.null(d)) return(NULL)
  d <- tibble::as_tibble(d); d <- d[d$week == week & d$position %in% c("QB", "RB", "WR", "TE", "FB"), ]
  if (!nrow(d)) return(NULL)
  tibble::tibble(source = "report", gsis_id = d$gsis_id, name = d$full_name, team = xp_team(d$team),
                 pos = ifelse(d$position == "FB", "RB", d$position), status = inj_canon(d$report_status),
                 practice = d$practice_status, injury = dplyr::coalesce(d$report_primary_injury, d$practice_primary_injury))
}
inj_sleeper <- function() {
  txt <- xp_get("https://api.sleeper.app/v1/players/nfl", mock = "sleeper_players.json"); if (is.null(txt)) return(NULL)
  js <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  g <- function(p, f) { v <- p[[f]]; if (is.null(v) || !length(v)) NA_character_ else as.character(v[[1]]) }
  js <- Filter(function(p) !is.null(p$team) && !is.null(p$position) && p$position %in% c("QB", "RB", "WR", "TE", "FB") &&
                 (!is.null(p$injury_status) || identical(g(p, "status"), "Injured Reserve")), js)
  if (!length(js)) return(NULL)
  tibble::tibble(source = "sleeper", gsis_id = trimws(vapply(js, g, "", "gsis_id")),
                 name = dplyr::coalesce(vapply(js, g, "", "full_name"), paste(vapply(js, g, "", "first_name"), vapply(js, g, "", "last_name"))),
                 team = xp_team(vapply(js, g, "", "team")), pos = ifelse(vapply(js, g, "", "position") == "FB", "RB", vapply(js, g, "", "position")),
                 status = dplyr::coalesce(inj_canon(vapply(js, g, "", "injury_status")), inj_canon(vapply(js, g, "", "status"))),
                 practice = vapply(js, g, "", "practice_participation"), injury = vapply(js, g, "", "injury_body_part")) |>
    dplyr::mutate(gsis_id = ifelse(grepl("^00-", gsis_id), gsis_id, NA_character_)) |> dplyr::filter(!is.na(status))
}
inj_snapshot <- function(file, season, week, now) {
  x <- dplyr::bind_rows(tryCatch(inj_report(season, week), error = function(e) { message("  injury report: ", conditionMessage(e)); NULL }),
                        tryCatch(inj_sleeper(), error = function(e) { message("  Sleeper injuries: ", conditionMessage(e)); NULL }))
  if (!nrow(x)) { message("players: injury report / Sleeper: nothing pulled"); return(invisible(NULL)) }
  x$pulled_at <- format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(x[INJ_COLS], file, sep = ",", row.names = FALSE, col.names = !file.exists(file), append = file.exists(file), qmethod = "double")
  tb <- table(x$source); message("players: injury snapshot: ", paste(names(tb), tb, collapse = ", "))
  invisible(x)
}
## per player (gsis_id, player_name, team, ko): status, badge, ruled-out flag and a detail line for the hover
inj_attach <- function(file, players) {
  if (!file.exists(file) || !nrow(players)) return(NULL)
  h <- tibble::as_tibble(utils::read.csv(file, stringsAsFactors = FALSE, colClasses = "character"))
  h$t <- as.POSIXct(sub("Z$", "", h$pulled_at), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"); h$key <- name_key(h$name)
  pl <- players |> dplyr::mutate(key = name_key(player_name))
  m <- dplyr::bind_rows(h |> dplyr::filter(!is.na(gsis_id), nzchar(gsis_id)) |> dplyr::inner_join(pl |> dplyr::select(pid = gsis_id, ko), by = c("gsis_id" = "pid")),
                        h |> dplyr::inner_join(pl |> dplyr::select(pid = gsis_id, key, team, ko), by = c("key", "team")) |> dplyr::mutate(gsis_id = pid) |> dplyr::select(-pid)) |>
    dplyr::distinct(source, gsis_id, pulled_at, .keep_all = TRUE)
  if (!nrow(m)) return(NULL)
  pre <- m |> dplyr::filter(t < ko)
  ## a team's latest pre-kickoff pull counts as a whole (a player missing from it has no status any more)
  last_pull <- pre |> dplyr::group_by(source, gsis_id) |> dplyr::summarise(tl = max(t), .groups = "drop")
  pre <- pre |> dplyr::inner_join(last_pull, by = c("source", "gsis_id")) |> dplyr::filter(t == tl)
  post_rep <- m |> dplyr::filter(t >= ko, source == "report") |> dplyr::anti_join(pre |> dplyr::distinct(gsis_id), by = "gsis_id") |>
    dplyr::group_by(gsis_id) |> dplyr::slice_min(t, n = 1, with_ties = FALSE) |> dplyr::ungroup()
  x <- dplyr::bind_rows(pre, post_rep)
  x |> dplyr::group_by(gsis_id) |> dplyr::summarise(
    rep = dplyr::first(status[source == "report"]), slp = dplyr::first(status[source == "sleeper"]),
    prac = dplyr::first(practice[source == "report"]), inj = dplyr::coalesce(dplyr::first(injury[source == "report"]), dplyr::first(injury[source == "sleeper"])),
    .groups = "drop") |>
    dplyr::mutate(inj_status = dplyr::case_when(rep %in% INJ_HARD ~ rep, slp %in% INJ_HARD ~ slp, !is.na(rep) ~ rep,
                                                slp %in% c("Out", "Doubtful") ~ slp, slp %in% "Questionable" ~ "Questionable", TRUE ~ NA_character_),
                  inj_out = inj_status %in% c("Out", "Doubtful", INJ_HARD),
                  inj_badge = ifelse(is.na(inj_status), "", unname(INJ_ABBR[inj_status])),
                  inj_detail = paste0(ifelse(is.na(rep) & is.na(prac), "", paste0("Official report: ", dplyr::coalesce(rep, "no game status yet"),
                                                                                  ifelse(is.na(prac) | !nzchar(prac), "", paste0(" (practice: ", prac, ")")))),
                                      ifelse(is.na(slp), "", paste0(ifelse(is.na(rep) & is.na(prac), "", "; "), "Sleeper: ", slp)),
                                      ifelse(is.na(inj) | !nzchar(inj), "", paste0(" · ", inj)))) |>
    dplyr::filter(!is.na(inj_status) | nzchar(inj_detail))
}
