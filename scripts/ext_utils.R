# ==============================================================================
# ext_utils.R — Sleeper and ESPN weekly D/ST + kicker projections → ESPN-standard RANKS, for the track record
# (62_track_record.R) and the snapshots the daily refresh (45_dst_refresh.R) takes before each kickoff.
#
#   Sleeper : api.sleeper.com/projections/nfl/<season>/<week> (public; Rotowire's numbers). Projected stats are
#             re-scored into ESPN standard (D/ST: points / yards-allowed tiers applied to the projected mean, as
#             Sleeper does; kickers: FG by distance, misses, PATs — exact).
#   ESPN    : lm-api-reads.fantasy.espn.com kona_player_info, ESPN standard league defaults: ESPN's own projected
#             points for D/ST; kickers re-scored from the projected stats (falls back to ESPN's points).
# Only each source's weekly RANK is stored (data/lines/ext_rank_history.csv in the public site repo), not their
# projections. Base R + dplyr / tibble / purrr + jsonlite / curl (runs on the GitHub runner).
# ==============================================================================

EXT_COLS <- c("pulled_at", "season", "week", "source", "pos", "team", "rank")
ext_norm_team <- function(x) dplyr::recode(x, OAK = "LV", SD = "LAC", STL = "LA", LAR = "LA", JAC = "JAX", WSH = "WAS", ARZ = "ARI")
ext_get <- function(url, headers = NULL, tries = 2) {
  for (k in seq_len(tries)) {
    h <- curl::new_handle(timeout = 45, useragent = "Mozilla/5.0 (personal projections track record)")
    if (length(headers)) curl::handle_setheaders(h, .list = headers)
    r <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) { message("  ", sub("\\?.*", "", url), ": ", conditionMessage(e)); NULL })
    if (!is.null(r) && r$status_code == 200) { txt <- rawToChar(r$content); Encoding(txt) <- "UTF-8"; return(txt) }
    if (!is.null(r)) message(sprintf("  %s: HTTP %d %s", sub("\\?.*", "", url), r$status_code, substr(rawToChar(r$content), 1, 200)))
    if (!is.null(r) && r$status_code >= 400 && r$status_code < 500) return(NULL)
    Sys.sleep(2 * k)
  }
  NULL
}
.num <- function(x) if (is.null(x) || !length(x)) NA_real_ else suppressWarnings(as.numeric(x[[1]]))
.chr <- function(x) if (is.null(x) || !length(x)) NA_character_ else as.character(x[[1]])
.z0  <- function(x) dplyr::coalesce(x, 0)
ESPN_DST_RULE <- list(sack = 1, int = 2, fr = 2, safety = 2, blk = 2, td = 6, pa_b = c(-Inf, 0, 6, 13, 17, 27, 34, 45, Inf), pa_p = c(5, 4, 3, 1, 0, -1, -3, -5),
                      ya_b = c(-Inf, 99, 199, 299, 349, 399, 449, 499, 549, Inf), ya_p = c(5, 3, 2, 0, -1, -3, -5, -6, -7))
.tier <- function(x, b, p) ifelse(is.na(x), 0, p[cut(x, b, labels = FALSE)])

# Sleeper → one row per player: pos (K / DEF), team, ESPN-standard projected points
ext_sleeper <- function(season, week) {
  st_names <- c("gp", "fga", "fgm", "fgm_0_19", "fgm_20_29", "fgm_30_39", "fgm_40_49", "fgm_50p", "xpm",
                "sack", "int", "fum_rec", "def_td", "safe", "blk_kick", "def_st_td", "st_td", "pr_td", "kr_td", "pts_allow", "yds_allow")
  purrr::map_dfr(c("K", "DEF"), function(pos) {
    txt <- ext_get(sprintf("https://api.sleeper.com/projections/nfl/%d/%d?season_type=regular&position[]=%s", season, week, pos))
    if (is.null(txt)) return(NULL)
    js <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    x <- purrr::map_dfr(js, function(e) { st <- e$stats; if (is.null(st) || is.null(e$team)) return(NULL)
      tibble::as_tibble(c(list(pos = pos, team = .chr(e$team)), stats::setNames(lapply(st_names, function(s) .num(st[[s]])), st_names))) })
    if (!nrow(x)) return(NULL)
    x <- x[dplyr::coalesce(x$gp, 1) > 0, ]
    if (pos == "K") {
      b3 <- .z0(x$fgm_0_19) + .z0(x$fgm_20_29) + .z0(x$fgm_30_39); b3 <- b3 + pmax(.z0(x$fgm) - b3 - .z0(x$fgm_40_49) - .z0(x$fgm_50p), 0)
      x$pts <- 3 * b3 + 4 * .z0(x$fgm_40_49) + 5 * .z0(x$fgm_50p) - pmax(.z0(x$fga) - .z0(x$fgm), 0) + .z0(x$xpm)
    } else { r <- ESPN_DST_RULE
      x$pts <- r$sack * .z0(x$sack) + r$int * .z0(x$int) + r$fr * .z0(x$fum_rec) + r$safety * .z0(x$safe) + r$blk * .z0(x$blk_kick) +
        r$td * (.z0(x$def_td) + .z0(x$def_st_td) + .z0(x$st_td) + .z0(x$pr_td) + .z0(x$kr_td)) + .tier(x$pts_allow, r$pa_b, r$pa_p) + .tier(x$yds_allow, r$ya_b, r$ya_p) }
    tibble::tibble(source = "Sleeper", pos = pos, team = ext_norm_team(x$team), pts = x$pts)
  })
}
ESPN_TEAM_ID <- c(`1` = "ATL", `2` = "BUF", `3` = "CHI", `4` = "CIN", `5` = "CLE", `6` = "DAL", `7` = "DEN", `8` = "DET", `9` = "GB", `10` = "TEN",
                  `11` = "IND", `12` = "KC", `13` = "LV", `14` = "LA", `15` = "MIA", `16` = "MIN", `17` = "NE", `18` = "NO", `19` = "NYG", `20` = "NYJ",
                  `21` = "PHI", `22` = "ARI", `23` = "PIT", `24` = "LAC", `25` = "SF", `26` = "SEA", `27` = "TB", `28` = "WAS", `29` = "CAR", `30` = "JAX",
                  `33` = "BAL", `34` = "HOU")
# ESPN → one row per player: pos, team, ESPN-standard projected points (stat ids: 74/77/80 FG made 50+/40–49/<40, 201 = 60+, 85 missed, 86 PAT)
ext_espn <- function(season, week) {
  # Same request as the 2025 pull that worked (60_ext_proj_pull.R): first ask for this week's projection split
  # explicitly (needed for past weeks), then the plain request. I(1L) keeps the split filter a JSON array.
  flt <- list(filterSlotIds = list(value = c(16L, 17L)), filterStatsForSourceIds = list(value = c(0L, 1L)),
              filterStatsForSplitTypeIds = list(value = I(1L)), limit = 400L, sortPercOwned = list(sortPriority = 1L, sortAsc = FALSE))
  url <- sprintf("https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/%d/segments/0/leaguedefaults/3?scoringPeriodId=%d&view=kona_player_info", season, week)
  for (variant in 1:2) {
    f <- if (variant == 1) c(flt, list(filterStatsForTopScoringPeriodIds = list(value = 25L,
                                         additionalValue = list(sprintf("11%d%d", season, week), sprintf("01%d%d", season, week))))) else flt
    hdr <- c(`X-Fantasy-Filter` = as.character(jsonlite::toJSON(list(players = f), auto_unbox = TRUE)), Accept = "application/json")
    txt <- ext_get(url, hdr); if (is.null(txt)) next
    js <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL); if (is.null(js)) next
    x <- purrr::map_dfr(js$players, function(pe) {
      p <- if (!is.null(pe$player)) pe$player else pe; slot <- .num(p$defaultPositionId)
      pos <- if (identical(slot, 16)) "DEF" else if (identical(slot, 5)) "K" else return(NULL)
      for (s in p$stats) if (identical(.num(s$seasonId), as.numeric(season)) && identical(.num(s$scoringPeriodId), as.numeric(week)) &&
                             identical(.num(s$statSourceId), 1) && identical(.num(s$statSplitTypeId), 1)) {
        tm <- unname(ESPN_TEAM_ID[as.character(.num(s$proTeamId))]); if (is.na(tm)) tm <- unname(ESPN_TEAM_ID[as.character(.num(p$proTeamId))])
        g <- function(k) .num(s$stats[[k]])
        pts <- if (pos == "K" && !all(is.na(c(g("80"), g("77"), g("74"))))) 3 * .z0(g("80")) + 4 * .z0(g("77")) + 5 * (.z0(g("74")) - .z0(g("201"))) +
          6 * .z0(g("201")) - .z0(g("85")) + .z0(g("86")) else .num(s$appliedTotal)
        return(tibble::tibble(source = "ESPN", pos = pos, team = tm, pts = pts))
      }
      NULL
    })
    if (nrow(x)) return(x)
    message(sprintf("  ESPN week %d (request %d): %d players returned but no week-%d projections in them", week, variant, length(js$players), week))
  }
  NULL
}
# both sources → rank per source × position (a team's top-projected kicker = that source's kicker; 1 = best)
ext_ranks <- function(season, week, sources = c("Sleeper", "ESPN")) {
  x <- dplyr::bind_rows(if ("Sleeper" %in% sources) tryCatch(ext_sleeper(season, week), error = function(e) NULL),
                        if ("ESPN" %in% sources) tryCatch(ext_espn(season, week), error = function(e) NULL))
  if (!nrow(x)) return(NULL)
  x <- x[!is.na(x$team) & !is.na(x$pts), ]
  x <- dplyr::ungroup(dplyr::slice_max(dplyr::group_by(x, source, pos, team), pts, n = 1, with_ties = FALSE))
  x <- dplyr::mutate(dplyr::group_by(x, source, pos), rank = rank(-pts, ties.method = "first"))
  dplyr::ungroup(x)[c("source", "pos", "team", "rank")]
}
read_ext_hist <- function(file) {
  if (!file.exists(file)) return(tibble::tibble(pulled_at = character(), season = integer(), week = integer(), source = character(),
                                                pos = character(), team = character(), rank = integer()))
  tibble::as_tibble(utils::read.csv(file, stringsAsFactors = FALSE, colClasses = c(pulled_at = "character", source = "character", pos = "character", team = "character")))
}
# snapshot this week's ranks for teams whose game hasn't kicked off (ko: tibble team, ko [POSIXct UTC])
ext_snapshot <- function(file, season, week, now, ko) {
  r <- ext_ranks(season, week); if (is.null(r) || !nrow(r)) { message("Sleeper / ESPN ranks: nothing pulled"); return(invisible(read_ext_hist(file))) }
  open <- ko$team[is.na(ko$ko) | ko$ko > now]
  r <- r[r$team %in% open, ]
  h <- dplyr::bind_rows(read_ext_hist(file), dplyr::mutate(r, pulled_at = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), season = as.integer(season), week = as.integer(week))[EXT_COLS])
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE); utils::write.csv(h, file, row.names = FALSE)
  tb <- table(paste(r$source, r$pos))
  message("Sleeper / ESPN ranks snapshotted (teams not yet kicked off): ", paste(names(tb), tb, collapse = ", "))
  invisible(h)
}
# the last snapshot before each team's kickoff (ko: tibble season, week, team, ko)
ext_latest <- function(h, ko) {
  if (!nrow(h)) return(h)
  h$t <- as.POSIXct(sub("Z$", "", h$pulled_at), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  h <- dplyr::inner_join(h, ko, by = c("season", "week", "team"))
  h <- h[is.na(h$ko) | h$t < h$ko, ]
  dplyr::ungroup(dplyr::slice_max(dplyr::group_by(h, season, week, source, pos, team), t, n = 1, with_ties = FALSE))
}
