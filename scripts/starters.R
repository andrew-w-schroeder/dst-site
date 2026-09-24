# ==============================================================================
# starters.R — who actually starts, re-checked at every refresh. Sourced by 45_dst_refresh.R (starting QBs →
# D/ST projections) and 55_k_refresh.R (kicker status flags). Base R + dplyr/tibble + jsonlite/curl only.
#
# Sources (each optional: a failed source is skipped and the page says so):
#   override : data/lines/qb_override.csv (season, week, team, qb_name, note) — edit it on GitHub; wins outright
#   schedule : nflverse games.rds home/away_qb_name — hand-curated projected starters, updated through the week
#   report   : nflverse injuries_<season>.rds — the official NFL injury report (practice Wed–Fri, game status Fri/Sat)
#   sleeper  : api.sleeper.app/v1/players/nfl — depth-chart order + injury status for every player (one call)
#   ourlads  : ourlads.com team depth charts — order + status flags (O, D, Q, IR, …), 32 pages
#
# Status of a player: the official game status once it exists this week (Out / Doubtful / Questionable);
# before that, Out if Sleeper or Ourlads says Out / Doubtful. IR, PUP, NFI and suspensions from any source count.
# "Ruled out" = Out, Doubtful, IR, PUP, NFI or suspended.
#
# Starting QB per offense — the first rule that gives an answer:
#   1. override file
#   2. Sleeper and Ourlads agree on a starter who differs from the schedule's (e.g. a benching) → that QB
#   3. the schedule's QB, unless ruled out
#   4. Sleeper depth chart: QB1 unless ruled out, else the first backup not ruled out (Questionable backups last)
#   5. Ourlads depth chart, same rule
#   6. the weekly-run QB
# Games that have kicked off keep their last pre-kickoff starter (like the lines). Every refresh appends its
# choices to data/lines/starter_history.csv; sources that disagree with the choice are listed on the page.
# ==============================================================================

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
ST_HARD <- c("IR", "PUP", "NFI", "Suspended")
ST_OUT  <- c("Out", "Doubtful", ST_HARD)
st_team <- function(x) dplyr::recode(x, OAK = "LV", SD = "LAC", STL = "LA", LAR = "LA", JAC = "JAX", WSH = "WAS", ARZ = "ARI")
st_norm <- function(x) {                                   # "Penix Jr., Michael" / "Michael Penix Jr." → "michael penix"
  x <- ifelse(grepl(",", x), paste(trimws(sub("^[^,]*,", "", x)), trimws(sub(",.*$", "", x))), x)
  x <- tolower(iconv(x, to = "ASCII//TRANSLIT")); x <- gsub("[.'`-]", "", x); x <- gsub("[^a-z ]", " ", x)
  x <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", " ", x); trimws(gsub(" +", " ", x))
}
st_title <- function(x) {                                  # Ourlads "MARIOTA, MARCUS" / "Daniels, Jayden" → "Marcus Mariota" / "Jayden Daniels"
  x <- ifelse(grepl("^[A-Z' .-]+,", x) | !grepl("[a-z]", x), tools::toTitleCase(tolower(x)), x)
  ifelse(grepl(",", x), paste(trimws(sub("^[^,]*,", "", x)), trimws(sub(",.*$", "", x))), x)
}
st_last <- function(key) sub(".* ", "", key)
st_canon <- function(x) {
  u <- toupper(trimws(as.character(x)))
  dplyr::case_when(is.na(u) | u %in% c("", "NA", "P", "PROBABLE", "ACTIVE", "HEALTHY") ~ NA_character_,
                   u %in% c("O", "OUT", "COV", "DNR", "INACTIVE") ~ "Out", u %in% c("D", "DOUBTFUL") ~ "Doubtful",
                   u %in% c("Q", "QUESTIONABLE", "GTD") ~ "Questionable",
                   u %in% c("IR", "IR-R", "INJURED RESERVE", "INJURED_RESERVE", "RESERVE/INJURED") ~ "IR",
                   u %in% c("PUP", "PHYSICALLY UNABLE TO PERFORM") ~ "PUP", u %in% c("NFI", "NON-FOOTBALL INJURY") ~ "NFI",
                   u %in% c("SUS", "SUSP", "SUSPENDED", "SUSPENSION") ~ "Suspended", TRUE ~ NA_character_)
}
st_abbr <- function(s) dplyr::recode(s, Out = "O", Doubtful = "D", Questionable = "Q", Suspended = "SUS", .default = s, .missing = "")
st_get <- function(url, timeout = 45) {
  h <- curl::new_handle(timeout = timeout, useragent = "Mozilla/5.0 (fantasy projections site; weekly refresh)")
  r <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) NULL)
  if (is.null(r) || r$status_code != 200) return(NULL)
  x <- rawToChar(r$content); Encoding(x) <- "UTF-8"; x
}

## ---- Sources ----
src_sleeper <- function() {
  txt <- st_get("https://api.sleeper.app/v1/players/nfl", 90); if (is.null(txt)) stop("no response from Sleeper")
  js <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  js <- Filter(function(p) !is.null(p$team) && !is.null(p$position) && p$position %in% c("QB", "K"), js)
  g <- function(p, f) { v <- p[[f]]; if (is.null(v) || !length(v)) NA_character_ else as.character(v[[1]]) }
  tibble::tibble(src = "Sleeper", team = st_team(vapply(js, g, "", "team")), pos = vapply(js, g, "", "position"),
                 name = dplyr::coalesce(vapply(js, g, "", "full_name"), paste(vapply(js, g, "", "first_name"), vapply(js, g, "", "last_name"))),
                 gsis_id = trimws(vapply(js, g, "", "gsis_id")), order = suppressWarnings(as.integer(vapply(js, g, "", "depth_chart_order"))),
                 status = dplyr::coalesce(st_canon(vapply(js, g, "", "injury_status")), st_canon(vapply(js, g, "", "status"))),
                 flag = vapply(js, g, "", "injury_status")) |>
    dplyr::filter(!is.na(order)) |> dplyr::mutate(gsis_id = ifelse(grepl("^00-", gsis_id), gsis_id, NA_character_)) |>
    dplyr::arrange(team, pos, order)
}
OL_CODE <- c(ARI = "ARZ", LA = "LAR")
ol_parse <- function(html, pos) {                            # first depth-chart row whose first cell is `pos`
  ent <- function(x) gsub("&#39;|&#039;|&apos;", "'", gsub("&amp;", "&", gsub("&nbsp;|&#160;", " ", x)))
  strip <- function(x) trimws(gsub("\\s+", " ", ent(gsub("<[^>]+>", " ", x))))
  rows <- regmatches(html, gregexpr("(?is)<tr[^>]*>.*?</tr>", html, perl = TRUE))[[1]]
  for (r in rows) {
    cells <- regmatches(r, gregexpr("(?is)<td[^>]*>.*?</td>", r, perl = TRUE))[[1]]
    if (length(cells) < 3 || toupper(strip(cells[1])) != pos) next
    cells <- cells[-1]; txt <- strip(cells)
    keep <- nzchar(txt) & !grepl("^[0-9]+$", txt) & grepl("[A-Za-z]", txt)
    cells <- cells[keep]; txt <- txt[keep]; if (!length(txt)) next
    has_a <- grepl("(?i)<a[^>]*>", cells, perl = TRUE)
    nm <- ifelse(has_a, strip(sub("(?is)^.*?<a[^>]*>(.*?)</a>.*$", "\\1", cells, perl = TRUE)), sub("\\s+\\S*[0-9/]\\S*.*$", "", txt))
    rest <- ifelse(has_a, strip(sub("(?is)^.*?</a>", "", cells, perl = TRUE)), sub("^.*?\\s+(\\S*[0-9/]\\S*.*)$", "\\1", txt))
    last <- toupper(sub("^.*\\s", "", paste("", rest)))
    flag <- ifelse(last %in% c("O", "D", "Q", "IR", "PUP", "SUS", "NFI"), last, NA_character_)
    return(tibble::tibble(order = seq_along(nm), name = st_title(nm), status = st_canon(flag), flag = flag))
  }
  NULL
}
src_ourlads <- function(teams, pause = 0.5) {
  out <- list(); upd <- character()
  for (tm in teams) {
    code <- if (tm %in% names(OL_CODE)) OL_CODE[[tm]] else tm
    html <- st_get(paste0("https://www.ourlads.com/nfldepthcharts/depthchart/", code), 30); Sys.sleep(pause)
    if (is.null(html)) next
    u <- regmatches(html, regexpr("(?i)Updated:?\\s*[0-9/]+\\s+[0-9:]+\\s*[AP]M", html, perl = TRUE))
    upd[tm] <- if (length(u)) trimws(sub("(?i)Updated:?", "", u, perl = TRUE)) else NA_character_
    for (p in c("QB", "PK")) { x <- ol_parse(html, p); if (!is.null(x)) out[[paste(tm, p)]] <- dplyr::mutate(x, team = tm, pos = ifelse(p == "PK", "K", p)) }
  }
  if (!length(out)) stop("no Ourlads pages parsed")
  res <- dplyr::bind_rows(out) |> dplyr::mutate(src = "Ourlads", gsis_id = NA_character_, updated = unname(upd[team]))
  attr(res, "n_teams") <- length(unique(res$team)); res
}
nv_rds <- function(release, file) {
  tmp <- tempfile(fileext = ".rds")
  ok <- tryCatch(utils::download.file(sprintf("https://github.com/nflverse/nflverse-data/releases/download/%s/%s", release, file), tmp, mode = "wb", quiet = TRUE) == 0,
                 error = function(e) FALSE)
  if (!ok) stop("download failed: ", file); readRDS(tmp)
}
src_report <- function(season, week) {
  d <- tibble::as_tibble(nv_rds("injuries", sprintf("injuries_%d.rds", season)))
  d <- d[d$week == week & d$position %in% c("QB", "K"), ]
  tibble::tibble(src = "Report", team = st_team(d$team), pos = d$position, name = d$full_name, gsis_id = d$gsis_id,
                 status = st_canon(d$report_status), practice = d$practice_status,
                 when = if ("date_modified" %in% names(d)) as.character(d$date_modified) else NA_character_)
}

# Fetch every source once (45 caches the result for 55 in work/starter_sources.rds). Each successful pull is also
# saved to snap_dir (data/lines/sources/<source>.csv); if a source fails, its last snapshot is used when it is
# less than snap_max_h hours old, so one flaky request doesn't flip a starter back and forth.
starter_sources <- function(season, week, teams, cache = NULL, max_age_min = 120, now = Sys.time(), snap_dir = NULL, snap_max_h = 30) {
  if (!is.null(cache) && file.exists(cache)) {
    S <- readRDS(cache)
    if (identical(S$season, season) && identical(S$week, week) && difftime(now, S$time, units = "mins") < max_age_min) return(S)
  }
  S <- list(season = season, week = week, time = now, ok = character(), fail = character())
  skip <- strsplit(Sys.getenv("NO_STARTER_SOURCES"), ",")[[1]]     # e.g. NO_STARTER_SOURCES=ourlads for testing
  for (nm in c("sleeper", "report", "ourlads")) {
    if (nm %in% skip) next
    t0 <- Sys.time()
    x <- tryCatch(switch(nm, sleeper = src_sleeper(), report = src_report(season, week), ourlads = src_ourlads(teams)),
                  error = function(e) { message("starters: ", nm, " unavailable — ", conditionMessage(e)); NULL })
    snap <- if (!is.null(snap_dir)) file.path(snap_dir, paste0(nm, ".csv")) else NULL
    if (is.null(x) && !is.null(snap) && file.exists(snap)) {
      y <- utils::read.csv(snap, stringsAsFactors = FALSE, colClasses = "character")
      age <- as.numeric(difftime(now, as.POSIXct(y$fetched[1], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), units = "hours"))
      if (nrow(y) && !is.na(age) && age < snap_max_h && identical(as.integer(y$season[1]), as.integer(season)) && identical(as.integer(y$week[1]), as.integer(week))) {
        x <- dplyr::mutate(tibble::as_tibble(y[setdiff(names(y), c("fetched", "season", "week"))]),
                           dplyr::across(dplyr::everything(), ~ dplyr::na_if(.x, "")), dplyr::across(dplyr::any_of("order"), as.integer))
        message(sprintf("starters: %s — using the snapshot from %.0f h ago", nm, age)); S$stale <- c(S$stale, sprintf("%s (%.0f h old)", nm, age))
      }
    } else if (!is.null(x) && !is.null(snap)) {
      dir.create(snap_dir, recursive = TRUE, showWarnings = FALSE)
      utils::write.csv(dplyr::mutate(x, fetched = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), season = season, week = week), snap, row.names = FALSE)
    }
    if (is.null(x)) { S$fail <- c(S$fail, nm); next }
    S[[nm]] <- x; S$ok <- c(S$ok, nm)
    message(sprintf("starters: %s OK · %d rows · %.0f s", nm, nrow(x), as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
  if (!is.null(cache)) { dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE); saveRDS(S, cache) }
  S
}

## ---- Status of one player ----
st_match <- function(tab, team, key, id) {                 # rows of a source table for this player on this team
  if (is.null(tab) || !nrow(tab)) return(tab[0, ])
  k <- st_norm(tab$name); t <- tab$team == team
  m <- (!is.na(id) & !is.na(tab$gsis_id) & tab$gsis_id == id) | (t & k == key)
  if (!any(m) && nzchar(key)) { l <- t & st_last(k) == st_last(key); if (sum(l) == 1) m <- l }
  tab[m, ]
}
player_status <- function(S, team, name, id = NA, pos = "QB") {
  key <- st_norm(name); f <- function(tab) { if (is.null(tab) || !nrow(tab)) return(NULL)
    r <- st_match(tab[tab$pos == pos, ], team, key, id); if (NROW(r)) r[1, ] else NULL }
  rep <- f(S$report); sl <- f(S$sleeper); ol <- f(S$ourlads)
  st <- c(report = if (!is.null(rep)) rep$status else NA, sleeper = if (!is.null(sl)) sl$status else NA, ourlads = if (!is.null(ol)) ol$status else NA)
  hard <- st[st %in% ST_HARD]
  status <- if (length(hard)) hard[[1]] else if (!is.na(st[["report"]])) st[["report"]] else
    if (any(st[c("sleeper", "ourlads")] %in% c("Out", "Doubtful"))) st[c("sleeper", "ourlads")][st[c("sleeper", "ourlads")] %in% c("Out", "Doubtful")][[1]] else
    if (any(st %in% "Questionable")) "Questionable" else NA_character_
  det <- c(if (!is.null(rep)) sprintf("report: %s%s", dplyr::coalesce(rep$status, "no game status yet"),
                                      if (!is.na(rep$practice)) paste0(" (", rep$practice, ")") else ""),
           if (!is.null(sl) && !is.na(sl$status)) paste("Sleeper:", sl$status),
           if (!is.null(ol) && !is.na(ol$flag)) paste("Ourlads:", ol$flag))
  ids <- c(id, if (!is.null(rep)) rep$gsis_id, if (!is.null(sl)) sl$gsis_id); ids <- ids[!is.na(ids) & grepl("^00-", ids)]
  list(status = status, out = !is.na(status) && status %in% ST_OUT, id = if (length(ids)) ids[1] else id,
       detail = paste(det, collapse = ", "))
}

## ---- Starting QBs ----
# weekly = tibble(team = offense, qb_id, qb_name) from the weekly run; games = week_games() (schedule QBs);
# pool = tibble(qb_id, qb_name) of QBs the bundle knows (maps names → ids); returns one row per offense.
pick_qbs <- function(S, weekly, games = NULL, pool = NULL, override_file = NULL, season, week) {
  ov <- if (!is.null(override_file) && file.exists(override_file)) utils::read.csv(override_file, stringsAsFactors = FALSE, colClasses = "character") else NULL
  if (!is.null(ov) && nrow(ov)) ov <- ov[ov$season == season & ov$week == week, ] else ov <- NULL
  sched <- if (!is.null(games) && all(c("home_qb_name", "away_qb_name") %in% names(games)))
    dplyr::bind_rows(tibble::tibble(team = st_team(games$home_team), qb_id = games$home_qb_id, qb_name = games$home_qb_name),
                     tibble::tibble(team = st_team(games$away_team), qb_id = games$away_qb_id, qb_name = games$away_qb_name)) else NULL
  id_for <- function(name, id = NA) {
    if (!is.na(id) && nzchar(id)) return(id)
    k <- st_norm(name); if (!nzchar(k)) return(NA_character_)
    hit <- c(pool$qb_id[st_norm(pool$qb_name) == k], S$sleeper$gsis_id[S$sleeper$pos == "QB" & st_norm(S$sleeper$name) == k])
    hit <- hit[!is.na(hit)]; if (length(hit)) hit[1] else paste0("NAME:", k)
  }
  same <- function(a, b) !is.null(a) && !is.null(b) && (identical(a$id, b$id) && !is.na(a$id) || st_norm(a$name) == st_norm(b$name))
  depth_pick <- function(tab, team) {                     # QB1 unless ruled out, else first healthy backup (Questionable last)
    if (is.null(tab)) return(NULL)
    d <- tab[tab$team == team & tab$pos == "QB", ]; d <- d[order(d$order), ]; if (!nrow(d)) return(NULL)
    s <- lapply(seq_len(nrow(d)), function(i) player_status(S, team, d$name[i], d$gsis_id[i]))
    out <- vapply(s, `[[`, TRUE, "out"); q <- vapply(s, function(z) identical(z$status, "Questionable"), TRUE)
    i <- if (!out[1]) 1 else c(which(!out & !q), which(!out & q))[1]
    if (is.na(i)) return(NULL)
    stat <- vapply(s, function(z) as.character(z$status), "")
    list(name = st_title(d$name[i]), id = id_for(d$name[i], s[[i]]$id),
         list = paste(ifelse(is.na(stat), st_title(d$name), paste0(st_title(d$name), " (", st_abbr(stat), ")")), collapse = ", "))
  }
  purrr::map_dfr(weekly$team, function(tm) {
    wk <- weekly[weekly$team == tm, ][1, ]
    sc <- if (!is.null(sched)) sched[sched$team == tm & !is.na(sched$qb_name), ][1, ] else NULL
    sc <- if (!is.null(sc) && nrow(sc) && !is.na(sc$qb_name)) list(name = sc$qb_name, id = id_for(sc$qb_name, sc$qb_id)) else NULL
    sc_st <- if (!is.null(sc)) player_status(S, tm, sc$name, sc$id) else NULL
    sl <- depth_pick(S$sleeper, tm); ol <- depth_pick(S$ourlads, tm)
    o <- if (!is.null(ov)) ov[ov$team == tm, ] else NULL
    pick <- NULL; rule <- NA_character_
    agree <- !is.null(sl) && !is.null(ol) && same(sl, ol)
    sc_out <- !is.null(sc) && sc_st$out
    pre <- if (sc_out) sprintf("schedule QB %s ruled out (%s) → ", sc$name, sc_st$status) else ""
    if (!is.null(o) && nrow(o)) { pick <- list(name = o$qb_name[1], id = id_for(o$qb_name[1])); rule <- paste0("override file", if (nzchar(o$note[1] %||% "")) paste0(" (", o$note[1], ")") else "") }
    else if (agree && !is.null(sc) && !sc_out && !same(sl, sc)) { pick <- sl; rule <- "Sleeper + Ourlads depth charts agree (schedule differs)" }
    else if (!is.null(sc) && !sc_out) { pick <- sc; rule <- "nflverse schedule" }
    else if (!is.null(sl)) { pick <- sl; rule <- paste0(pre, if (agree) "Sleeper + Ourlads depth charts" else "Sleeper depth chart") }
    else if (!is.null(ol)) { pick <- ol; rule <- paste0(pre, "Ourlads depth chart") }
    else { pick <- list(name = wk$qb_name, id = wk$qb_id); rule <- paste0(pre, "weekly run (no other source)") }
    ps <- player_status(S, tm, pick$name, pick$id)
    others <- c(Schedule = if (!is.null(sc) && !sc_out && !same(sc, pick)) sc$name,
                Sleeper = if (!is.null(sl) && !same(sl, pick)) sl$name, Ourlads = if (!is.null(ol) && !same(ol, pick)) ol$name)
    tibble::tibble(team = tm, qb_id = dplyr::coalesce(pick$id, ps$id), qb_name = pick$name, status = ps$status, status_detail = ps$detail, rule = rule,
                   disagree = if (length(others)) paste(sprintf("%s: %s", names(others), others), collapse = " · ") else "",
                   sleeper_depth = if (!is.null(sl)) sl$list else NA_character_, ourlads_depth = if (!is.null(ol)) ol$list else NA_character_,
                   ourlads_updated = if (!is.null(S$ourlads)) S$ourlads$updated[S$ourlads$team == tm][1] else NA_character_,
                   schedule_qb = if (!is.null(sc)) sc$name else NA_character_)
  })
}

# Hover text for the Opp QB cell
qb_tip <- function(p, weekly_name, changed) {
  paste0("<b>", p$qb_name, ifelse(is.na(p$status), "", paste0(" — ", p$status)), "</b>",
         "\nPicked by: ", p$rule,
         ifelse(changed, paste0("\nChanged since the weekly run (was ", weekly_name, "): projection re-scored with this QB"), ""),
         ifelse(nzchar(p$status_detail), paste0("\nStatus: ", p$status_detail), ""),
         ifelse(nzchar(p$disagree), paste0("\n⚠ Other sources: ", p$disagree), ""),
         ifelse(is.na(p$schedule_qb), "", paste0("\nSchedule: ", p$schedule_qb)),
         ifelse(is.na(p$sleeper_depth), "", paste0("\nSleeper depth: ", p$sleeper_depth)),
         ifelse(is.na(p$ourlads_depth), "", paste0("\nOurlads depth", ifelse(is.na(p$ourlads_updated), "", paste0(" (upd ", p$ourlads_updated, ")")), ": ", p$ourlads_depth)))
}

# Kickers: status of the listed kicker + the next healthy kicker on the team's depth chart
kicker_status <- function(S, k) purrr::map_dfr(seq_len(nrow(k)), function(i) {
  ps <- player_status(S, k$team[i], k$kicker[i], k$kicker_id[i], pos = "K")
  alt <- NA_character_
  if (ps$out) for (tab in list(S$sleeper, S$ourlads)) if (is.na(alt) && !is.null(tab)) {
    d <- tab[tab$team == k$team[i] & tab$pos == "K", ]; d <- d[order(d$order), ]
    d <- d[st_norm(d$name) != st_norm(k$kicker[i]) & !(d$status %in% ST_OUT), ]; if (nrow(d)) alt <- st_title(d$name[1])
  }
  tibble::tibble(team = k$team[i], k_status = ps$status, k_out = ps$out, k_status_detail = ps$detail, k_alt = alt)
})

# Log of every refresh's choices (one row per team and position)
log_starters <- function(file, now, season, week, rows) {
  rows <- dplyr::mutate(rows, pulled_at = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), season = season, week = week, .before = 1)
  old <- if (file.exists(file)) utils::read.csv(file, stringsAsFactors = FALSE, colClasses = "character") else NULL
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(dplyr::bind_rows(old, dplyr::mutate(rows, dplyr::across(dplyr::everything(), as.character))), file, row.names = FALSE)
}
# last starter chosen before kickoff (for games that have started)
last_pre_kickoff <- function(file, season, week, ko) {
  if (!file.exists(file)) return(NULL)
  h <- utils::read.csv(file, stringsAsFactors = FALSE, colClasses = "character")
  h <- h[h$season == season & h$week == week & h$pos == "QB", ]
  if (!nrow(h)) return(NULL)
  t <- as.POSIXct(sub("Z$", "", h$pulled_at), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  h <- h[!is.na(ko[h$team]) & t < ko[h$team], ]; if (!nrow(h)) return(NULL)
  h[order(h$pulled_at), ] |> dplyr::group_by(team) |> dplyr::slice_tail(n = 1) |> dplyr::ungroup()
}
