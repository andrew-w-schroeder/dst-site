## ---- props_utils.R: shared helpers for NFL player props (The Odds API) ----
## Used by 81_props_backfill.R now; meant for the live refresh later.
## Parsing, per-book main line / median estimate, consensus, anytime TD, and
## matching sportsbook names to gsis_id via weekly rosters.

suppressPackageStartupMessages({ library(dplyr); library(purrr); library(tidyr) })
`%||%` <- function(a, b) if (is.null(a)) b else a

## ---- Markets and teams ----
## Everything that scores in Standard / Half / PPR / FFPC for QB, RB, WR, TE.
## (player_tds_over duplicates anytime TD; completions don't score; rush/rec TD
## markets weren't offered in the 2026 probe.)
PROP_MARKETS <- c("player_pass_yds", "player_pass_tds", "player_pass_interceptions",
                  "player_pass_attempts", "player_rush_yds", "player_rush_attempts",
                  "player_receptions", "player_reception_yds", "player_anytime_td")

## Actual stat for each market (nflverse stats_player_week columns)
PROP_STAT <- c(player_pass_yds = "passing_yards", player_pass_tds = "passing_tds",
               player_pass_interceptions = "passing_interceptions",
               player_pass_attempts = "attempts", player_rush_yds = "rushing_yards",
               player_rush_attempts = "carries", player_receptions = "receptions",
               player_reception_yds = "receiving_yards", player_anytime_td = "any_td")

ODDS_TEAMS <- c(
  "Arizona Cardinals" = "ARI", "Atlanta Falcons" = "ATL", "Baltimore Ravens" = "BAL",
  "Buffalo Bills" = "BUF", "Carolina Panthers" = "CAR", "Chicago Bears" = "CHI",
  "Cincinnati Bengals" = "CIN", "Cleveland Browns" = "CLE", "Dallas Cowboys" = "DAL",
  "Denver Broncos" = "DEN", "Detroit Lions" = "DET", "Green Bay Packers" = "GB",
  "Houston Texans" = "HOU", "Indianapolis Colts" = "IND", "Jacksonville Jaguars" = "JAX",
  "Kansas City Chiefs" = "KC", "Las Vegas Raiders" = "LV", "Los Angeles Chargers" = "LAC",
  "Los Angeles Rams" = "LA", "Miami Dolphins" = "MIA", "Minnesota Vikings" = "MIN",
  "New England Patriots" = "NE", "New Orleans Saints" = "NO", "New York Giants" = "NYG",
  "New York Jets" = "NYJ", "Philadelphia Eagles" = "PHI", "Pittsburgh Steelers" = "PIT",
  "San Francisco 49ers" = "SF", "Seattle Seahawks" = "SEA", "Tampa Bay Buccaneers" = "TB",
  "Tennessee Titans" = "TEN", "Washington Commanders" = "WAS")

am_to_p <- function(a) ifelse(a < 0, -a / (-a + 100), 100 / (a + 100))

## ---- Parse one event-odds response (live or historical) ----
## Builds one small table per market (not per outcome), ~50x faster than a tibble per outcome.
flatten_event_odds <- function(body) {
  ev <- if (!is.null(body$data)) body$data else body          # historical wraps in $data
  if (is.null(ev$bookmakers) || length(ev$bookmakers) == 0) return(tibble())
  chr <- function(o, k) vapply(o, \(x) if (is.null(x[[k]])) NA_character_ else as.character(x[[k]]), "")
  num <- function(o, k) vapply(o, \(x) if (is.null(x[[k]])) NA_real_ else as.numeric(x[[k]]), 0)
  rows <- list()
  for (bk in ev$bookmakers) for (mk in bk$markets) {
    o <- mk$outcomes
    if (!length(o)) next
    rows[[length(rows) + 1]] <- list(
      book = rep(bk$key, length(o)), market = rep(mk$key, length(o)),
      market_update = rep(mk$last_update %||% NA_character_, length(o)),
      player = chr(o, "description"), side = chr(o, "name"),
      point = num(o, "point"), price = num(o, "price"))
  }
  if (!length(rows)) return(tibble())
  as_tibble(lapply(setNames(names(rows[[1]]), names(rows[[1]])), \(k) unlist(lapply(rows, `[[`, k), use.names = FALSE)))
}

## team defenses and pseudo-outcomes ("No Touchdown", "No Scorer") are not players
is_team_defense <- function(x) grepl("D/ST|Defen[cs]e|^No (Touchdown|Scorer|TD)", x %||% "", ignore.case = TRUE)

## ---- Per-book lines for over/under markets ----
## A book can list several points for one player in the main market (a ladder,
## e.g. 209.5 / 239.5 / 269.5). For each book x player x market:
##   main_line  = the point whose no-vig P(over) is closest to 50%
##   med_est    = the point where P(over) = 50%, interpolated between the two
##                points that straddle it (= main_line when there is no straddle)
book_lines <- function(long) {
  pairs <- long |>
    filter(side %in% c("Over", "Under"), !is.na(point), !is_team_defense(player)) |>
    mutate(p = am_to_p(price)) |>
    select(any_of(c("game_id", "event_id")), book, market, player, point, side, p) |>
    pivot_wider(names_from = side, values_from = p, values_fn = mean) |>
    filter(!is.na(Over), !is.na(Under)) |>
    mutate(p_over = Over / (Over + Under), vig = Over + Under - 1)

  interp <- function(pt, po) {
    o <- order(pt); pt <- pt[o]; po <- po[o]
    if (length(pt) < 2) return(NA_real_)
    i <- which(po[-length(po)] >= 0.5 & po[-1] <= 0.5)[1]     # P(over) falls with the point
    if (is.na(i)) return(NA_real_)
    if (po[i] == po[i + 1]) return(mean(pt[c(i, i + 1)]))
    pt[i] + (po[i] - 0.5) / (po[i] - po[i + 1]) * (pt[i + 1] - pt[i])
  }

  pairs |>
    group_by(across(any_of(c("game_id", "event_id"))), book, market, player) |>
    summarise(n_points = n(),
              main_line = point[which.min(abs(p_over - 0.5))],
              p_over    = p_over[which.min(abs(p_over - 0.5))],
              vig       = vig[which.min(abs(p_over - 0.5))],
              med_interp = interp(point, p_over), .groups = "drop") |>
    mutate(med_est = coalesce(med_interp, main_line))
}

## ---- Consensus across books (median) ----
props_consensus <- function(bl) {
  bl |>
    group_by(across(any_of(c("game_id", "event_id"))), market, player) |>
    summarise(n_books = n(), line = median(main_line), p_over = median(p_over),
              med_est = median(med_est), line_min = min(main_line), line_max = max(main_line),
              ladder_books = sum(n_points > 1), vig = median(vig), .groups = "drop")
}

## ---- Anytime TD (usually Yes-only, so no per-book de-vig) ----
props_anytime <- function(long) {
  long |>
    filter(market == "player_anytime_td", !is_team_defense(player), side %in% c("Yes", "No")) |>
    mutate(p = am_to_p(price)) |>
    group_by(across(any_of(c("game_id", "event_id"))), book, player) |>
    summarise(p_yes = mean(p[side == "Yes"]), p_no = mean(p[side == "No"]), .groups = "drop") |>
    mutate(p_nv = ifelse(is.finite(p_no), p_yes / (p_yes + p_no), NA_real_)) |>
    group_by(across(any_of(c("game_id", "event_id"))), player) |>
    summarise(market = "player_anytime_td", n_books = sum(is.finite(p_yes)),
              p_td_raw = median(p_yes, na.rm = TRUE),
              p_td_nv  = if (any(is.finite(p_nv))) median(p_nv, na.rm = TRUE) else NA_real_,
              .groups = "drop")
}

## ---- Names -> gsis_id ----
## sportsbook spellings / nicknames that no roster spelling covers
NAME_ALIAS <- c("adam theilen" = "adam thielen", "hollywood brown" = "marquise brown",
                "chosen anderson" = "robbie chosen")
name_key <- function(x) {
  x <- gsub("\\s*\\([^)]*\\)", " ", x)   # "Lamar Jackson (BAL)", "Michael (Saints) Thomas"
  x <- sub("\\s+-\\s+.*$", "", sub("^\\s*-\\s*", "", x))   # "- Jahmyr Gibbs - Junior"
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[.'`’]", "", x)          # punctuation first (D.J. -> dj, O'Neil -> oneil)
  x <- gsub("[^a-z ]", " ", x)        # hyphens etc. -> space
  x <- gsub("\\s+", " ", trimws(x))
  sfx <- "\\s(jr|sr|junior|ii|iii|iv|v)$"
  x <- sub(sfx, "", x); x <- sub(sfx, "", x)   # twice for stacked suffixes
  ifelse(x %in% names(NAME_ALIAS), NAME_ALIAS[x], x)
}

## rosters: weekly rosters (season, week, team, gsis_id, full_name, first_name,
## last_name, football_name, position). games: game_id, season, week, home_team, away_team.
## Candidates = each team's roster that week (or its latest earlier week that season).
## A player's name keys come from every roster row he has, so a name that changes
## during the season (Nathaniel Dell -> Tank Dell) still matches in early weeks.
match_players <- function(players, games, rosters) {
  fantasy_pos <- c("QB", "RB", "WR", "TE", "FB")
  ro <- rosters |> filter(!is.na(gsis_id))

  keys <- bind_rows(
    ro |> transmute(gsis_id, key = name_key(full_name)),
    ro |> transmute(gsis_id, key = name_key(paste(first_name, last_name))),
    ro |> transmute(gsis_id, key = name_key(paste(football_name, last_name)))) |>
    filter(!is.na(key), nzchar(key)) |> distinct()
  lastn <- ro |> transmute(gsis_id, k_last = name_key(last_name),
                           init = substr(name_key(coalesce(football_name, first_name)), 1, 1)) |>
    bind_rows(ro |> transmute(gsis_id, k_last = name_key(last_name),
                              init = substr(name_key(first_name), 1, 1))) |> distinct()

  gt <- games |>
    select(game_id, season, week, home_team, away_team) |>
    pivot_longer(c(home_team, away_team), values_to = "team") |>
    select(game_id, season, week, team)
  wk <- ro |> distinct(season, team, week) |> rename(rweek = week)
  gw <- gt |> inner_join(wk, by = c("season", "team"), relationship = "many-to-many") |>
    filter(rweek <= week) |> group_by(game_id, team) |> slice_max(rweek, n = 1) |> ungroup()
  gr <- gw |>
    inner_join(ro |> select(season, team, week, gsis_id, position),
               by = c("season", "team", "rweek" = "week"), relationship = "many-to-many") |>
    distinct(game_id, gsis_id, .keep_all = TRUE) |>
    select(game_id, team, gsis_id, position)

  pk <- players |> distinct(game_id, player) |>
    mutate(k = name_key(player), k_last = sub("^\\S+\\s", "", k), init = substr(k, 1, 1))

  ## 1. exact name key (any spelling the player has on a roster), unique in the game
  exact <- pk |>
    inner_join(gr |> inner_join(keys, by = "gsis_id", relationship = "many-to-many"),
               by = c("game_id", "k" = "key"), relationship = "many-to-many") |>
    distinct(game_id, player, gsis_id, .keep_all = TRUE) |>
    group_by(game_id, player) |> filter(n() == 1) |> ungroup() |> mutate(method = "exact")

  ## 2. same last name + first initial, fantasy position, unique in the game
  rest <- pk |> anti_join(exact, by = c("game_id", "player"))
  cand <- gr |> filter(position %in% fantasy_pos) |>
    inner_join(lastn, by = "gsis_id", relationship = "many-to-many")
  li <- rest |>
    inner_join(cand, by = c("game_id", "k_last", "init"), relationship = "many-to-many") |>
    distinct(game_id, player, gsis_id, .keep_all = TRUE) |>
    group_by(game_id, player) |> filter(n() == 1) |> ungroup() |> mutate(method = "last_initial")

  ## 3. last name only, fantasy position, unique in the game (nicknames)
  rest <- rest |> anti_join(li, by = c("game_id", "player"))
  lo <- rest |>
    inner_join(cand |> distinct(game_id, gsis_id, team, position, k_last),
               by = c("game_id", "k_last"), relationship = "many-to-many") |>
    distinct(game_id, player, gsis_id, .keep_all = TRUE) |>
    group_by(game_id, player) |> filter(n() == 1) |> ungroup() |> mutate(method = "last_only")

  ## 4. spacing differences ("Amon-Ra St.Brown" vs "Amon-Ra St. Brown")
  nosp <- function(x) gsub(" ", "", x)
  rest <- rest |> anti_join(lo, by = c("game_id", "player"))
  ns <- rest |> mutate(k0 = nosp(k)) |>
    inner_join(gr |> inner_join(keys |> mutate(k0 = nosp(key)), by = "gsis_id", relationship = "many-to-many"),
               by = c("game_id", "k0"), relationship = "many-to-many") |>
    distinct(game_id, player, gsis_id, .keep_all = TRUE) |>
    group_by(game_id, player) |> filter(n() == 1) |> ungroup() |> mutate(method = "no_space")

  ## 5. one-letter misspellings ("Andrei Iosivias"): edit distance <= 1 on the full
  ##    name, fantasy position, unique in the game
  rest <- rest |> anti_join(ns, by = c("game_id", "player"))
  fz <- if (nrow(rest)) {
    cand_k <- gr |> filter(position %in% fantasy_pos) |>
      inner_join(keys, by = "gsis_id", relationship = "many-to-many")
    rest |> inner_join(cand_k, by = "game_id", relationship = "many-to-many") |>
      filter(abs(nchar(k) - nchar(key)) <= 1) |>
      mutate(d = mapply(\(a, b) utils::adist(a, b)[1, 1], k, key)) |>
      filter(d <= 1) |>
      distinct(game_id, player, gsis_id, .keep_all = TRUE) |>
      group_by(game_id, player) |> filter(n() == 1) |> ungroup() |> mutate(method = "fuzzy")
  } else tibble()

  bind_rows(exact, li, lo, ns, fz) |>
    select(game_id, player, gsis_id, team, position, method) |>
    right_join(pk |> select(game_id, player), by = c("game_id", "player")) |>
    mutate(method = coalesce(method, "unmatched"))
}
