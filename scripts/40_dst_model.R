# ==============================================================================
# 40_dst_model.R — D/ST weekly projection model (ESPN, Yahoo or FFPC scoring)
#
# One end-to-end script:
#   * pulls nflverse pbp / schedules / PFR pressure data (cached as .rds)
#   * computes D/ST fantasy points (scoring system chosen at run time) for every team-game since 2018
#   * builds leak-free "as-of" features (decayed, shrunk team rates for the
#     defense AND the opposing offense, opposing-QB career rates, Vegas, venue)
#   * back-tests several model families season-by-season vs a Vegas-only baseline
#   * fits the winner on everything and projects PRED_SEASON / PRED_WEEK
#
# Weekly use: set PRED_WEEK, run the whole script (≈2–4 min; the first run downloads ~150 MB and
# tunes the feature decay — cached afterwards). Section 9 prints the ranked projections and section
# 11–12 write output/dst/<system>/dst_proj_<season>_wk<ww>.{csv,html,md} plus dst_glossary.{csv,md};
# 43_dst_run_all.R runs all three systems and 44_dst_report.R builds the combined report. Section 9c saves a
# scoring bundle (bundle_<season>_wk<ww>.rds) that 45_dst_refresh.R re-scores daily with new Vegas lines. For a late QB change, add a row to
# `qb_override` (find ids with `qb_lookup`); for an outdoor game with a nasty forecast, add a row to
# `weather_override` (temp °F, wind mph).
#
# Data notes
#   * Everything comes from nflverse GitHub release assets as .rds (no arrow
#     needed); nflreadr::load_pbp()/load_schedules()/load_pfr_advstats() give
#     the same tables if you prefer those loaders.
#   * PFR pressure (times_pressured / hurried / hit / blitzed) is nflverse's
#     mirror of PFR advanced passing (2018+) — the same source as
#     pro-football-reference.com/years/<yr>/opp.htm, aggregated to team-game.
#     NGS "QBP %" (pro.nfl.com) is not in nflverse; add it via `extra_team_week`.
# ==============================================================================

## ---- 1. Setup & configuration ----
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr); library(tibble)
  library(glmnet); library(mgcv); library(ranger); library(gbm)
})
options(timeout = 900, dplyr.summarise.inform = FALSE, width = 130)

PROJ_DIR    <- Sys.getenv("FF_PROJ_DIR", path.expand("~/ML/ff"))
DATA_DIR    <- file.path(PROJ_DIR, "data/dst")
# Scoring system: first command-line argument (Rscript 40_dst_model.R yahoo 4), else env var DST_SCORING, else ESPN.
# Prediction week: second argument, else env var DST_WEEK, else the default below.
# Each system gets its own output folder, tuning caches and feature selection.
.args <- commandArgs(trailingOnly = TRUE)
SCORING_SYSTEM <- tolower(if (length(.args) && nzchar(.args[1])) .args[1] else Sys.getenv("DST_SCORING", "espn"))
OUT_DIR     <- file.path(PROJ_DIR, "output/dst", SCORING_SYSTEM)
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR,  recursive = TRUE, showWarnings = FALSE)

PRED_SEASON   <- 2026
PRED_WEEK     <- as.integer(if (length(.args) >= 2 && nzchar(.args[2])) .args[2] else Sys.getenv("DST_WEEK", "3"))   # week to project
PBP_SEASONS   <- 2016:PRED_SEASON          # 2 extra seasons so 2018 rows already have history
PFR_SEASONS   <- 2018:PRED_SEASON
TRAIN_SEASONS <- 2018:PRED_SEASON          # rows used for modelling
CV_SEASONS    <- 2025                      # back-test season for every model choice (trained on 2018–2024)
SEED          <- 40
Y_CAP         <- 25                        # winsorisation cap for the enet_w variant
GBM_TREES     <- 800
RETUNE        <- FALSE                     # TRUE re-runs the decay/shrinkage grid (slow-ish)

# Optional manual inputs for the prediction week
weather_override <- tibble(game_id = character(), temp = numeric(), wind = numeric())  # outdoor forecasts
qb_override      <- tibble(team = character(), qb_id = character())                    # e.g. injury news

# D/ST scoring systems (edit to match your leagues). Sources: support.espn.com D/ST scoring + scoring formats;
# help.yahoo.com SLN6489 (defaults) + SLN6442 (what counts); myffpc.com Classic league official rules.
# pa_excl = which of the opponent's points are NOT charged to this D/ST:
#   espn : opponent defensive TDs (pick-six / fumble return) incl. the conversion; ST return TDs still count
#   yahoo: opponent INT / fumble / blocked- or missed-FG return TDs (TD only; PATs count) and safeties;
#          kickoff / punt return TDs still count
#   ffpc : every non-offensive point: defensive and special-teams TDs incl. conversions, safeties, 2-pt returns
SCORING <- list(
  espn  = list(label = "ESPN", sack = 1, int = 2, fum_rec = 2, safety = 2, block_kick = 2, block_pat = 2, td = 6, two_pt_ret = 2,
               pa_breaks = c(-Inf, 0, 6, 13, 17, 27, 34, 45, Inf), pa_pts = c(5, 4, 3, 1, 0, -1, -3, -5),
               ya_breaks = c(-Inf, 99, 199, 299, 349, 399, 449, 499, 549, Inf), ya_pts = c(5, 3, 2, 0, -1, -3, -5, -6, -7),
               pa_excl = "pa_excl_espn",
               rules = "Sack 1 · INT 2 · fumble recovery 2 · safety 2 · blocked punt/FG/PAT 2 · return or defensive TD 6 · 2-pt/XP return 2 · Points allowed: 0 → 5, 1–6 → 4, 7–13 → 3, 14–17 → 1, 18–27 → 0, 28–34 → −1, 35–45 → −3, 46+ → −5 (opponent pick-six / fumble-return TDs excluded) · Yards allowed: <100 → 5, 100–199 → 3, 200–299 → 2, 300–349 → 0, 350–399 → −1, 400–449 → −3, 450–499 → −5, 500–549 → −6, 550+ → −7"),
  yahoo = list(label = "Yahoo", sack = 1, int = 2, fum_rec = 2, safety = 2, block_kick = 2, block_pat = 2, td = 6, two_pt_ret = 0,
               pa_breaks = c(-Inf, 0, 6, 13, 20, 27, 34, Inf), pa_pts = c(10, 7, 4, 1, 0, -1, -4),
               ya_breaks = NULL, ya_pts = NULL, pa_excl = "pa_excl_yahoo",
               rules = "Sack 1 · INT 2 · fumble recovery 2 · safety 2 · blocked punt/FG/PAT 2 · defensive or kick/punt return TD 6 · Points allowed: 0 → 10, 1–6 → 7, 7–13 → 4, 14–20 → 1, 21–27 → 0, 28–34 → −1, 35+ → −4 (opponent INT / fumble / blocked-FG return TDs and safeties excluded; kick/punt return TDs count) · no yards-allowed scoring"),
  ffpc  = list(label = "FFPC", sack = 1, int = 2, fum_rec = 2, safety = 5, block_kick = 2, block_pat = 0, td = 6, two_pt_ret = 2,
               pa_breaks = c(-Inf, 0, 6, 13, 19, 23, 29, Inf), pa_pts = c(12, 8, 5, 3, 1, 0, -2),
               ya_breaks = NULL, ya_pts = NULL, pa_excl = "pa_excl_ffpc",
               rules = "Sack 1 · takeaway (INT or fumble recovery) 2 · safety 5 · blocked punt/FG 2 · return or defensive TD 6 · return after blocked/failed XP or 2-pt 2 · Points allowed: 0 → 12, 1–6 → 8, 7–13 → 5, 14–19 → 3, 20–23 → 1, 24–29 → 0, 30+ → −2 (only offensive points count) · no yards-allowed scoring")
)
SC <- SCORING[[SCORING_SYSTEM]]
if (is.null(SC)) stop("unknown scoring system '", SCORING_SYSTEM, "': use one of ", paste(names(SCORING), collapse = ", "))
message("scoring system: ", SC$label)
pa_points <- function(pa) SC$pa_pts[cut(pa, SC$pa_breaks, labels = FALSE)]
ya_points <- function(ya) if (is.null(SC$ya_breaks)) ifelse(is.na(ya), NA_real_, 0) else SC$ya_pts[cut(ya, SC$ya_breaks, labels = FALSE)]

# Franchise moves → current codes so team history carries across relocation
norm_team <- function(x) recode(x, OAK = "LV", SD = "LAC", STL = "LA", LAR = "LA", JAC = "JAX", WSH = "WAS")

## ---- 2. Data loading (nflverse release assets, cached) ----
NV_BASE <- "https://github.com/nflverse/nflverse-data/releases/download"
nv_load <- function(release, file, refresh = FALSE, tries = 3) {
  dest <- file.path(DATA_DIR, file)
  if (refresh || !file.exists(dest)) {
    message("downloading ", file); tmp <- tempfile(fileext = ".rds"); ok <- FALSE
    for (k in seq_len(tries)) {                       # download to temp → never clobber a good cache on failure
      ok <- tryCatch({ download.file(sprintf("%s/%s/%s", NV_BASE, release, file), tmp, mode = "wb", quiet = TRUE) == 0 &&
                         !inherits(try(readRDS(tmp), silent = TRUE), "try-error") }, error = function(e) FALSE, warning = function(w) FALSE)
      if (ok) break; Sys.sleep(3 * k)
    }
    if (ok) { tmp2 <- paste0(dest, ".tmp", Sys.getpid()); file.copy(tmp, tmp2, overwrite = TRUE); file.rename(tmp2, dest) }   # atomic swap
    else if (file.exists(dest)) warning("download of ", file, " failed; using cached copy from ", format(file.mtime(dest)))
    else stop("download of ", file, " failed and no cached copy exists")
  }
  as_tibble(readRDS(dest))
}
sched <- nv_load("schedules", "games.rds", refresh = TRUE) %>%
  filter(season %in% PBP_SEASONS, game_type == "REG") %>%
  mutate(across(c(home_team, away_team), norm_team))

pbp_cols <- c("game_id","season","week","posteam","defteam","play_type","yards_gained","epa","success",
              "pass","rush","qb_dropback","qb_kneel","qb_spike","sack","qb_hit","interception",
              "complete_pass","incomplete_pass","fumble","fumble_lost","fumbled_1_team","fumble_recovery_1_team",
              "fumbled_2_team","fumble_recovery_2_team","touchdown","td_team","return_touchdown","safety",
              "punt_blocked","field_goal_result","extra_point_result","defensive_two_point_conv",
              "defensive_extra_point_conv","penalty","penalty_team","penalty_yards","third_down_converted",
              "third_down_failed","fixed_drive","punt_attempt","passer_player_id","pass_oe","cpoe","play_id",
              "extra_point_attempt","two_point_attempt","two_point_conv_result")

read_pbp_season <- function(yr) {
  nv_load("pbp", sprintf("play_by_play_%d.rds", yr), refresh = (yr == PRED_SEASON)) %>%
    select(any_of(pbp_cols)) %>% filter(!is.na(posteam), posteam != "") %>%
    mutate(across(c(posteam, defteam, td_team, fumbled_1_team, fumble_recovery_1_team,
                    fumbled_2_team, fumble_recovery_2_team, penalty_team), norm_team))
}

## ---- 3. Team-game box score from pbp ----
# One row per (game_id, team): offensive production + D/ST scoring events credited to the team.
team_game_from_pbp <- function(p) {
  p <- p %>% filter(game_id %in% sched$game_id)
  scrim <- p %>% filter((pass == 1 | rush == 1), qb_kneel == 0, qb_spike == 0, !is.na(epa))
  off <- scrim %>% group_by(game_id, season, week, team = posteam, opp = defteam) %>%
    summarise(
      plays = n(), epa_sum = sum(epa), succ = sum(success),
      pass_plays = sum(pass), pass_epa = sum(epa[pass == 1]), pass_succ = sum(success[pass == 1]),
      rush_plays = sum(rush), rush_epa = sum(epa[rush == 1]), rush_succ = sum(success[rush == 1]),
      dropbacks = sum(qb_dropback), att = sum(complete_pass + incomplete_pass + interception),
      sacks = sum(sack), qb_hits = sum(qb_hit), ints = sum(interception),
      fumbles = sum(fumble), fum_lost = sum(fumble_lost),
      explosive = sum(yards_gained >= 20, na.rm = TRUE), explosive_pass = sum(yards_gained >= 20 & pass == 1, na.rm = TRUE),
      explosive_rush = sum(yards_gained >= 12 & rush == 1, na.rm = TRUE), neg_plays = sum(yards_gained < 0, na.rm = TRUE),
      run_loss = sum(rush == 1 & yards_gained < 0, na.rm = TRUE),
      third_conv = sum(third_down_converted), third_att = sum(third_down_converted + third_down_failed),
      off_td = sum(touchdown == 1 & td_team == posteam & return_touchdown == 0),
      drives = n_distinct(fixed_drive), pass_oe = sum(pass_oe, na.rm = TRUE), pass_oe_n = sum(!is.na(pass_oe))
    ) %>% ungroup()
  yards <- p %>% filter(play_type %in% c("pass", "run", "qb_kneel", "qb_spike")) %>%
    group_by(game_id, team = posteam) %>% summarise(yards = sum(yards_gained, na.rm = TRUE)) %>% ungroup()
  pen <- p %>% filter(penalty == 1, !is.na(penalty_team)) %>%
    group_by(game_id, team = penalty_team) %>% summarise(pen = n(), pen_yds = sum(penalty_yards, na.rm = TRUE)) %>% ungroup()
  punts <- p %>% filter(punt_attempt == 1) %>% count(game_id, team = posteam, name = "punts")
  # --- D/ST scoring events ---
  dst_def <- p %>% group_by(game_id, team = defteam) %>%
    summarise(
      dst_sacks = sum(sack, na.rm = TRUE), dst_ints = sum(interception, na.rm = TRUE), dst_safety = sum(safety, na.rm = TRUE),
      dst_blk_kick = sum(coalesce(punt_blocked == 1, FALSE) | coalesce(field_goal_result == "blocked", FALSE)),
      dst_blk_pat = sum(coalesce(extra_point_result == "blocked", FALSE)),
      dst_blocks = dst_blk_kick + dst_blk_pat,
      dst_2pt = sum(coalesce(defensive_two_point_conv, 0) + coalesce(defensive_extra_point_conv, 0))
    ) %>% ungroup()
  # any TD that is a return, or scored by the non-possessing team (kickoff returns: posteam = receiving team)
  dst_td <- p %>% filter(touchdown == 1, (coalesce(return_touchdown, 0) == 1 | td_team != posteam)) %>%
    count(game_id, team = td_team, name = "dst_td")
  fr <- bind_rows(
    p %>% filter(fumble == 1, !is.na(fumble_recovery_1_team), fumble_recovery_1_team != fumbled_1_team) %>%
      transmute(game_id, team = fumble_recovery_1_team),
    p %>% filter(fumble == 1, !is.na(fumble_recovery_2_team), fumble_recovery_2_team != fumbled_2_team) %>%
      transmute(game_id, team = fumble_recovery_2_team)
  ) %>% count(game_id, team, name = "dst_fr")
  # Points allowed exclusions (see SCORING): every non-offensive score, credited to the team scored upon.
  p <- p %>% arrange(game_id, play_id)
  conv_pts <- with(p, ifelse(coalesce(extra_point_attempt, 0) == 1, as.integer(coalesce(extra_point_result == "good", FALSE)),
                             ifelse(coalesce(two_point_attempt, 0) == 1, 2L * as.integer(coalesce(two_point_conv_result == "success", FALSE)), NA_integer_)))
  offensive_td <- with(p, touchdown == 1 & play_type %in% c("pass", "run") & td_team == posteam & coalesce(return_touchdown, 0) == 0)
  nonoff <- which(p$touchdown == 1 & !offensive_td & !is.na(p$td_team))
  td_ex <- map_dfr(nonoff, function(i) {
    k <- (i + 1):min(i + 5, nrow(p)); j <- k[which(!is.na(conv_pts[k]) & p$game_id[k] == p$game_id[i])[1]]
    scrim_def <- p$play_type[i] %in% c("pass", "run") && p$td_team[i] == p$defteam[i]            # pick-six, fumble return
    fg_ret    <- identical(p$play_type[i], "field_goal") && p$td_team[i] == p$defteam[i]       # blocked / missed FG return
    victim    <- ifelse(p$td_team[i] == p$posteam[i], p$defteam[i], p$posteam[i])
    conv      <- ifelse(is.na(j), 1L, conv_pts[j])
    tibble(game_id = p$game_id[i], team = victim,
           pa_excl_espn = if (scrim_def) 6L + conv else 0L,
           pa_excl_yahoo = if (scrim_def || fg_ret) 6L else 0L,
           pa_excl_ffpc = 6L + conv)
  })
  saf_ex <- p %>% filter(coalesce(safety, 0) == 1) %>% transmute(game_id, team = posteam, pa_excl_espn = 0L, pa_excl_yahoo = 2L, pa_excl_ffpc = 2L)
  ret2_ex <- p %>% filter(coalesce(defensive_two_point_conv, 0) + coalesce(defensive_extra_point_conv, 0) > 0) %>%
    transmute(game_id, team = posteam, pa_excl_espn = 0L, pa_excl_yahoo = 0L, pa_excl_ffpc = 2L)
  pa_excl <- bind_rows(td_ex, saf_ex, ret2_ex) %>% group_by(game_id, team) %>%
    summarise(across(starts_with("pa_excl_"), sum), .groups = "drop")
  off %>% left_join(yards, by = c("game_id", "team")) %>% left_join(pen, by = c("game_id", "team")) %>%
    left_join(punts, by = c("game_id", "team")) %>% left_join(dst_def, by = c("game_id", "team")) %>%
    left_join(dst_td, by = c("game_id", "team")) %>% left_join(fr, by = c("game_id", "team")) %>%
    left_join(pa_excl, by = c("game_id", "team")) %>%
    mutate(across(c(pen, pen_yds, punts, dst_td, dst_fr, pa_excl_espn, pa_excl_yahoo, pa_excl_ffpc), ~ coalesce(.x, 0L)))
}

# QB-game rows (for opposing-QB career rates)
qb_game_from_pbp <- function(p) {
  p %>% filter(qb_dropback == 1, !is.na(passer_player_id), qb_spike == 0) %>%
    group_by(game_id, season, week, team = posteam, qb_id = passer_player_id) %>%
    summarise(qb_db = n(), qb_att = sum(complete_pass + incomplete_pass + interception),
              qb_sacks = sum(sack), qb_ints = sum(interception), qb_fum = sum(fumble),
              qb_epa = sum(epa, na.rm = TRUE), qb_hits = sum(qb_hit)) %>% ungroup()
}

tg_cache <- file.path(DATA_DIR, "team_game_box.rds")
if (file.exists(tg_cache)) {                       # rebuild only the current season each run
  box <- readRDS(tg_cache)
  p_cur <- read_pbp_season(PRED_SEASON)
  box$tg <- bind_rows(filter(box$tg, season != PRED_SEASON), team_game_from_pbp(p_cur))
  box$qb <- bind_rows(filter(box$qb, season != PRED_SEASON), qb_game_from_pbp(p_cur))
  rm(p_cur)
} else {
  res <- map(PBP_SEASONS, function(yr) {
    message("processing pbp ", yr); p <- read_pbp_season(yr)
    list(tg = team_game_from_pbp(p), qb = qb_game_from_pbp(p))
  })
  box <- list(tg = bind_rows(map(res, "tg")), qb = bind_rows(map(res, "qb"))); rm(res)
}
save_atomic <- function(obj, path) { tmp <- paste0(path, ".tmp", Sys.getpid()); saveRDS(obj, tmp); file.rename(tmp, path) }
save_atomic(box, tg_cache)

## ---- 4. PFR pressure data (team-game level) ----
pfr <- map_dfr(PFR_SEASONS, ~ nv_load("pfr_advstats", sprintf("advstats_week_pass_%d.rds", .x),
                                      refresh = (.x == PRED_SEASON))) %>%
  filter(game_type == "REG") %>% mutate(team = norm_team(team))
# QB-level pressure (PFR ids → gsis ids via the nflverse players table) for QB-portable pressure/sack skill
players <- nv_load("players", "players.rds")
pfr_qb <- pfr %>% filter(!is.na(pfr_player_id)) %>%
  left_join(players %>% filter(!is.na(pfr_id), !is.na(gsis_id)) %>% distinct(pfr_id, .keep_all = TRUE) %>% select(pfr_id, qb_id = gsis_id),
            by = c("pfr_player_id" = "pfr_id")) %>%
  filter(!is.na(qb_id)) %>% group_by(season, week, qb_id) %>%
  summarise(qb_press = sum(times_pressured, na.rm = TRUE), qb_sacked_pfr = sum(times_sacked, na.rm = TRUE)) %>% ungroup()
box$qb <- box$qb %>% select(-any_of(c("qb_press", "qb_sacked_pfr"))) %>% left_join(pfr_qb, by = c("season", "week", "qb_id")) %>%
  mutate(has_pfr = !is.na(qb_press), qb_press = coalesce(qb_press, 0), qb_sacked_pfr = coalesce(qb_sacked_pfr, 0),
         qb_db_pfr = ifelse(has_pfr, qb_db, 0))
pfr_team <- pfr %>% group_by(game_id, team) %>%
  summarise(pressured = sum(times_pressured, na.rm = TRUE), hurried = sum(times_hurried, na.rm = TRUE),
            hit = sum(times_hit, na.rm = TRUE), blitzed = sum(times_blitzed, na.rm = TRUE),
            bad_throws = sum(passing_bad_throws, na.rm = TRUE)) %>% ungroup()

## ---- 4b. NFL Pro Next Gen Stats team-week tables (from 41_nflpro_harvest.R) ----
# Only the team-OFFENSE tables are used: they mirror the opposing defense's tables exactly (pressures,
# sacks, time to throw, separation), so the d_ versions come from the same mirroring as every other rate.
# Get-off exists only in the defense tables and is stored as the defense's own stat. Rates are carried as
# numerator/denominator pairs (e.g. TTT × attempts / attempts) so they decay and shrink like the others.
USE_NGS <- TRUE
NP_FILE <- file.path(PROJ_DIR, "data/nflpro/nflpro_team_week.rds")
ngs_cols <- c("np_pass", "np_att", "np_qbp", "np_sack", "np_ttt_x", "np_ttp_x", "np_blitz_x", "np_sep_x", "np_yacoe", "np_pa_x",
              "np_run", "np_ryoe", "np_ybco_x", "np_stuff_x", "np_light_x", "np_stacked_x", "np_go_x", "np_dpass")
np_team <- NULL
np_src  <- c(NP_FILE, sub("rds$", "csv", NP_FILE))
if (USE_NGS && any(file.exists(np_src))) {
  np <- if (file.exists(np_src[1])) readRDS(np_src[1]) else read.csv(np_src[2])
  np <- as_tibble(np) %>% mutate(team = norm_team(team))
  np_off <- np %>% filter(side == "team-offense") %>% transmute(
    season, week, team, np_pass = pass__pass, np_att = pass__att, np_qbp = pass__qbp, np_sack = pass__sack,
    np_ttt_x = pass__ttt * pass__att, np_ttp_x = pass__ttp * pass__qbp, np_blitz_x = pass__blitzPct * pass__pass,
    np_sep_x = pass__sep * pass__att, np_yacoe = pass__yacoe, np_pa_x = pass__paPct * pass__pass,
    np_run = rush__run, np_ryoe = rush__ryoe, np_ybco_x = rush__ybcoAtt * rush__run, np_stuff_x = rush__stuffPct * rush__run,
    np_light_x = rush__lightPct * rush__run, np_stacked_x = rush__stackedPct * rush__run)
  np_def <- np %>% filter(side == "team-defense") %>% transmute(season, week, team, np_go_x = pass__go * pass__pass, np_dpass = ifelse(is.na(pass__go), NA, pass__pass))
  np_team <- full_join(np_off, np_def, by = c("season", "week", "team"))
  message(sprintf("NFL Pro NGS: %d team-weeks, %d–%d (latest week %d)", nrow(np_team), min(np_team$season), max(np_team$season),
                  max(np_team$week[np_team$season == max(np_team$season)])))
} else if (USE_NGS) message("NFL Pro NGS file not found (", NP_FILE, "): NGS features will sit at league average")

## ---- 4c. FTN charting (nflverse, 2022+, published in-season) ----
# Charted by FTN per play; joined to pbp for the possession team. Offense-side counts; the d_ versions come
# from mirroring like every other rate. Seasons before 2022 are missing and fall back to the league prior.
FTN_SEASONS <- 2022:PRED_SEASON
ftn_team_from <- function(yr) {
  f <- nv_load("ftn_charting", sprintf("ftn_charting_%d.rds", yr), refresh = (yr == PRED_SEASON))
  p <- read_pbp_season(yr) %>% select(game_id, play_id, posteam, qb_dropback, qb_spike, sack, complete_pass, incomplete_pass, interception)
  f %>% transmute(game_id = nflverse_game_id, play_id = nflverse_play_id, iw = as.logical(is_interception_worthy),
                  oop = as.logical(is_qb_out_of_pocket), ta = as.logical(is_throw_away), qfs = as.logical(is_qb_fault_sack)) %>%
    inner_join(p, by = c("game_id", "play_id")) %>% filter(qb_dropback == 1, qb_spike == 0) %>%
    mutate(att = complete_pass + incomplete_pass + interception) %>%
    group_by(game_id, team = posteam) %>%
    summarise(ftn_db = n(), ftn_att = sum(att, na.rm = TRUE), ftn_iw = sum(iw & att == 1, na.rm = TRUE),
              ftn_oop = sum(oop, na.rm = TRUE), ftn_ta = sum(ta, na.rm = TRUE), ftn_qfs = sum(qfs & sack == 1, na.rm = TRUE), .groups = "drop")
}
ftn_cache <- file.path(DATA_DIR, "ftn_team_game.rds")
ftn_team <- if (file.exists(ftn_cache)) readRDS(ftn_cache) %>% filter(!startsWith(game_id, as.character(PRED_SEASON))) else
  map_dfr(setdiff(FTN_SEASONS, PRED_SEASON), ftn_team_from)
ftn_team <- bind_rows(ftn_team, ftn_team_from(PRED_SEASON))
save_atomic(ftn_team, ftn_cache)
ftn_cols <- c("ftn_db", "ftn_att", "ftn_iw", "ftn_oop", "ftn_ta", "ftn_qfs")

## ---- 5. Team-game table with fantasy points ----
sched_long <- bind_rows(
  sched %>% transmute(game_id, season, week, gameday, team = home_team, opp = away_team, home = 1L,
                      pts_for = home_score, pts_against = away_score, spread = spread_line, total_line,
                      qb_id = home_qb_id, qb_name = home_qb_name,
                      rest = home_rest, opp_rest = away_rest, roof, surface, temp, wind, div_game),
  sched %>% transmute(game_id, season, week, gameday, team = away_team, opp = home_team, home = 0L,
                      pts_for = away_score, pts_against = home_score, spread = -spread_line, total_line,
                      qb_id = away_qb_id, qb_name = away_qb_name,
                      rest = away_rest, opp_rest = home_rest, roof, surface, temp, wind, div_game)
) %>% mutate(played = !is.na(pts_for))
qb_lookup <- sched_long %>% filter(!is.na(qb_id)) %>% group_by(qb_id, qb_name) %>%
  summarise(last_season = max(season), .groups = "drop") %>% arrange(desc(last_season))
# future games sometimes carry a projected QB name but no id → fill from the lookup (most recent id for that name)
sched_long <- sched_long %>%
  left_join(qb_lookup %>% distinct(qb_name, .keep_all = TRUE) %>% select(qb_name, qb_id_fill = qb_id), by = "qb_name") %>%
  mutate(qb_id = coalesce(qb_id, qb_id_fill)) %>% select(-qb_id_fill)
# manual starter override for the prediction week: qb_override = tibble(team, qb_id)
if (nrow(qb_override)) sched_long <- sched_long %>% left_join(qb_override %>% rename(qb_new = qb_id), by = "team") %>%
  mutate(qb_id = ifelse(season == PRED_SEASON & week == PRED_WEEK & !is.na(qb_new), qb_new, qb_id),
         qb_name = ifelse(season == PRED_SEASON & week == PRED_WEEK & !is.na(qb_new), qb_lookup$qb_name[match(qb_new, qb_lookup$qb_id)], qb_name)) %>%
  select(-qb_new)
# offensive play caller per team-game (data/dst/play_callers.csv: team, season, from_week, play_caller; edit freely)
play_callers <- read.csv(file.path(DATA_DIR, "play_callers.csv"), stringsAsFactors = FALSE) %>% mutate(team = norm_team(team))
pc_game <- sched_long %>% select(game_id, team, season, week) %>%
  inner_join(play_callers %>% select(team, season, from_week, play_caller), by = c("team", "season"), relationship = "many-to-many") %>%
  filter(from_week <= week) %>% group_by(game_id, team) %>% slice_max(from_week, n = 1, with_ties = FALSE) %>% ungroup() %>%
  select(game_id, team, play_caller)
sched_long <- sched_long %>% left_join(pc_game, by = c("game_id", "team")) %>%
  group_by(game_id) %>% mutate(opp_qb_id = rev(qb_id), opp_qb_name = rev(qb_name), opp_play_caller = rev(play_caller)) %>% ungroup()
cat("play-caller coverage:", round(mean(!is.na(sched_long$play_caller)), 3), "| starter-id coverage:", round(mean(!is.na(sched_long$qb_id)), 3), "\n")

tg <- sched_long %>%
  left_join(box$tg %>% select(-season, -week, -opp), by = c("game_id", "team")) %>%
  left_join(pfr_team, by = c("game_id", "team")) %>%
  left_join(box$tg %>% select(game_id, opp = team, ya = yards), by = c("game_id", "opp")) %>%
  mutate(
    pa = pts_against - .data[[SC$pa_excl]],             # points charged to this D/ST under the active scoring system
    fp_sack = SC$sack * dst_sacks, fp_int = SC$int * dst_ints, fp_fr = SC$fum_rec * dst_fr,
    fp_td = SC$td * dst_td, fp_saf = SC$safety * dst_safety, fp_blk = SC$block_kick * dst_blk_kick + SC$block_pat * dst_blk_pat,
    fp_2pt = SC$two_pt_ret * dst_2pt, fp_pa = pa_points(pa), fp_ya = ya_points(ya),
    fp = fp_sack + fp_int + fp_fr + fp_td + fp_saf + fp_blk + fp_2pt + fp_pa + fp_ya,
    fp_big = fp_td + fp_saf + fp_blk + fp_2pt,
    g = as.integer(played), turnovers = ints + fum_lost, dst_big = dst_td + dst_safety + dst_blocks + dst_2pt
  ) %>% group_by(game_id) %>% mutate(fp_conceded = rev(fp), sacks_taken = rev(dst_sacks), to_given = rev(dst_ints + dst_fr)) %>% ungroup() %>%
  arrange(team, season, week)

tg <- if (!is.null(np_team)) tg %>% left_join(np_team, by = c("season", "week", "team")) else tg %>% mutate(!!!setNames(rep(list(NA_real_), length(ngs_cols)), ngs_cols))
tg <- tg %>% left_join(ftn_team, by = c("game_id", "team")) %>% arrange(team, season, week)

cat(sprintf("\n%s D/ST points by season (per team-game):\n", SC$label))
print(tg %>% filter(played, season >= 2018) %>% group_by(season) %>%
        summarise(n = n(), mean_fp = round(mean(fp), 2), sd_fp = round(sd(fp), 2), sacks = round(mean(dst_sacks), 2),
                  takeaways = round(mean(dst_ints + dst_fr), 2), td = round(mean(dst_td), 3), pa = round(mean(pa), 1), ya = round(mean(ya), 0)))

## ---- 6. As-of features: decayed, shrunk team rates ----
# Each rate = ratio of exponentially-decayed sums over the team's PREVIOUS games (decay LAMBDA per
# game, extra OFFSEASON discount at season boundaries), shrunk toward the prior-season league rate
# with M_GAMES pseudo-games. Only games strictly before the row's game are used → no leakage.
rate_defs <- tribble(
  ~name,        ~num,             ~den,        ~kind,
  "epa_play",   "epa_sum",        "plays",     "off",
  "epa_pass",   "pass_epa",       "pass_plays","off",
  "epa_rush",   "rush_epa",       "rush_plays","off",
  "succ",       "succ",           "plays",     "off",
  "succ_pass",  "pass_succ",      "pass_plays","off",
  "succ_rush",  "rush_succ",      "rush_plays","off",
  "ypp",        "yards",          "plays",     "off",
  "expl",       "explosive",      "plays",     "off",
  "expl_pass",  "explosive_pass", "pass_plays","off",
  "expl_rush",  "explosive_rush", "rush_plays","off",
  "sack_rate",  "sacks",          "dropbacks", "off",
  "hit_rate",   "qb_hits",        "dropbacks", "off",
  "press_rate", "pressured",      "dropbacks", "off",
  "hurry_rate", "hurried",        "dropbacks", "off",
  "blitz_rate", "blitzed",        "dropbacks", "off",
  "int_rate",   "ints",           "att",       "off",
  "bad_throw",  "bad_throws",     "att",       "off",
  "fum_rate",   "fumbles",        "plays",     "off",
  "fuml_rate",  "fum_lost",       "plays",     "off",
  "to_rate",    "turnovers",      "plays",     "off",
  "third_conv", "third_conv",     "third_att", "off",
  "neg_rate",   "neg_plays",      "plays",     "off",
  "pass_rate",  "pass_plays",     "plays",     "off",
  "pass_oe",    "pass_oe",        "pass_oe_n", "off",
  "td_drive",   "off_td",         "drives",    "off",
  "punt_drive", "punts",          "drives",    "off",
  "pen_play",   "pen",            "plays",     "off",
  "penyds_g",   "pen_yds",        "g",         "off",
  "plays_g",    "plays",          "g",         "off",
  "pts_g",      "pts_for",        "g",         "off",
  "yds_g",      "yards",          "g",         "off",
  "fpc_g",      "fp_conceded",    "g",         "off",
  "sacked_g",   "sacks_taken",    "g",         "off",
  "to_given_g", "to_given",       "g",         "off",
  "runloss_rate","run_loss",      "rush_plays","off",
  "ftn_iw_rate","ftn_iw",         "ftn_att",   "off",
  "ftn_oop_rate","ftn_oop",       "ftn_db",    "off",
  "ftn_ta_rate","ftn_ta",         "ftn_db",    "off",
  "ftn_qfs_rate","ftn_qfs",       "ftn_db",    "off",
  "ngs_press",  "np_qbp",         "np_pass",   "off",
  "ngs_ttt",    "np_ttt_x",       "np_att",    "off",
  "ngs_ttp",    "np_ttp_x",       "np_qbp",    "off",
  "ngs_p2s",    "np_sack",        "np_qbp",    "off",
  "ngs_blitz",  "np_blitz_x",     "np_pass",   "off",
  "ngs_sep",    "np_sep_x",       "np_att",    "off",
  "ngs_yacoe",  "np_yacoe",       "np_att",    "off",
  "ngs_pa",     "np_pa_x",        "np_pass",   "off",
  "ngs_ryoe",   "np_ryoe",        "np_run",    "off",
  "ngs_ybco",   "np_ybco_x",      "np_run",    "off",
  "ngs_stuff",  "np_stuff_x",     "np_run",    "off",
  "ngs_light",  "np_light_x",     "np_run",    "off",
  "ngs_stacked","np_stacked_x",   "np_run",    "off",
  "dst_sack_g", "dst_sacks",      "g",         "dst",
  "dst_int_g",  "dst_ints",       "g",         "dst",
  "dst_fr_g",   "dst_fr",         "g",         "dst",
  "dst_td_g",   "dst_td",         "g",         "dst",
  "dst_big_g",  "dst_big",        "g",         "dst",
  "fp_g",       "fp",             "g",         "dst",
  "ngs_getoff", "np_go_x",        "np_dpass",  "dst"
)
off_rates <- rate_defs$name[rate_defs$kind == "off"]; dst_rates <- rate_defs$name[rate_defs$kind == "dst"]
stat_cols <- setdiff(unique(c(rate_defs$num, rate_defs$den)), "g")
pfr_stats <- c("pressured", "hurried", "blitzed", "bad_throws")

# league priors: previous season's league-wide rate and denominator-per-game (per stat)
league_tot <- tg %>% filter(played) %>% group_by(season) %>%
  summarise(across(all_of(c(stat_cols, "g")), ~ sum(.x, na.rm = TRUE))) %>% arrange(season)
prior_for <- function(s) {   # returns list(rate = named vec, dpg = named vec) for season s
  rate <- dpg <- setNames(numeric(nrow(rate_defs)), rate_defs$name)
  for (i in seq_len(nrow(rate_defs))) {
    num <- rate_defs$num[i]; den <- rate_defs$den[i]
    ok <- league_tot %>% filter(.data[[den]] > 0, if (num %in% pfr_stats) season >= min(PFR_SEASONS) else TRUE)
    prev <- ok %>% filter(season < s) %>% slice_tail(n = 1); if (nrow(prev) == 0) prev <- ok %>% slice_head(n = 1)
    rate[i] <- prev[[num]] / prev[[den]]; dpg[i] <- prev[[den]] / prev$g
  }
  list(rate = rate, dpg = dpg)
}
priors <- map(set_names(PBP_SEASONS), prior_for)

asof_features <- function(d, lambda, offseason, m_games) {
  nums <- as.matrix(d[rate_defs$num]); dens <- as.matrix(d[rate_defs$den])
  nums[is.na(nums)] <- 0; dens[is.na(dens)] <- 0
  no_pfr <- d$season < min(PFR_SEASONS); pcol <- which(rate_defs$num %in% pfr_stats)
  nums[no_pfr, pcol] <- 0; dens[no_pfr, pcol] <- 0        # pre-2018: no pressure data → prior only
  out <- matrix(NA_real_, nrow(d), nrow(rate_defs), dimnames = list(NULL, rate_defs$name)); n_hist <- numeric(nrow(d))
  for (tm in unique(d$team)) {
    idx <- which(d$team == tm); S_num <- S_den <- rep(0, ncol(nums)); last_season <- NA; n <- 0
    for (i in idx) {
      s <- d$season[i]
      if (!is.na(last_season) && s != last_season) { S_num <- S_num * offseason; S_den <- S_den * offseason; n <- n * offseason }
      pr <- priors[[as.character(s)]]; m <- m_games * pr$dpg
      out[i, ] <- (S_num + m * pr$rate) / (S_den + m); n_hist[i] <- n
      if (d$played[i]) { S_num <- S_num * lambda + nums[i, ]; S_den <- S_den * lambda + dens[i, ]; n <- n * lambda + 1 }
      last_season <- s
    }
  }
  as_tibble(out) %>% mutate(n_hist = n_hist)
}

# Continuity-weighted offense rates. Same decayed/shrunk ratio as asof_features(), but each past game's
# weight is multiplied by
#   [share + (1 - share) * d_qb]  where share = fraction of that game's dropbacks by the CURRENT starter
#   [1 if same play caller else d_pc]
# d_qb = d_pc = 1 reproduces asof_features(); d = 0 uses only games with the current QB / play caller.
# Also returns qb_cont / pc_cont: share of the (decay-weighted) history that belongs to the current QB / caller.
qb_share <- box$qb %>% group_by(game_id, team) %>% mutate(share = qb_db / sum(qb_db)) %>% ungroup() %>% select(game_id, team, qb_id, share)
continuity_offense <- function(d, lambda, offseason, m_games, d_qb, d_pc) {
  nums <- as.matrix(d[rate_defs$num]); dens <- as.matrix(d[rate_defs$den])
  nums[is.na(nums)] <- 0; dens[is.na(dens)] <- 0
  no_pfr <- d$season < min(PFR_SEASONS); pcol <- which(rate_defs$num %in% pfr_stats)
  nums[no_pfr, pcol] <- 0; dens[no_pfr, pcol] <- 0
  out <- matrix(NA_real_, nrow(d), nrow(rate_defs), dimnames = list(NULL, rate_defs$name))
  qb_cont <- pc_cont <- numeric(nrow(d))
  for (tm in unique(d$team)) {
    idx <- which(d$team == tm)
    S <- qb_share %>% filter(team == tm) %>% pivot_wider(id_cols = game_id, names_from = qb_id, values_from = share, values_fill = 0)
    M <- as.matrix(S[-1]); gid <- S$game_id
    for (ii in seq_along(idx)) {
      i <- idx[ii]; prev <- idx[seq_len(ii - 1)]; prev <- prev[d$played[prev]]
      pr <- priors[[as.character(d$season[i])]]; m <- m_games * pr$dpg
      if (!length(prev)) { out[i, ] <- pr$rate; next }
      np <- length(prev)
      base_w <- lambda^(np - seq_len(np)) * offseason^(d$season[i] - d$season[prev])
      q <- d$qb_id[i]; rows <- match(d$game_id[prev], gid)
      shr <- if (is.na(q)) rep(1, np) else if (q %in% colnames(M)) coalesce(M[rows, q], 1) else ifelse(is.na(rows), 1, 0)
      pcm <- if (is.na(d$play_caller[i])) rep(1, np) else as.numeric(coalesce(d$play_caller[prev] == d$play_caller[i], TRUE))
      w <- base_w * (shr + (1 - shr) * d_qb) * (pcm + (1 - pcm) * d_pc)
      out[i, ] <- (colSums(w * nums[prev, , drop = FALSE]) + m * pr$rate) / (colSums(w * dens[prev, , drop = FALSE]) + m)
      qb_cont[i] <- sum(base_w * shr) / sum(base_w); pc_cont[i] <- sum(base_w * pcm) / sum(base_w)
    }
  }
  as_tibble(out) %>% mutate(qb_cont = qb_cont, pc_cont = pc_cont)
}

# Opposing-QB career rates (career-to-date before each game, shrunk with m_db pseudo-dropbacks)
qb_career <- function(box_qb, m_db = 150, m_press = 60) {
  lg <- box_qb %>% summarise(sack = sum(qb_sacks) / sum(qb_db), int = sum(qb_ints) / sum(qb_att),
                             fum = sum(qb_fum) / sum(qb_db), epa = sum(qb_epa) / sum(qb_db), hit = sum(qb_hits) / sum(qb_db),
                             p2s = sum(qb_sacked_pfr) / sum(qb_press), prate = sum(qb_press) / sum(qb_db_pfr))
  box_qb %>% arrange(qb_id, season, week) %>% group_by(qb_id) %>%
    mutate(across(c(qb_db, qb_att, qb_sacks, qb_ints, qb_fum, qb_epa, qb_hits, qb_press, qb_sacked_pfr, qb_db_pfr),
                  ~ cumsum(.x) - .x, .names = "c_{.col}")) %>%
    group_by(qb_id, season) %>% mutate(season_db_prior = cumsum(qb_db) - qb_db) %>% ungroup() %>%
    transmute(game_id, qb_id, qb_career_db = c_qb_db, qb_season_db = season_db_prior,
              qb_sack_rate = (c_qb_sacks + m_db * lg$sack) / (c_qb_db + m_db),
              qb_int_rate  = (c_qb_ints + m_db * lg$int) / (c_qb_att + m_db),
              qb_fum_rate  = (c_qb_fum + m_db * lg$fum) / (c_qb_db + m_db),
              qb_hit_rate  = (c_qb_hits + m_db * lg$hit) / (c_qb_db + m_db),
              qb_epa_db    = (c_qb_epa + m_db * lg$epa) / (c_qb_db + m_db),
              qb_p2s       = (c_qb_sacked_pfr + m_press * lg$p2s) / (c_qb_press + m_press),   # sacks per pressure: QB skill
              qb_press_rate = (c_qb_press + m_db * lg$prate) / (c_qb_db_pfr + m_db))          # pressures per dropback
}
qb_latest <- function(box_qb, m_db = 150) {     # career state after the last game played (for upcoming games)
  fut <- box_qb %>% group_by(qb_id) %>% slice_tail(n = 1) %>% ungroup() %>% mutate(game_id = "FUTURE", season = 9999L, week = 99L)
  qb_career(bind_rows(box_qb, fut), m_db) %>% filter(game_id == "FUTURE") %>% select(-game_id)
}
qb_vars <- c("qb_career_db", "qb_season_db", "qb_sack_rate", "qb_int_rate", "qb_fum_rate", "qb_hit_rate", "qb_epa_db", "qb_p2s", "qb_press_rate")

## ---- 7. Modelling frame ----
# CENTER_BY_WEEK: express every team / QB rate relative to the league average of the same week (as-of
# values of all teams that week), so drift in charting standards across seasons (FTN, NGS model updates,
# league-wide trends) isn't mistaken for team differences. Interactions are rebuilt from centred rates.
CENTER_BY_WEEK <- TRUE
center_exclude <- c("qb_career_db", "qb_season_db", "qb_log_db", "qb_new_starter", "o_qb_cont", "o_pc_cont", "o_n_hist")
build_frame <- function(lambda = 0.9, offseason = 0.5, m_games = 4, d_qb = 1, d_pc = 1, center = CENTER_BY_WEEK) {
  # own offense + own D/ST event rates
  own <- bind_cols(tg %>% select(game_id, team), asof_features(tg, lambda, offseason, m_games))
  # "allowed" rates: what opponents produced against this team, accumulated by defending team
  tg_def <- tg %>% select(game_id, season, week, team, opp, played, g) %>%
    left_join(tg %>% select(game_id, opp = team, all_of(stat_cols)), by = c("game_id", "opp")) %>% arrange(team, season, week)
  allowed <- bind_cols(tg_def %>% select(game_id, team), asof_features(tg_def, lambda, offseason, m_games) %>% select(all_of(off_rates))) %>%
    rename_with(~ paste0("d_", .x), all_of(off_rates))
  dst_own <- own %>% select(game_id, team, all_of(dst_rates)) %>% rename_with(~ paste0("d_", .x), all_of(dst_rates))
  cont <- continuity_offense(tg, lambda, offseason, m_games, d_qb, d_pc)
  offense <- bind_cols(tg %>% select(game_id, team), own %>% select(n_hist), cont %>% select(all_of(off_rates), qb_cont, pc_cont)) %>%
    rename_with(~ paste0("o_", .x), -c(game_id, team))
  qbc <- qb_career(box$qb); qbl <- qb_latest(box$qb) %>% rename_with(~ paste0(.x, "_latest"), all_of(qb_vars))
  out_med <- tg %>% filter(played, !(roof %in% c("dome", "closed"))) %>% summarise(temp = median(temp, na.rm = TRUE), wind = median(wind, na.rm = TRUE))
  fr <- tg %>%
    left_join(allowed, by = c("game_id", "team")) %>%
    left_join(dst_own, by = c("game_id", "team")) %>%
    left_join(offense, by = c("game_id", "opp" = "team")) %>%
    left_join(qbc, by = c("game_id", "opp_qb_id" = "qb_id")) %>%
    left_join(qbl, by = c("opp_qb_id" = "qb_id"))
  for (v in qb_vars) fr[[v]] <- coalesce(fr[[v]], fr[[paste0(v, "_latest")]])
  if (nrow(weather_override)) fr <- fr %>% left_join(weather_override %>% rename(temp_o = temp, wind_o = wind), by = "game_id") %>%
    mutate(temp = coalesce(temp_o, temp), wind = coalesce(wind_o, wind)) %>% select(-temp_o, -wind_o)
  if (!is.null(extra_team_week)) fr <- fr %>% left_join(extra_team_week, by = c("game_id", "team"))
  fr %>% select(-ends_with("_latest")) %>%
    mutate(
      qb_career_db = coalesce(qb_career_db, 0), qb_season_db = coalesce(qb_season_db, 0),
      across(c(qb_sack_rate, qb_int_rate, qb_fum_rate, qb_hit_rate, qb_epa_db, qb_p2s, qb_press_rate), ~ coalesce(.x, median(.x, na.rm = TRUE))),
      qb_log_db = log1p(qb_career_db), qb_new_starter = as.integer(qb_season_db < 30),
      implied_opp = (total_line - spread) / 2, implied_own = (total_line + spread) / 2,
      indoor = as.integer(is.na(roof) | roof %in% c("dome", "closed")),
      grass = as.integer(str_detect(coalesce(surface, "grass"), "grass")),
      temp = case_when(indoor == 1 ~ 70, is.na(temp) ~ out_med$temp, TRUE ~ temp),
      wind = case_when(indoor == 1 ~ 0,  is.na(wind) ~ out_med$wind, TRUE ~ wind),
      wind_hi = as.integer(wind >= 15), cold = as.integer(temp <= 35), rest_diff = rest - opp_rest,
      press_x = d_press_rate * o_press_rate, sack_x = d_sack_rate * o_sack_rate,
      to_x = (d_int_rate + d_fuml_rate) * (o_int_rate + o_fuml_rate)
    ) %>%
    { if (!center) . else {
        cc <- setdiff(grep("^(o_|d_|qb_)", names(.)[vapply(., is.numeric, logical(1))], value = TRUE), center_exclude)
        group_by(., season, week) %>% mutate(across(all_of(cc), ~ .x - mean(.x, na.rm = TRUE))) %>% ungroup() %>%
          mutate(press_x = d_press_rate * o_press_rate, sack_x = d_sack_rate * o_sack_rate,
                 to_x = (d_int_rate + d_fuml_rate) * (o_int_rate + o_fuml_rate))
      } }
}
# Hook for extra team-week features from your own exports (e.g. NGS pro.nfl.com QBP %):
# a tibble with game_id, team, <numeric features>; it is left-joined in build_frame().
extra_team_week <- NULL

## ---- 8. Feature sets, models, forward-chained CV ----
ctx_vars <- c("spread", "total_line", "implied_opp", "implied_own", "home", "indoor", "grass",
              "temp", "wind", "wind_hi", "cold", "rest_diff", "div_game", "week")
# qb_p2s / qb_press_rate are computed in the frame but left out: no gain for sack prediction or D/ST CV (2026-09 test)
qbf_vars <- c("qb_sack_rate", "qb_int_rate", "qb_fum_rate", "qb_hit_rate", "qb_epa_db", "qb_log_db", "qb_new_starter")
# Feature families: each family holds the o_ (opponent offense) and d_ (this defense) versions of related
# rates. 42_dst_feature_selection.R picks families by forward selection on 2021–24 back-test folds (2025 held
# out); its choice is saved to output/dst/selected_groups.rds and used here. Vegas is always in.
family_of <- function(v) {
  b <- sub("^(o_|d_)", "", v)
  case_when(
    v %in% c("spread", "total_line", "implied_opp", "implied_own", "home") ~ "vegas",
    v %in% c("indoor", "grass", "temp", "wind", "wind_hi", "cold", "rest_diff", "div_game", "week") ~ "environment",
    startsWith(v, "qb_") ~ "qb_career",
    b %in% c("qb_cont", "pc_cont", "n_hist") ~ "continuity",
    b %in% c("epa_play", "epa_pass", "epa_rush", "succ", "succ_pass", "succ_rush", "ypp", "third_conv", "td_drive", "pts_g", "yds_g") ~ "efficiency",
    b %in% c("expl", "expl_pass", "expl_rush") ~ "explosives",
    b %in% c("sack_rate", "hit_rate", "sacked_g") | v == "sack_x" ~ "sacks_hits",
    b %in% c("press_rate", "hurry_rate", "blitz_rate", "bad_throw") | v == "press_x" ~ "pfr_pressure",
    b %in% c("int_rate", "fum_rate", "fuml_rate", "to_rate", "to_given_g") | v == "to_x" ~ "turnovers",
    b %in% c("neg_rate", "runloss_rate") ~ "negative_plays",
    b %in% c("pass_rate", "pass_oe", "plays_g", "punt_drive") ~ "volume_tendency",
    b %in% c("pen_play", "penyds_g") ~ "penalties",
    startsWith(b, "dst_") | b %in% c("fp_g", "fpc_g") ~ "dst_history",
    b %in% c("ngs_press", "ngs_ttp", "ngs_p2s", "ngs_blitz", "ngs_getoff") ~ "ngs_pass_rush",
    b %in% c("ngs_ttt", "ngs_sep", "ngs_yacoe", "ngs_pa") ~ "ngs_passing",
    b %in% c("ngs_ryoe", "ngs_ybco", "ngs_stuff", "ngs_light", "ngs_stacked") ~ "ngs_run_game",
    startsWith(b, "ftn_") ~ "ftn_charting",
    TRUE ~ "other")
}
candidate_vars <- function(fr) c(ctx_vars, qbf_vars, grep("^(d_|o_)", names(fr), value = TRUE), "press_x", "sack_x", "to_x")
SELECTED_FILE <- file.path(OUT_DIR, "selected_groups.rds")
# Before any selection run: the original feature set (everything except NGS, FTN and run-loss).
DEFAULT_GROUPS <- c("vegas", "environment", "qb_career", "continuity", "efficiency", "explosives", "sacks_hits", "pfr_pressure",
                    "turnovers", "negative_plays", "volume_tendency", "penalties", "dst_history")
model_vars <- function(fr, groups = NULL) {
  v <- candidate_vars(fr)
  if (is.null(groups)) {
    groups <- if (file.exists(SELECTED_FILE)) readRDS(SELECTED_FILE)$groups else DEFAULT_GROUPS
    if (!file.exists(SELECTED_FILE)) v <- setdiff(v, c("o_runloss_rate", "d_runloss_rate"))   # original set
  }
  v[family_of(v) %in% union("vegas", groups)]
}

metrics <- function(y, yhat, season, week) {
  d <- tibble(y, yhat, season, week) %>% group_by(season, week) %>%
    mutate(rk = rank(-yhat, ties.method = "first"), n = n()) %>%
    summarise(rho = suppressWarnings(cor(y, yhat, method = "spearman")),
              top8 = mean(y[rk <= 8]), bot8 = mean(y[rk > n - 8]), all = mean(y), .groups = "drop")
  tibble(rmse = sqrt(mean((y - yhat)^2)), mae = mean(abs(y - yhat)), spearman = mean(d$rho, na.rm = TRUE),
         top8_avg = mean(d$top8), bot8_avg = mean(d$bot8), edge_top8 = mean(d$top8 - d$all))
}

sv <- function(v) paste(v, collapse = " + ")
sack_vars <- c("implied_opp", "implied_own", "spread", "home", "indoor", "d_press_rate", "d_sack_rate", "d_hit_rate", "d_blitz_rate",
               "o_press_rate", "o_sack_rate", "o_hit_rate", "o_pass_rate", "o_pass_oe", "o_neg_rate", "o_plays_g",
               "qb_sack_rate", "qb_hit_rate", "qb_log_db", "d_dst_sack_g", "o_qb_cont", "o_pc_cont")
to_vars   <- c("implied_opp", "spread", "home", "wind_hi", "cold", "d_int_rate", "d_fuml_rate", "d_press_rate", "o_int_rate", "o_fuml_rate",
               "o_bad_throw", "o_press_rate", "o_to_rate", "qb_int_rate", "qb_fum_rate", "qb_log_db", "d_dst_int_g", "d_dst_fr_g")
pa_vars   <- c("implied_opp", "implied_own", "spread", "home", "indoor", "wind_hi", "cold", "d_epa_play", "d_succ", "d_td_drive", "d_pts_g",
               "o_epa_play", "o_succ", "o_td_drive", "o_pts_g", "o_to_rate", "d_to_rate", "qb_epa_db")
ya_vars   <- c("implied_opp", "total_line", "spread", "home", "indoor", "wind_hi", "cold", "d_ypp", "d_yds_g", "d_plays_g", "d_expl", "d_succ",
               "o_ypp", "o_yds_g", "o_plays_g", "o_pass_rate", "o_expl", "o_succ", "qb_epa_db")

fit_predict <- function(model, tr, te, vars) {
  X <- as.matrix(tr[vars]); Xte <- as.matrix(te[vars]); y <- tr$fp
  pr <- function(fml, fam) predict(glm(fml, data = tr, family = fam), te, type = "response")
  switch(model,
    vegas_lm = predict(lm(fp ~ spread + total_line + implied_opp + home, data = tr), te),
    vegas_plus = predict(lm(fp ~ spread + implied_opp + home + o_fpc_g + d_fp_g + o_sack_rate + d_press_rate + o_to_rate + qb_sack_rate + qb_int_rate, data = tr), te),
    enet_w = { set.seed(SEED); as.numeric(predict(cv.glmnet(X, pmin(y, Y_CAP), alpha = 0.5, nfolds = 5), Xte, s = "lambda.min")) },
    ridge = { set.seed(SEED); as.numeric(predict(cv.glmnet(X, y, alpha = 0,   nfolds = 5), Xte, s = "lambda.min")) },
    enet  = { set.seed(SEED); as.numeric(predict(cv.glmnet(X, y, alpha = 0.5, nfolds = 5), Xte, s = "lambda.min")) },
    gam = {
      fml <- fp ~ s(implied_opp) + s(spread) + s(d_press_rate) + s(o_press_rate) + s(o_sack_rate) + s(d_sack_rate) +
        s(o_int_rate) + s(d_int_rate) + s(o_epa_play) + s(d_epa_play) + s(qb_sack_rate) + s(qb_int_rate) +
        s(d_fp_g) + s(o_to_rate) + s(o_fuml_rate) + s(d_fuml_rate) + s(qb_log_db, k = 5) + home + indoor
      as.numeric(predict(bam(fml, data = tr, method = "fREML", select = TRUE, discrete = TRUE), te))
    },
    rf = { set.seed(SEED); predict(ranger(x = tr[vars], y = y, num.trees = 600, mtry = floor(length(vars) / 3), min.node.size = 25), te[vars])$predictions },
    gbm = {
      set.seed(SEED)
      m <- gbm.fit(x = as.data.frame(tr[vars]), y = y, distribution = "gaussian", n.trees = GBM_TREES, interaction.depth = 2,
                   shrinkage = 0.005, n.minobsinnode = 60, bag.fraction = 0.6, verbose = FALSE)
      predict(m, as.data.frame(te[vars]), n.trees = GBM_TREES)
    },
    components = SC$sack * pr(as.formula(paste("dst_sacks ~", sv(sack_vars))), quasipoisson) +
      SC$int * pr(as.formula(paste("dst_ints ~", sv(to_vars))), quasipoisson) +
      SC$fum_rec * pr(as.formula(paste("dst_fr ~", sv(to_vars))), quasipoisson) +
      pr(as.formula(paste("fp_big ~", sv(c(to_vars, "d_dst_td_g")))), gaussian) +
      pr(as.formula(paste("fp_pa ~", sv(pa_vars))), gaussian) +
      (if (is.null(SC$ya_breaks)) 0 else pr(as.formula(paste("fp_ya ~", sv(ya_vars))), gaussian))
  )
}

run_cv <- function(fr, models, vars, seasons = CV_SEASONS, verbose = TRUE) {
  fr <- fr %>% filter(played, season %in% TRAIN_SEASONS)
  preds <- map_dfr(seasons, function(s) {
    tr <- fr %>% filter(season < s); te <- fr %>% filter(season == s)
    out <- te %>% select(game_id, team, season, week, fp)
    for (m in models) out[[m]] <- fit_predict(m, tr, te, vars)
    if (verbose) message("  cv season ", s, " done"); out
  })
  list(preds = preds, summary = map_dfr(models, ~ metrics(preds$fp, preds[[.x]], preds$season, preds$week) %>% mutate(model = .x, .before = 1)))
}

# Portable blend: the production models reduced to coefficient vectors, so a fitted blend can be saved and
# re-scored later without refitting or re-reading any data (45_dst_refresh.R re-scores with new Vegas lines).
# Gives exactly the same numbers as fit_predict() for enet / ridge / components.
GLM_SPECS <- list(
  sacks = list(y = "dst_sacks",            x = sack_vars,                  fam = "quasipoisson"),
  ints  = list(y = "dst_ints",             x = to_vars,                    fam = "quasipoisson"),
  fr    = list(y = "dst_fr",               x = to_vars,                    fam = "quasipoisson"),
  big   = list(y = "fp_big",               x = c(to_vars, "d_dst_td_g"),   fam = "gaussian"),
  pa    = list(y = "fp_pa",                x = pa_vars,                    fam = "gaussian"),
  ya    = list(y = "fp_ya",                x = ya_vars,                    fam = "gaussian"),
  to    = list(y = "I(dst_ints + dst_fr)", x = to_vars,                    fam = "quasipoisson"),   # E[TO] column
  td    = list(y = "I(dst_td > 0)",        x = c(to_vars, "d_dst_td_g"),   fam = "binomial"))       # P(TD) column
COMP_PARTS <- c("sacks", "ints", "fr", "big", "pa", "ya")
glm_coefs <- function(tr, which = names(GLM_SPECS)) map(GLM_SPECS[which], function(s) {
  b <- coef(glm(as.formula(paste(s$y, "~", sv(s$x))), data = tr, family = get(s$fam))); b[is.na(b)] <- 0; b })
glmnet_coef <- function(fit, s = NULL) { m <- as.matrix(if (is.null(s)) coef(fit) else coef(fit, s = s)); setNames(m[, 1], rownames(m)) }
source(file.path(PROJ_DIR, "scripts/dst_blend_utils.R"))     # lin_pred, glm_pred, comp_pred, blend_pred, outcome_dist, …

# 8a. tune decay / shrinkage on a cheap model (elastic net)
cat("\n--- Tuning feature-decay parameters (elastic net, forward CV) ---\n")
tune_cache <- file.path(OUT_DIR, "tune_grid.rds")
if (file.exists(tune_cache) && !RETUNE) tune <- readRDS(tune_cache) else {
  grid <- expand.grid(lambda = c(0.85, 0.92, 0.97), offseason = c(0.3, 0.6), m_games = c(3, 8))
  tune <- pmap_dfr(grid, function(lambda, offseason, m_games) {
    fr <- build_frame(lambda, offseason, m_games)
    run_cv(fr, "enet", model_vars(fr), verbose = FALSE)$summary %>% mutate(lambda, offseason, m_games)
  })
  saveRDS(tune, tune_cache)
}
print(tune %>% arrange(rmse) %>% select(lambda, offseason, m_games, rmse, mae, spearman, edge_top8) %>% mutate(across(where(is.numeric), ~ round(.x, 4))))
best  <- tune %>% arrange(rmse) %>% slice(1)

# 8a-2. QB / play-caller continuity discounts (decay knobs fixed at the stage-1 optimum).
# d = 1 is the old model (no continuity adjustment), so the grid contains the baseline.
cat("\n--- Tuning QB / play-caller continuity discounts (elastic net, forward CV) ---\n")
cont_cache <- file.path(OUT_DIR, "tune_continuity.rds")
changed_rows <- function(fr) fr$o_qb_cont < 0.5 | fr$o_pc_cont < 0.5      # opponent mostly new QB or new caller
if (file.exists(cont_cache) && !RETUNE) tune_c <- readRDS(cont_cache) else {
  grid_c <- expand.grid(d_qb = c(1, 0.5, 0.25, 0.1), d_pc = c(1, 0.5, 0.25))
  tune_c <- pmap_dfr(grid_c, function(d_qb, d_pc) {
    fr <- build_frame(best$lambda, best$offseason, best$m_games, d_qb, d_pc)
    cvr <- run_cv(fr, "enet", model_vars(fr), verbose = FALSE)
    chg <- cvr$preds %>% left_join(fr %>% select(game_id, team, o_qb_cont, o_pc_cont), by = c("game_id", "team")) %>% filter(changed_rows(.))
    message(sprintf("  d_qb=%.2f d_pc=%.2f rmse=%.4f", d_qb, d_pc, cvr$summary$rmse))
    cvr$summary %>% mutate(d_qb, d_pc, n_changed = nrow(chg), rmse_changed = sqrt(mean((chg$fp - chg$enet)^2)))
  })
  saveRDS(tune_c, cont_cache)
}
print(tune_c %>% arrange(rmse) %>% select(d_qb, d_pc, rmse, mae, spearman, edge_top8, n_changed, rmse_changed) %>% mutate(across(where(is.numeric), ~ round(.x, 4))))
best_c <- tune_c %>% arrange(rmse) %>% slice(1)
frame <- build_frame(best$lambda, best$offseason, best$m_games, best_c$d_qb, best_c$d_pc)
vars  <- model_vars(frame)
stopifnot(!anyNA(frame[frame$played & frame$season >= 2018, vars]))
saveRDS(frame, file.path(OUT_DIR, "model_frame.rds"))

# 8b. compare model families
cat(sprintf("\n--- Back-test %s (trained on all earlier seasons) ---\n", paste(CV_SEASONS, collapse = ", ")))
cv <- run_cv(frame, c("vegas_lm", "vegas_plus", "ridge", "enet", "enet_w", "gam", "rf", "gbm", "components"), vars)
cv$preds <- cv$preds %>% mutate(ens = (enet + components + ridge) / 3, ens_gbm = (enet + components + gbm) / 3,
                                ens_all = (ridge + enet + gam + rf + gbm + components) / 6)
for (m in c("ens", "ens_gbm", "ens_all")) cv$summary <- bind_rows(cv$summary, metrics(cv$preds$fp, cv$preds[[m]], cv$preds$season, cv$preds$week) %>% mutate(model = m, .before = 1))
# paired weekly test: does the model's top-8 beat the Vegas-only top-8?
wk_top8 <- function(yhat) cv$preds %>% mutate(yhat = yhat) %>% group_by(season, week) %>%
  summarise(top8 = mean(fp[rank(-yhat, ties.method = "first") <= 8]), .groups = "drop") %>% pull(top8)
base <- wk_top8(cv$preds$vegas_lm)
cv$summary <- cv$summary %>% rowwise() %>% mutate(
  vs_vegas_top8 = mean(wk_top8(cv$preds[[model]]) - base),
  t_stat = { d <- wk_top8(cv$preds[[model]]) - base; if (sd(d) > 0) mean(d) / (sd(d) / sqrt(length(d))) else NA_real_ }) %>% ungroup()
print(cv$summary %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% arrange(rmse), width = 200)
saveRDS(cv, file.path(OUT_DIR, "cv_results.rds"))

# 8c. what drives it
trn <- frame %>% filter(played, season %in% TRAIN_SEASONS)
set.seed(SEED)
gbm_fit <- gbm.fit(x = as.data.frame(trn[vars]), y = trn$fp, distribution = "gaussian", n.trees = GBM_TREES,
                   interaction.depth = 2, shrinkage = 0.005, n.minobsinnode = 60, bag.fraction = 0.6, verbose = FALSE)
imp <- summary(gbm_fit, plotit = FALSE) %>% as_tibble() %>% transmute(feature = var, rel_inf = rel.inf)
cat("\nTop 25 GBM features (relative influence):\n"); print(imp %>% slice_head(n = 25) %>% mutate(rel_inf = round(rel_inf, 2)), n = 25)
set.seed(SEED)
enet_fit <- cv.glmnet(as.matrix(trn[vars]), trn$fp, alpha = 0.5, nfolds = 5)
coefs <- as.matrix(coef(enet_fit, s = "lambda.min")) %>% as.data.frame() %>% rownames_to_column("feature") %>% rename(coef = 2) %>%
  filter(coef != 0, feature != "(Intercept)") %>% mutate(std_coef = coef * sapply(feature, function(v) sd(trn[[v]]))) %>% arrange(desc(abs(std_coef)))
cat("\nElastic-net coefficients (points per 1 SD of feature):\n"); print(as_tibble(coefs) %>% mutate(across(where(is.numeric), ~ round(.x, 3))), n = 40)

## ---- 9. Final fit & projections for PRED_SEASON / PRED_WEEK ----
FINAL_MODELS <- c("enet", "components", "ridge")   # edit after inspecting 8b
te <- frame %>% filter(season == PRED_SEASON, week == PRED_WEEK); stopifnot(nrow(te) > 0)
pred <- te %>% select(game_id, team, opp, home, opp_qb_name, opp_play_caller, o_qb_cont, o_pc_cont, spread, total_line, implied_opp, indoor, wind, temp,
                      d_press_rate, o_press_rate, d_sack_rate, o_sack_rate, o_int_rate, o_fuml_rate, qb_sack_rate, qb_int_rate, d_fp_g)
for (m in FINAL_MODELS) pred[[m]] <- fit_predict(m, trn, te, vars)
pr_te <- function(fml, fam) predict(glm(fml, data = trn, family = fam), te, type = "response")
pred <- pred %>% mutate(
  proj = rowMeans(across(all_of(FINAL_MODELS))),
  e_sacks = pr_te(as.formula(paste("dst_sacks ~", sv(sack_vars))), quasipoisson),
  e_to    = pr_te(as.formula(paste("I(dst_ints + dst_fr) ~", sv(to_vars))), quasipoisson),
  e_pa    = pr_te(as.formula(paste("fp_pa ~", sv(pa_vars))), gaussian),
  e_ya    = if (is.null(SC$ya_breaks)) 0 else pr_te(as.formula(paste("fp_ya ~", sv(ya_vars))), gaussian),
  p_td    = pr_te(as.formula(paste("I(dst_td > 0) ~", sv(c(to_vars, "d_dst_td_g")))), binomial)
) %>% arrange(desc(proj)) %>% mutate(rank = row_number(), .before = 1)
## ---- 9b. Uncertainty: outcome ranges, confidence intervals, top-8 odds ----
# (1) Outcome range — how much the actual score can vary. Built from the blend's OUT-OF-SAMPLE back-test
#     predictions (each season predicted by models trained on earlier seasons): take back-test games whose
#     projection was close to this one (Gaussian kernel, UNC_BW points) and apply their errors to this
#     projection. Keeps D/ST's right skew (TDs). Checked 2026-09-23: ranges built from 2021–24 covered 80.3% of
#     2025 scores (80% range) and 51.1% (50% range). Pool extended to 2019+ the same day (2025 test: CRPS tied or
#     slightly better in all three formats, 80% coverage 80.7 / 79.8 / 79.6%). Alternatives tested and rejected (no gain):
#     feature-based boom/bust logits, quantile regression, and a component Monte Carlo (sacks/INT/FR/TD counts +
#     points/yards allowed joined by an empirical copula) — outcome shape tracks the projection level.
# (2) 90% CI of the projection — how sure the model is about the expected score: refit the blend on
#     BOOT_REPS bootstrap resamples of whole weeks (penalties fixed at the tuned values).
# (3) P(top 8) — share of simulated weeks in which the D/ST finishes top 8 among this week's teams.
UNC_SEASONS <- 2019:(PRED_SEASON - 1)   # every season that can be predicted out of sample (2018 has no earlier training season)
UNC_BW      <- 0.75
BOOT_REPS   <- 100
N_SIM       <- 4000
unc_cache <- file.path(OUT_DIR, "uncertainty_backtest.rds")
unc_deps  <- c(SELECTED_FILE, tune_cache, cont_cache)
if (!file.exists(unc_cache) || any(file.exists(unc_deps) & file.mtime(unc_deps) > file.mtime(unc_cache))) {
  message("building back-test residuals for uncertainty (", paste(range(UNC_SEASONS), collapse = "–"), ")")
  unc_bt <- run_cv(frame, FINAL_MODELS, vars, seasons = UNC_SEASONS, verbose = FALSE)$preds %>%
    mutate(proj = rowMeans(across(all_of(FINAL_MODELS)))) %>% select(season, week, game_id, team, fp, proj)
  saveRDS(unc_bt, unc_cache)
} else unc_bt <- readRDS(unc_cache)
unc_resid <- unc_bt$fp - unc_bt$proj
od <- outcome_dist(pred$proj, unc_bt$proj, unc_resid, UNC_BW, N_SIM, SEED)         # q10…q90, P(10+), P(<3), P(top 8)
# bootstrap CI of the projection
Xb <- as.matrix(trn[vars]); yb <- trn$fp
set.seed(SEED); cv_e <- cv.glmnet(Xb, yb, alpha = 0.5, nfolds = 5); lam_e <- cv_e$lambda.min   # same fits as fit_predict()
set.seed(SEED); cv_r <- cv.glmnet(Xb, yb, alpha = 0,   nfolds = 5); lam_r <- cv_r$lambda.min
wk_rows <- split(seq_len(nrow(trn)), paste(trn$season, trn$week))
set.seed(SEED)
boot_models <- lapply(seq_len(BOOT_REPS), function(b) {      # coefficients only → the daily refresh can re-score them
  i <- unlist(wk_rows[sample(names(wk_rows), replace = TRUE)], use.names = FALSE)
  list(enet  = glmnet_coef(glmnet(Xb[i, ], yb[i], alpha = 0.5, lambda = lam_e)),
       ridge = glmnet_coef(glmnet(Xb[i, ], yb[i], alpha = 0,   lambda = lam_r)),
       comp  = glm_coefs(trn[i, ], COMP_PARTS))
})
te_p <- te[match(pred$team, te$team), ]                       # te in pred's (ranked) order
boot <- sapply(boot_models, blend_pred, te = te_p, sc = SC, specs = GLM_SPECS, models = FINAL_MODELS)
pred <- bind_cols(pred, od, boot_summary(boot))

## ---- 9c. Scoring bundle for the daily Vegas refresh (45_dst_refresh.R) ----
# Everything needed to re-score this week with new spreads / totals, without data or refitting:
# the blend as coefficients, the bootstrap refits, the back-test residual pool and this week's feature rows.
main_model <- list(enet = glmnet_coef(cv_e, "lambda.min"), ridge = glmnet_coef(cv_r, "lambda.min"), comp = glm_coefs(trn))
chk <- max(abs(blend_pred(main_model, te_p, SC, GLM_SPECS, FINAL_MODELS) - pred$proj),
           abs(as.matrix(component_cols(main_model$comp, GLM_SPECS, te_p, SC)) - as.matrix(pred[c("e_sacks", "e_to", "e_pa", "e_ya", "p_td")])))
if (chk > 1e-6) warning(sprintf("portable blend differs from fitted models by %.2g points: check the refresh bundle", chk))
bundle_vars <- unique(c(vars, unlist(map(GLM_SPECS, "x"))))
bundle <- list(system = SCORING_SYSTEM, SC = SC, season = PRED_SEASON, week = PRED_WEEK, final_models = FINAL_MODELS,
               specs = GLM_SPECS, main = main_model, boot = boot_models,
               te = te_p %>% select(game_id, team, opp, home, gameday, all_of(bundle_vars)),
               unc = list(proj = unc_bt$proj, resid = unc_resid, bw = UNC_BW, n_sim = N_SIM, seed = SEED),
               max_diff = chk, created = Sys.time())
saveRDS(bundle, file.path(OUT_DIR, sprintf("bundle_%d_wk%02d.rds", PRED_SEASON, PRED_WEEK)), compress = "xz")
message(sprintf("saved refresh bundle (portable-vs-fitted max diff %.1e)", chk))

cat(sprintf("\n=== %s D/ST projections: %d week %d ===\n", SC$label, PRED_SEASON, PRED_WEEK))
print(pred %>% transmute(rank, team, opp = paste0(ifelse(home == 1, "vs ", "@ "), opp), opp_qb = opp_qb_name, spread, imp_opp = implied_opp,
                         proj = round(proj, 2), across(all_of(FINAL_MODELS), ~ round(.x, 2)),
                         e_sacks = round(e_sacks, 2), e_to = round(e_to, 2), e_pa = round(e_pa, 2), e_ya = round(e_ya, 2), p_td = round(p_td, 2),
                         ci = sprintf("%.1f–%.1f", ci_lo, ci_hi), range80 = paste(as.integer(round(q10)), "to", as.integer(round(q90))), p10 = round(p_boom, 2), p_top8 = round(p_top8, 2)), n = 32, width = 250)
write.csv(pred, file.path(OUT_DIR, sprintf("dst_proj_%d_wk%02d.csv", PRED_SEASON, PRED_WEEK)), row.names = FALSE)

## ---- 10. Glossary (generated from the feature definitions) ----
# Every model feature, report column, model name and metric gets a plain-English definition. Rates are
# as-of (games before this one only), decayed and shrunk as described in section 6.
off_desc <- tribble(
  ~name,        ~o_desc,                                                                                   ~d_desc,
  "epa_play",   "EPA per play (expected points added; all scrimmage plays)",                               "EPA per play allowed",
  "epa_pass",   "EPA per pass play (dropbacks incl. sacks and scrambles)",                                 "passing EPA per play allowed",
  "epa_rush",   "EPA per designed run",                                                                    "rushing EPA per play allowed",
  "succ",       "success rate: share of plays with EPA > 0",                                               "success rate allowed",
  "succ_pass",  "pass success rate",                                                                       "pass success rate allowed",
  "succ_rush",  "rush success rate",                                                                       "rush success rate allowed",
  "ypp",        "yards per scrimmage play",                                                                "yards per play allowed",
  "expl",       "explosive-play rate: plays gaining 20+ yards per play",                                   "20+ yard plays allowed per play",
  "expl_pass",  "20+ yard pass plays per pass play",                                                       "20+ yard pass plays allowed per pass play",
  "expl_rush",  "12+ yard runs per rush",                                                                  "12+ yard runs allowed per rush",
  "sack_rate",  "sacks taken per dropback (pass protection + QB)",                                         "sacks per opponent dropback (this defense's sack rate)",
  "hit_rate",   "QB hits taken per dropback (pbp qb_hit flag)",                                            "QB hits per opponent dropback",
  "press_rate", "pressures allowed per dropback (PFR hurries + hits + sacks; 2018+)",                       "pressure rate generated (PFR; 2018+)",
  "hurry_rate", "hurries allowed per dropback (PFR; 2018+)",                                               "hurries generated per dropback (PFR; 2018+)",
  "blitz_rate", "how often this offense's QB faced a blitz, per dropback (PFR; 2018+)",                   "this defense's blitz rate per dropback (PFR; 2018+)",
  "int_rate",   "interceptions thrown per pass attempt",                                                   "interceptions per opponent pass attempt",
  "bad_throw",  "bad-throw % (PFR bad throws per attempt; 2018+)",                                         "opponents' bad-throw % against this defense (PFR; 2018+)",
  "fum_rate",   "fumbles per play (lost or not)",                                                          "opponent fumbles forced per play",
  "fuml_rate",  "fumbles lost per play",                                                                   "opponent fumbles recovered per play",
  "to_rate",    "turnovers (INT + fumbles lost) per play",                                                 "takeaways per opponent play",
  "third_conv", "3rd-down conversion rate",                                                                "3rd-down conversion rate allowed",
  "neg_rate",   "negative-play rate: plays losing yards (sacks, runs for loss, other losses) per play",   "negative plays forced per opponent play",
  "pass_rate",  "pass plays as a share of all plays",                                                      "opponents' pass share against this defense (reflects game script)",
  "pass_oe",    "pass rate over expected (nflfastR pass_oe, % points; aggressiveness net of situation)",  "opponents' pass rate over expected against this defense",
  "td_drive",   "offensive TDs per drive",                                                                 "offensive TDs allowed per drive",
  "punt_drive", "punts per drive",                                                                         "opponent punts forced per drive",
  "pen_play",   "penalties committed (offense, defense and ST) per offensive play",                        "penalties committed by opponents per their offensive play (penalties drawn)",
  "penyds_g",   "penalty yards committed per game",                                                        "opponent penalty yards per game (drawn)",
  "plays_g",    "offensive plays per game (pace / volume)",                                                "opponent offensive plays per game",
  "pts_g",      "points scored per game",                                                                  "points allowed per game",
  "yds_g",      "offensive yards per game",                                                                "yards allowed per game",
  "fpc_g",      "D/ST fantasy points conceded per game (what opposing D/STs scored against this offense)", "this D/ST's fantasy points per game (mirror of d_fp_g)",
  "sacked_g",   "sacks taken per game",                                                                    "sacks per game",
  "to_given_g", "takeaways given up per game (INTs + fumbles recovered by the other team)",                "takeaways per game (INTs + fumble recoveries)",
  "runloss_rate","runs for a loss per rush (the run-blocking half of negative plays)",                   "runs for a loss forced per opponent rush",
  "ftn_iw_rate","interception-worthy throws per pass attempt (FTN charting, 2022+)",                        "interception-worthy throws forced per opponent attempt (FTN, 2022+)",
  "ftn_oop_rate","dropbacks where the QB left the pocket (FTN, 2022+)",                                     "opponent QB out-of-pocket rate against this defense (FTN, 2022+)",
  "ftn_ta_rate","throwaways per dropback (FTN, 2022+)",                                                      "opponent throwaways per dropback (FTN, 2022+)",
  "ftn_qfs_rate","sacks charted as the QB's fault, per dropback (FTN, 2022+)",                              "opponent QB-fault sacks per dropback (FTN, 2022+)",
  "ngs_press",  "NGS pressure rate allowed: QB pressures (NGS pressure-probability model; hurries, hits, sacks) per dropback (NFL Pro, 2018+)", "NGS pressure rate generated per opponent dropback (NFL Pro QBP %, 2018+)",
  "ngs_ttt",    "NGS average time to throw, seconds (snap to release; excludes sacks)",                   "opponents' average time to throw against this defense (NGS)",
  "ngs_ttp",    "NGS average time to pressure allowed, seconds (lower = pressure arrives faster)",         "this defense's average time to pressure, seconds (NGS; lower = faster)",
  "ngs_p2s",    "sacks taken per NGS pressure (how often pressure becomes a sack)",                       "sacks per NGS pressure generated",
  "ngs_blitz",  "share of dropbacks facing 5+ rushers (NGS blitz rate faced)",                             "this defense's NGS blitz rate (5+ rushers per dropback)",
  "ngs_sep",    "NGS average target separation, yards (receiver to nearest defender at pass arrival)",    "average separation allowed by this defense, yards (NGS)",
  "ngs_yacoe",  "NGS yards after catch over expected per pass attempt",                                   "YAC over expected allowed per opponent pass attempt (NGS)",
  "ngs_pa",     "play-action rate (NGS)",                                                                  "opponents' play-action rate against this defense (NGS)",
  "ngs_ryoe",   "NGS rushing yards over expected per carry",                                              "rushing yards over expected allowed per carry (NGS)",
  "ngs_ybco",   "NGS yards before contact per carry (run blocking)",                                      "yards before contact allowed per carry (NGS)",
  "ngs_stuff",  "NGS run stuff rate: carries for no gain or a loss",                                      "run stuff rate forced (NGS)",
  "ngs_light",  "share of carries against light boxes (≤ 6 defenders; NGS)",                              "share of opponent carries this defense played with a light box (NGS)",
  "ngs_stacked","share of carries against stacked boxes (8+ defenders; NGS)",                             "share of opponent carries this defense played with a stacked box (NGS)"
)
dst_desc <- c(dst_sack_g = "this D/ST's sacks per game", dst_int_g = "this D/ST's interceptions per game",
              dst_fr_g = "this D/ST's fumble recoveries per game (defense and special teams)",
              dst_td_g = "this D/ST's return / defensive TDs per game", dst_big_g = "this D/ST's TDs + safeties + blocked kicks + 2-pt returns per game",
              fp_g = "this D/ST's fantasy points per game (active scoring system)",
              ngs_getoff = "this defense's average pass-rush get-off, seconds from snap to rusher crossing the line (NGS; lower = quicker)")
RATE_NOTE <- "[as-of rate]"
RATE_NOTE_LONG <- "[as-of rate] = computed only from games before the one being predicted; each past game weighted 0.92^(games ago), ×0.3 per offseason crossed, then shrunk toward last season's league average with 3 pseudo-games. o_ = the opponent's offense; d_ = this team's defense (what it allowed or forced) or its own D/ST scoring events."
other_desc <- tribble(
  ~feature, ~group, ~definition,
  "spread", "Vegas & game", "point spread from this D/ST's side: + = its team is favored by that many points",
  "total_line", "Vegas & game", "Vegas over/under (total points)",
  "implied_opp", "Vegas & game", "opponent's Vegas implied points = (total − spread) / 2; the single strongest predictor",
  "implied_own", "Vegas & game", "this team's Vegas implied points = (total + spread) / 2",
  "home", "Vegas & game", "1 = this D/ST is at home",
  "indoor", "Vegas & game", "1 = dome or closed roof",
  "grass", "Vegas & game", "1 = natural grass",
  "temp", "Vegas & game", "game temperature °F (70 indoors; outdoor median if unknown; use weather_override for forecasts)",
  "wind", "Vegas & game", "wind speed mph (0 indoors; outdoor median if unknown)",
  "wind_hi", "Vegas & game", "1 = wind ≥ 15 mph",
  "cold", "Vegas & game", "1 = temperature ≤ 35 °F",
  "rest_diff", "Vegas & game", "days of rest for this team minus the opponent's",
  "div_game", "Vegas & game", "1 = divisional game",
  "week", "Vegas & game", "week of the season",
  "qb_sack_rate", "Opposing QB (career)", "opposing starter's career sacks per dropback, shrunk with 150 league-average dropbacks (all teams he played for)",
  "qb_int_rate", "Opposing QB (career)", "opposing starter's career INTs per attempt (shrunk)",
  "qb_fum_rate", "Opposing QB (career)", "opposing starter's career fumbles per dropback (shrunk)",
  "qb_hit_rate", "Opposing QB (career)", "opposing starter's career QB hits taken per dropback (shrunk)",
  "qb_epa_db", "Opposing QB (career)", "opposing starter's career EPA per dropback (shrunk)",
  "qb_log_db", "Opposing QB (career)", "log(1 + opposing starter's career dropbacks): experience; D/STs score less against veterans",
  "qb_new_starter", "Opposing QB (career)", "1 = opposing starter has < 30 dropbacks so far this season",
  "qb_p2s", "Opposing QB (career)", "career sacks per PFR pressure (computed, not used: no gain in testing)",
  "qb_press_rate", "Opposing QB (career)", "career PFR pressures per dropback (computed, not used: no gain in testing)",
  "o_n_hist", "Opponent offense", "effective number of games in the opponent's decay-weighted history",
  "o_qb_cont", "Opponent offense", "QB continuity: share of the opponent's decay-weighted history played by this week's starter (dropback share). 1 = all his games; 0 = none",
  "o_pc_cont", "Opponent offense", "play-caller continuity: share of the opponent's decay-weighted history called by this week's offensive play caller",
  "press_x", "Interactions", "d_press_rate × o_press_rate: pass rush that gets pressure vs a line that allows it",
  "sack_x", "Interactions", "d_sack_rate × o_sack_rate",
  "to_x", "Interactions", "(d_int_rate + d_fuml_rate) × (o_int_rate + o_fuml_rate): ball-hawking defense vs turnover-prone offense"
)
feat_glossary <- bind_rows(
  off_desc %>% transmute(feature = paste0("o_", name), group = "Opponent offense", definition = paste0("opponent's ", o_desc, " ", RATE_NOTE)),
  off_desc %>% transmute(feature = paste0("d_", name), group = "This defense (allowed / forced)", definition = paste0(d_desc, " ", RATE_NOTE)),
  tibble(feature = paste0("d_", names(dst_desc)), group = "This D/ST (own scoring events)", definition = paste0(dst_desc, " ", RATE_NOTE)),
  other_desc
) %>% mutate(in_model = feature %in% vars)
if (length(miss <- setdiff(vars, feat_glossary$feature))) warning("features without a glossary entry: ", paste(miss, collapse = ", "))
col_glossary <- tribble(
  ~term, ~definition,
  "Rank", "rank by Proj this week",
  "Team", "the D/ST being projected",
  "Opp", "opponent; 'vs' = D/ST at home, '@' = D/ST on the road",
  "Opp QB", "opponent's projected starting QB (nflverse schedule; override with qb_override)",
  "Spread", "point spread from the D/ST's side: + = D/ST's team favored by that many points",
  "Opp implied", "opponent's Vegas implied points = (total − spread) / 2",
  "Proj", paste0("projected ", SC$label, " D/ST points: average of the models in FINAL_MODELS (elastic net + component model + ridge)"),
  "90% CI", "90% confidence interval of the projection itself (model uncertainty): 5th–95th percentile of the blend refit on 100 bootstrap resamples of whole weeks of history. Narrow = the model is sure about the expected score",
  "80% range", "range the ACTUAL score should land in 8 times out of 10 (10th–90th percentile): this week's projection plus the out-of-sample errors of back-test games (2021 onward) with similar projections. Calibrated: 80.3% of 2025 scores fell inside ranges built from 2021–24",
  "P(10+)", "chance of a 10+ point game (same back-test error distribution)",
  "P(<3)", "chance of a dud: fewer than 3 points",
  "P(top 8)", "chance this D/ST finishes as a top-8 scorer among this week's teams (4,000 simulated weeks). The most direct start/sit number",
  "E[sacks]", "expected sacks (quasi-Poisson sub-model)",
  "E[TO]", "expected takeaways = interceptions + fumble recoveries (quasi-Poisson sub-model)",
  "PA pts", paste0("expected ", SC$label, " points from the points-allowed tier (", SC$pa_pts[1], " for a shutout down to ", tail(SC$pa_pts, 1), " for the worst tier)"),
  "YA pts", if (is.null(SC$ya_breaks)) paste0(SC$label, " has no yards-allowed scoring (always 0)") else "expected points from the yards-allowed tier (+5 under 100 yds down to −7 for 550+)",
  "P(TD)", "probability the D/ST scores at least one return or defensive TD",
  "Venue", "indoor = dome/closed roof; outdoor otherwise",
  "Opp QB hist %", "share of the opponent's decay-weighted history (the games behind its o_ features) played by this week's QB. 100 = same QB all along; ~43 this early in 2026 means only the 2026 games are his; 0 = he has not played for them (e.g. a backup starting)",
  "Opp caller hist %", "same idea for the opponent's offensive play caller (from data/dst/play_callers.csv). Low values = the opponent's team-level stats were produced by a different offense; lean more on Vegas and the QB career rates",
  "rel_inf", "GBM relative influence: % of the total error reduction from splits on that feature (sums to 100 over all features)"
)
model_glossary <- tribble(
  ~term, ~definition,
  "vegas_lm", "baseline: linear model on spread, total, opponent implied points and home only",
  "vegas_plus", "linear model on Vegas + a handful of hand-picked team/QB rates",
  "ridge", "ridge regression (L2-penalised linear model) on all features",
  "enet", "elastic net (α = 0.5; L1 + L2 penalty, drops weak features) on all features",
  "enet_w", "elastic net with the target capped at 25 points (limits the pull of big TD games)",
  "gam", "generalized additive model: smooth non-linear curves for ~17 key features",
  "rf", "random forest (600 trees)",
  "gbm", "gradient-boosted trees (800 trees, depth 2)",
  "components", "separate models per scoring event (sacks, INTs, fumble recoveries: quasi-Poisson; TDs/blocks, PA tier, YA tier: linear), converted to fantasy points and summed",
  "ens", "average of enet + components + ridge (the model used for projections)",
  "ens_gbm", "average of enet + components + gbm",
  "ens_all", "average of all six non-Vegas models"
)
metric_glossary <- tribble(
  ~term, ~definition,
  "rmse", "root-mean-square error of projected vs actual fantasy points (lower = better)",
  "mae", "mean absolute error (points)",
  "spearman", "average weekly rank correlation between projected and actual scores (higher = better ordering)",
  "top8_avg", "average actual points of each week's top-8 projected D/STs",
  "bot8_avg", "average actual points of each week's bottom-8 projected D/STs",
  "edge_top8", "top8_avg minus the weekly league average: points gained per week by starting the model's top-8",
  "vs_vegas_top8", "top8_avg minus the Vegas-only baseline's top8_avg, paired by week (the model's edge over the market)",
  "t_stat", "paired t-statistic for vs_vegas_top8 across the test weeks (18 per season; |t| > 2 ≈ significant)"
)
glossary_all <- bind_rows(
  col_glossary %>% mutate(section = "Report columns", .before = 1),
  model_glossary %>% mutate(section = "Models (back-test table)", .before = 1),
  metric_glossary %>% mutate(section = "Back-test metrics", .before = 1),
  feat_glossary %>% filter(in_model | feature %in% c("qb_p2s", "qb_press_rate") | grepl("ngs_|ftn_|runloss", feature)) %>%
    mutate(definition = ifelse(!in_model & grepl("ngs_|ftn_|runloss", feature), paste0(definition, " — computed, not in the current model (see feature selection)"), definition)) %>%
    transmute(section = paste0("Features — ", group), term = feature, definition)
)
write.csv(glossary_all, file.path(OUT_DIR, "dst_glossary.csv"), row.names = FALSE)
gloss_lookup <- setNames(glossary_all$definition, glossary_all$term)
describe <- function(x) unname(coalesce(gloss_lookup[x], ""))

## ---- 11. Self-contained HTML report ----
esc <- function(x) { x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub('"', "&quot;", gsub(">", "&gt;", x, fixed = TRUE), fixed = TRUE) }
html_table <- function(df, tips = TRUE, left = 1) {
  th <- vapply(names(df), function(n) { d <- describe(n)
    if (tips && nzchar(d)) sprintf('<th title="%s">%s<sup>?</sup></th>', esc(d), esc(n)) else sprintf("<th>%s</th>", esc(n)) }, "")
  rows <- apply(df, 1, function(r) paste0("<tr>", paste0("<td>", esc(r), "</td>", collapse = ""), "</tr>"))
  sprintf('<table class="l%d"><tr>%s</tr>%s</table>', left, paste(th, collapse = ""), paste(rows, collapse = ""))
}
gloss_html <- function(df) html_table(df %>% select(term, definition) %>% rename(Term = term, Definition = definition), tips = FALSE, left = 2)
proj_tbl <- pred %>% transmute(Rank = rank, Team = team, Opp = paste0(ifelse(home == 1, "vs ", "@ "), opp), `Opp QB` = opp_qb_name,
                               Spread = spread, `Opp implied` = round(implied_opp, 1), Proj = round(proj, 2),
                               `E[sacks]` = round(e_sacks, 2), `E[TO]` = round(e_to, 2), `PA pts` = round(e_pa, 2),
                               `YA pts` = round(e_ya, 2), `P(TD)` = round(p_td, 2), Venue = ifelse(indoor == 1, "indoor", "outdoor"),
                               `Opp QB hist %` = round(100 * o_qb_cont), `Opp caller hist %` = round(100 * o_pc_cont))
fmt0 <- function(x) as.character(as.integer(round(x)))            # avoids "-0"
proj_tbl <- proj_tbl %>% mutate(`90% CI` = sprintf("%.1f–%.1f", pred$ci_lo, pred$ci_hi), `80% range` = paste(fmt0(pred$q10), "to", fmt0(pred$q90)),
                                `P(10+)` = sprintf("%.0f%%", 100 * pred$p_boom), `P(<3)` = sprintf("%.0f%%", 100 * pred$p_bust),
                                `P(top 8)` = sprintf("%.0f%%", 100 * pred$p_top8), .after = Proj)
cv_tbl  <- cv$summary %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% arrange(rmse) %>% mutate(what = describe(model))
imp_tbl <- imp %>% slice_head(n = 25) %>% mutate(rel_inf = round(rel_inf, 2), meaning = describe(feature))
feat_sections <- glossary_all %>% filter(str_starts(section, "Features")) %>% split(.$section)
css <- 'body{font-family:system-ui,sans-serif;max-width:1150px;margin:2rem auto;padding:0 1rem;color:#222;background:#fff}
table{border-collapse:collapse;font-size:14px;margin:1rem 0}th,td{border:1px solid #ddd;padding:4px 8px;text-align:right;vertical-align:top}
th{background:#f3f3f3;cursor:help}th sup{color:#888;font-size:10px;margin-left:2px}
table.l1 td:nth-child(-n+4){text-align:left}table.l2 td{text-align:left}table.l2 td:first-child{font-family:ui-monospace,monospace;white-space:nowrap}
td:last-child{text-align:left}h2{margin-top:2rem}small{color:#666}details{margin:.5rem 0}summary{cursor:pointer;font-weight:600}
@media (max-width:700px){table{display:block;overflow-x:auto}}'
html <- paste0('<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  sprintf("<title>D/ST projections %d wk %d</title><style>%s</style></head><body>", PRED_SEASON, PRED_WEEK, css),
  sprintf("<h1>%s D/ST projections — %d week %d</h1><small>Blend of %s. Generated %s. Hover any column header (<sup>?</sup>) for its definition; full glossary at the bottom.<br>Feature families (%s): %s.</small>",
          SC$label, PRED_SEASON, PRED_WEEK, paste(FINAL_MODELS, collapse = " + "), format(Sys.time(), "%Y-%m-%d %H:%M"),
          if (file.exists(SELECTED_FILE)) "from 42_dst_feature_selection.R" else "default", paste(sort(unique(family_of(vars))), collapse = ", ")),
  html_table(proj_tbl, left = 1),
  sprintf("<h2>Back-test: %s</h2><small>Predicted by models trained only on earlier seasons (2018–%d). The same season is used to choose features and settings, so these numbers are somewhat optimistic.</small>", paste(unique(range(CV_SEASONS)), collapse = "–"), min(CV_SEASONS) - 1),
  html_table(cv_tbl, left = 1),
  "<h2>Top 25 GBM features</h2><small>What the gradient-boosted model leans on. Prefix <code>o_</code> = opponent offense, <code>d_</code> = this defense, <code>qb_</code> = opposing QB career. [as-of rate] is explained in the glossary.</small>",
  html_table(imp_tbl, left = 1),
  "<h2>Glossary</h2>",
  "<details open><summary>Report columns</summary>", gloss_html(col_glossary), "</details>",
  "<details><summary>Models</summary>", gloss_html(model_glossary), "</details>",
  "<details><summary>Back-test metrics</summary>", gloss_html(metric_glossary), "</details>",
  sprintf("<p><small>%s</small></p>", esc(RATE_NOTE_LONG)),
  paste0(imap_chr(feat_sections, ~ sprintf("<details><summary>%s</summary>%s</details>", esc(.y), gloss_html(.x))), collapse = ""),
  "</body></html>")
writeLines(html, file.path(OUT_DIR, sprintf("dst_proj_%d_wk%02d.html", PRED_SEASON, PRED_WEEK)))

## ---- 12. Markdown report + standalone glossary ----
md_table <- function(df) {
  cell <- function(x) gsub("|", "\\|", as.character(x), fixed = TRUE)
  c(paste0("| ", paste(cell(names(df)), collapse = " | "), " |"), paste0("|", strrep("---|", ncol(df))),
    apply(df, 1, function(r) paste0("| ", paste(cell(r), collapse = " | "), " |")))
}
gloss_md <- c("## Glossary", "", "### Report columns", "", md_table(col_glossary %>% rename(Term = term, Definition = definition)), "",
              "### Models", "", md_table(model_glossary %>% rename(Term = term, Definition = definition)), "",
              "### Back-test metrics", "", md_table(metric_glossary %>% rename(Term = term, Definition = definition)), "",
              paste0("*", RATE_NOTE_LONG, "*"), "",
              unlist(imap(feat_sections, ~ c(paste0("### ", .y), "", md_table(.x %>% transmute(Feature = paste0("`", term, "`"), Definition = definition)), ""))))
md <- c(sprintf("# %s D/ST projections — %d week %d", SC$label, PRED_SEASON, PRED_WEEK), "", paste("Scoring:", SC$rules), "",
        sprintf("Blend of %s. Generated %s. Definitions of every column, model, metric and feature are in the glossary below.",
                paste(FINAL_MODELS, collapse = " + "), format(Sys.time(), "%Y-%m-%d %H:%M")), "",
        md_table(proj_tbl), "",
        sprintf("## Back-test: %s (trained on 2018–%d)", paste(unique(range(CV_SEASONS)), collapse = "–"), min(CV_SEASONS) - 1), "",
        md_table(cv_tbl %>% select(-what)), "",
        "## Top 25 GBM features", "", md_table(imp_tbl %>% mutate(feature = paste0("`", feature, "`"))), "", gloss_md)
writeLines(md, file.path(OUT_DIR, sprintf("dst_proj_%d_wk%02d.md", PRED_SEASON, PRED_WEEK)))
writeLines(c("# D/ST model glossary", "", sprintf("Generated by 40_dst_model.R on %s. Rates marked 'as-of' use only games before the one being predicted.", format(Sys.Date())), "", gloss_md[-1]),
           file.path(OUT_DIR, "dst_glossary.md"))
saveRDS(list(system = SCORING_SYSTEM, SC = SC, pred = pred, proj_tbl = proj_tbl, cv_tbl = cv_tbl, imp_tbl = imp_tbl,
             col_glossary = col_glossary, model_glossary = model_glossary, metric_glossary = metric_glossary,
             glossary_all = glossary_all, rate_note = RATE_NOTE_LONG, families = sort(unique(family_of(vars))),
             cv_season = CV_SEASONS, generated = Sys.time()),
        file.path(OUT_DIR, sprintf("report_parts_%d_wk%02d.rds", PRED_SEASON, PRED_WEEK)))
cat("\nWrote:", file.path(OUT_DIR, sprintf("dst_proj_%d_wk%02d.{csv,html,md}", PRED_SEASON, PRED_WEEK)), "+ dst_glossary.{csv,md} + report parts\n")
