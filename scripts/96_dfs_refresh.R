# ==============================================================================
# 96_dfs_refresh.R — DraftKings DFS page (…/dfs/), refreshed on GitHub Actions after every Player refresh
#
# Runs in .github/workflows/dfs_refresh.yml (written by 97_dfs_publish.R), or locally.
#   1. Player projections: re-runs 65_player_refresh.R in a scratch copy of the repo with no API key and no outside
#      pulls (NO_EXT=1), so it only re-scores the props already stored, and takes its table. The Players page itself
#      and 65 are untouched; the DFS page always shows exactly the Players page's projections and locks.
#   2. DK salaries for the main slate (dfs_utils.R): public lobby + draftables JSON, every run until the slate leaves the
#      lobby (it does at lock); stored in data/dfs/dk_salaries_<season>_wk<ww>.csv. A DKSalaries.csv exported from the
#      DK lobby and uploaded as data/dfs/DKSalaries_<season>_wk<ww>.csv takes priority.
#   3. DK projection = DK points of the expected stats + 3 x each yardage-bonus probability (94's fits), P(boom),
#      P(4x salary), ceiling and range from 94's outcome distribution; D/ST = the Yahoo D/ST projection (the D/ST page's
#      last pre-kickoff refresh, data/lines/proj_history.csv) with outcome odds from the Yahoo bundle's back-test residuals.
#   4. Points per $1K, value vs the slate's salary curve, projected ownership (BETA formula) and leverage.
#   5. Writes site/dfs/index.html (+ archive) and data/dfs/dfs_proj_<season>_wk<ww>.csv (latest table; for fitting
#      ownership and a track record later).
# Usage: Rscript scripts/96_dfs_refresh.R   (env FF_PROJ_DIR, REFRESH_NOW, DK_MOCK_DIR for tests, NO_DK=1 to skip DK)
# ==============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(purrr); library(tibble) })
if (!isTRUE(l10n_info()$`UTF-8`)) invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")))
PROJ_DIR <- normalizePath(Sys.getenv("FF_PROJ_DIR", getwd()))
SITE_DIR <- file.path(PROJ_DIR, "site"); SITE_BASE <- Sys.getenv("SITE_BASE", "/dst-site/")
source(file.path(PROJ_DIR, "scripts/dfs_utils.R"))
utc <- function(x) as.POSIXct(sub("Z$", "", x), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
isoz <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
NOW <- if (nzchar(Sys.getenv("REFRESH_NOW"))) utc(Sys.getenv("REFRESH_NOW")) else as.POSIXct(format(Sys.time(), tz = "UTC"), tz = "UTC")
placeholder <- function(msg) {
  dir.create(file.path(SITE_DIR, "dfs"), recursive = TRUE, showWarnings = FALSE)
  writeLines(sprintf('<!doctype html><html><head><meta charset="utf-8"><title>DraftKings DFS</title></head><body><h1>DraftKings DFS</h1><p>%s</p><p><a href="%splayers/">Player projections</a></p></body></html>', msg, SITE_BASE),
             file.path(SITE_DIR, "dfs", "index.html"))
  message("dfs: ", msg); quit(save = "no")
}

## ---- 1. Week, specs, and the Players page's projections ----
bf <- list.files(file.path(PROJ_DIR, "output/players_site"), pattern = "^bundle_\\d{4}_wk\\d{2}\\.rds$")
if (!length(bf)) placeholder("Not published yet: the player bundle (63 / 66) is missing.")
key <- max(sub("bundle_(\\d{4})_wk(\\d{2}).*", "\\1\\2", bf)); SEASON <- as.integer(substr(key, 1, 4)); WEEK <- as.integer(substr(key, 5, 6))
sf <- list.files(file.path(PROJ_DIR, "output/dfs"), pattern = "^dfs_spec_\\d{4}_wk\\d{2}\\.rds$", full.names = TRUE)
if (!length(sf)) placeholder("Not published yet: the DFS fit (94 / 97) is missing.")
sk <- sub(".*dfs_spec_(\\d{4})_wk(\\d{2}).*", "\\1\\2", sf); S <- readRDS(sf[which.max(sk)])
if (max(sk) != key) message(sprintf("dfs: note: newest DFS fit is %s, players bundle %s (run 94 + 97 for this week)", max(sk), key))
dst_b <- tryCatch(readRDS(file.path(PROJ_DIR, "output/dst/yahoo", sprintf("bundle_%d_wk%02d.rds", SEASON, WEEK))), error = function(e) NULL)
message(sprintf("dfs: %d week %d", SEASON, WEEK))

## 65 in a scratch copy (it writes its page there, not here). No key, no outside pulls: re-scores stored props only.
scratch <- file.path(tempdir(), "p65"); unlink(scratch, recursive = TRUE); dir.create(scratch, recursive = TRUE)
for (dd in c("scripts", "output/players_site", "data/players", "data/lines")) {
  if (!dir.exists(file.path(PROJ_DIR, dd))) next
  dir.create(file.path(scratch, dirname(dd)), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(PROJ_DIR, dd), file.path(scratch, dirname(dd)), recursive = TRUE)
}
old_env <- Sys.getenv(c("FF_PROJ_DIR", "PROPS_API_KEY", "NO_EXT", "PLAYER_PROPS_MOCK"), unset = NA)
Sys.setenv(FF_PROJ_DIR = scratch, PROPS_API_KEY = "", NO_EXT = "1", PLAYER_PROPS_MOCK = "")
E <- new.env()
ok65 <- tryCatch({ sys.source(file.path(scratch, "scripts/65_player_refresh.R"), envir = E); TRUE },
                 error = function(e) { message("dfs: re-running 65 failed — ", conditionMessage(e)); FALSE })
for (k in names(old_env)) if (is.na(old_env[[k]])) Sys.unsetenv(k) else do.call(Sys.setenv, setNames(list(old_env[[k]]), k))
source(file.path(PROJ_DIR, "scripts/site_utils.R")); source(file.path(PROJ_DIR, "scripts/props_utils.R"))
source(file.path(PROJ_DIR, "scripts/player_site_utils.R")); source(file.path(PROJ_DIR, "scripts/64_player_page.R"))
source(file.path(PROJ_DIR, "scripts/95_dfs_page.R"))
cur <- if (ok65 && !is.null(E$cur) && nrow(E$cur)) E$cur else tibble()
games <- if (!is.null(E$games)) E$games else readRDS(file.path(PROJ_DIR, "output/players_site", sprintf("bundle_%d_wk%02d.rds", SEASON, WEEK)))$games |> mutate(ko = utc(kickoff_utc))
imp <- if (!is.null(E$imp)) E$imp else tibble(game_id = character(), team = character(), implied = numeric(), spread = numeric())
Bp <- readRDS(file.path(PROJ_DIR, "output/players_site", sprintf("bundle_%d_wk%02d.rds", SEASON, WEEK)))
message(sprintf("dfs: %d player rows from the Players page's projections", nrow(cur)))

## ---- 2. DK main-slate salaries ----
dir.create(file.path(PROJ_DIR, "data/dfs"), recursive = TRUE, showWarnings = FALSE)
SAL_CSV <- file.path(PROJ_DIR, sprintf("data/dfs/dk_salaries_%d_wk%02d.csv", SEASON, WEEK))
MAN_CSV <- file.path(PROJ_DIR, sprintf("data/dfs/DKSalaries_%d_wk%02d.csv", SEASON, WEEK))
gl <- bind_rows(games |> transmute(game_id, team = home_team, opp = away_team, home = 1L, ko),
                games |> transmute(game_id, team = away_team, opp = home_team, home = 0L, ko))
sal <- NULL; sal_src <- NA; dk_err <- NA_character_
## an uploaded lobby export: DKSalaries_<season>_wk<ww>.csv, or any DKSalaries*.csv in data/dfs whose teams play this week
## (newest first), so the default export name works too
man <- c(MAN_CSV[file.exists(MAN_CSV)], { f <- list.files(file.path(PROJ_DIR, "data/dfs"), pattern = "^DKSalaries.*\\.csv$", full.names = TRUE)
  f <- setdiff(f, MAN_CSV); f[order(file.mtime(f), decreasing = TRUE)] })
for (f in man) {
  x <- tryCatch(dk_parse_csv(f), error = function(e) { message("dfs: ", basename(f), " — ", conditionMessage(e)); NULL })
  if (is.null(x) || !nrow(x)) next
  if (dk_game_share(x$game, games) < 0.9) { message("dfs: ", basename(f), " is not this week's slate (its games aren't this week's matchups), skipped"); next }
  sal <- x |> mutate(pulled_at = isoz(file.mtime(f)), dg_id = "csv"); sal_src <- "csv"
  message(sprintf("dfs: salaries from the uploaded %s (%d players)", basename(f), nrow(sal))); break
}
## the automatic pull (also done from Stratus by 97, which stores the same file). GitHub's runners may be refused by DK's
## draftables API (HTTP 403); then the stored file from 97 or an uploaded export is used.
if (is.null(sal) && !nzchar(Sys.getenv("NO_DK")) && any(games$ko > NOW)) tryCatch({
  x <- dk_pull_main(games, NOW)
  write.csv(x, SAL_CSV, row.names = FALSE)
  message(sprintf("dfs: DK main slate = draft group %s (%s), %d games, %d players", x$dg_id[1], attr(x, "example"), n_distinct(x$game), nrow(x)))
}, error = function(e) { dk_err <<- dk_err_text(conditionMessage(e))
  message("dfs: DK salaries not pulled — ", conditionMessage(e), if (file.exists(SAL_CSV)) " (using the stored pull)" else "") })
if (is.null(sal) && file.exists(SAL_CSV)) { sal <- as_tibble(read.csv(SAL_CSV, stringsAsFactors = FALSE, colClasses = c(dk_id = "character", dg_id = "character"))); sal_src <- "dk" }
## slate games = this week's games with a team on the salary list
slate_games <- if (!is.null(sal) && nrow(sal)) unique(gl$game_id[gl$team %in% sal$team]) else character()

## ---- 3. Player rows: DK projection, odds, ranges ----
rows <- tibble()
if (nrow(cur)) {
  d <- cur |> filter(include %in% TRUE)
  if (length(slate_games)) d <- d |> filter(game_id %in% slate_games)
  d <- d |> mutate(fallback = fallback %in% TRUE)
  for (s in c(paste0("e.", c("pass_yds", "pass_td", "pass_int", "rush_yds", "rec", "rec_yds", "tds", "fum_lost", "two_pt")),
              paste0("line.", c("pass_yds", "rush_yds", "rec_yds")), "p_td_raw", "ci_lo_ppr", "ci_hi_ppr", "inj_status", "inj_badge", "inj_out", "inj_detail"))
    if (!s %in% names(d)) d[[s]] <- NA
  hp <- list(pass_yds = !is.na(d$line.pass_yds), rush_yds = !is.na(d$line.rush_yds), rec_yds = !is.na(d$line.rec_yds))
  pj <- dk_project(d, hp, S$bonus)
  d <- bind_cols(d, pj) |>
    mutate(p_bonus = case_when(pos == "QB" ~ p300, pos == "RB" ~ pmax(p100r, p100c), TRUE ~ p100c),
           ci_half = ifelse(fallback | is.na(ci_lo_ppr), NA_real_, (ci_hi_ppr - ci_lo_ppr) / 2),
           exp_line = sprintf("%s%s%s%.2f TDs",
                              ifelse(pos == "QB", sprintf("%.0f pass yds (300+ %s), %.2f pass TD, %.2f INT, ", e.pass_yds, dfs_pct(p300), e.pass_td, e.pass_int), ""),
                              ifelse(pos %in% c("QB", "RB"), sprintf("%.0f rush yds%s, ", e.rush_yds, ifelse(pos == "RB", paste0(" (100+ ", dfs_pct(p100r), ")"), "")), ""),
                              ifelse(pos != "QB", sprintf("%.1f catches, %.0f rec yds (100+ %s), ", e.rec, e.rec_yds, dfs_pct(p100c)), ""), e.tds))
  rows <- d |> select(game_id, gsis_id, player_name, pos, team, opp, home, ko, locked, fallback, implied, spread, p_td_raw,
                      starts_with("e."), starts_with("src."), dk_base, dk_bonus, dk_proj, p300, p100r, p100c, p_bonus, ci_half, exp_line,
                      inj_status, inj_badge, inj_out, inj_detail) |> rename(any_of(c(t_pull = "t")))
}
## D/ST: Yahoo projection, last D/ST-page refresh before kickoff
dst_rows <- tibble()
ph <- tryCatch(as_tibble(read.csv(file.path(PROJ_DIR, "data/lines/proj_history.csv"), stringsAsFactors = FALSE)), error = function(e) NULL)
if (!is.null(ph) && !is.null(dst_b)) {
  ph <- ph |> filter(model == "dst", system == "yahoo", season == SEASON, week == WEEK) |> mutate(t = utc(time)) |>
    inner_join(gl, by = "team") |> filter(t < ko) |> group_by(team) |> slice_max(t, n = 1, with_ties = FALSE) |> ungroup()
  if (length(slate_games)) ph <- ph |> filter(game_id %in% slate_games)
  if (nrow(ph)) {
    oi <- imp |> select(game_id, team, implied) |> rename(opp = team, opp_implied = implied)
    dst_rows <- ph |> left_join(oi, by = c("game_id", "opp")) |>
      transmute(game_id, gsis_id = paste0("DST_", team), player_name = paste(team, "D/ST"), pos = "DST", team, opp, home, ko,
                locked = NOW >= ko, fallback = FALSE, implied = opp_implied, dk_proj = proj, dk_base = proj, dk_bonus = 0, p_bonus = NA_real_,
                ci_half = NA_real_, exp_line = NA_character_)
  }
}
rows <- bind_rows(rows, dst_rows)
if (!nrow(rows)) placeholder(sprintf("%d week %d: no projections yet (props not posted and no fallback).", SEASON, WEEK))
DST_NAMES <- c(ARI = "Cardinals", ATL = "Falcons", BAL = "Ravens", BUF = "Bills", CAR = "Panthers", CHI = "Bears", CIN = "Bengals", CLE = "Browns",
  DAL = "Cowboys", DEN = "Broncos", DET = "Lions", GB = "Packers", HOU = "Texans", IND = "Colts", JAX = "Jaguars", KC = "Chiefs", LA = "Rams",
  LAC = "Chargers", LV = "Raiders", MIA = "Dolphins", MIN = "Vikings", NE = "Patriots", NO = "Saints", NYG = "Giants", NYJ = "Jets",
  PHI = "Eagles", PIT = "Steelers", SEA = "Seahawks", SF = "49ers", TB = "Buccaneers", TEN = "Titans", WAS = "Commanders")

## ---- 4. Salaries onto rows (names -> gsis_id within each game; D/ST by team) ----
n_unm <- 0L
if (!is.null(sal) && nrow(sal)) {
  sk_ <- sal |> filter(dk_pos != "DST") |> inner_join(gl |> select(game_id, team), by = "team")
  mp <- match_players(sk_ |> transmute(game_id, player = name), games |> select(game_id, season, week, home_team, away_team), Bp$roster) |>
    filter(!is.na(gsis_id)) |> select(game_id, player, gsis_id)
  sk_ <- sk_ |> left_join(mp, by = c("game_id", "name" = "player"))
  sd_ <- sal |> filter(dk_pos == "DST") |> mutate(gsis_id = paste0("DST_", team))
  sj <- bind_rows(sk_, sd_) |> filter(!is.na(gsis_id)) |> distinct(gsis_id, .keep_all = TRUE)
  rows <- rows |> left_join(sj |> select(gsis_id, salary, dk_status = status, dk_pos), by = "gsis_id")
  ## DK players with a real salary but no projection row (backups without props or history)
  n_unm <- sum(!(sj$gsis_id %in% rows$gsis_id)) + sum(is.na(sk_$gsis_id))
  rows <- rows |> filter(!is.na(salary))                               # the slate: only players DK lists
  unmatched <- sk_ |> filter(is.na(gsis_id), salary > 3500) |> pull(name)
  if (length(unmatched)) message("dfs: DK names not matched (salary > $3,500): ", paste(head(unmatched, 15), collapse = ", "))
} else { rows$salary <- NA_real_; rows$dk_status <- NA_character_ }
rows <- rows |> mutate(dk_status = ifelse(dk_status %in% c("None", "NA", "") | is.na(dk_status), "", dk_status))

## ---- 5. Outcome odds, ranges, value, ownership, leverage ----
sk <- rows$pos != "DST"
rows$p_boom <- NA_real_; rows$p_4x <- NA_real_
for (q in c("q10", "q25", "q50", "q75", "q90")) rows[[q]] <- NA_real_
if (any(sk)) {
  t_ <- coalesce(rows$p_td_raw[sk], S$td_fill$t_fill[match(rows$pos[sk], S$td_fill$pos)])
  rows$p_boom[sk] <- dk_odds(rows$dk_proj[sk], unname(S$dk_cut[rows$pos[sk]]), t_, rows$pos[sk], S)
  rows$p_4x[sk] <- ifelse(is.na(rows$salary[sk]), NA, dk_odds(rows$dk_proj[sk], coalesce(4 * rows$salary[sk] / 1000, 99), t_, rows$pos[sk], S))
  qq <- dk_quant(rows$dk_proj[sk], rows$pos[sk], S$quant); for (q in names(qq)) rows[[q]][sk] <- qq[[q]]
}
if (any(!sk) && !is.null(dst_b$unc)) {
  i <- which(!sk)
  rows$p_boom[i] <- dst_odds(rows$dk_proj[i], rep(10, length(i)), dst_b$unc)
  rows$p_4x[i] <- ifelse(is.na(rows$salary[i]), NA, dst_odds(rows$dk_proj[i], coalesce(4 * rows$salary[i] / 1000, 99), dst_b$unc))
  qq <- dst_quant(rows$dk_proj[i], dst_b$unc); for (q in names(qq)) rows[[q]][i] <- qq[[q]]
}
rows <- rows |> mutate(ppk = ifelse(is.na(salary), NA, dk_proj / (salary / 1000)))
## value vs the slate's salary curve: straight line of projection on salary per position (players with a projection)
rows$value <- NA_real_
for (p in unique(rows$pos)) { i <- which(rows$pos == p & !is.na(rows$salary) & rows$dk_proj > 0)
  if (length(i) >= 5) { m <- lm(dk_proj ~ salary, data = rows[i, ]); rows$value[i] <- rows$dk_proj[i] - predict(m, rows[i, ]) } }
## recent form (94's table: this season's DK points per player-game)
fm <- form_points(S$form)
rows <- rows |> left_join(fm, by = "gsis_id")
avail <- !(rows$inj_out %in% TRUE) & !(rows$dk_status %in% c("O", "OUT", "IR", "Out"))
rows$own <- if (any(!is.na(rows$salary))) own_project(rows |> transmute(pos, salary, proj = dk_proj, form,
              imp = ifelse(pos == "DST", -implied, implied), avail = avail), n_games = max(length(slate_games), 2)) else NA_real_
rows$lev <- rows$p_4x - rows$own
rows <- rows |> mutate(player_name = ifelse(pos == "DST", paste(coalesce(unname(DST_NAMES[team]), team), "D/ST"), player_name))

## ---- 6. Page + table ----
slate <- if (length(slate_games)) games |> filter(game_id %in% slate_games) |> arrange(ko) |> transmute(game_id, label = paste0(away_team, "@", home_team), ko) else NULL
last_pull <- if (nrow(cur) && "t" %in% names(cur) && any(!is.na(cur$t))) max(cur$t, na.rm = TRUE) else as.POSIXct(NA)
P <- list(season = SEASON, week = WEEK, now = NOW, rows = rows, spec = S, slate = slate, site_base = SITE_BASE,
          sal_time = if (!is.null(sal) && nrow(sal)) utc(max(sal$pulled_at)) else as.POSIXct(NA), sal_src = sal_src,
          last_pull = last_pull, n_locked = if (!is.null(slate)) sum(slate$ko <= NOW) else 0L, n_slate = if (!is.null(slate)) nrow(slate) else nrow(games),
          n_unmatched = n_unm, dk_err = if (is.null(sal)) dk_err else NA_character_)
html <- dfs_page(P)
dir.create(file.path(SITE_DIR, "dfs", "archive"), recursive = TRUE, showWarnings = FALSE)
arch_name <- sprintf("dfs_%d_wk%02d.html", SEASON, WEEK)
writeLines(html, file.path(SITE_DIR, "dfs", "index.html")); writeLines(html, file.path(SITE_DIR, "dfs", "archive", arch_name))
arch <- sort(list.files(file.path(SITE_DIR, "dfs", "archive"), pattern = "^dfs_.*\\.html$"), decreasing = TRUE)
writeLines(c('<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>DFS archive</title>',
             '<style>body{font-family:system-ui,sans-serif;max-width:700px;margin:2rem auto;padding:0 16px}</style></head><body><h1>DFS: past weeks</h1><ul>',
             sprintf('<li><a href="%s">%s</a></li>', arch, sub("dfs_(\\d{4})_wk(\\d{2})\\.html", "\\1 week \\2", arch)),
             sprintf('</ul><p><a href="%sdfs/">Current week</a> · <a href="%splayers/">Players</a></p></body></html>', SITE_BASE, SITE_BASE)),
           file.path(SITE_DIR, "dfs", "archive", "index.html"))
out <- rows |> transmute(season = SEASON, week = WEEK, refreshed = isoz(NOW), game_id, gsis_id, player_name, pos, team, opp, kickoff = isoz(ko), locked,
                         fallback, salary, dk_proj = round(dk_proj, 3), dk_bonus = round(dk_bonus, 3), p_boom = round(p_boom, 4), p_4x = round(p_4x, 4),
                         q90 = round(q90, 2), ppk = round(ppk, 3), value = round(value, 3), form = round(form, 2), last_pts = last_pts,
                         implied, own = round(own, 4), lev = round(lev, 4), dk_status, inj_status)
write.csv(out, file.path(PROJ_DIR, sprintf("data/dfs/dfs_proj_%d_wk%02d.csv", SEASON, WEEK)), row.names = FALSE)
message(sprintf("dfs: page written (%d players: %s; %d slate games; salaries %s)", nrow(rows),
                paste(names(table(rows$pos)), table(rows$pos), collapse = " "), length(slate_games), if (is.na(sal_src)) "none yet" else sal_src))
