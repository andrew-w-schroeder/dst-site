# ==============================================================================
# 97s_sd_refresh.R — DK NFL Showdown page (…/dfs/showdown/): 20 lineups for each primetime game (Andrew 2026-11-05)
#   + every game with its own kickoff slot: international mornings, holiday games, Saturday games (Andrew 2026-11-07)
#
# Sourced at the end of 96_dfs_refresh.R (GitHub Site refresh, DFS step) and uses its objects: the Players page's props
# (cur), the DFS spec (S, DK bonus fits), the week's games, the D/ST bundle (dst_b) and line history. A failure here never
# affects the DFS page.
#   1. Primetime games of the week (kickoff 7 pm ET or later). Their DK showdown salaries (captain + flex rows, DK ids)
#      from stat-api (STAT_API_KEY, a repo secret), re-pulled every run until kickoff (injury status), stored in
#      data/dfs/showdown/live/sd_sal_<game_id>.csv.
#   2. Projections in DK points, as in the back-test (91s stage 1 / 2):
#      skill players = the Players page's props (dk_project, 94's bonus fits; no-props rows are not used);
#      kickers = Andrew's kicker model (ESPN, the kicker page's last pre-kickoff refresh) + salary + DK average
#      (output/dfs/showdown/sd_live_spec.rds, fitted on 2023-26 showdowns: beat salary + average with t 4.8);
#      D/ST = Vegas-only (the Yahoo bundle's Vegas fit on the game's last pre-kickoff line, as the DFS page).
#   3. The build that won the back-test (162 flagship Play-Actions 2023-26, leaving each season out): the 20 best
#      projections with no player lacking props / a model, $1K or less left, at least 2 of 6 slots different
#      (captain swaps count). Rebuilt every run until kickoff, then frozen (data/dfs/showdown/live/sd_lineups_<game_id>.rds).
#   4. site/dfs/showdown/index.html + one DK upload CSV per game (CPT, FLEX x 5 by DK id).
# 2026-11-09 (Andrew): two models, each with its own tab.
#   "Highest projected" (dfs/showdown/, the build above, unchanged)
#   "Captain spread" (dfs/showdown-cpt/): every QB / RB / WR / TE projected >= SDS$rules$cpt_thr DK points captains at least
#      one lineup (that player's best lineup), then the rest are the best projections; >= 2 of 6 slots different throughout.
#      Why: TB @ CHI (wk 9 SNF) went 0 for 20 with Caleb Williams in all 20 and the MVP (Bucky Irving) in none; 91s_sd_spread.R
#      tested ways of spreading the 20. Both models are frozen at kickoff in the same file (lineups / lineups_cpt).
# Env: STAT_API_KEY; SD_SA_BASE (test server only).
# ==============================================================================
local({
SD_LIVE <- file.path(PROJ_DIR, "data/dfs/showdown/live"); dir.create(SD_LIVE, recursive = TRUE, showWarnings = FALSE)
SPEC_F <- file.path(PROJ_DIR, "output/dfs/showdown/sd_live_spec.rds")
SDS <- if (file.exists(SPEC_F)) readRDS(SPEC_F) else NULL
if (is.null(SDS)) { message("showdown: output/dfs/showdown/sd_live_spec.rds missing (91s_sd_live_spec.R): skipped"); return(invisible()) }
KEY <- Sys.getenv("STAT_API_KEY"); BASE <- Sys.getenv("SD_SA_BASE", "https://api.stat-api.com/api/v1")
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
ET <- "America/New_York"
OUT_ST <- c("O", "OUT", "IR", "D", "DOUBTFUL", "SUSP", "PUP", "NFI")

## ---- 1. the tab's games + salaries: primetime (7 pm ET or later) or alone in its kickoff slot (international, holiday, Saturday games) ----
gm <- games |> mutate(et_h = as.integer(format(ko, "%H", tz = ET)), et_date = as.Date(format(ko, "%Y-%m-%d", tz = ET))) |>
  add_count(ko, name = "n_slot") |> filter(et_h >= 19 | n_slot == 1) |> arrange(ko)
if (!nrow(gm)) { message("showdown: no primetime / standalone games this week"); return(invisible()) }
sa_get <- function(path, query = list()) {
  if (!nzchar(KEY)) return(NULL)
  q <- if (length(query)) paste0("?", paste(names(query), vapply(query, \(v) utils::URLencode(as.character(v), reserved = TRUE), ""), sep = "=", collapse = "&")) else ""
  h <- curl::new_handle(); curl::handle_setheaders(h, Authorization = paste("Bearer", KEY), Accept = "application/json")
  for (k in 1:4) { r <- tryCatch(curl::curl_fetch_memory(paste0(BASE, "/", path, q), handle = h), error = function(e) NULL)
    if (!is.null(r) && r$status_code == 200) return(tryCatch(jsonlite::fromJSON(rawToChar(r$content), flatten = TRUE), error = function(e) NULL))
    if (!is.null(r) && r$status_code %in% c(400, 401, 403, 404)) { message(sprintf("showdown: stat-api %s: HTTP %d", path, r$status_code)); return(NULL) }
    Sys.sleep(2^k) }
  NULL }
rows_of <- function(j, nm) { d <- j[[nm]]; if (is.data.frame(d)) d else NULL }
team_of <- function(ids) SDS$team_map$team[match(as.numeric(ids), SDS$team_map$team_id)]
pull_sal <- function(g) {
  sl <- rows_of(sa_get("dfs/slates", list(operator = "draftkings", league = "NFL", date = format(g$et_date))), "slates")
  if (is.null(sl) || !nrow(sl)) return(NULL)
  if (!"part_of_game" %in% names(sl)) sl$part_of_game <- NA
  sl <- sl |> filter(game_type == "showdown", !grepl("in-game", tolower(name)), is.na(part_of_game) | part_of_game == "full")
  for (sid in sl$id) {
    sp <- rows_of(sa_get("dfs/slate_players", list(slate_id = sid, limit = 5000)), "slate_players"); if (is.null(sp) || !nrow(sp)) next
    tm <- unique(na.omit(team_of(sp$team_id)))
    if (setequal(tm, c(g$home_team, g$away_team)))
      return(sp |> transmute(slate_id = sid, player_id, name = display_name, position, team = team_of(team_id), salary = as.numeric(salary),
                             export_id = as.character(export_id), avg_points = suppressWarnings(as.numeric(avg_points)),
                             injury_status = toupper(coalesce(as.character(injury_status), "")), pulled_at = isoz(NOW)))
  }
  NULL }

## ---- 2. projections ----
dk_rows <- if (nrow(cur)) {
  d <- cur |> filter(include %in% TRUE, !(fallback %in% TRUE), game_id %in% gm$game_id)
  for (s in c(paste0("e.", c("pass_yds", "pass_td", "pass_int", "rush_yds", "rec", "rec_yds", "tds", "fum_lost", "two_pt")), paste0("line.", c("pass_yds", "rush_yds", "rec_yds")), "inj_out"))
    if (!s %in% names(d)) d[[s]] <- NA
  if (nrow(d)) bind_cols(d |> select(game_id, gsis_id, player_name, pos, team, inj_out), dk_project(d, list(pass_yds = !is.na(d$line.pass_yds), rush_yds = !is.na(d$line.rush_yds), rec_yds = !is.na(d$line.rec_yds)), S$bonus) |> select(dk_proj)) else tibble()
} else tibble()
ph <- tryCatch(as_tibble(read.csv(file.path(PROJ_DIR, "data/lines/proj_history.csv"), stringsAsFactors = FALSE)), error = function(e) NULL)
k_espn <- if (!is.null(ph)) ph |> filter(model == "k", system == "espn", season == SEASON, week == WEEK) |> mutate(t = utc(time)) |>
  inner_join(gl |> select(game_id, team, ko), by = "team") |> filter(t < ko, game_id %in% gm$game_id) |> group_by(team) |> slice_max(t, n = 1, with_ties = FALSE) |> ungroup() |>
  select(team, k_espn = proj) else tibble(team = character(), k_espn = numeric())
dst_v <- tryCatch({
  lh <- as_tibble(read.csv(file.path(PROJ_DIR, "data/lines/line_history.csv"), stringsAsFactors = FALSE)) |> mutate(t = utc(pulled_at), ct = utc(commence_time)) |> filter(t < ct)
  lx <- bind_rows(lh |> inner_join(gm |> select(game_id, home = home_team, away = away_team, ko), by = c("home", "away")) |> mutate(sp = home_spread),
                  lh |> inner_join(gm |> select(game_id, home = away_team, away = home_team, ko), by = c("home", "away")) |> mutate(sp = -home_spread)) |>
    filter(abs(as.numeric(difftime(ct, ko, units = "days"))) <= 2, t < ko) |> group_by(game_id) |> slice_max(t, n = 1, with_ties = FALSE) |> ungroup()
  g2 <- gm |> select(game_id, home_team, away_team) |> inner_join(lx |> select(game_id, sp, total), by = "game_id")
  df <- bind_rows(g2 |> transmute(game_id, team = home_team, spread = sp, total_line = total, home = 1), g2 |> transmute(game_id, team = away_team, spread = -sp, total_line = total, home = 0)) |>
    mutate(implied_opp = (total_line - spread) / 2, implied_own = (total_line + spread) / 2)
  df$dst_proj <- vegas_proj(dst_b$vegas, df); df |> select(game_id, team, dst_proj, spread, total_line) }, error = function(e) tibble(game_id = character(), team = character(), dst_proj = numeric()))
kc <- SDS$kicker_combo
k_combo <- function(k, sal, avg) { b <- kc; b[is.na(b)] <- 0
  b[["(Intercept)"]] + b[["k_espn"]] * k + b[["salary"]] * sal + b[["avg0"]] * coalesce(avg, 0) + (b[["has_avgTRUE"]] %||% 0) * (!is.na(avg) & avg > 0) }

## ---- 3. lineups: the 20 best projections, >= md of 6 slots different, <= max_left unused (50000 = no rule, Andrew 2026-11-05) ----
## force_cpt (Captain spread, Andrew 2026-11-09): first one lineup captained by each of these players (his best lineup that
## still differs from the earlier ones), then the best projections; the same cuts throughout (91s_sd_spread.R's build_cpt)
sd_best_n <- function(d, n_l = SDS$rules$n, md = SDS$rules$md, floor_sal = 50000 - SDS$rules$max_left, force_cpt = integer()) {
  n <- nrow(d); tA <- d$team == d$team[1]
  A <- rbind(c(rep(1, n), rep(0, n)), c(rep(0, n), rep(1, n)), cbind(diag(n), diag(n)), c(d$salary, d$cpt_salary), c(d$salary, d$cpt_salary), c(tA, tA) * 1, c(!tA, !tA) * 1)
  dir <- c("=", "=", rep("<=", n), "<=", ">=", ">=", ">="); rhs <- c(5, 1, rep(1, n), 50000, floor_sal, 1, 1); obj <- c(d$proj, 1.5 * d$proj); out <- list()
  one <- function(cp = NA) { A2 <- A; d2 <- dir; r2 <- rhs
    if (!is.na(cp)) { r <- rep(0, 2 * n); r[n + cp] <- 1; A2 <- rbind(A2, r); d2 <- c(d2, "="); r2 <- c(r2, 1) }
    s <- lpSolve::lp("max", obj, A2, d2, r2, all.bin = TRUE, scale = 0); if (s$status != 0) return(NULL)   # scale 0: default scaling returned non-optimal lineups in tests
    which(round(s$solution) == 1) }
  add <- function(sel) { out[[length(out) + 1]] <<- list(cpt = sel[sel > n] - n, flex = sel[sel <= n])
    cut <- rep(0, 2 * n); cut[sel] <- 1; A <<- rbind(A, cut); dir <<- c(dir, "<="); rhs <<- c(rhs, 6 - md) }
  for (cp in head(force_cpt, n_l)) { if (cp %in% vapply(out, `[[`, 0, "cpt")) next; sel <- one(cp); if (!is.null(sel)) add(sel) }
  while (length(out) < n_l) { sel <- one(); if (is.null(sel)) break; add(sel) }
  out }
CPT_THR <- SDS$rules$cpt_thr %||% 8.5                                      # Captain spread threshold (DK points), from 91s_sd_live_spec.R
lu_tbl <- function(bl, d) { L <- bind_rows(lapply(seq_along(bl), \(k) { b <- bl[[k]]; bind_rows(
    tibble(lineup = k, slot = "CPT", name = d$name[b$cpt], pos = d$pos[b$cpt], team = d$team[b$cpt], salary = d$cpt_salary[b$cpt], proj = 1.5 * d$proj[b$cpt], dk_id = d$cpt_id[b$cpt]),
    tibble(lineup = k, slot = "FLEX", name = d$name[b$flex], pos = d$pos[b$flex], team = d$team[b$flex], salary = d$salary[b$flex], proj = d$proj[b$flex], dk_id = d$export_id[b$flex])) }))
  ## cards in order of projection (Captain spread builds its forced captains first)
  o <- L |> group_by(lineup) |> summarise(p = sum(proj), .groups = "drop") |> arrange(desc(p)) |> mutate(new = row_number())
  L |> mutate(lineup = o$new[match(lineup, o$lineup)]) |> arrange(lineup, slot != "CPT") }

SEC <- list(); n_built <- 0L
for (i in seq_len(nrow(gm))) {
  g <- gm[i, ]; started <- NOW >= g$ko; lab <- sprintf("%s @ %s", g$away_team, g$home_team)
  sal_f <- file.path(SD_LIVE, sprintf("sd_sal_%s.csv", g$game_id)); lu_f <- file.path(SD_LIVE, sprintf("sd_lineups_%s.rds", g$game_id))
  sal <- if (file.exists(sal_f)) read.csv(sal_f, stringsAsFactors = FALSE, colClasses = c(export_id = "character")) else NULL
  if (!started) { new <- tryCatch(pull_sal(g), error = function(e) { message("showdown: salary pull — ", conditionMessage(e)); NULL })
    if (!is.null(new) && nrow(new)) { sal <- new; write.csv(sal, sal_f, row.names = FALSE) } }
  if (started && file.exists(lu_f)) { LU <- readRDS(lu_f); LU$frozen <- TRUE; SEC[[length(SEC) + 1]] <- list(g = g, lab = lab, LU = LU); next }
  if (is.null(sal) || !nrow(sal)) { SEC[[length(SEC) + 1]] <- list(g = g, lab = lab, LU = NULL,
      msg = if (nzchar(KEY)) "DK showdown salaries aren't on stat-api yet." else "STAT_API_KEY isn't set for this run, and no salaries are stored."); next }
  fx <- sal |> filter(position != "CPT") |> distinct(player_id, .keep_all = TRUE)
  cp <- sal |> filter(position == "CPT") |> distinct(player_id, .keep_all = TRUE) |> transmute(player_id, cpt_salary = salary, cpt_id = export_id)
  pool <- fx |> left_join(cp, by = "player_id") |> mutate(cpt_salary = coalesce(cpt_salary, 1.5 * salary), pos = ifelse(position %in% c("D", "DST"), "DST", position),
                                                          key = own_key(name), out = injury_status %in% OUT_ST)
  sk <- dk_rows |> filter(game_id == g$game_id) |> mutate(key = own_key(player_name)) |> distinct(key, team, .keep_all = TRUE)
  pool <- pool |> left_join(sk |> select(key, team, dk_proj, inj_out), by = c("key", "team"))
  ks <- k_espn |> filter(team %in% c(g$home_team, g$away_team))
  pool <- pool |> group_by(team) |> mutate(k_start = pos == "K" & salary == max(c(salary[pos == "K"], -Inf))) |> ungroup() |>
    left_join(ks, by = "team") |> left_join(dst_v |> filter(game_id == g$game_id) |> select(team, dst_proj), by = "team") |>
    mutate(proj = case_when(out | inj_out %in% TRUE ~ NA_real_, pos %in% c("QB", "RB", "WR", "TE") ~ dk_proj,
                            pos == "K" & k_start & !is.na(k_espn) ~ pmax(k_combo(k_espn, salary, avg_points), 0), pos == "DST" ~ dst_proj, TRUE ~ NA_real_),
           src = case_when(pos %in% c("QB", "RB", "WR", "TE") & !is.na(proj) ~ "props", pos == "K" & !is.na(proj) ~ "kicker model", pos == "DST" & !is.na(proj) ~ "Vegas D/ST", TRUE ~ NA_character_))
  d <- pool |> filter(!is.na(proj), proj > 0) |> arrange(team, desc(proj))
  if (nrow(d) < 8 || n_distinct(d$team) < 2) { SEC[[length(SEC) + 1]] <- list(g = g, lab = lab, LU = NULL, pool = pool,
      msg = sprintf("Only %d players with props / model numbers so far (props usually post Tuesday-Wednesday).", nrow(d))); next }
  L <- lu_tbl(sd_best_n(d), d)
  sk_i <- which(d$pos %in% c("QB", "RB", "WR", "TE") & d$proj >= CPT_THR); sk_i <- sk_i[order(-d$proj[sk_i])]
  Lc <- lu_tbl(sd_best_n(d, force_cpt = sk_i), d)
  LU <- list(game_id = g$game_id, label = lab, ko = g$ko, built = NOW, lineups = L, lineups_cpt = Lc, cpt_thr = CPT_THR, cpt_forced = d$name[sk_i], pool = pool, frozen = FALSE)
  saveRDS(LU, lu_f); n_built <- n_built + 1L
  write.csv(pool |> transmute(game_id = g$game_id, refreshed = isoz(NOW), name, pos, team, salary, cpt_salary, proj = round(proj, 3), src, injury_status, dk_id = export_id, cpt_id),
            file.path(SD_LIVE, sprintf("sd_proj_%s.csv", g$game_id)), row.names = FALSE)
  SEC[[length(SEC) + 1]] <- list(g = g, lab = lab, LU = LU)
}

## ---- 4. pages + upload CSVs: one page per model (Andrew 2026-11-09) ----
f1 <- function(x) ifelse(is.na(x), "", sprintf("%.1f", x)); dol <- function(x) paste0("$", format(round(x), big.mark = ","))
BT <- SDS$backtest; BTS <- SDS$backtest_spread                               # BTS: 91s_sd_spread.R, same scoring for both models
MODELS <- list(top = list(dir = "showdown", nav = "showdown", name = "Highest projected", lu = "lineups", csv = "DK_showdown_%s.csv", bt = "cur"),
               cpt = list(dir = "showdown-cpt", nav = "showdown_cpt", name = "Captain spread", lu = "lineups_cpt", csv = "DK_showdown_cpt_%s.csv", bt = "cpt"))
bts_txt <- function(b) if (is.null(b)) "" else sprintf("%d flagship showdowns 2023–26, each season left out: %.2f cashes per 20, %.1f%% of games with no cash, %.1f%% with 2 or fewer, %d games with a top-0.1%% lineup, ROI %+.0f%% (%+.0f%% without the best game)",
  b$games, b$cash, 100 * b$zero, 100 * b$le2, as.integer(round(b$top01)), 100 * b$roi, 100 * b$roi_wo)
for (MD in names(MODELS)) { M <- MODELS[[MD]]
odir <- file.path(SITE_DIR, "dfs", M$dir); dir.create(odir, recursive = TRUE, showWarnings = FALSE)
sec_html <- vapply(SEC, \(s) {
  h <- sprintf("<h2>%s <span class='s'>%s ET</span></h2>", s$lab, pp_et(s$g$ko, "%a %b %d %I:%M %p"))
  if (is.null(s$LU)) return(paste0(h, "<p class='s'>", s$msg %||% "No lineups yet.", "</p>"))
  L <- s$LU[[M$lu]]
  if (is.null(L) || !nrow(L)) return(paste0(h, "<p class='s'>This game started before the ", M$name, " tab existed: no lineups.</p>"))
  nl <- n_distinct(L$lineup)
  csv <- sprintf(M$csv, s$g$game_id)
  fc <- if (MD == "cpt") sprintf("<p class='s'><b>Captains required</b> (QB / RB / WR / TE projected %s+ DK points): %s.</p>", format(s$LU$cpt_thr %||% CPT_THR),
                                 if (length(s$LU$cpt_forced)) paste(s$LU$cpt_forced, collapse = ", ") else "none") else ""
  up <- L |> group_by(lineup) |> summarise(ids = paste(c(dk_id[slot == "CPT"], dk_id[slot == "FLEX"]), collapse = ","), .groups = "drop")
  writeLines(c("CPT,FLEX,FLEX,FLEX,FLEX,FLEX", up$ids), file.path(odir, csv))
  when <- if (isTRUE(s$LU$frozen)) sprintf("<b>Frozen at kickoff</b> (built %s ET).", pp_et(s$LU$built, "%a %I:%M %p")) else
    sprintf("Built %s ET; rebuilt at every refresh until kickoff, then frozen.", pp_et(s$LU$built, "%a %I:%M %p"))
  cards <- vapply(seq_len(nl), \(k) { x <- L |> filter(lineup == k)
    sprintf("<div class='lu'><h3><span>#%d</span><span>%s pts · %s</span></h3><table><tbody>%s</tbody></table></div>", k, f1(sum(x$proj)), dol(sum(x$salary)),
            paste0(sprintf("<tr><td class='sl'>%s</td><td class='l'>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>", x$slot, x$name, x$pos, x$team, dol(x$salary), f1(x$proj)), collapse = "")) }, "")
  ## exposure across the 20 (Andrew 2026-11-07: as on the Play-Action tabs, split by slot)
  ex <- L |> group_by(name, pos, team) |> summarise(cpt = sum(slot == "CPT"), flex = sum(slot == "FLEX"), .groups = "drop") |> mutate(n = cpt + flex) |> arrange(desc(n), desc(cpt), name)
  ex_html <- paste0("<h3 class='ex'>Exposure across the ", nl, "</h3><div class='tw'><table><thead><tr><th class='l'>Player</th><th class='l'>Pos</th><th class='l'>Team</th><th>Captain</th><th>Flex</th><th>Lineups</th></tr></thead><tbody>",
    paste0(sprintf("<tr><td class='l'>%s</td><td class='l'>%s</td><td class='l'>%s</td><td>%s</td><td>%s</td><td>%d of %d</td></tr>", ex$name, ex$pos, ex$team,
                   ifelse(ex$cpt > 0, as.character(ex$cpt), ""), ifelse(ex$flex > 0, as.character(ex$flex), ""), ex$n, nl), collapse = ""), "</tbody></table></div>")
  pl <- s$LU$pool |> filter(!is.na(proj)) |> arrange(desc(proj))
  ptab <- sprintf("<details><summary>Player pool (%d with props / model numbers)</summary><div class='tw'><table><thead><tr><th class='l'>Player</th><th>Pos</th><th>Team</th><th>Flex $</th><th>Captain $</th><th>Proj</th><th>Captain proj</th><th class='l'>Source</th></tr></thead><tbody>%s</tbody></table></div></details>",
                  nrow(pl), paste0(sprintf("<tr><td class='l'>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td class='l'>%s</td></tr>", pl$name, pl$pos, pl$team, dol(pl$salary), dol(pl$cpt_salary), f1(pl$proj), f1(1.5 * pl$proj), pl$src), collapse = ""))
  paste0(h, fc, sprintf("<p class='s'>%s %d lineups. Not reserving? <a href='%s' download>New-lineups CSV (%d lineups)</a> for DK &rarr; Lineups &rarr; Upload Lineups.</p>", when, nl, csv, nl),
         sprintf("<div class='sdwrap'><div class='sdrow'>%s</div></div>", paste0(cards, collapse = "")), ex_html, ptab) }, "")
note <- paste0("<p class='s'>For the DK NFL Showdown Play-Action [20 Entry Max] of each primetime game and every game with its own kickoff slot (international, holiday and Saturday games) ($200K–$400K, ~80–120K entries). ",
  "Captain scores 1.5× (salary 1.5×). Projections in DK points: props for skill players (the Players page), the kicker model + salary + DK average for kickers, Vegas-only D/ST. ",
  sprintf("<b>Build:</b> %sthe 20 best projections with no player lacking props / a model, %s, at least 2 of 6 slots different (a captain swap counts).",
          if (MD == "cpt") sprintf("every QB / RB / WR / TE projected %s+ DK points captains at least one lineup (his best lineup); the rest are ", format(CPT_THR)) else "",
          if ((SDS$rules$max_left %||% 1000) >= 50000) "any amount of salary left unused" else sprintf("$%s or less left", format(SDS$rules$max_left, big.mark = ","))),
  if (MD == "cpt") " <b>Why:</b> the Highest projected build puts its 20 lineups on one game script; when its core busts, all 20 bust together (TB @ CHI, 2026 week 9: 0 of 20, the game's MVP in none). This build makes sure each good skill player captains at least once." else "",
  if (!is.null(BTS[[M$bt]])) paste0(" <b>Back-test</b> (", bts_txt(BTS[[M$bt]]), if (MD == "cpt" && !is.null(BTS$cur)) sprintf("; Highest projected: %s", sub("^.*?: ", "", bts_txt(BTS$cur), perl = TRUE)) else "",
    "). Levels are optimistic; the comparison is the point.") else
  if (MD == "top" && !is.null(BT)) sprintf(" <b>Back-test</b> (%d flagship showdowns 2023–26, each season left out): %.1f cashes per 20, %.2f top-1%% lineups per game, %d games with a top-0.01%% lineup, ROI %+.0f%% (plain top projection %.1f / %.2f / %d / %+.0f%%). Levels are optimistic; the comparison is the point. Game script (spread / total) and fading chalk did not help.",
                            BT$games, BT$cash, BT$top1, BT$top001, 100 * BT$roi, BT$tp_cash, BT$tp_top1, BT$tp_top001, 100 * BT$tp_roi) else "",
  sprintf(" Other model: <a href='%sdfs/%s/'>%s</a>.", SITE_BASE, MODELS[[setdiff(names(MODELS), MD)]]$dir, MODELS[[setdiff(names(MODELS), MD)]]$name), "</p>")
## reserved entries (Andrew 2026-11-08): one form for every game; the game whose players are in DK's file is used
fill_sets <- Filter(Negate(is.null), lapply(SEC, \(s) if (is.null(s$LU) || is.null(s$LU[[M$lu]]) || !nrow(s$LU[[M$lu]])) NULL else {
  L <- s$LU[[M$lu]] |> group_by(lineup) |> arrange(slot != "CPT", .by_group = TRUE) |> ungroup()
  list(label = s$lab, lineups = dk_fill_lineups(L)) }))
fill_html <- if (length(fill_sets) && exists("dk_fill_box")) dk_fill_box(fill_sets, "our showdown lineups (the right game is picked from your file)") else ""
html <- paste0('<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  sprintf("<title>DK Showdown %s %d wk %d</title>", M$name, SEASON, WEEK),
  '<style>:root{--bg:#fff;--fg:#1d1d1f;--mut:#666;--bd:#ddd;--th:#f3f3f5;--acc:#2b6cb0}
@media (prefers-color-scheme:dark){:root{--bg:#141416;--fg:#e8e8ea;--mut:#9a9aa0;--bd:#333;--th:#1f1f23;--acc:#7fb0ef}}
body{background:var(--bg);color:var(--fg);font-family:system-ui,sans-serif;max-width:1500px;margin:1.5rem auto;padding:0 16px}
h1{font-size:1.4rem;margin:.2rem 0}h2{font-size:1.1rem;margin:1.4rem 0 .4rem}.s{color:var(--mut);font-size:13px}a{color:var(--acc)}.tw{overflow-x:auto}
table{border-collapse:collapse;font-size:13px;white-space:nowrap}th,td{border:1px solid var(--bd);padding:3px 7px;text-align:right}th{background:var(--th)}td.l,th.l{text-align:left}
details{margin:.6rem 0}summary{cursor:pointer;color:var(--acc)}
.sdwrap{position:relative;margin:.4rem 0 .8rem}.sdrow{display:flex;gap:12px;overflow-x:auto;scrollbar-width:none;-webkit-overflow-scrolling:touch;padding:2px 0}
.sdrow::-webkit-scrollbar{display:none}.sdrow .lu{flex:0 0 auto;width:max-content;overflow:visible}.sdrow .lu table{width:auto;white-space:nowrap}
@media (max-width:560px){.sdrow .lu table{font-size:11.5px}.sdrow .lu td{padding:3px 4px}}
h3.ex{font-size:14px;margin:.8rem 0 .3rem}', DFS_SUBNAV_CSS, DFS_LU_CSS, SITE_CSS, if (exists("DKFILL_CSS")) DKFILL_CSS else "",
  '</style>', if (exists("DKFILL_JS")) paste0("<script>", DKFILL_JS, "</script>") else "", '<script>
/* lineup rows scroll like the D/ST and kicker tables (site_utils dragScroll): drag anywhere with the mouse, swipe on touch, or hold the arrows */
function sdDrag(){document.querySelectorAll(".sdrow").forEach(sc=>{if(sc.dataset.drag)return;sc.dataset.drag="1";sc.classList.add("dragx");
let down=false,x0=0,s0=0,moved=false;
sc.addEventListener("mousedown",e=>{if(e.button!==0)return;down=true;moved=false;x0=e.pageX;s0=sc.scrollLeft});
window.addEventListener("mousemove",e=>{if(!down)return;const dx=e.pageX-x0;if(Math.abs(dx)>5){moved=true;sc.classList.add("dragging")}if(moved){sc.scrollLeft=s0-dx;e.preventDefault()}});
window.addEventListener("mouseup",()=>{down=false;sc.classList.remove("dragging")});
sc.addEventListener("click",e=>{if(moved){e.stopPropagation();e.preventDefault();moved=false}},true);
const bar=document.createElement("div");bar.className="scrollbtns";bar.innerHTML=`<span>drag the lineups or hold</span><button type="button" data-d="-1" aria-label="scroll left">\u25C0</button><button type="button" data-d="1" aria-label="scroll right">\u25B6</button>`;
sc.parentNode.insertBefore(bar,sc);
bar.querySelectorAll("button").forEach(b=>{let iv=null;const go=()=>{sc.scrollLeft+=(+b.dataset.d)*14};
const start=e=>{e.preventDefault();go();clearInterval(iv);iv=setInterval(go,16)},stop=()=>{clearInterval(iv);iv=null};
b.addEventListener("mousedown",start);b.addEventListener("touchstart",start,{passive:false});
["mouseup","mouseleave","touchend","touchcancel"].forEach(ev=>b.addEventListener(ev,stop))})})}
window.addEventListener("load",sdDrag);
</script></head><body>', site_nav("dfs", SITE_BASE), dfs_subnav(M$nav, SITE_BASE),
  sprintf("<h1>DraftKings Showdown: %s — %d week %d</h1>", M$name, SEASON, WEEK), note, fill_html, paste0(sec_html, collapse = ""), "</body></html>")
writeLines(html, file.path(odir, "index.html"))
}
message(sprintf("showdown: pages written (2 models; %d showdown game%s, %d built this run)", nrow(gm), if (nrow(gm) == 1) "" else "s", n_built))
})
