# ==============================================================================
# dst_blend_utils.R — scoring helpers shared by 40_dst_model.R (weekly fit) and 45_dst_refresh.R (daily
# re-score with new Vegas lines). Only base R + dplyr/tibble/purrr, so the GitHub runner stays light.
#
# A fitted blend is stored as coefficient vectors:
#   M$enet, M$ridge : named numeric, "(Intercept)" first, then feature names
#   M$comp          : list of GLM coefficient vectors, one per component (names match `specs`)
# specs = GLM_SPECS from 40_dst_model.R: list(<part> = list(y, x, fam)).
# ==============================================================================

lin_pred <- function(b, te) drop(cbind(1, as.matrix(te[names(b)[-1]])) %*% b)
glm_pred <- function(b, fam, te) { eta <- lin_pred(b, te); switch(fam, quasipoisson = exp(eta), binomial = plogis(eta), eta) }
comp_pred <- function(B, specs, te, sc) {
  g <- function(k) glm_pred(B[[k]], specs[[k]]$fam, te)
  sc$sack * g("sacks") + sc$int * g("ints") + sc$fum_rec * g("fr") + g("big") + g("pa") + (if (is.null(sc$ya_breaks)) 0 else g("ya"))
}
blend_pred <- function(M, te, sc, specs, models = c("enet", "components", "ridge")) {
  parts <- list(enet = lin_pred(M$enet, te), ridge = lin_pred(M$ridge, te), components = comp_pred(M$comp, specs, te, sc))
  rowMeans(do.call(cbind, parts[models]))
}
# per-team expected components shown in the report (E[sacks], E[TO], PA pts, YA pts, P(TD))
component_cols <- function(B, specs, te, sc) tibble::tibble(
  e_sacks = glm_pred(B$sacks, specs$sacks$fam, te),
  e_to    = glm_pred(B$to,    specs$to$fam,    te),
  e_pa    = glm_pred(B$pa,    specs$pa$fam,    te),
  e_ya    = if (is.null(sc$ya_breaks)) 0 else glm_pred(B$ya, specs$ya$fam, te),
  p_td    = glm_pred(B$td,    specs$td$fam,    te))

# Outcome ranges, boom/bust odds and P(top 8) from out-of-sample back-test residuals (see 40_dst_model.R, 9b).
wq <- function(x, w, p) { o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w); x[which(cw >= p)[1]] }
# D/ST scores are whole points but proj + residual is continuous, so the cuts sit half a point below the
# whole-point thresholds: 10+ ≡ ≥ 9.5, < 3 ≡ < 2.5 (continuity correction, Andrew 2026-09-25; before it
# P(boom) ran 2 pts low in the back-test: said 24.2% vs 26.2% happened).
outcome_dist <- function(proj, unc_proj, unc_resid, bw, n_sim, seed, boom_cut = 9.5, bust_cut = 2.5) {
  w_all <- sapply(proj, function(p) dnorm((unc_proj - p) / bw))                       # n_backtest × n_teams
  od <- purrr::map_dfr(seq_along(proj), function(k) { w <- w_all[, k]; sim <- proj[k] + unc_resid
    tibble::tibble(q10 = wq(sim, w, .10), q25 = wq(sim, w, .25), q75 = wq(sim, w, .75), q90 = wq(sim, w, .90),
                   p_boom = sum(w * (sim >= boom_cut)) / sum(w), p_bust = sum(w * (sim < bust_cut)) / sum(w)) })
  set.seed(seed)
  sims <- sapply(seq_along(proj), function(k) proj[k] + sample(unc_resid, n_sim, replace = TRUE, prob = w_all[, k]))
  od$p_top8 <- colMeans(t(apply(-sims, 1, rank, ties.method = "random")) <= 8)
  od
}
boot_summary <- function(boot) tibble::tibble(proj_se = apply(boot, 1, sd), ci_lo = apply(boot, 1, quantile, 0.05), ci_hi = apply(boot, 1, quantile, 0.95))

# Swap in new starting QBs (see 40_dst_model.R, 9d). starters = tibble(team = offense, qb_id, qb_name); NA / missing
# teams keep the weekly QB. Rebuilds the swapped rows' opponent-offense (o_) and opposing-QB (qb_) features from the
# bundle's scenario tables, re-centres the week exactly as build_frame() does and recomputes the interactions.
# force = TRUE rebuilds every row from the scenario tables (self-check in 40). Bundles without $qb are returned as is.
swap_qbs <- function(b, starters, force = FALSE) {
  te <- b$te; Q <- b$qb
  if (is.null(Q)) return(list(te = te, info = NULL))
  raw <- Q$raw[match(paste(te$game_id, te$team), paste(Q$raw$game_id, Q$raw$team)), ]
  new_id <- if (is.null(starters)) rep(NA_character_, nrow(raw)) else starters$qb_id[match(raw$opp, starters$team)]
  new_nm <- if (is.null(starters)) rep(NA_character_, nrow(raw)) else starters$qb_name[match(raw$opp, starters$team)]
  # games already played at the weekly run keep their rows exactly (their QB is known and the projection is locked)
  locked <- if (is.null(Q$locked)) rep(FALSE, nrow(raw)) else raw$game_id %in% Q$locked
  changed <- !locked & !is.na(new_id) & (is.na(raw$opp_qb_id) | new_id != raw$opp_qb_id)
  id <- ifelse(changed, new_id, raw$opp_qb_id)
  for (i in which(if (force) !is.na(id) & !locked else changed)) {
    a <- Q$alt[Q$alt$team == raw$opp[i] & Q$alt$qb_id == id[i], ]
    if (!nrow(a)) a <- Q$alt[Q$alt$team == raw$opp[i] & Q$alt$qb_id == "NEW", ]
    p <- Q$pool[Q$pool$qb_id == id[i], ]
    for (v in Q$swap_o)  raw[[v]][i] <- a[[v]][1]
    for (v in Q$swap_qb) raw[[v]][i] <- if (nrow(p)) p[[v]][1] else Q$fill[[v]][1]
  }
  val <- function(v) if (Q$center && v %in% Q$centred) raw[[v]] - mean(raw[[v]], na.rm = TRUE) else raw[[v]]
  for (v in intersect(c(Q$swap_o, Q$swap_qb), names(te))) te[[v]] <- val(v)
  dv <- function(v) raw[[paste0("cen_", v)]]
  if ("press_x" %in% names(te)) te$press_x <- dv("d_press_rate") * val("o_press_rate")
  if ("sack_x"  %in% names(te)) te$sack_x  <- dv("d_sack_rate") * val("o_sack_rate")
  if ("to_x"    %in% names(te)) te$to_x    <- (dv("d_int_rate") + dv("d_fuml_rate")) * (val("o_int_rate") + val("o_fuml_rate"))
  pool_nm <- Q$pool$qb_name[match(id, Q$pool$qb_id)]
  list(te = te, info = tibble::tibble(team = te$team, opp = raw$opp, opp_qb_id = id,
                                      opp_qb_name = ifelse(changed, dplyr::coalesce(new_nm, pool_nm, id), raw$opp_qb_name),
                                      base_qb_name = raw$opp_qb_name, qb_changed = changed, o_qb_cont = raw$o_qb_cont,
                                      qb_new = changed & !(paste(raw$opp, id) %in% paste(Q$alt$team, Q$alt$qb_id))))
}

## ---- Component simulation: P(10+) / P(<3) / P(15+) per matchup (Andrew, 2026-09-25; tested in 71_component_sim_test.R) ----
# Each game is simulated from its parts (sacks, INTs, fumble recoveries, defensive/return TDs as counts; points and
# yards allowed as regressions + a past game's miss; safeties / blocks / 2-pt returns as they happened), with all parts
# drawn JOINTLY from one past game (each past game's quantile per part = an empirical copula), so game script links
# them. The draws keep their SHAPE but are re-centred on the production projection, so the projection never changes.
# The pool (quantiles of every training game) is stored in the bundle, so the daily refresh re-simulates with new
# lines / QBs exactly, without data. Back-test 2019–25 (Andrew's NGS model, ESPN): average said = happened (boom 26.1 vs
# 26.2%, bust 31.9 vs 32.3%), log loss slightly better than the kernel method; the simulated spread tracks the actual
# spread (t 3.3). Used only when the system's points rebuild exactly from the parts (else the kernel method stays).
.sim_pit_pois <- function(y, mu) stats::ppois(y - 1, mu) + stats::runif(length(y)) * stats::dpois(y, mu)
.sim_lin  <- function(b, te) drop(cbind(1, as.matrix(te[setdiff(names(b), "(Intercept)")])) %*% b)
.sim_fill <- function(te, med) { for (f in names(med)) { if (!f %in% names(te)) te[[f]] <- med[[f]]; te[[f]][is.na(te[[f]])] <- med[[f]] }; te }
.sim_pa_pts <- function(pa, SC) SC$pa_pts[cut(pa, SC$pa_breaks, labels = FALSE)]
.sim_ya_pts <- function(ya, SC) if (is.null(SC$ya_breaks)) 0 * ya else SC$ya_pts[cut(ya, SC$ya_breaks, labels = FALSE)]
DST_SIM_Y <- c(sacks = "dst_sacks", ints = "dst_ints", fr = "dst_fr", td = "dst_td")
DST_SIM_CUTS <- list(p_boom = list(cut = 9.5, above = TRUE), p_bust = list(cut = 2.5, above = FALSE), p_ceiling = list(cut = 14.5, above = TRUE))
# Per-format log-odds offsets (72_sim_calibration.R on Andrew's 2019–25 back-test, 2026-09-25): on Yahoo / FFPC the raw
# simulation ran ~1.2 pts high on P(10+) and ~1.5 pts low on P(<3). The offsets fix the average; ESPN needs none.
# Yahoo P(<3) stays on the kernel method: even calibrated it scored worse (log loss −0.41%, t −1.6).
DST_SIM_OFFSET <- list(Yahoo = c(p_boom = -0.060, p_ceiling = -0.022), FFPC = c(p_boom = -0.061, p_bust = 0.085, p_ceiling = -0.038))
DST_SIM_KERNEL <- list(Yahoo = "p_bust")        # outcomes that keep the kernel method

dst_sim_fit <- function(tr, specs, SC, seed = 1) {
  X <- list(sacks = specs$sacks$x, ints = specs$ints$x, fr = specs$fr$x, td = specs$big$x, pa = specs$pa$x,
            ya = if (!is.null(SC$ya_breaks)) specs$ya$x)
  allx <- unique(unlist(X)); med <- vapply(allx, function(f) { m <- stats::median(tr[[f]], na.rm = TRUE); if (is.na(m)) 0 else m }, 0)
  tr <- .sim_fill(tr, med)
  fit <- function(y, x, fam) { b <- stats::coef(stats::glm(stats::as.formula(paste(y, "~", paste(c("1", x), collapse = " + "))), data = tr, family = fam))
    b[is.na(b)] <- 0; b }
  B <- c(lapply(stats::setNames(names(DST_SIM_Y), names(DST_SIM_Y)), function(p) fit(DST_SIM_Y[[p]], X[[p]], stats::quasipoisson())),
         list(pa = fit("pa", X$pa, stats::gaussian()), ya = if (!is.null(X$ya)) fit("ya", X$ya, stats::gaussian())))
  set.seed(seed)
  pool <- as.data.frame(lapply(stats::setNames(names(DST_SIM_Y), paste0("u_", names(DST_SIM_Y))),
                               function(p) .sim_pit_pois(tr[[DST_SIM_Y[[p]]]], exp(.sim_lin(B[[p]], tr)))))
  pool$r_pa <- tr$pa - .sim_lin(B$pa, tr)
  pool$r_ya <- if (!is.null(B$ya)) tr$ya - .sim_lin(B$ya, tr) else 0
  pool$rare <- with(tr, SC$safety * dst_safety + SC$block_kick * dst_blk_kick + SC$block_pat * dst_blk_pat + SC$two_pt_ret * dst_2pt)
  rebuilt <- with(tr, SC$sack * dst_sacks + SC$int * dst_ints + SC$fum_rec * dst_fr + SC$td * dst_td) + pool$rare +
    .sim_pa_pts(tr$pa, SC) + .sim_ya_pts(tr$ya, SC)
  list(B = B, med = med, pool = pool, exact = mean(abs(rebuilt - tr$fp) < 1e-9), n = nrow(tr), seed = seed, cuts = DST_SIM_CUTS)
}
dst_sim_draws <- function(S, te, SC) {                    # games × past games matrix of simulated points (not re-centred)
  te <- .sim_fill(te, S$med); nt <- nrow(te); np <- nrow(S$pool)
  RU <- function(u) matrix(u, nt, np, byrow = TRUE); RM <- function(m) matrix(m, nt, np)
  cnt <- function(p) matrix(stats::qpois(pmin(RU(S$pool[[paste0("u_", p)]]), 1 - 1e-12), RM(exp(.sim_lin(S$B[[p]], te)))), nt, np)
  M <- SC$sack * cnt("sacks") + SC$int * cnt("ints") + SC$fum_rec * cnt("fr") + SC$td * cnt("td") + RU(S$pool$rare) +
    matrix(.sim_pa_pts(pmax(0, round(RM(.sim_lin(S$B$pa, te)) + RU(S$pool$r_pa))), SC), nt, np)
  if (!is.null(S$B$ya)) M <- M + matrix(.sim_ya_pts(round(RM(.sim_lin(S$B$ya, te)) + RU(S$pool$r_ya)), SC), nt, np)
  M
}
# shared: re-centre draws on the projection (+ ±½-step jitter so whole-point draws don't snap to the lattice), then P per cut
sim_probs <- function(M, proj, cuts, step = 1, seed = 1, offset = NULL) {
  set.seed(seed); Mj <- M + matrix(stats::runif(length(M), -step / 2, step / 2), nrow(M)); Mj <- Mj - rowMeans(Mj) + proj
  out <- lapply(stats::setNames(names(cuts), names(cuts)), function(k) { p <- rowMeans(if (cuts[[k]]$above) Mj >= cuts[[k]]$cut else Mj < cuts[[k]]$cut)
    if (!is.null(offset) && !is.null(offset[[k]])) p <- stats::plogis(stats::qlogis(pmin(pmax(p, 1e-4), 1 - 1e-4)) + offset[[k]]); p })
  out$sim_sd <- apply(M, 1, stats::sd); out$sim_check <- max(abs(rowMeans(Mj) - proj))
  tibble::as_tibble(out)
}
# P(10+), P(<3), P(15+) for this week's rows; NULL when the bundle has no simulation or the points don't rebuild exactly
# (offsets / kernel choices are looked up by format at scoring time, so a new dst_blend_utils.R applies to this week's bundles)
dst_sim_probs <- function(S, te, proj, SC) {
  if (is.null(S) || !isTRUE(S$exact >= 0.99)) return(NULL)
  out <- sim_probs(dst_sim_draws(S, te, SC), proj, S$cuts, step = 1, seed = S$seed, offset = as.list(DST_SIM_OFFSET[[SC$label]]))
  out[setdiff(names(out), DST_SIM_KERNEL[[SC$label]])]
}

## ---- Player availability (Andrew 2026-10-22; 40_dst_model.R 9e) ----
# b$avail: cols (feature names), raw (this week's values before centring: game_id, team, cols), shares (game_id, team = the
# player's team, fam = rush / front / db / cov / ol, player = gsis id, name, position, share, out, gone), locked (games already
# played at the weekly run), center. status = tibble(team, player, out, gone) for the players in shares (NULL = keep the weekly
# values). Rebuilds raw from the shares: d_<fam>_out / _gone = sum of the shares of this team's players who are out / gone;
# o_ol_out / _gone = the same for the opponent's linemen. Locked games and teams in `keep` keep their values. Then re-centres
# the week (value − mean over this week's rows) as build_frame() does.
AVAIL_FAM_COL <- c(rush = "d_rush", front = "d_front", db = "d_db", cov = "d_cov", tfl = "d_tfl", ol = "o_ol")
avail_raw_from <- function(A, st) {                      # st: shares with logical out / gone → raw per (game_id, team = the D/ST)
  s <- A$shares
  if (!is.null(st)) s <- dplyr::left_join(dplyr::select(s, -dplyr::any_of(c("out", "gone"))), st, by = c("team", "player")) %>%
    dplyr::mutate(out = dplyr::coalesce(out, FALSE), gone = dplyr::coalesce(gone, FALSE) & !out)
  agg <- s %>% dplyr::group_by(game_id, team, fam) %>% dplyr::summarise(out = sum(share[out]), gone = sum(share[gone]), .groups = "drop")
  r <- dplyr::select(A$raw, game_id, team)
  rows <- A$raw %>% dplyr::select(game_id, team)
  opp <- rows %>% dplyr::group_by(game_id) %>% dplyr::mutate(opp = rev(team)) %>% dplyr::ungroup()
  for (f in names(AVAIL_FAM_COL)) {
    if (!any(paste0(AVAIL_FAM_COL[[f]], c("_out", "_gone")) %in% A$cols)) next   # families this bundle doesn't use
    a <- agg[agg$fam == f, ]
    key_team <- if (f == "ol") opp$opp else opp$team            # o_ol_* = the opponent's linemen
    i <- match(paste(opp$game_id, key_team), paste(a$game_id, a$team))
    r[[paste0(AVAIL_FAM_COL[[f]], "_out")]]  <- dplyr::coalesce(a$out[i], 0)
    r[[paste0(AVAIL_FAM_COL[[f]], "_gone")]] <- dplyr::coalesce(a$gone[i], 0)
  }
  r
}
avail_weekly_status <- function(b) if (is.null(b$avail$shares)) NULL else dplyr::distinct(b$avail$shares, team, player, out, gone)
# override = tibble(team, <cols>): values to use for those teams instead (e.g. games that have kicked off keep their last
# pre-kickoff availability).
swap_avail <- function(b, status, override = NULL) {
  A <- b$avail; te <- b$te
  if (is.null(A)) return(list(te = te, raw = NULL, changed = character()))
  raw <- A$raw
  if (!is.null(status) && !is.null(A$shares)) {
    new <- avail_raw_from(A, status)
    lock <- raw$game_id %in% A$locked
    for (v in A$cols) raw[[v]] <- ifelse(lock, raw[[v]], new[[v]][match(paste(raw$game_id, raw$team), paste(new$game_id, new$team))])
  }
  if (!is.null(override) && nrow(override)) {
    k <- match(raw$team, override$team); ok <- !is.na(k) & !(raw$game_id %in% A$locked)
    for (v in intersect(A$cols, names(override))) raw[[v]][ok] <- override[[v]][k[ok]]
  }
  i <- match(paste(te$game_id, te$team), paste(raw$game_id, raw$team))
  for (v in intersect(A$cols, names(te))) { x <- raw[[v]][i]; te[[v]] <- if (isTRUE(A$center)) x - mean(x, na.rm = TRUE) else x }
  ch <- if (is.null(status)) character() else raw$team[rowSums(abs(as.matrix(raw[A$cols]) - as.matrix(A$raw[A$cols]))) > 1e-9]
  list(te = te, raw = raw, changed = ch)
}

# Live statuses for every player in b$avail$shares. Needs starters.R (st_get, st_canon, st_norm, st_team, st_title) and
# S = starter_sources() (its injury report is QB / K only, so the report is re-read here for all positions). Sources:
#   official report (nflverse injuries, this week): game status Out / Doubtful once it exists
#   Sleeper (all players, one call): team, roster status (IR, PUP, NFI, suspended, practice squad, inactive) and injury status
#   Ourlads depth charts (all positions, 32 pages): O / D / IR / PUP / SUS / NFI flags
# out  = IR / PUP / NFI / suspended in any source, or the report's Out / Doubtful, or (no report game status yet) Sleeper or
#        Ourlads Out / Doubtful - the same rule as player_status() for QBs and kickers
# gone = not out, and Sleeper has him on another team, no team, or the practice squad / inactive list
# A source that fails is skipped; if Sleeper fails, gone keeps the weekly value.
avail_sleeper_all <- function() {
  txt <- st_get("https://api.sleeper.app/v1/players/nfl", 90); if (is.null(txt)) stop("no response from Sleeper")
  js <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  g <- function(p, f) { v <- p[[f]]; if (is.null(v) || !length(v)) NA_character_ else as.character(v[[1]]) }
  tibble::tibble(team = st_team(vapply(js, g, "", "team")), name = dplyr::coalesce(vapply(js, g, "", "full_name"), paste(vapply(js, g, "", "first_name"), vapply(js, g, "", "last_name"))),
                 gsis_id = trimws(vapply(js, g, "", "gsis_id")), roster = vapply(js, g, "", "status"), injury = vapply(js, g, "", "injury_status")) %>%
    dplyr::mutate(gsis_id = ifelse(grepl("^00-", gsis_id), gsis_id, NA_character_))
}
avail_ourlads_all <- function(teams, pause = 0.5) {
  ent <- function(x) gsub("&#39;|&#039;|&apos;", "'", gsub("&amp;", "&", gsub("&nbsp;|&#160;", " ", x)))
  strip <- function(x) trimws(gsub("\\s+", " ", ent(gsub("<[^>]+>", " ", x))))
  out <- list()
  for (tm in teams) {
    code <- if (exists("OL_CODE") && tm %in% names(OL_CODE)) OL_CODE[[tm]] else tm
    html <- st_get(paste0("https://www.ourlads.com/nfldepthcharts/depthchart/", code), 30); Sys.sleep(pause)
    if (is.null(html)) next
    rows <- regmatches(html, gregexpr("(?is)<tr[^>]*>.*?</tr>", html, perl = TRUE))[[1]]
    for (r in rows) {
      cells <- regmatches(r, gregexpr("(?is)<td[^>]*>.*?</td>", r, perl = TRUE))[[1]]
      if (length(cells) < 3) next
      pos <- toupper(strip(cells[1])); if (!grepl("^[A-Z]{1,5}$", pos)) next
      cells <- cells[-1]; txt <- strip(cells)
      keep <- nzchar(txt) & !grepl("^[0-9]+$", txt) & grepl("[A-Za-z]", txt); cells <- cells[keep]; txt <- txt[keep]; if (!length(txt)) next
      has_a <- grepl("(?i)<a[^>]*>", cells, perl = TRUE)
      nm <- ifelse(has_a, strip(sub("(?is)^.*?<a[^>]*>(.*?)</a>.*$", "\\1", cells, perl = TRUE)), sub("\\s+\\S*[0-9/]\\S*.*$", "", txt))
      rest <- ifelse(has_a, strip(sub("(?is)^.*?</a>", "", cells, perl = TRUE)), sub("^.*?\\s+(\\S*[0-9/]\\S*.*)$", "\\1", txt))
      last <- toupper(sub("^.*\\s", "", paste("", rest)))
      flag <- ifelse(last %in% c("O", "D", "Q", "IR", "PUP", "SUS", "NFI"), last, NA_character_)
      out[[length(out) + 1]] <- tibble::tibble(team = tm, pos = pos, name = st_title(nm), flag = flag)
    }
  }
  if (!length(out)) stop("no Ourlads pages parsed")
  dplyr::bind_rows(out)
}
avail_report_all <- function(season, week) {
  d <- tibble::as_tibble(nv_rds("injuries", sprintf("injuries_%d.rds", season)))
  d <- d[d$week == week & d$game_type %in% "REG", ]
  tibble::tibble(team = st_team(d$team), name = d$full_name, gsis_id = d$gsis_id, status = st_canon(d$report_status))
}
avail_canon <- function(x) {                             # st_canon() plus Sleeper's long roster-status wording
  u <- toupper(trimws(as.character(x)))
  dplyr::coalesce(st_canon(x), dplyr::case_when(grepl("INJURED RESERVE|^RESERVE", u) ~ "IR", grepl("PHYSICALLY|PUP", u) ~ "PUP",
                                                grepl("NON.?FOOTBALL", u) ~ "NFI", grepl("SUSPEN", u) ~ "Suspended", TRUE ~ NA_character_))
}
avail_live_status <- function(b, season, week, teams = unique(b$te$team), sources = c("report", "sleeper", "ourlads")) {
  sh <- b$avail$shares; if (is.null(sh)) return(NULL)
  pl <- dplyr::distinct(sh, team, player, name, out0 = out, gone0 = gone)
  get <- function(nm, f) tryCatch(f(), error = function(e) { message("availability: ", nm, " unavailable - ", conditionMessage(e)); NULL })
  rep <- if ("report" %in% sources) get("report", function() avail_report_all(season, week)) else NULL
  sl  <- if ("sleeper" %in% sources) get("Sleeper", avail_sleeper_all) else NULL
  ol  <- if ("ourlads" %in% sources) get("Ourlads", function() avail_ourlads_all(teams)) else NULL
  key <- st_norm(pl$name)
  by_id_or_name <- function(tab, same_team = TRUE) {     # row index in tab per player: gsis id, else team + name (last name if unique)
    if (is.null(tab) || !nrow(tab)) return(rep(NA_integer_, nrow(pl)))
    i <- if ("gsis_id" %in% names(tab)) match(pl$player, tab$gsis_id) else rep(NA_integer_, nrow(pl))
    tk <- paste(tab$team, st_norm(tab$name)); j <- match(paste(pl$team, key), tk)
    i[is.na(i)] <- j[is.na(i)]; i
  }
  ir <- by_id_or_name(rep); is <- by_id_or_name(sl); io <- by_id_or_name(ol)
  if (is.null(rep) && is.null(sl) && is.null(ol)) { message("availability: no live source - weekly values kept"); return(NULL) }
  r_st <- if (!is.null(rep)) rep$status[ir] else rep(NA_character_, nrow(pl))
  s_inj <- if (!is.null(sl)) avail_canon(sl$injury[is]) else rep(NA_character_, nrow(pl))
  s_ros <- if (!is.null(sl)) sl$roster[is] else rep(NA_character_, nrow(pl))
  s_hard <- if (!is.null(sl)) avail_canon(s_ros) else rep(NA_character_, nrow(pl))
  o_st <- if (!is.null(ol)) avail_canon(ol$flag[io]) else rep(NA_character_, nrow(pl))
  hard <- s_inj %in% ST_HARD | s_hard %in% ST_HARD | o_st %in% ST_HARD
  out <- hard | r_st %in% c("Out", "Doubtful") | (is.na(r_st) & (s_inj %in% c("Out", "Doubtful") | o_st %in% c("Out", "Doubtful")))
  if (is.null(sl)) out <- out | dplyr::coalesce(pl$out0, FALSE)     # without Sleeper, IR / PUP lists are unknown: keep the weekly outs
  gone <- if (!is.null(sl)) !out & !is.na(is) & (is.na(sl$team[is]) | sl$team[is] != pl$team | toupper(dplyr::coalesce(s_ros, "")) %in% c("INACTIVE", "PRACTICE SQUAD")) else
    dplyr::coalesce(pl$gone0, FALSE) & !out
  src <- paste(c(if (!is.null(rep)) "report", if (!is.null(sl)) "Sleeper", if (!is.null(ol)) "Ourlads"), collapse = ", ")
  pick <- function(x, ok) ifelse(ok, x, NA_character_)
  why <- ifelse(out, dplyr::coalesce(pick(s_hard, s_hard %in% ST_HARD), pick(s_inj, s_inj %in% ST_HARD), pick(o_st, o_st %in% ST_HARD),
                                     pick(r_st, r_st %in% c("Out", "Doubtful")), pick(s_inj, s_inj %in% c("Out", "Doubtful")), o_st, ""),
                ifelse(gone, "not with the team", ""))
  out_tbl <- tibble::tibble(team = pl$team, player = pl$player, name = pl$name, out = out, gone = gone & !out, why = why)
  attr(out_tbl, "sources") <- src
  out_tbl
}
# "Missing" note per D/ST for the page (biggest shares first): its defenders out / gone and the opponent's linemen
avail_note <- function(b, status, min_share = 0.25, min_rush = 0.08) {
  sh <- b$avail$shares; if (is.null(sh) || is.null(status)) return(NULL)
  s <- dplyr::left_join(dplyr::select(sh, -dplyr::any_of(c("out", "gone"))), dplyr::select(status, team, player, out, gone, why), by = c("team", "player")) %>%
    dplyr::filter(out | gone) %>%
    dplyr::filter((fam %in% c("front", "db", "ol") & share >= min_share) | (fam %in% c("rush", "cov", "tfl") & share >= min_rush)) %>%
    dplyr::filter(is.na(position) | (fam == "ol") == (position %in% c("T", "G", "C", "OL", "OT", "OG")),          # defenders in the defense line,
                  is.na(position) | fam == "ol" | !position %in% c("QB", "RB", "FB", "WR", "TE", "K", "P", "LS")) %>%   # linemen in the OL line
    dplyr::group_by(game_id, team, player, name, position, why) %>% dplyr::summarise(share = max(share), ol = any(fam == "ol"), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(share))
  rows <- dplyr::distinct(b$te, game_id, team) %>% dplyr::group_by(game_id) %>% dplyr::mutate(opp = rev(team)) %>% dplyr::ungroup()
  lab <- function(x) if (!nrow(x)) "" else paste(sprintf("%s (%s%s)", x$name, x$position, ifelse(nzchar(x$why), paste0(", ", x$why), "")), collapse = "; ")
  tibble::tibble(team = rows$team,
                 note = vapply(seq_len(nrow(rows)), function(k) {
                   d <- lab(s[s$game_id == rows$game_id[k] & s$team == rows$team[k] & !s$ol, ])
                   o <- lab(s[s$game_id == rows$game_id[k] & s$team == rows$opp[k] & s$ol, ])
                   paste(c(if (nzchar(d)) paste("Defense missing:", d), if (nzchar(o)) paste0(rows$opp[k], " OL missing: ", o)), collapse = "\n") }, ""))
}

## ---- Start/sit calls in the rosterable range (Andrew 2026-10-22) ----
# Every pair of teams inside the range a manager would consider (either source ranks both in its top `band`) that the two
# sources order differently is one start/sit call; gain = actual points of our pick minus theirs. Used as a check on model
# changes (39, 39b, 42) and on the Track record tab (62). a = ours, b = the other source, y = actual points.
start_sit_band <- function(a, b, y, band = 20) {
  ok <- !is.na(a) & !is.na(b) & !is.na(y); a <- a[ok]; b <- b[ok]; y <- y[ok]
  ra <- rank(-a, ties.method = "first"); rb <- rank(-b, ties.method = "first"); k <- which(ra <= band | rb <= band)
  if (length(k) < 2) return(tibble::tibble(calls = 0L, right = NA_real_, gain = NA_real_))
  ij <- utils::combn(k, 2); i <- ij[1, ]; j <- ij[2, ]
  dis <- sign(a[i] - a[j]) * sign(b[i] - b[j]) < 0
  if (!any(dis)) return(tibble::tibble(calls = 0L, right = NA_real_, gain = NA_real_))
  i <- i[dis]; j <- j[dis]; ours <- ifelse(a[i] > a[j], i, j); theirs <- ifelse(a[i] > a[j], j, i); g <- y[ours] - y[theirs]
  tibble::tibble(calls = length(g), right = mean(g > 0) + 0.5 * mean(g == 0), gain = mean(g))
}
# per week: start/sit calls of column `col` vs column `ref` (default the Vegas-only baseline) in a back-test prediction table
band_by_week <- function(p, col, ref = "vegas_lm", band = 20)
  dplyr::bind_rows(lapply(split(p, paste(p$season, p$week)), function(d)
    dplyr::bind_cols(tibble::tibble(season = d$season[1], week = d$week[1]), start_sit_band(d[[col]], d[[ref]], d$fp, band)))) %>%
  dplyr::arrange(season, week)
# summary + paired weekly t of `col` in p vs `col0` in p0 (same weeks): points per call vs Vegas
band_compare <- function(p, col, p0 = NULL, col0 = col) {
  a <- band_by_week(p, col)
  out <- c(band_calls = mean(a$calls), band_right = stats::weighted.mean(a$right, a$calls, na.rm = TRUE), band_gain = stats::weighted.mean(a$gain, a$calls, na.rm = TRUE))
  if (!is.null(p0)) { b <- band_by_week(p0, col0); m <- merge(a, b, by = c("season", "week")); d <- m$gain.x - m$gain.y; d <- d[!is.na(d)]
    out <- c(out, band_gain_vs_ref = mean(d), band_t_vs_ref = if (length(d) > 2 && stats::sd(d) > 0) mean(d) / (stats::sd(d) / sqrt(length(d))) else NA_real_) }
  out
}
