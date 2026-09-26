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
dst_sim_probs <- function(S, te, proj, SC) {
  if (is.null(S) || !isTRUE(S$exact >= 0.99)) return(NULL)
  sim_probs(dst_sim_draws(S, te, SC), proj, S$cuts, step = 1, seed = S$seed)
}
