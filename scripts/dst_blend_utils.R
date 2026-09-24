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
outcome_dist <- function(proj, unc_proj, unc_resid, bw, n_sim, seed) {
  w_all <- sapply(proj, function(p) dnorm((unc_proj - p) / bw))                       # n_backtest × n_teams
  od <- purrr::map_dfr(seq_along(proj), function(k) { w <- w_all[, k]; sim <- proj[k] + unc_resid
    tibble::tibble(q10 = wq(sim, w, .10), q25 = wq(sim, w, .25), q75 = wq(sim, w, .75), q90 = wq(sim, w, .90),
                   p_boom = sum(w * (sim >= 10)) / sum(w), p_bust = sum(w * (sim < 3)) / sum(w)) })
  set.seed(seed)
  sims <- sapply(seq_along(proj), function(k) proj[k] + sample(unc_resid, n_sim, replace = TRUE, prob = w_all[, k]))
  od$p_top8 <- colMeans(t(apply(-sims, 1, rank, ties.method = "random")) <= 8)
  od
}
boot_summary <- function(boot) tibble::tibble(proj_se = apply(boot, 1, sd), ci_lo = apply(boot, 1, quantile, 0.05), ci_hi = apply(boot, 1, quantile, 0.95))
