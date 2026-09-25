# ==============================================================================
# k_blend_utils.R — portable kicker scoring, shared by 50_k_model.R (weekly fit) and 55_k_refresh.R
# (daily re-score with new Vegas lines and weather forecasts). Base R + dplyr/tibble/purrr only.
#
# Bundle layout (written by 50_k_model.R section 9c):
#   B$enet[[system]] : named coefficient vector, "(Intercept)" first
#   B$comp           : list(counts = list(u30, 30s, 40s, 50p, xp) coefficient vectors (log link),
#                           make = coefficient vector of the kick-level logistic model,
#                           qd = list(bin -> 5 representative distances), basis = list(bin -> 5 x 4 spline basis))
#   B$SCORING        : scoring rules (fg(), fg_miss, xp, xp_miss, step)
# ==============================================================================

lin_pred <- function(b, te) { b[is.na(b)] <- 0; drop(cbind(1, as.matrix(te[names(b)[-1]])) %*% b) }

# kick-level make probability at distance d (spline basis row `bas`) for every row of te
make_prob <- function(b, te, d, bas) {
  b[is.na(b)] <- 0
  ns_c <- b[grep("^ns\\(dist", names(b))]; wd <- b[grep("^I\\(wind_o", names(b))]
  other <- b[setdiff(names(b), c("(Intercept)", names(ns_c), names(wd)))]
  eta <- b[["(Intercept)"]] + sum(ns_c * bas) + (if (length(wd)) wd * te$wind_o * d / 50 else 0) +
    drop(as.matrix(te[names(other)]) %*% other)
  plogis(eta)
}

# component model: expected attempts by distance bin x P(make) at typical distances x points, plus E[PAT made]
comp_score <- function(C, te, SCORING) {
  bins <- names(C$qd)
  out <- tibble::tibble(e_xp = exp(lin_pred(C$counts$xp, te)))
  for (sy in names(SCORING)) out[[paste0("fp_", sy)]] <- out$e_xp * SCORING[[sy]]$xp
  for (bn in bins) {
    ea <- exp(lin_pred(C$counts[[bn]], te)); out[[paste0("e_a_", bn)]] <- ea
    pm <- sapply(seq_along(C$qd[[bn]]), function(j) make_prob(C$make, te, C$qd[[bn]][j], C$basis[[bn]][j, ]))
    pm <- matrix(pm, nrow = nrow(te)); out[[paste0("p_", bn)]] <- rowMeans(pm)
    for (sy in names(SCORING)) {
      sc <- SCORING[[sy]]; pts <- sc$fg(C$qd[[bn]])
      ev <- rowMeans(sweep(pm, 2, pts, `*`) + (1 - pm) * sc$fg_miss)
      out[[paste0("fp_", sy)]] <- out[[paste0("fp_", sy)]] + ea * ev
    }
  }
  out$e_fga <- rowSums(as.matrix(out[paste0("e_a_", bins)]))
  out$e_fgm <- rowSums(as.matrix(out[paste0("e_a_", bins)]) * as.matrix(out[paste0("p_", bins)]))
  out
}

# production blend = mean of the portable models in `models` (B$blend; default enet + components) for one system
blend_score <- function(B, te, sy, comp = NULL, models = NULL) {
  models <- if (!is.null(models)) models else if (!is.null(B$blend)) B$blend else c("enet", "components")
  parts <- list()
  if ("enet" %in% models) parts$enet <- lin_pred(B$enet[[sy]], te)
  if ("components" %in% models) { if (is.null(comp)) comp <- comp_score(B$comp, te, B$SCORING); parts$components <- comp[[paste0("fp_", sy)]] }
  rowMeans(do.call(cbind, parts))
}

# environment features derived from roof / wind / temp (must match 50_k_model.R build_frame)
derive_env <- function(te) dplyr::mutate(te,
  wind = ifelse(indoor == 1, 0, wind), temp = ifelse(indoor == 1, 70, temp),
  wind_o = ifelse(indoor == 1, 0, wind), cold = as.integer(indoor == 0 & temp <= 40), wind_hi = as.integer(wind_o >= 15),
  wind_x_long = wind_o * k_long_share_raw)

# Vegas features from a team's spread (+ = favoured) and the game total
derive_vegas <- function(te) dplyr::mutate(te, implied_own = (total_line + spread) / 2, implied_opp = (total_line - spread) / 2)

# outcome ranges / boom / bust / P(top N) from out-of-sample back-test residuals, kernel-weighted on the projection
wq <- function(v, w, p) { o <- order(v); cw <- cumsum(w[o]); v[o][pmin(length(v), findInterval(p, cw) + 1)] }
outcome_cols <- function(proj, U, sy) {
  pool <- U$pool[U$pool$system == sy, ]; hs <- U$step[[sy]] / 2
  if (is.list(U$boom)) U$boom <- U$boom[[sy]]            # per-format boom cut (bundles from 2026-09-24 on)
  dist1 <- function(pj) { w <- dnorm((pool$proj - pj) / U$bw); list(v = pj + (pool$y - pool$proj), w = w / sum(w)) }
  q <- t(sapply(proj, function(pj) { d <- dist1(pj)
    c(wq(d$v, d$w, c(.1, .25, .75, .9)), sum(d$w[d$v >= U$boom - hs]), sum(d$w[d$v < U$bust - hs])) }))
  set.seed(U$seed)
  sims <- sapply(proj, function(pj) { d <- dist1(pj); sample(d$v, U$n_sim, replace = TRUE, prob = d$w) })
  rk <- t(apply(sims + matrix(runif(length(sims), 0, 1e-6), nrow(sims)), 1, function(r) rank(-r)))
  tibble::tibble(q10 = q[, 1], q25 = q[, 2], q75 = q[, 3], q90 = q[, 4], p_boom = q[, 5], p_bust = q[, 6], p_top = colMeans(rk <= U$top_n))
}

# full re-score of one bundle for a (possibly updated) te: projections, components, ranges, bootstrap CI
score_bundle <- function(B, te) {
  comp <- comp_score(B$comp, te, B$SCORING)
  out <- dplyr::bind_cols(tibble::tibble(game_id = te$game_id, team = te$team),
                          comp[c("e_fga", "e_fgm", "e_xp", "e_a_50p", "p_u30", "p_30s", "p_40s", "p_50p")])
  for (sy in names(B$SCORING)) {
    out[[paste0("enet_", sy)]] <- lin_pred(B$enet[[sy]], te); out[[paste0("comp_", sy)]] <- comp[[paste0("fp_", sy)]]
    models <- if (!is.null(B$blend)) B$blend else c("enet", "components")
    out[[paste0("proj_", sy)]] <- rowMeans(cbind(if ("enet" %in% models) out[[paste0("enet_", sy)]], if ("components" %in% models) out[[paste0("comp_", sy)]]))
    oc <- outcome_cols(out[[paste0("proj_", sy)]], B$unc, sy); names(oc) <- paste0(names(oc), "_", sy)
    out <- dplyr::bind_cols(out, oc)
    bm <- sapply(B$boot, function(bb) blend_score(bb, te, sy, models = if (!is.null(B$blend)) B$blend else c("enet", "components")))
    out[[paste0("ci_lo_", sy)]] <- apply(bm, 1, quantile, .05); out[[paste0("ci_hi_", sy)]] <- apply(bm, 1, quantile, .95)
    out[[paste0("pm_", sy)]] <- (out[[paste0("ci_hi_", sy)]] - out[[paste0("ci_lo_", sy)]]) / 2
    out[[paste0("rank_", sy)]] <- rank(-out[[paste0("proj_", sy)]], ties.method = "first")
  }
  out
}
