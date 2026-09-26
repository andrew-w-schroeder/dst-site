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
  te_sim <- te; if (!is.null(B$sim) && isTRUE(B$sim$exact >= 0.99)) attr(te_sim, "k_sim_draws") <- k_sim_draws(B$sim, te)   # draw once, score both formats
  out <- dplyr::bind_cols(tibble::tibble(game_id = te$game_id, team = te$team),
                          comp[c("e_fga", "e_fgm", "e_xp", "e_a_50p", "p_u30", "p_30s", "p_40s", "p_50p")])
  for (sy in names(B$SCORING)) {
    out[[paste0("enet_", sy)]] <- lin_pred(B$enet[[sy]], te); out[[paste0("comp_", sy)]] <- comp[[paste0("fp_", sy)]]
    models <- if (!is.null(B$blend)) B$blend else c("enet", "components")
    out[[paste0("proj_", sy)]] <- rowMeans(cbind(if ("enet" %in% models) out[[paste0("enet_", sy)]], if ("components" %in% models) out[[paste0("comp_", sy)]]))
    oc <- outcome_cols(out[[paste0("proj_", sy)]], B$unc, sy)
    sp <- k_sim_probs(B$sim, te_sim, out[[paste0("proj_", sy)]], sy, B$unc)       # component simulation (bundles from 2026-09-25 on)
    if (!is.null(sp)) { oc$p_boom <- sp$p_boom; oc$p_ceiling <- sp$p_ceiling; oc$sim_sd <- sp$sim_sd }
    names(oc) <- paste0(names(oc), "_", sy)
    out <- dplyr::bind_cols(out, oc)
    bm <- sapply(B$boot, function(bb) blend_score(bb, te, sy, models = if (!is.null(B$blend)) B$blend else c("enet", "components")))
    out[[paste0("ci_lo_", sy)]] <- apply(bm, 1, quantile, .05); out[[paste0("ci_hi_", sy)]] <- apply(bm, 1, quantile, .95)
    out[[paste0("pm_", sy)]] <- (out[[paste0("ci_hi_", sy)]] - out[[paste0("ci_lo_", sy)]]) / 2
    out[[paste0("rank_", sy)]] <- rank(-out[[paste0("proj_", sy)]], ties.method = "first")
  }
  out
}

## ---- Component simulation: P(more than 10) and P(15+) per matchup (Andrew, 2026-09-25; tested in 71_component_sim_test.R) ----
# FG attempts per distance band (u30 / 30s / 40s / 50p) and XP attempts are counts; makes per band are binomial with this
# kicker's / conditions' make chance; every part is drawn JOINTLY from one past game (each past game's quantile per part,
# an empirical copula), so game script links them. Draws are scored per format (ESPN: band points, 60+ bonus; decimal:
# 0.1 × the band's average made distance + that spread) and re-centred on the production projection (unchanged).
# Back-test 2021–25: P(15+) clearly better than the kernel method (ESPN t 2.7, decimal t 2.7 vs a projection curve);
# P(>10) a wash (so a small log-odds offset keeps its average right: said 27.2% = happened, ESPN); P(<5) ran high,
# so P(bust) stays on the kernel method.
K_SIM_BANDS <- c("u30", "30s", "40s", "50p")
K_SIM_OFFSET <- list(espn = c(p_boom = 0.061, p_ceiling = -0.052), dec = c(p_boom = 0.098, p_ceiling = 0.021))   # 2021–25 back-test fit
.ksim_pit_pois <- function(y, mu) stats::ppois(y - 1, mu) + stats::runif(length(y)) * stats::dpois(y, mu)
.ksim_pit_bin  <- function(y, n, p) ifelse(n > 0, stats::pbinom(y - 1, n, p) + stats::runif(length(y)) * stats::dbinom(y, n, p), stats::runif(length(y)))
.ksim_lin  <- function(b, te) drop(cbind(1, as.matrix(te[setdiff(names(b), "(Intercept)")])) %*% b)
.ksim_fill <- function(te, med) { for (f in names(med)) { if (!f %in% names(te)) te[[f]] <- med[[f]]; te[[f]][is.na(te[[f]])] <- med[[f]] }; te }
.ksim_make_X <- function(te, mx) {             # (games × 4 bands) rows: band intercepts, shared kicker/conditions terms, band wind slopes
  do.call(rbind, lapply(seq_along(K_SIM_BANDS), function(k) { D <- matrix(0, nrow(te), 4); D[, k] <- 1
    cbind(D, as.matrix(te[mx]), D * te$wind_o) }))
}
# tr needs: the count / make variables, a_u30..a_50p (attempts), m_u30..m_50p (makes), m60 (60+ makes), k_xpa, k_xpm, fp_<format>
k_sim_fit <- function(tr, count_vars, make_vars, made_dist, seed = 1) {
  allx <- unique(c(unlist(count_vars), make_vars, "k_xp_pct", "wind_o", "cold", "indoor"))
  med <- vapply(allx, function(f) { m <- stats::median(tr[[f]], na.rm = TRUE); if (is.na(m)) 0 else m }, 0); tr <- .ksim_fill(tr, med)
  fitc <- function(y, x) { b <- stats::coef(stats::glm(stats::as.formula(paste(y, "~", paste(c("1", x), collapse = " + "))), data = tr, family = stats::quasipoisson()))
    b[is.na(b)] <- 0; b }
  Bc <- lapply(stats::setNames(K_SIM_BANDS, K_SIM_BANDS), function(bd) fitc(paste0("a_", bd), count_vars[[bd]])); Bx <- fitc("k_xpa", count_vars$xp)
  X <- .ksim_make_X(tr, make_vars); att <- unlist(lapply(K_SIM_BANDS, function(bd) tr[[paste0("a_", bd)]])); mad <- unlist(lapply(K_SIM_BANDS, function(bd) tr[[paste0("m_", bd)]]))
  ok <- att > 0; gm <- stats::glm.fit(X[ok, ], mad[ok] / att[ok], weights = att[ok], family = stats::binomial()); Bm <- gm$coefficients; Bm[is.na(Bm)] <- 0
  xpX <- cbind(1, as.matrix(tr[c("k_xp_pct", "wind_o", "cold", "indoor")])); okx <- tr$k_xpa > 0
  gx <- stats::glm.fit(xpX[okx, ], tr$k_xpm[okx] / tr$k_xpa[okx], weights = tr$k_xpa[okx], family = stats::binomial()); Bxp <- gx$coefficients; Bxp[is.na(Bxp)] <- 0
  set.seed(seed)
  pm <- matrix(stats::plogis(drop(X %*% Bm)), nrow(tr)); px <- stats::plogis(drop(xpX %*% Bxp))
  pool <- as.data.frame(c(
    lapply(stats::setNames(K_SIM_BANDS, paste0("ua_", K_SIM_BANDS)), function(bd) .ksim_pit_pois(tr[[paste0("a_", bd)]], exp(.ksim_lin(Bc[[bd]], tr)))),
    lapply(stats::setNames(seq_along(K_SIM_BANDS), paste0("um_", K_SIM_BANDS)), function(k) .ksim_pit_bin(tr[[paste0("m_", K_SIM_BANDS[k])]], tr[[paste0("a_", K_SIM_BANDS[k])]], pm[, k])),
    list(ux = .ksim_pit_pois(tr$k_xpa, exp(.ksim_lin(Bx, tr))), uxm = .ksim_pit_bin(tr$k_xpm, tr$k_xpa, px))), check.names = FALSE)
  rebuilt <- 3 * (tr$m_u30 + tr$m_30s) + 4 * tr$m_40s + 5 * tr$m_50p + tr$m60 - (tr$a_u30 + tr$a_30s + tr$a_40s + tr$a_50p - tr$m_u30 - tr$m_30s - tr$m_40s - tr$m_50p) + tr$k_xpm
  list(Bc = Bc, Bx = Bx, Bm = Bm, Bxp = Bxp, make_vars = make_vars, med = med, pool = pool, share60 = sum(tr$m60) / max(1, sum(tr$m_50p)),
       dist = made_dist, exact = mean(abs(rebuilt - tr$fp_espn) < 1e-9), n = nrow(tr), seed = seed, offset = K_SIM_OFFSET)
}
k_sim_draws <- function(S, te) {                 # list of games × past games matrices per format (not re-centred)
  te <- .ksim_fill(te, S$med); nt <- nrow(te); np <- nrow(S$pool); set.seed(S$seed)
  RU <- function(u) matrix(u, nt, np, byrow = TRUE); RM <- function(m) matrix(m, nt, np); cl <- function(u) pmin(pmax(u, 1e-12), 1 - 1e-12)
  pm <- matrix(stats::plogis(drop(.ksim_make_X(te, S$make_vars) %*% S$Bm)), nt); px <- stats::plogis(drop(cbind(1, as.matrix(te[c("k_xp_pct", "wind_o", "cold", "indoor")])) %*% S$Bxp))
  A <- lapply(stats::setNames(K_SIM_BANDS, K_SIM_BANDS), function(bd) matrix(stats::qpois(cl(RU(S$pool[[paste0("ua_", bd)]])), RM(exp(.ksim_lin(S$Bc[[bd]], te)))), nt, np))
  Mk <- lapply(stats::setNames(seq_along(K_SIM_BANDS), K_SIM_BANDS), function(k) matrix(stats::qbinom(cl(RU(S$pool[[paste0("um_", K_SIM_BANDS[k])]])), A[[k]], RM(pm[, k])), nt, np))
  XA <- matrix(stats::qpois(cl(RU(S$pool$ux)), RM(exp(.ksim_lin(S$Bx, te)))), nt, np); XM <- matrix(stats::qbinom(cl(RU(S$pool$uxm)), XA, RM(px)), nt, np)
  M60 <- matrix(stats::rbinom(length(Mk$`50p`), Mk$`50p`, S$share60), nt, np)
  miss <- Reduce(`+`, A) - Reduce(`+`, Mk)
  dm <- S$dist$mean; dv <- S$dist$var
  fg_dec <- Reduce(`+`, lapply(K_SIM_BANDS, function(bd) 0.1 * dm[[bd]] * Mk[[bd]]))
  sd_dec <- sqrt(Reduce(`+`, lapply(K_SIM_BANDS, function(bd) 0.01 * dv[[bd]] * Mk[[bd]])))
  list(espn = 3 * (Mk$u30 + Mk$`30s`) + 4 * Mk$`40s` + 5 * Mk$`50p` + M60 - miss + XM,
       dec  = fg_dec + matrix(stats::rnorm(nt * np), nt, np) * sd_dec - miss + XM)
}
# per format: P(more than BOOM) and P(15+) (continuity-corrected cuts), NULL when unavailable
k_sim_probs <- function(S, te, proj, sy, U) {
  if (is.null(S) || !isTRUE(S$exact >= 0.99)) return(NULL)
  D <- if (!is.null(attr(te, "k_sim_draws"))) attr(te, "k_sim_draws") else k_sim_draws(S, te)
  step <- U$step[[sy]]; boom <- if (is.list(U$boom)) U$boom[[sy]] else U$boom
  cuts <- list(p_boom = list(cut = boom - step / 2, above = TRUE), p_ceiling = list(cut = 15 - step / 2, above = TRUE))
  set.seed(S$seed); Mj <- D[[sy]] + matrix(stats::runif(length(D[[sy]]), -step / 2, step / 2), nrow(D[[sy]])); Mj <- Mj - rowMeans(Mj) + proj
  off <- S$offset[[sy]]
  out <- lapply(stats::setNames(names(cuts), names(cuts)), function(k) { p <- rowMeans(Mj >= cuts[[k]]$cut)
    if (!is.null(off) && !is.na(off[k])) p <- stats::plogis(stats::qlogis(pmin(pmax(p, 1e-4), 1 - 1e-4)) + off[[k]]); p })
  out$sim_sd <- apply(D[[sy]], 1, stats::sd); out$sim_check <- max(abs(rowMeans(Mj) - proj))
  tibble::as_tibble(out)
}
