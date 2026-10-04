## ---- dfs_opt.R: DraftKings lineup simulator and optimizer (cash and large-field tournaments) ----
## Andrew 2026-10-04: 10 lineups for cash games (max P(cash)) and 10 for one large tournament (max P(at least one of the 10
## finishes in the top 1%)), downloadable as a DK upload CSV. Used by 96_dfs_refresh.R; needs dfs_utils.R and lpSolve.
##
## 1. Outcomes: every slate player's DK points simulated jointly, S times. Each player's own distribution is the outcome
##    model at his projection (94's quantile grid; D/ST: the Yahoo bundle's back-test residuals); players in the same game
##    are tied together by a Gaussian copula with 94's role-pair correlations for that game's environment (QB + his
##    receivers, RB + his D/ST, D/ST vs the opposing QB, RB + opposing WR in blowouts, opposing WRs in shootouts, ...).
## 2. The field: F lineups drawn from projected ownership under the $50K cap (60% stack their QB with a teammate). In every
##    simulation the cash line is the field's 55th percentile (double-ups pay the top ~45%) and the top-1% line its 99th.
## 3. Candidates: lineups from an integer program (lpSolve) that maximises points of one simulated week, or the
##    projection with noise and an ownership discount, under different stack rules (none; QB + 1 or 2 pass catchers;
##    + a bring-back; RB + his D/ST). Each candidate's P(cash) and P(top 1%) = its share of simulations above the line.
## 4. Portfolios: cash = the 10 best P(cash) that each differ by at least 3 players; tournament = greedy, each lineup
##    adding the most simulations in which at least one of the 10 reaches the top 1% (the sims already covered count
##    for nothing), differing by 3+ players and no player in more than 6 of the 10.
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(purrr) })

OPT <- list(S = 4000, F = 4000, cash_q = 0.55, top_q = 0.99, n_gpp_cand = 900, n_cash_cand = 300, n_lineups = 10,
            min_diff = 3, max_expo_gpp = 6, field_stack = 0.60, cap = 50000, field_min_sal = 48500, seed = 7)

## ---- quantile functions ----
## player rows: matrix of quantiles on a fine u grid, from 94's quantile grid (skill) or the D/ST residual kernel
OPT_U <- c(0.001, seq(0.02, 0.98, by = 0.02), 0.999)
q_skill <- function(v, pos, qg) {
  taus <- sort(unique(qg$tau)); out <- matrix(NA_real_, length(v), length(OPT_U))
  for (p in unique(pos)) { i <- which(pos == p); r <- qg[qg$pos == p, ]
    b0 <- r$coef[r$term == "(Intercept)"][match(taus, r$tau[r$term == "(Intercept)"])]
    b1 <- r$coef[r$term == "v"][match(taus, r$tau[r$term == "v"])]
    Q <- outer(v[i], b1) + matrix(b0, length(i), length(taus), byrow = TRUE); Q <- t(apply(Q, 1, sort)); Q <- matrix(Q, nrow = length(i))
    j90 <- which.min(abs(taus - 0.90)); n <- ncol(Q)
    s <- pmax((Q[, n] - Q[, j90]) / log((1 - taus[j90]) / (1 - taus[n])), 0.5)
    lo <- pmax(-2, Q[, 1] - (Q[, 3] - Q[, 1]))
    hi <- Q[, n] + s * log((1 - taus[n]) / 0.001)
    out[i, ] <- cbind(lo, Q, hi) }
  out
}
q_dst <- function(proj, unc) {
  wq <- function(x, w, p) { o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w); vapply(p, \(pp) x[which(cw >= pp)[1]], 0) }
  t(vapply(proj, \(p) { w <- dnorm((unc$proj - p) / unc$bw); wq(p + unc$resid, w, OPT_U) }, numeric(length(OPT_U))))
}
## u (n x S) -> points through each row's quantile function (linear between grid points)
u_to_pts <- function(Q, U) {
  out <- matrix(0, nrow(U), ncol(U)); k <- findInterval(U, OPT_U, all.inside = TRUE)
  K <- matrix(k, nrow(U)); w <- (U - OPT_U[K]) / (OPT_U[K + 1] - OPT_U[K])
  for (i in seq_len(nrow(U))) { kk <- K[i, ]; out[i, ] <- Q[i, kk] + w[i, ] * (Q[i, kk + 1] - Q[i, kk]) }
  out
}

## ---- 1. Correlated outcomes ----
## P: players (id, pos, team, opp, game_id, proj, role, env, fav, q = quantile rows). cor: 94's table
near_pd <- function(C) { e <- eigen(C, symmetric = TRUE); C2 <- e$vectors %*% diag(pmax(e$values, 1e-4)) %*% t(e$vectors)
  d <- sqrt(diag(C2)); C2 / outer(d, d) }
sim_outcomes <- function(P, Q, cor, S, seed) {
  set.seed(seed); Z <- matrix(0, nrow(P), S)
  for (g in unique(P$game_id)) {
    i <- which(P$game_id == g); n <- length(i)
    C <- diag(n)
    if (!is.null(cor) && n > 1) for (a in 1:(n - 1)) for (b in (a + 1):n) {
      ia <- i[a]; ib <- i[b]; same <- P$team[ia] == P$team[ib]
      r <- dfs_pair_r(cor, same, P$role[ia], P$role[ib], P$env[ia], if (!same && P$env[ia] == "blow") P$fav[ia] else NA)
      C[a, b] <- C[b, a] <- r }
    L <- chol(near_pd(C))
    Z[i, ] <- t(L) %*% matrix(rnorm(n * S), n, S)
  }
  u_to_pts(Q, pnorm(Z))
}

## ---- 2. The simulated field ----
## Entrants' lineups are sampled from P(lineup) proportional to the product of per-player weights over all valid lineups
## (9 slots, $48.5K-$50K: real entrants leave little salary unused), by Gibbs sampling: swap one slot at a time for a
## player drawn from those that keep the lineup valid, 25 swaps between recorded lineups. Receivers on the lineup's QB's
## team get a stacking boost. The weights are tuned (iterative proportional fitting, 5 rounds) until the field's exposure
## to every player matches our projected ownership.
FIELD_SLOTS <- list("QB", "RB", "RB", "WR", "WR", "WR", "TE", c("RB", "WR", "TE"), "DST")
sim_field <- function(P, F, opt = OPT, seed = 11, say = message) {
  set.seed(seed); n <- nrow(P); sal <- P$salary
  pools <- lapply(FIELD_SLOTS, \(ps) which(P$pos %in% ps))
  target <- pmin(pmax(coalesce(P$own, 0), 0.001), 0.95); wt <- target
  ## start: the projection-maximising lineup (valid by construction), in slot order
  l <- dk_slots(lp_solve_lineup(P$dk_proj, lp_base(P, opt$cap), list(A = list(), dir = c(), rhs = c())), P)
  rec <- P$pos %in% c("WR", "TE"); boost <- 1 + opt$field_stack * 6
  chain <- function(m, thin = 25) {
    out <- matrix(NA_integer_, m, 9)
    for (e in seq_len(m)) {
      for (t in seq_len(thin)) {
        j <- sample.int(9, 1); cand <- pools[[j]]; cand <- cand[!cand %in% l[-j]]
        so <- sum(sal[l[-j]]); ok <- cand[so + sal[cand] <= opt$cap & so + sal[cand] >= opt$field_min_sal]
        if (!length(ok)) next
        w <- wt[ok] * ifelse(rec[ok] & P$team[ok] == P$team[l[1]], boost, 1)
        l[j] <<- if (length(ok) == 1) ok else sample(ok, 1, prob = w) }
      out[e, ] <- l }
    out }
  for (round in 1:5) { L <- chain(600); ex <- pmax(tabulate(L, n) / nrow(L), 0.25 / nrow(L))
    wt <- wt * (target / ex)^0.8; wt <- wt / mean(wt) }
  L <- chain(F); ex <- tabulate(L, n) / nrow(L)
  stk <- mean(vapply(seq_len(nrow(L)), \(k) any(rec[L[k, ]] & P$team[L[k, ]] == P$team[L[k, 1]]), TRUE))
  say(sprintf("opt: field exposure vs projected ownership: correlation %.3f, mean |gap| %.1f points among players owned 5%%+; %.0f%% QB stacks; mean salary $%s",
              cor(ex, target), 100 * mean(abs(ex - target)[target >= 0.05]), 100 * stk, format(round(mean(rowSums(matrix(sal[L], nrow(L))))), big.mark = ",")))
  L
}
## lineup matrix (k x 9 player indices) -> scores in every simulation (k x S), in chunks
lineup_scores <- function(L, X) { k <- nrow(L); out <- matrix(0, k, ncol(X))
  for (j in 1:9) out <- out + X[L[, j], , drop = FALSE]
  out }

## ---- 3. Candidates (integer program) ----
STACKS <- list(none = list(k = 0, bb = FALSE, rbd = FALSE), qb1 = list(k = 1, bb = FALSE, rbd = FALSE),
               qb2 = list(k = 2, bb = FALSE, rbd = FALSE), qb1_bb = list(k = 1, bb = TRUE, rbd = FALSE),
               qb2_bb = list(k = 2, bb = TRUE, rbd = FALSE), qb1_rbd = list(k = 1, bb = FALSE, rbd = TRUE),
               qb2_bb_rbd = list(k = 2, bb = TRUE, rbd = TRUE))
lp_base <- function(P, cap) {
  n <- nrow(P); A <- list(); dir <- c(); rhs <- c()
  add <- function(a, d, r) { A[[length(A) + 1]] <<- a; dir <<- c(dir, d); rhs <<- c(rhs, r) }
  add(P$salary, "<=", cap)
  for (p in c("QB", "DST")) add(as.numeric(P$pos == p), "=", 1)
  add(as.numeric(P$pos == "RB"), ">=", 2); add(as.numeric(P$pos == "RB"), "<=", 3)
  add(as.numeric(P$pos == "WR"), ">=", 3); add(as.numeric(P$pos == "WR"), "<=", 4)
  add(as.numeric(P$pos == "TE"), ">=", 1); add(as.numeric(P$pos == "TE"), "<=", 2)
  add(as.numeric(P$pos %in% c("RB", "WR", "TE")), "=", 7)
  list(A = A, dir = dir, rhs = rhs)
}
lp_stack <- function(P, st) {
  A <- list(); dir <- c(); rhs <- c(); n <- nrow(P)
  add <- function(a, d, r) { A[[length(A) + 1]] <<- a; dir <<- c(dir, d); rhs <<- c(rhs, r) }
  for (q in which(P$pos == "QB")) {
    if (st$k > 0) { a <- as.numeric(P$team == P$team[q] & P$pos %in% c("WR", "TE")); a[q] <- -st$k; add(a, ">=", 0) }
    if (st$bb) { a <- as.numeric(P$team == P$opp[q] & P$pos %in% c("WR", "TE", "RB")); a[q] <- -1; add(a, ">=", 0) }
  }
  if (st$rbd) for (dd in which(P$pos == "DST")) { a <- as.numeric(P$team == P$team[dd] & P$pos == "RB"); a[dd] <- -1; add(a, ">=", 0) }
  list(A = A, dir = dir, rhs = rhs)
}
lp_solve_lineup <- function(w, base, stk) {
  A <- do.call(rbind, c(base$A, stk$A)); r <- lpSolve::lp("max", w, A, c(base$dir, stk$dir), c(base$rhs, stk$rhs), all.bin = TRUE)
  if (r$status != 0) return(NULL)
  which(r$solution > 0.5)
}
## order a lineup's 9 players into DK's slots: QB, RB, RB, WR, WR, WR, TE, FLEX, DST
dk_slots <- function(l, P) {
  qb <- l[P$pos[l] == "QB"]; dst <- l[P$pos[l] == "DST"]
  rb <- l[P$pos[l] == "RB"]; wr <- l[P$pos[l] == "WR"]; te <- l[P$pos[l] == "TE"]
  ## FLEX = the extra player at the latest-kickoff slot (late-swap friendly)
  fl_c <- c(if (length(rb) == 3) rb, if (length(wr) == 4) wr, if (length(te) == 2) te)
  fl <- fl_c[which.max(as.numeric(P$ko[fl_c]))]
  rb <- setdiff(rb, fl); wr <- setdiff(wr, fl); te <- setdiff(te, fl)
  c(qb, rb[1:2], wr[1:3], te[1], fl, dst)
}
stack_label <- function(l, P) {
  qb <- l[P$pos[l] == "QB"]; mates <- sum(P$team[l] == P$team[qb] & P$pos[l] %in% c("WR", "TE"))
  bb <- any(P$team[l] == P$opp[qb] & P$pos[l] %in% c("WR", "TE", "RB")); dst <- l[P$pos[l] == "DST"]
  rbd <- any(P$team[l] == P$team[dst] & P$pos[l] == "RB"); dvq <- P$opp[dst] == P$team[qb]
  paste0(if (mates == 0) "no QB stack" else sprintf("QB + %d", mates), if (bb) " + bring-back" else "", if (rbd) ", RB + his D/ST" else "",
         if (dvq) ", D/ST vs own QB" else "")
}

## ---- 4. Portfolios ----
pick_cash <- function(pc, Lc, n, min_diff) { o <- order(-pc); keep <- integer()
  for (k in o) { if (length(keep) >= n) break
    if (all(vapply(keep, \(j) 9 - length(intersect(Lc[k, ], Lc[j, ])) >= min_diff, TRUE))) keep <- c(keep, k) }
  keep }
pick_gpp <- function(H, Lc, n, min_diff, max_expo) { keep <- integer(); covered <- rep(FALSE, ncol(H)); expo <- integer()
  for (it in 1:n) {
    ok <- vapply(seq_len(nrow(H)), \(k) { if (k %in% keep) return(FALSE)
      if (length(keep) && !all(vapply(keep, \(j) 9 - length(intersect(Lc[k, ], Lc[j, ])) >= min_diff, TRUE))) return(FALSE)
      if (length(expo) && any(Lc[k, ] %in% as.integer(names(expo)[expo >= max_expo]))) return(FALSE); TRUE }, TRUE)
    if (!any(ok)) break
    gain <- rowSums(H[, !covered, drop = FALSE]); gain[!ok] <- -1
    k <- which.max(gain); keep <- c(keep, k); covered <- covered | H[k, ]
    tb <- table(c(as.integer(unlist(lapply(keep, \(j) Lc[j, ])))))
    expo <- setNames(as.integer(tb), names(tb)) }
  keep }

## ---- main ----
## rows: 96's slate table (pos QB/RB/WR/TE/DST, salary, dk_proj, own, team, opp, game_id, ko, dk_id, implied, spread ...)
dfs_optimize <- function(rows, S_spec, dst_unc, env_tab, opt = OPT, say = message) {
  t0 <- Sys.time()
  P <- rows |> filter(pos %in% c("QB", "RB", "WR", "TE", "DST"), !is.na(salary), !is.na(dk_proj), dk_proj > 0,
                      !(inj_out %in% TRUE), !(dk_status %in% c("O", "OUT", "IR", "Out"))) |>
    mutate(own = coalesce(own, 0)) |> arrange(game_id, team, pos, desc(dk_proj))
  P$role <- dfs_roles(P$game_id, P$team, P$pos, P$dk_proj)
  P <- P |> left_join(env_tab, by = c("game_id", "team"))
  P$env <- coalesce(P$env, "norm"); P$fav <- coalesce(P$fav, FALSE)
  ## quantile rows
  Q <- matrix(NA_real_, nrow(P), length(OPT_U)); sk <- P$pos != "DST"
  Q[sk, ] <- q_skill(P$dk_proj[sk], P$pos[sk], S_spec$qgrid)
  if (any(!sk)) Q[!sk, ] <- q_dst(P$dk_proj[!sk], dst_unc)
  X <- sim_outcomes(P, Q, S_spec$cor, opt$S, opt$seed)
  say(sprintf("opt: %d players, %d simulations (%.0f s)", nrow(P), opt$S, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  ## field and lines
  FL <- sim_field(P, opt$F, opt, say = say); FS <- lineup_scores(FL, X)
  cash_line <- apply(FS, 2, quantile, opt$cash_q, names = FALSE); top_line <- apply(FS, 2, quantile, opt$top_q, names = FALSE)
  field_own <- tabulate(FL, nrow(P)) / nrow(FL)
  say(sprintf("opt: field of %d lineups; cash line median %.1f, top-1%% line median %.1f (%.0f s)", nrow(FL), median(cash_line), median(top_line),
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  ## candidates
  base <- lp_base(P, opt$cap); cands <- list(); typ <- c()
  set.seed(opt$seed + 1)
  for (it in seq_len(opt$n_gpp_cand)) {
    st_n <- names(STACKS)[(it - 1) %% length(STACKS) + 1]
    w <- if (it %% 2 == 1) X[, sample.int(opt$S, 1)] else P$dk_proj * exp(rnorm(nrow(P), 0, 0.25)) - 8 * P$own * P$dk_proj / 10
    l <- lp_solve_lineup(w, base, lp_stack(P, STACKS[[st_n]])); if (!is.null(l)) { cands[[length(cands) + 1]] <- l; typ <- c(typ, paste0("gpp_", st_n)) } }
  for (it in seq_len(opt$n_cash_cand)) {
    st_n <- c("none", "qb1")[(it - 1) %% 2 + 1]
    w <- P$dk_proj + rnorm(nrow(P), 0, 0.8) - (Q[, which.min(abs(OPT_U - 0.9))] - Q[, which.min(abs(OPT_U - 0.25))]) * runif(1, 0, 0.15)
    l <- lp_solve_lineup(w, base, lp_stack(P, STACKS[[st_n]])); if (!is.null(l)) { cands[[length(cands) + 1]] <- l; typ <- c(typ, paste0("cash_", st_n)) } }
  Lc <- t(vapply(cands, \(l) sort(l), integer(9)))
  ## at least two different games (DK rule); drop duplicates
  ok <- apply(Lc, 1, \(l) length(unique(P$game_id[l])) >= 2) & !duplicated(Lc)
  Lc <- Lc[ok, , drop = FALSE]; typ <- typ[ok]
  SC <- lineup_scores(Lc, X)
  Hc <- sweep(SC, 2, cash_line, ">="); Ht <- sweep(SC, 2, top_line, ">=")
  p_cash <- rowMeans(Hc); p_top <- rowMeans(Ht)
  say(sprintf("opt: %d distinct candidates (%.0f s)", nrow(Lc), as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  kc <- pick_cash(p_cash, Lc, opt$n_lineups, opt$min_diff)
  kg <- pick_gpp(Ht, Lc, opt$n_lineups, opt$min_diff, opt$max_expo_gpp)
  lab <- vapply(seq_len(nrow(Lc)), \(k) stack_label(Lc[k, ], P), "")
  mk <- function(keep, kind) {
    if (!length(keep)) return(tibble())
    bind_rows(lapply(seq_along(keep), \(r) { k <- keep[r]; sl <- dk_slots(Lc[k, ], P)
      tibble(kind = kind, lineup = r, slot = c("QB", "RB", "RB", "WR", "WR", "WR", "TE", "FLEX", "DST"), idx = sl,
             p_cash = p_cash[k], p_top = p_top[k], proj = sum(P$dk_proj[sl]), salary = sum(P$salary[sl]),
             own_sum = sum(P$own[sl]), mean_score = mean(SC[k, ]), q90 = quantile(SC[k, ], 0.9, names = FALSE), stack = lab[k]) })) |>
      mutate(name = P$player_name[idx], pos = P$pos[idx], team = P$team[idx], opp = P$opp[idx], player_salary = P$salary[idx],
             player_proj = P$dk_proj[idx], player_own = P$own[idx], dk_id = P$dk_id[idx], ko = P$ko[idx])
  }
  ## portfolio-level odds
  any_top <- if (length(kg)) mean(colSums(Ht[kg, , drop = FALSE]) > 0) else NA
  n_cash <- if (length(kc)) mean(colSums(Hc[kc, , drop = FALSE])) else NA
  ## stack exploration: every candidate's P(top 1%) by the stack it ended up with
  stx <- tibble(stack = lab, p_top = p_top, p_cash = p_cash, src = typ) |> group_by(stack) |>
    summarise(n = n(), best_top = max(p_top), mean_top10 = mean(sort(p_top, decreasing = TRUE)[1:min(10, n())]), best_cash = max(p_cash), .groups = "drop") |>
    arrange(desc(mean_top10))
  say(sprintf("opt: done in %.0f s; tournament P(any of 10 in top 1%%) %.1f%%, cash expected cashes %.1f of 10",
              as.numeric(difftime(Sys.time(), t0, units = "secs")), 100 * any_top, n_cash))
  list(cash = mk(kc, "cash"), gpp = mk(kg, "gpp"), any_top = any_top, n_cash = n_cash, stacks = stx,
       lines = tibble(cash_med = median(cash_line), top_med = median(top_line), cash_q = opt$cash_q, top_q = opt$top_q),
       field_own = tibble(id = P$gsis_id, name = P$player_name, pos = P$pos, own = P$own, field = field_own),
       n_cand = nrow(Lc), S = opt$S, F = nrow(FL), secs = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}
## DK upload file: one row per lineup, columns QB,RB,RB,WR,WR,WR,TE,FLEX,DST holding DK player IDs
dk_upload_csv <- function(L, file) {
  if (!nrow(L)) return(invisible(NULL))
  w <- L |> select(lineup, slot, dk_id) |> group_by(lineup) |> summarise(v = list(dk_id), .groups = "drop")
  m <- do.call(rbind, lapply(w$v, \(x) as.character(x)))
  writeLines(c("QB,RB,RB,WR,WR,WR,TE,FLEX,DST", apply(m, 1, paste, collapse = ",")), file)
}
