## ---- sd_mix_utils.R: DK Showdown lineup builder with captain rules and a winners'-mix portfolio (Andrew 2026-11-12) ----
## One implementation, used by the back-test (91s_sd_captain.R), the live tab (97s_sd_refresh.R, "Winners' mix") and its
## Track record (98s_sd_track.R).
## Why: 93u (2026-11-12) pulled every $100+ lineup of 277 Showdown Play-Actions / Millionaires. Their captains are WR 31-41%,
## RB 27-31%, QB 18-20%, TE 7-10%, D/ST 5%, K 2-3%; our Highest projected build captained QBs 56% and WRs 8% (captain =
## 1.5 x mean projection, and QBs project highest). Andrew: a new model that "balance[s] captain spread and highest projections
## while also keeping our 20 lineups consistent with >$100 cash frequencies across both play-action and milly".
##
## sd_mix_build(d, ...) -> list of lineups, each list(cpt = row of d, flex = rows of d) (the 97s format)
##   d: one game's pool with proj, salary, cpt_salary, team, pos (players the build may use)
##   oc        captain values (default 1.5 x proj; sd_ceiling() for a ceiling captain)
##   cs_thr    Captain spread: every QB/RB/WR/TE projected >= cs_thr captains at least once (NA = off)
##   cpt_caps  most captains by position over the 20, e.g. c(QB = 4, K = 0)
##   prof      a portfolio of n_l lineup profiles (mix_profiles()): lineup j must match profile j (captain position, 2 QBs or
##             not, K / D/ST / TE in or out, captain's team heavy / even / light), relaxed step by step when nothing fits
##   cpt_rel   a captain must project at least cpt_rel x the best projection at his position in the game (e.g. 0.5): stops a
##             position quota from being filled with punt captains (NA = off)
##   md, floor_sal: >= md of 6 slots different from every earlier lineup; salary floor
## Every lineup is the highest-projected lineup that satisfies its rules (sequential ILPs, lpSolve, scale 0).

sd_both <- function(z) c(z, z) * 1
## constraint groups for one profile (rows over x = (flex_1..n, cpt_1..n))
sd_prof_rows <- function(d, p, cpt_only = FALSE) {
  n <- nrow(d); P <- d$pos; tA <- d$team == d$team[1]; z <- rep(0, n); g <- list()
  g$cpt <- list(list(a = c(z, (P == p$cpt_pos) * 1), dir = "=", rhs = 1))
  if (cpt_only) return(g)
  g$qb2 <- list(list(a = sd_both(P == "QB"), dir = if (isTRUE(p$qb2)) "=" else "<=", rhs = if (isTRUE(p$qb2)) 2 else 1))
  for (k in c("k", "dst", "te")) { pos <- c(k = "K", dst = "DST", te = "TE")[[k]]
    g[[k]] <- list(list(a = sd_both(P == pos), dir = if (isTRUE(p[[k]])) ">=" else "=", rhs = if (isTRUE(p[[k]])) 1 else 0)) }
  nA <- sd_both(tA); cA <- c(z, tA * 1)                                   # players from team A; captain on team A (0/1)
  g$side <- switch(as.character(p$side),
    heavy = list(list(a = nA - 4 * cA, dir = ">=", rhs = 0), list(a = nA - 4 * cA, dir = "<=", rhs = 2)),     # captain's team 4-5 of 6
    even  = list(list(a = nA, dir = "=", rhs = 3)),
    light = list(list(a = nA + 4 * cA, dir = "<=", rhs = 6), list(a = nA + 4 * cA, dir = ">=", rhs = 4)),     # captain's team 1-2 of 6
    NULL)
  g
}
SD_RELAX <- c("side", "te", "dst", "k", "qb2", "cpt")                     # order in which a profile is relaxed when nothing fits

sd_mix_build <- function(d, n_l = 20, md = 2, floor_sal = 0, oc = NULL, cs_thr = NA, cpt_caps = NULL, prof = NULL, cpt_rel = NA) {
  n <- nrow(d); pr <- d$proj; tA <- d$team == d$team[1]; P <- d$pos
  ocv <- if (is.null(oc)) 1.5 * pr else oc
  A <- rbind(c(rep(1, n), rep(0, n)), c(rep(0, n), rep(1, n)), cbind(diag(n), diag(n)), c(d$salary, d$cpt_salary), c(d$salary, d$cpt_salary), sd_both(tA), sd_both(!tA))
  dir <- c("=", "=", rep("<=", n), "<=", ">=", ">=", ">="); rhs <- c(5, 1, rep(1, n), 50000, floor_sal, 1, 1); obj <- c(pr, ocv)
  if (!is.na(cpt_rel)) { ok <- pr >= cpt_rel * ave(pr, P, FUN = max); if (any(!ok)) { A <- rbind(A, c(rep(0, n), (!ok) * 1)); dir <- c(dir, "="); rhs <- c(rhs, 0) } }
  out <- list(); ccnt <- c(QB = 0, RB = 0, WR = 0, TE = 0, K = 0, DST = 0); relaxed <- 0L
  solve <- function(groups = list(), cp = NA) {
    A2 <- A; d2 <- dir; r2 <- rhs
    add <- function(a, dd, rr) { A2 <<- rbind(A2, a); d2 <<- c(d2, dd); r2 <<- c(r2, rr) }
    if (length(cpt_caps)) for (ps in names(cpt_caps)) if (ccnt[[ps]] >= cpt_caps[[ps]]) add(c(rep(0, n), (P == ps) * 1), "=", 0)
    for (g in groups) for (r in g) add(r$a, r$dir, r$rhs)
    if (!is.na(cp)) { r <- rep(0, 2 * n); r[n + cp] <- 1; add(r, "=", 1) }
    s <- lpSolve::lp("max", obj, A2, d2, r2, all.bin = TRUE, scale = 0); if (s$status != 0) return(NULL)
    sel <- which(round(s$solution) == 1); if (length(sel) != 6) NULL else sel }
  take <- function(sel) { cp <- sel[sel > n] - n; out[[length(out) + 1]] <<- list(cpt = cp, flex = sel[sel <= n])
    if (P[cp] %in% names(ccnt)) ccnt[[P[cp]]] <<- ccnt[[P[cp]]] + 1
    cut <- rep(0, 2 * n); cut[sel] <- 1; A <<- rbind(A, cut); dir <<- c(dir, "<="); rhs <<- c(rhs, 6 - md) }
  elig <- if (is.na(cs_thr)) integer() else { e <- which(P %in% c("QB", "RB", "WR", "TE") & pr >= cs_thr); e[order(-pr[e])] }
  used_cpt <- function() vapply(out, `[[`, 0, "cpt")
  if (is.null(prof)) {
    for (cp in head(elig, n_l)) { if (cp %in% used_cpt()) next; if (length(cpt_caps) && P[cp] %in% names(cpt_caps) && ccnt[[P[cp]]] >= cpt_caps[[P[cp]]]) next
      sel <- solve(cp = cp); if (!is.null(sel)) take(sel) }
  } else {
    for (j in seq_len(min(nrow(prof), n_l))) { p <- prof[j, , drop = FALSE]; G <- sd_prof_rows(d, p, cpt_only = !all(c("qb2", "k", "dst", "te", "side") %in% names(prof)))
      cands <- setdiff(elig[P[elig] == p$cpt_pos], used_cpt())
      sel <- NULL
      for (lv in 0:length(SD_RELAX)) { keep <- G[setdiff(names(G), head(SD_RELAX, lv))]
        if (length(cands)) sel <- solve(keep, cands[1]); if (is.null(sel)) sel <- solve(keep); if (!is.null(sel)) { relaxed <- relaxed + (lv > 0); break } }
      if (!is.null(sel)) take(sel) }
  }
  while (length(out) < n_l) { sel <- solve(); if (is.null(sel)) break; take(sel) }
  attr(out, "relaxed") <- relaxed; out
}

## ceiling captain value: projection x the p-quantile of actual/projected for the player's group (stage 3's outcome model: QB / RB /
## WR / TE by projection tercile, K, DST; SF = fit_sim() output with $cuts and $Q on the grid (0:400)/400)
sd_ceiling <- function(d, SF, p = 0.85) {
  g <- d$pos; for (ps in c("QB", "RB", "WR", "TE")) { i <- d$pos == ps; if (any(i) && !is.null(SF$cuts[[ps]])) g[i] <- paste0(ps, findInterval(d$proj[i], SF$cuts[[ps]]) + 1) }
  k <- round(p * 400) + 1; q <- vapply(seq_len(nrow(d)), \(i) { x <- SF$Q[[g[i]]] %||% SF$Q[[d$pos[i]]]; if (is.null(x)) 1 else x[k] }, 0)
  1.5 * d$proj * q
}
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

## winners' mix: the weighted joint distribution of $100+ lineup profiles (93u lineups.csv), each contest weighted equally within its
## contest type and the types (Play-Action, Millionaire) weighted equally
mix_table <- function(W) {
  W <- W[W$cpt_pos %in% c("QB", "RB", "WR", "TE", "K", "DST"), ]
  W$w <- 1 / ave(rep(1, nrow(W)), W$contest_id, FUN = sum)                    # each contest sums to 1
  W$w <- W$w / ave(W$w, W$tier, FUN = sum) / length(unique(W$tier))         # each type sums to 1 / types
  W$qb2 <- W$n_QB >= 2; W$k <- W$n_K >= 1; W$dst <- W$n_DST >= 1; W$te <- W$n_TE >= 1
  W$side <- ifelse(W$cpt_team_n >= 4, "heavy", ifelse(W$cpt_team_n == 3, "even", "light"))
  T <- aggregate(w ~ cpt_pos + qb2 + k + dst + te + side, data = W, FUN = sum); T$w <- T$w / sum(T$w); T[order(-T$w), ]
}
## n profiles by systematic sampling down the table ordered captain position (most common first), then the other attributes:
## the n lineups reproduce the captain mix (to rounding) and, within each captain position, the mix of the rest
mix_profiles <- function(T, n = 20, cpt_only = FALSE) {
  if (cpt_only) { T <- aggregate(w ~ cpt_pos, data = T, FUN = sum) }
  pw <- tapply(T$w, T$cpt_pos, sum); T$po <- match(T$cpt_pos, names(sort(pw, decreasing = TRUE)))
  T <- if (cpt_only) T[order(T$po), ] else T[order(T$po, -T$qb2, -T$k, -T$dst, -T$te, T$side), ]
  cw <- cumsum(T$w) / sum(T$w); u <- (seq_len(n) - 0.5) / n
  T[findInterval(u, c(0, cw), left.open = TRUE), setdiff(names(T), c("w", "po")), drop = FALSE]
}
