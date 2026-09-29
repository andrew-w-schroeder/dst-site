## ---- player_site_utils.R: Vegas-only player projections from props, portable ----
## The conversion of 83_vegas_baseline.R (props -> expected stats -> fantasy points) as plain
## functions of coefficients stored in the weekly bundle (63_player_site_bundle.R), so the
## website refresh (65_player_refresh.R, GitHub Actions) needs only dplyr / tidyr / purrr.
## Checked against 83's fitted models to rounding (63 stops if they differ).

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(purrr) })

PS_YARDS  <- c(player_pass_yds = "pass_yds", player_rush_yds = "rush_yds", player_reception_yds = "rec_yds")
PS_COUNTS <- c(player_pass_tds = "pass_td", player_pass_interceptions = "pass_int",
               player_pass_attempts = "pass_att", player_receptions = "rec", player_rush_attempts = "rush_att")
PS_PROP_STATS <- c(PS_YARDS, PS_COUNTS, tds = "tds")
PS_STATS <- c("pass_att", "pass_yds", "pass_td", "pass_int", "rush_att", "rush_yds", "rec", "rec_yds",
              "tds", "fum_lost", "two_pt")
PS_RARE  <- c("fum_lost", "two_pt")
PS_POS   <- c("QB", "RB", "WR", "TE")
PS_FORMATS <- c(std = "Standard", half = "Half PPR", ppr = "PPR", ffpc = "FFPC")

## scoring (same as player_utils.R): ESPN defaults for std / half / ppr; FFPC 0.05 / pass yd,
## INT -1, fumble lost -1, 1 per catch (1.5 for TEs)
ps_score <- function(d, fmt) {
  z <- function(v) if (is.null(d[[v]])) 0 else coalesce(d[[v]], 0)
  py  <- if (fmt == "ffpc") 0.05 else 0.04
  neg <- if (fmt == "ffpc") -1 else -2
  rec <- switch(fmt, std = 0, half = 0.5, ppr = 1, ffpc = ifelse(d$pos == "TE", 1.5, 1))
  py * z("pass_yds") + 4 * z("pass_td") + neg * z("pass_int") +
    0.1 * (z("rush_yds") + z("rec_yds")) + 6 * (z("rush_td") + z("rec_td") + z("st_td")) +
    2 * z("two_pt") + neg * z("fum_lost") + rec * z("rec")
}

## Poisson mean with P(X > L) / (P(X > L) + P(X < L)) = p  (as 83)
.ps_pois_cache <- new.env(hash = TRUE)
ps_pois_invert <- function(L, p) {                 # memoized: the bootstrap (ps_ci) repeats the same (line, P) many times
  p <- pmin(pmax(p, 0.02), 0.98)
  k <- paste(L, format(p, digits = 15)); out <- rep(NA_real_, length(k))
  hit <- vapply(k, \(z) exists(z, envir = .ps_pois_cache, inherits = FALSE), TRUE)
  if (any(hit)) out[hit] <- vapply(k[hit], \(z) get(z, envir = .ps_pois_cache), 0)
  if (any(!hit)) { v <- ps_pois_invert0(L[!hit], p[!hit]); out[!hit] <- v
    for (j in which(!hit)) assign(k[j], out[j], envir = .ps_pois_cache) }
  out
}
ps_pois_invert0 <- function(L, p) {
  mapply(\(L, p) {
    f <- function(lam) {
      over <- ppois(floor(L), lam, lower.tail = FALSE)
      under <- ppois(ceiling(L) - 1, lam)
      over / (over + under) - p
    }
    tryCatch(uniroot(f, c(1e-4, 200))$root, error = function(e) NA_real_)
  }, L, p)
}

## one fitted calibration -> portable form: terms (environment dropped) + coefficients
ps_portable <- function(m) {
  tt <- delete.response(terms(m)); attr(tt, ".Environment") <- globalenv()
  list(terms = tt, coef = coef(m), family = if (inherits(m, "glm")) m$family$family else "gaussian")
}
ps_predict <- function(pm, nd) {
  mf <- model.frame(pm$terms, nd, na.action = na.pass)
  X <- model.matrix(pm$terms, mf)
  eta <- drop(X[, names(pm$coef), drop = FALSE] %*% pm$coef)
  if (pm$family == "poisson") exp(eta) else eta
}

## wide rows (one per game x player): x.* / z.* inputs from the consensus lines
## cons: game_id, gsis_id, market, line, med_est, p_over, n_books ; anyt: game_id, gsis_id, p_td_raw, n_books
ps_inputs <- function(cons, anyt) {
  ou <- cons |> filter(market %in% c(names(PS_YARDS), names(PS_COUNTS))) |>
    group_by(game_id, gsis_id, market) |> slice_max(n_books, n = 1, with_ties = FALSE) |> ungroup() |>
    transmute(game_id, gsis_id, stat = c(PS_YARDS, PS_COUNTS)[market], line, med_est, p_over, n_books)
  w <- if (nrow(ou)) ou |> pivot_wider(names_from = stat, values_from = c(line, med_est, p_over, n_books), names_sep = ".") else
    tibble(game_id = character(), gsis_id = character())
  td <- anyt |> filter(!is.na(p_td_raw)) |> group_by(game_id, gsis_id) |>
    slice_max(n_books, n = 1, with_ties = FALSE) |> ungroup() |> transmute(game_id, gsis_id, p_td_raw, n_books.tds = n_books)
  d <- full_join(w, td, by = c("game_id", "gsis_id"))
  for (s in PS_PROP_STATS) for (p in c("line.", "med_est.", "p_over.", "n_books."))
    if (!paste0(p, s) %in% names(d)) d[[paste0(p, s)]] <- NA_real_
  if (!"p_td_raw" %in% names(d)) d$p_td_raw <- NA_real_
  for (s in PS_COUNTS) {
    L <- d[[paste0("line.", s)]]; P <- d[[paste0("p_over.", s)]]
    d[[paste0("x.", s)]] <- ifelse(is.na(L) | is.na(P), NA_real_, if (nrow(d)) ps_pois_invert(L, P) else numeric())
  }
  for (s in PS_YARDS) {
    d[[paste0("x.", s)]] <- d[[paste0("med_est.", s)]]
    d[[paste0("z.", s)]] <- qlogis(pmin(pmax(d[[paste0("p_over.", s)]], 0.02), 0.98))
  }
  d$x.tds <- -log(1 - pmin(d$p_td_raw, 0.97))
  d
}

## expected stats (83's expect_stats with portable calibrations). d needs pos, core, x.* / z.*,
## S_* (recency-weighted career sums) and S_n; B = the bundle
ps_expect <- function(d, B, conv = B$conv) {
  st <- B$settings; out <- d |> select(game_id, gsis_id)
  has <- function(s) if (s %in% PS_PROP_STATS) !is.na(d[[paste0("x.", s)]]) else rep(FALSE, nrow(d))
  for (s in PS_PROP_STATS) {
    h <- has(s); pm <- conv[[s]]
    nd <- tibble(x = if (s == "tds") pmax(d$x.tds, 1e-4) else d[[paste0("x.", s)]],
                 z = if (s %in% PS_YARDS) d[[paste0("z.", s)]] else 0)
    v <- if (nrow(d)) ps_predict(pm, nd) else numeric()
    if (s != "tds") v <- pmax(v, 0)
    out[[paste0("e.", s)]] <- ifelse(h, v, NA_real_)
    out[[paste0("src.", s)]] <- ifelse(h, "prop", NA_character_)
  }
  fp <- B$fill
  ypr <- (d$S_rec_yds + st$K_YPR * fp$ypr$pos_ypr[match(d$pos, fp$ypr$pos)]) / (d$S_rec + st$K_YPR)
  i <- is.na(out$e.rec) & !is.na(out$e.rec_yds)
  out$e.rec[i] <- out$e.rec_yds[i] / ypr[i]; out$src.rec[i] <- "rec yds prop / yds per catch"
  i <- is.na(out$e.rec_yds) & !is.na(out$e.rec)
  out$e.rec_yds[i] <- out$e.rec[i] * ypr[i]; out$src.rec_yds[i] <- "rec prop x yds per catch"
  for (s in PS_STATS) {
    col <- paste0("e.", s); src <- paste0("src.", s)
    if (!col %in% names(out)) { out[[col]] <- NA_real_; out[[src]] <- NA_character_ }
    i <- is.na(out[[col]])
    g <- fp$gm |> filter(stat == s); g0 <- fp$gm_pos |> filter(stat == s)
    gm <- coalesce(g$gm[match(paste(d$pos, d$core), paste(g$pos, g$core))], g0$gm[match(d$pos, g0$pos)], 0)
    K <- if (s %in% PS_RARE) st$K_RARE else st$K_CAREER
    out[[col]][i] <- ((coalesce(d[[paste0("S_", s)]], 0) + K * gm) / (coalesce(d$S_n, 0) + K))[i]
    out[[src]][i] <- if (s %in% PS_RARE) "career rate" else "career average (no prop)"
  }
  out
}

## core / TD-only flags (83): core QB = pass yds prop; RB = rush yds, rec yds or receptions
## prop; WR / TE = rec yds or receptions prop. td_keep = TD-only with a price at or above the
## median of core players at the position (B$td_bar)
ps_flags <- function(d, td_bar) {
  d |> mutate(core = case_when(
    pos == "QB" ~ !is.na(line.pass_yds),
    pos == "RB" ~ !is.na(line.rush_yds) | !is.na(line.rec_yds) | !is.na(line.rec),
    TRUE        ~ !is.na(line.rec_yds) | !is.na(line.rec)),
    td_only = !core & !is.na(p_td_raw),
    td_keep = td_only & p_td_raw >= td_bar$td_bar[match(pos, td_bar$pos)])
}

## expected points in every format, plus P(boom) / P(bust) from the bundle's logistic fits
ps_points <- function(e, pos) {
  proj <- tibble(pos = pos, pass_yds = e$e.pass_yds, pass_td = e$e.pass_td, pass_int = e$e.pass_int,
                 rush_yds = e$e.rush_yds, rec = e$e.rec, rec_yds = e$e.rec_yds, rush_td = e$e.tds,
                 fum_lost = e$e.fum_lost, two_pt = e$e.two_pt)
  setNames(lapply(names(PS_FORMATS), \(f) ps_score(proj, f)), paste0("vfp_", names(PS_FORMATS))) |> as_tibble()
}
ps_bb <- function(vfp, p_td, pos, bb) {          # bb: pos, fmt, event, b0, b1, b2, b3, td_fill
  out <- list()
  for (f in names(PS_FORMATS)) for (ev in c("boom", "bust")) {
    k <- match(paste(pos, f, ev), paste(bb$pos, bb$fmt, bb$event)); c <- bb[k, ]
    v <- vfp[[paste0("vfp_", f)]]; t <- coalesce(p_td, c$td_fill)
    ## the fitted curve bends over at the top (b2 < 0); hold it flat past its peak so a higher projection
    ## never lowers P(boom) (matters for elite TEs: the TE curve peaks near 13 half-PPR points)
    top <- ifelse(c$b2 < 0, -c$b1 / (2 * c$b2), Inf); v <- pmin(v, top)
    out[[paste0("p_", ev, "_", f)]] <- plogis(c$b0 + c$b1 * v + c$b2 * v^2 + c$b3 * t)
  }
  as_tibble(out)
}

## outcome quantiles (q10, q25, q50, q75, q90) from the bundle's quantile regressions; inputs as in 68:
## v = projection, t = anytime-TD price, imp = team implied total, sp = spread (+ = favored)
ps_ranges <- function(vfp, pos, p_td, imp, sp, R, bb) {
  if (is.null(R)) return(NULL)
  out <- list()
  for (f in names(PS_FORMATS)) {
    v <- vfp[[paste0("vfp_", f)]]
    tdf <- bb$td_fill[match(paste(pos, f, "boom"), paste(bb$pos, bb$fmt, bb$event))]
    X <- list(`(Intercept)` = rep(1, length(v)), v = v, t = coalesce(p_td, tdf), imp = coalesce(imp, 22), sp = coalesce(sp, 0))
    Q <- sapply(c(0.10, 0.25, 0.50, 0.75, 0.90), \(tau) {
      r <- R[R$fmt == f & R$tau == tau, ]; q <- rep(0, length(v))
      for (k in unique(r$term)) { cf <- r$coef[r$term == k][match(pos, r$pos[r$term == k])]; q <- q + coalesce(cf, 0) * X[[k]] }
      q })
    Q <- t(apply(matrix(Q, ncol = 5), 1, sort))
    for (j in 1:5) out[[paste0("q", c(10, 25, 50, 75, 90)[j], "_", f)]] <- Q[, j]
  }
  as_tibble(out)
}

## ---- Model uncertainty of the Vegas-only projection (the page's "±"), like the kicker / D/ST bootstrap ----
## Two sources, drawn together nboot times: (1) which sportsbooks: each game's books are resampled with
## replacement and the median consensus is rebuilt (books that disagree widen it); (2) the conversion from props
## to expected stats: one of the bundle's bootstrap refits of 83's calibrations (B$conv_boot). Everything else
## (career-average fills, flags) is held fixed. Returns per game x player x format the 5th / 95th percentiles;
## "±" = half the 90% interval. long: per-book outcomes (flatten_event_odds) with game_id; B: the bundle.
ps_ci <- function(long, B, nboot = 60, seed = 1) {
  if (!nrow(long) || is.null(B$conv_boot)) return(NULL)
  long <- long |> filter(!is_team_defense(player))
  bl <- book_lines(long)                                      # per book: main line, P(over), 50/50 point
  td <- long |> filter(market == "player_anytime_td", side == "Yes") |> mutate(p = am_to_p(price)) |>
    group_by(game_id, book, player) |> summarise(p = mean(p), .groups = "drop")
  names_all <- bind_rows(bl |> distinct(game_id, player), td |> distinct(game_id, player)) |> distinct()
  mp <- match_players(names_all, B$games |> select(game_id, season, week, home_team, away_team), B$roster) |>
    filter(!is.na(gsis_id)) |> select(game_id, player, gsis_id)
  bl <- bl |> inner_join(mp, by = c("game_id", "player")); td <- td |> inner_join(mp, by = c("game_id", "player"))
  books <- bind_rows(bl |> distinct(game_id, book), td |> distinct(game_id, book)) |> distinct()
  info <- B$players |> select(gsis_id, pos)
  wmed <- function(x, w) { o <- order(x); x <- x[o]; w <- w[o]; x[which(cumsum(w) >= sum(w) / 2)[1]] }
  set.seed(seed); nb <- length(B$conv_boot)
  draws <- lapply(seq_len(nboot), function(b) {
    smp <- books |> group_by(game_id) |> reframe(book = sample(book, n(), replace = TRUE)) |> count(game_id, book, name = "w")
    cons <- bl |> inner_join(smp, by = c("game_id", "book")) |> group_by(game_id, gsis_id, market) |>
      summarise(n_books = sum(w), line = wmed(main_line, w), p_over = wmed(p_over, w), med_est = wmed(med_est, w), .groups = "drop")
    anyt <- td |> inner_join(smp, by = c("game_id", "book")) |> group_by(game_id, gsis_id) |>
      summarise(n_books = sum(w), p_td_raw = wmed(p, w), .groups = "drop")
    d <- ps_inputs(cons, anyt) |> inner_join(info, by = "gsis_id") |> left_join(B$career, by = "gsis_id") |> ps_flags(B$td_bar)
    if (!nrow(d)) return(NULL)
    cb <- B$conv_boot[[(b - 1) %% nb + 1]]                     # coefficients only; terms from the main fits
    e <- ps_expect(d, B, conv = Map(function(pm, cf) { pm$coef <- cf[names(pm$coef)]; pm }, B$conv, cb[names(B$conv)]))
    bind_cols(d |> select(game_id, gsis_id), ps_points(e, d$pos))
  })
  bind_rows(draws) |> group_by(game_id, gsis_id) |>
    summarise(across(starts_with("vfp_"), list(lo = \(v) unname(quantile(v, 0.05)), hi = \(v) unname(quantile(v, 0.95)))),
              draws = n(), .groups = "drop") |>
    rename_with(\(v) sub("^vfp_(\\w+)_(lo|hi)$", "ci_\\2_\\1", v))
}
