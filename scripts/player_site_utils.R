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
ps_pois_invert <- function(L, p) {
  p <- pmin(pmax(p, 0.02), 0.98)
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
ps_expect <- function(d, B) {
  st <- B$settings; out <- d |> select(game_id, gsis_id)
  has <- function(s) if (s %in% PS_PROP_STATS) !is.na(d[[paste0("x.", s)]]) else rep(FALSE, nrow(d))
  for (s in PS_PROP_STATS) {
    h <- has(s); pm <- B$conv[[s]]
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
    out[[paste0("p_", ev, "_", f)]] <- plogis(c$b0 + c$b1 * v + c$b2 * v^2 + c$b3 * t)
  }
  as_tibble(out)
}
