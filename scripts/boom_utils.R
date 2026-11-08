## ---- boom_utils.R: milestone ("boom") odds for the Players page and the DFS Players tab (Andrew 2026-11-07) ----
## P(2+ TDs), P(no TD), P(100+ rushing / receiving / rush + rec yards), P(300+ passing yards), P(2+ TDs or 100+ yards).
## Two sources per number:
##   market    the sportsbooks' own milestone prices, pulled every refresh by 65 (BOOM_MARKETS): player TDs over 1.5, the
##             alternate yardage ladders + the main over / unders, the rush + rec yards market, the anytime-TD "No" side.
##             Per book: the P(over) at the milestone (two-sided prices de-vigged; Over-only prices / BOOM_K, as the ladder
##             fill-in in props_utils.R), interpolated on the logit scale between the book's rungs that straddle it (never
##             extrapolated); median across books.
##   estimate  a logistic fit on 2023+ results from the Vegas-only expected stats and the anytime-TD price (66b_boom_fit.R,
##             output/players_site/boom_fit.rds), for players without a market price.
## Shown: the market number where a book posts one (after 66b's calibration when it beats the raw price on past results),
## else the estimate (italic*). Without boom_fit.rds: market only, 2+ TDs / no TD from the TD price (Poisson), union by independence.
## Used by 65_player_refresh.R (pull + attach), 64_player_page.R / 95_dfs_page.R (columns) and 66b_boom_fit.R (fits).

BOOM_MARKETS <- c("player_tds_over", "player_pass_yds_alternate", "player_rush_yds_alternate", "player_reception_yds_alternate",
                  "player_rush_reception_yds", "player_rush_reception_yds_alternate")
BOOM_K <- 1.066                                     # Over-only margin (the ladder test's live estimate, 2026-09-29)
## stat -> markets whose rungs count, and the milestone point
BOOM_DEF <- list(td2     = list(mk = "player_tds_over", at = 1.5),
                 pass300 = list(mk = c("player_pass_yds", "player_pass_yds_alternate"), at = 299.5),
                 rush100 = list(mk = c("player_rush_yds", "player_rush_yds_alternate"), at = 99.5),
                 rec100  = list(mk = c("player_reception_yds", "player_reception_yds_alternate"), at = 99.5),
                 scr100  = list(mk = c("player_rush_reception_yds", "player_rush_reception_yds_alternate"), at = 99.5))
BOOM_STATS <- c(names(BOOM_DEF), "notd")
## which positions each number is for (QB: 300+ passing only; RB rushing; RB / WR / TE the rest)
BOOM_POS <- list(td2 = c("RB", "WR", "TE"), notd = c("RB", "WR", "TE"), pass300 = "QB", rush100 = "RB",
                 rec100 = c("RB", "WR", "TE"), scr100 = "RB")
bm_lg <- function(p) qlogis(pmin(pmax(p, 0.002), 0.998))
## expected yards behind each yardage milestone (from boom_x features); below 66b's floor the books' long-shot price
## (often their minimum price, e.g. 100+ for a 10-yard receiver) is ignored and the estimate is shown (Andrew 2026-11-07)
boom_eyds <- function(X, s) switch(s, pass300 = exp(X$ly_p), rush100 = exp(X$ly_r), rec100 = exp(X$ly_c), scr100 = exp(X$ly_s), NULL)

## ---- Market: per book x player x stat P(over the milestone), then the median across books ----
## long: flatten_event_odds() rows (+ game_id): book, market, player, side, point, price
boom_market <- function(long) {
  empty <- tibble(game_id = character(), player = character(), stat = character(), p = numeric(), n_books = integer())
  if (is.null(long) || !nrow(long)) return(empty)
  long <- long |> filter(!is_team_defense(player))
  out <- list()
  for (s in names(BOOM_DEF)) {
    x <- long |> filter(market %in% BOOM_DEF[[s]]$mk, side %in% c("Over", "Under"), !is.na(point), is.finite(price))
    if (!nrow(x)) next
    at <- BOOM_DEF[[s]]$at
    pb <- x |> mutate(q = am_to_p(price)) |>
      group_by(game_id, book, player, point) |>
      summarise(o = mean(q[side == "Over"]), u = mean(q[side == "Under"]), .groups = "drop") |>
      filter(is.finite(o)) |>
      mutate(p = ifelse(is.finite(u), o / (o + u), pmin(o / BOOM_K, 0.995))) |>
      group_by(game_id, book, player) |>
      summarise(p = { o <- order(point); pt <- point[o]; pp <- p[o]
        if (any(pt == at)) mean(pp[pt == at]) else if (at < min(pt) || at > max(pt)) NA_real_ else
          plogis(approx(pt, bm_lg(pp), at, ties = mean)$y) }, .groups = "drop") |>
      filter(is.finite(p))
    if (nrow(pb)) out[[s]] <- pb |> group_by(game_id, player) |>
      summarise(stat = s, p = median(p), n_books = n(), .groups = "drop")
  }
  ## P(no TD): the anytime-TD "No" side (two-sided books de-vigged; No-only / BOOM_K)
  nt <- long |> filter(market == "player_anytime_td", side %in% c("Yes", "No"), is.finite(price)) |>
    mutate(q = am_to_p(price)) |> group_by(game_id, book, player) |>
    summarise(y = mean(q[side == "Yes"]), n = mean(q[side == "No"]), .groups = "drop") |> filter(is.finite(n)) |>
    mutate(p = ifelse(is.finite(y), n / (y + n), pmin(n / BOOM_K, 0.995)))
  if (nrow(nt)) out$notd <- nt |> group_by(game_id, player) |> summarise(stat = "notd", p = median(p), n_books = n(), .groups = "drop")
  if (!length(out)) return(empty)
  bind_rows(out)
}

## ---- Features for the estimates (any table with pos, p_td_raw, e.tds, e.rush_yds, e.rec_yds, e.pass_yds) ----
boom_x <- function(d, fit = NULL) {
  g <- function(v) if (v %in% names(d)) as.numeric(d[[v]]) else rep(NA_real_, nrow(d))
  pt <- coalesce(g("p_td_raw"), pmin((1 - exp(-pmax(coalesce(g("e.tds"), 0), 0))) * BOOM_K, 0.95))   # no TD price: from expected TDs
  X <- tibble(pos = d$pos, lt = bm_lg(pt),
              ly_r = log(pmax(coalesce(g("e.rush_yds"), 0), 1)), ly_c = log(pmax(coalesce(g("e.rec_yds"), 0), 1)),
              ly_s = log(pmax(coalesce(g("e.rush_yds"), 0) + coalesce(g("e.rec_yds"), 0), 1)),
              ly_p = log(pmax(coalesce(g("e.pass_yds"), 0), 1)))
  X$pt1 <- boom_pred(fit$est$td1, X, default = pmin(pt / BOOM_K, 0.95))   # calibrated P(1+ rush / rec TD)
  X
}
## one estimator: list(rhs = "~ lt + I(lt^2)", coef = named numeric) per position, or a rule ("pois" = 2+ TDs from P(1+ TD))
boom_pred1 <- function(m, X) {
  if (is.null(m) || !nrow(X)) return(rep(NA_real_, nrow(X)))
  if (identical(m$rhs, "pois")) { lam <- -log(pmax(1 - X$pt1, 1e-6)); return(1 - exp(-lam) * (1 + lam)) }
  f <- as.formula(m$rhs)
  for (v in names(m$rng)) if (v %in% names(X)) X[[v]] <- pmin(pmax(X[[v]], m$rng[[v]][1]), m$rng[[v]][2])   # no extrapolation past the fit's data
  mm <- model.matrix(f, model.frame(f, X, na.action = na.pass))
  plogis(drop(mm %*% m$coef[colnames(mm)]))
}
boom_pred <- function(ms, X, default = rep(NA_real_, nrow(X))) {    # ms: list by position
  out <- default
  if (is.null(ms)) return(out)
  for (p in intersect(names(ms), unique(X$pos))) { i <- which(X$pos == p); out[i] <- boom_pred1(ms[[p]], X[i, , drop = FALSE]) }
  out
}
## market price -> the number shown, as 66b chose on past results: raw, calibrated, or calibrated together with the estimate
boom_mkt_apply <- function(mc, pm, pe) {
  if (is.null(mc) || identical(mc$method, "raw")) return(pm)
  if (identical(mc$method, "est")) return(ifelse(is.na(pe), pm, pe))
  cf <- mc$coef
  eta <- cf[["(Intercept)"]] + cf[["lm"]] * bm_lg(pm) + if ("le" %in% names(cf)) cf[["le"]] * bm_lg(coalesce(pe, pm)) else 0
  plogis(eta)
}

## ---- Attach to the projection rows (65's cur) ----
## cur: gsis_id, game_id, pos, p_td_raw, e.*; mk: boom_market() rows matched to gsis_id (game_id, gsis_id, stat, p, n_books)
## Adds per stat s: bp_<s> shown, bm_<s> market (median of books, NA = none posted), be_<s> estimate, bn_<s> books, bs_<s> "mkt" / "est";
## and bp_boom (2+ TDs or 100+ yards: RB rush + rec, WR / TE receiving) / bp_boomdk (DK bonuses: RB 100+ rushing or receiving),
## with bs_boom / bs_boomdk = "mkt" when any part came from a market price.
boom_attach <- function(cur, mk, fit = NULL) {
  if (!nrow(cur)) return(cur)
  cur <- cur |> select(-any_of(grep("^b[pmens]_", names(cur), value = TRUE)))
  X <- boom_x(cur, fit)
  est <- list(td2 = boom_pred(fit$est$td2, X, default = boom_pred1(list(rhs = "pois"), X)),
              notd = 1 - X$pt1,
              pass300 = boom_pred(fit$est$pass300, X), rush100 = boom_pred(fit$est$rush100, X),
              rec100 = boom_pred(fit$est$rec100, X), scr100 = boom_pred(fit$est$scr100, X))
  for (s in BOOM_STATS) {
    m <- if (!is.null(mk) && nrow(mk)) mk |> filter(stat == s) else NULL
    i <- if (!is.null(m) && nrow(m)) match(paste(cur$game_id, cur$gsis_id), paste(m$game_id, m$gsis_id)) else rep(NA_integer_, nrow(cur))
    ok <- cur$pos %in% BOOM_POS[[s]]
    pm <- ifelse(ok, m$p[i] %||% NA_real_, NA_real_); nb <- ifelse(ok, m$n_books[i] %||% NA_integer_, NA_integer_)
    pe <- ifelse(ok, est[[s]], NA_real_)
    shown_m <- if (any(!is.na(pm))) boom_mkt_apply(fit$mkt[[s]], pm, pe) else pm
    fl <- fit$mkt[[s]]$floor %||% 0; ey <- boom_eyds(X, s)
    low <- if (fl > 0 && !is.null(ey)) ey < fl & !is.na(pe) else rep(FALSE, nrow(cur))   # below the floor: estimate
    use_m <- !is.na(pm) & !low
    cur[[paste0("bm_", s)]] <- pm; cur[[paste0("be_", s)]] <- pe; cur[[paste0("bn_", s)]] <- nb
    cur[[paste0("bp_", s)]] <- ifelse(use_m, shown_m, pe)
    use_est <- identical(fit$mkt[[s]]$method, "est")            # 66b found the estimate better even where books price it
    cur[[paste0("bs_", s)]] <- ifelse(use_m & !use_est, "mkt", ifelse(is.na(cur[[paste0("bp_", s)]]), NA_character_, "est"))
  }
  ## union: 66b's fit on the two (three) parts, else independence
  p100 <- ifelse(cur$pos == "RB", cur$bp_scr100, cur$bp_rec100)
  s100 <- ifelse(cur$pos == "RB", cur$bs_scr100, cur$bs_rec100)
  U <- tibble(pos = cur$pos, l2 = bm_lg(cur$bp_td2), l100 = bm_lg(p100), lr = bm_lg(coalesce(cur$bp_rush100, 0.002)), lc = bm_lg(cur$bp_rec100))
  ind <- 1 - (1 - cur$bp_td2) * (1 - p100)
  ind_dk <- 1 - (1 - cur$bp_td2) * (1 - coalesce(cur$bp_rush100, 0)) * (1 - cur$bp_rec100)
  cur$bp_boom <- ifelse(cur$pos %in% c("RB", "WR", "TE"), boom_pred(fit$est$boom, U, default = ind), NA_real_)
  cur$bp_boomdk <- ifelse(cur$pos %in% c("RB", "WR", "TE"), boom_pred(fit$est$boomdk, U, default = ind_dk), NA_real_)
  cur$bp_boom <- pmax(cur$bp_boom, cur$bp_td2, p100, na.rm = FALSE)                              # a union is never below a part
  cur$bp_boomdk <- pmax(cur$bp_boomdk, cur$bp_td2, coalesce(cur$bp_rush100, 0), cur$bp_rec100, na.rm = FALSE)
  cur$bs_boom <- ifelse(is.na(cur$bp_boom), NA_character_, ifelse(cur$bs_td2 %in% "mkt" | s100 %in% "mkt", "mkt", "est"))
  cur$bs_boomdk <- ifelse(is.na(cur$bp_boomdk), NA_character_,
                          ifelse(cur$bs_td2 %in% "mkt" | cur$bs_rush100 %in% "mkt" | cur$bs_rec100 %in% "mkt", "mkt", "est"))
  cur
}

## ---- Display: percent, italic* when it is an estimate; hover = market and estimate side by side ----
boom_cell <- function(d, s, lab) {
  p <- d[[paste0("bp_", s)]]; if (is.null(p)) return(rep("", nrow(d)))
  src <- d[[paste0("bs_", s)]]; pm <- d[[paste0("bm_", s)]]; pe <- d[[paste0("be_", s)]]; nb <- d[[paste0("bn_", s)]]
  pc <- function(x) ifelse(is.na(x), "–", paste0(round(100 * x), "%"))
  val <- ifelse(is.na(p), "", paste0(round(100 * p), "%"))
  val <- ifelse(src %in% "est", paste0("<i class='fill'>", val, "*</i>"), val)
  tip <- if (is.null(pm)) ifelse(src %in% "est", paste0(lab, ": estimate from his expected stats (no sportsbook price)"), "") else
    ifelse(is.na(p), "", paste0("<b>", lab, "</b>\nSportsbooks: ", ifelse(is.na(pm), "not posted", paste0(pc(pm), " (median of ", nb, " book", ifelse(nb == 1, "", "s"), ")")),
                                "\nEstimate from his expected stats: ", pc(pe),
                                ifelse(!is.na(pm) & src %in% "est", "\nShown: the estimate (closer than the books' price on past results)",
                                       ifelse(!is.na(pm) & abs(p - pm) > 0.005, paste0("\nShown: the books' price recalibrated on past results (", pc(p), ")"), ""))))
  ifelse(nzchar(val) & nzchar(tip), tip_span(val, tip), val)
}
