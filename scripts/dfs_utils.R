## ---- dfs_utils.R: DraftKings DFS helpers (scoring, bonus odds, outcome odds, salaries, ownership) ----
## Used by 94_dfs_fit.R (Stratus, weekly fit) and 96_dfs_refresh.R (GitHub Actions, every refresh).
## Kept apart from the Players / D/ST / kicker code: nothing here is sourced by 44 / 45 / 55 / 64 / 65.
## Needs only dplyr / tidyr / purrr / jsonlite / curl, like 65.

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(purrr) })

## ---- 1. DraftKings classic scoring ----
## Pass TD 4, 0.04 / pass yd, +3 at 300+ pass yds, INT -1; 0.1 / rush or rec yd, +3 at 100+ rush yds, +3 at 100+ rec yds;
## 1 per catch; rush / rec / return TD 6; 2-pt 2; fumble lost -1. D/ST = Yahoo's defaults (Andrew 2026-10-03).
DK_BONUS <- c(pass_yds = 300, rush_yds = 100, rec_yds = 100)
DK_POS <- c("QB", "RB", "WR", "TE")
dk_base <- function(pass_yds, pass_td, pass_int, rush_yds, rec, rec_yds, tds, fum_lost, two_pt) {
  z <- function(v) coalesce(as.numeric(v), 0)
  0.04 * z(pass_yds) + 4 * z(pass_td) - z(pass_int) + 0.1 * (z(rush_yds) + z(rec_yds)) + z(rec) +
    6 * z(tds) + 2 * z(two_pt) - z(fum_lost)
}
## actual DK points of played games (82's player_games columns)
dk_actual <- function(pg) {
  z <- function(v) if (is.null(pg[[v]])) 0 else coalesce(as.numeric(pg[[v]]), 0)
  dk_base(z("pass_yds"), z("pass_td"), z("pass_int"), z("rush_yds"), z("rec"), z("rec_yds"),
          z("rush_td") + z("rec_td") + z("st_td"), z("fum_lost"), z("two_pt")) +
    3 * (z("pass_yds") >= 300) + 3 * (z("rush_yds") >= 100) + 3 * (z("rec_yds") >= 100)
}
## half PPR as ESPN (as 83 / 63: the Players page's boom cut-offs are defined in half PPR)
half_actual <- function(pg) {
  z <- function(v) if (is.null(pg[[v]])) 0 else coalesce(as.numeric(pg[[v]]), 0)
  0.04 * z("pass_yds") + 4 * z("pass_td") - 2 * z("pass_int") + 0.1 * (z("rush_yds") + z("rec_yds")) + 0.5 * z("rec") +
    6 * (z("rush_td") + z("rec_td") + z("st_td")) + 2 * z("two_pt") - 2 * z("fum_lost")
}

## ---- 2. Yardage bonuses: P(yds >= 300 / 100 | expected yards) ----
## A projection of 85 receiving yards never "earns" the bonus, but he reaches 100 about a quarter of the time, so the
## bonus is worth 3 x P(>= 100), not 0 or 3. Logistic per stat on log(expected yards), its square, and whether the stat
## had a sportsbook line (a prop pins his yards down more tightly than a career average). Fit by 94 on 2023+ results.
## bon: list per stat of list(coef = named numeric, lo = smallest log(e) seen, hi = largest, peak)
dk_bonus_p <- function(e, has_prop, stat, bon) {
  f <- bon[[stat]]; e <- as.numeric(e)
  if (is.null(f)) return(rep(0, length(e)))
  x <- log(pmax(coalesce(e, 0), 1)); x <- pmin(pmax(x, f$lo), f$hi)
  if (is.finite(f$peak)) x <- pmin(x, f$peak)                            # never lower P(bonus) for more expected yards
  cf <- f$coef; hp <- as.numeric(coalesce(has_prop, FALSE))
  eta <- cf[["(Intercept)"]] + cf[["x"]] * x + coalesce(cf["x2"], 0) * x^2 +
    coalesce(cf["hp"], 0) * hp + coalesce(cf["x:hp"], 0) * x * hp
  p <- plogis(unname(eta)); p[coalesce(e, 0) <= 0] <- 0
  p
}
## projection = DK points of the expected stats + 3 x each bonus probability
## e: e.pass_yds, e.pass_td, e.pass_int, e.rush_yds, e.rec, e.rec_yds, e.tds, e.fum_lost, e.two_pt;
## hp: data frame / list with has-prop flags for pass_yds, rush_yds, rec_yds
## Bonuses count only where the fit applies (pass: QB; rush: QB / RB; receiving: RB / WR / TE) and the expected yards are
## at least a fifth of the threshold; elsewhere (an RB's 0.01 expected passing yards) the chance is taken as 0.
DK_BON_POS <- list(pass_yds = "QB", rush_yds = c("QB", "RB"), rec_yds = c("RB", "WR", "TE"))
dk_project <- function(e, hp, bon) {
  base <- dk_base(e$e.pass_yds, e$e.pass_td, e$e.pass_int, e$e.rush_yds, e$e.rec, e$e.rec_yds, e$e.tds, e$e.fum_lost, e$e.two_pt)
  pb <- lapply(names(DK_BONUS), \(s) { y <- e[[paste0("e.", s)]]
    p <- dk_bonus_p(y, hp[[s]], s, bon)
    ifelse(e$pos %in% DK_BON_POS[[s]] & coalesce(y, 0) >= 0.2 * DK_BONUS[[s]], p, 0) }); names(pb) <- names(DK_BONUS)
  tibble(dk_base = base, p300 = pb$pass_yds, p100r = pb$rush_yds, p100c = pb$rec_yds,
         dk_bonus = 3 * (pb$pass_yds + pb$rush_yds + pb$rec_yds), dk_proj = base + 3 * (pb$pass_yds + pb$rush_yds + pb$rec_yds))
}

## ---- 3. Outcome odds: P(DK points >= c | projection v, anytime-TD price t), any c ----
## One logistic per position fit on "stacked" data (each player-game repeated at every threshold of a grid, y = 1 if he
## reached it): logit P = b0 + b1 v + b2 c + b3 v c + b4 c^2 + b5 t + b6 t c. Gives P(boom) at the DK cut-off and
## P(4x salary) for any salary from one curve. Made non-increasing in c by a running minimum over the grid.
## ex: tibble(pos, term, coef) ; grid: thresholds used in the fit
DK_GRID <- seq(2.5, 50, by = 2.5)
dk_exceed <- function(v, cut, t, pos, ex, grid = DK_GRID) {
  n <- length(v); out <- rep(NA_real_, n)
  for (p in unique(pos)) {
    i <- which(pos == p); cf <- ex[ex$pos == p, ]; b <- setNames(cf$coef, cf$term); b[is.na(b)] <- 0
    g <- function(nm) if (nm %in% names(b)) b[[nm]] else 0
    tt <- coalesce(t[i], g("t_fill"))
    G <- sort(unique(c(grid, pmin(pmax(cut[i], min(grid)), max(grid)))))
    ## P at every grid point for each player, then the running minimum (monotone), then read off at his cut
    M <- sapply(G, \(cc) plogis(g("(Intercept)") + g("v") * v[i] + g("c") * cc + g("v:c") * v[i] * cc + g("c2") * cc^2 +
                                  g("t") * tt + g("t:c") * tt * cc))
    M <- matrix(M, nrow = length(i))
    if (ncol(M) > 1) for (k in 2:ncol(M)) M[, k] <- pmin(M[, k], M[, k - 1])
    ci <- pmin(pmax(cut[i], min(grid)), max(grid))
    out[i] <- M[cbind(seq_along(i), match(ci, G))]
    hi <- cut[i] > max(grid); if (any(hi)) out[i][hi] <- out[i][hi] * exp(-(cut[i][hi] - max(grid)) / 5)   # beyond the grid: decay
  }
  out
}
## Alternative (spec "Q"): P(DK >= c) read off a fine grid of quantile regressions y ~ v per position (taus 0.02 ... 0.98):
## the predicted quantiles are sorted (monotone) and c is located between them; beyond the 98th percentile an
## exponential tail matched to the 90th-98th percentile gap. qg: tibble(pos, tau, term, coef) on that grid
dk_exceed_q <- function(v, cut, pos, qg) {
  out <- rep(NA_real_, length(v)); taus <- sort(unique(qg$tau))
  for (p in unique(pos)) {
    i <- which(pos == p); r <- qg[qg$pos == p, ]
    b0 <- r$coef[r$term == "(Intercept)"][match(taus, r$tau[r$term == "(Intercept)"])]
    b1 <- r$coef[r$term == "v"][match(taus, r$tau[r$term == "v"])]
    Q <- outer(v[i], b1) + matrix(b0, nrow = length(i), ncol = length(taus), byrow = TRUE)
    Q <- t(apply(Q, 1, sort)); Q <- matrix(Q, nrow = length(i))
    out[i] <- vapply(seq_along(i), \(k) { q <- Q[k, ]; cc <- cut[i][k]
      if (cc <= q[1]) return(1 - taus[1] * max(0, (cc - (q[1] - 5)) / 5))             # below the 2nd percentile: to 1 over 5 points
      if (cc >= q[length(q)]) { j90 <- which.min(abs(taus - 0.90)); s <- max((q[length(q)] - q[j90]) / log((1 - taus[j90]) / (1 - taus[length(taus)])), 0.5)
        return((1 - taus[length(taus)]) * exp(-(cc - q[length(q)]) / s)) }
      j <- max(which(q <= cc)); if (j >= length(q)) return(1 - taus[length(taus)])
      w <- if (q[j + 1] > q[j]) (cc - q[j]) / (q[j + 1] - q[j]) else 0
      1 - (taus[j] + w * (taus[j + 1] - taus[j])) }, 0)
  }
  out
}
## the chosen spec per position (94): "Q" = quantile grid, "E0" / "E1" = stacked logistic. S = the dfs spec
dk_odds <- function(v, cut, t, pos, S) {
  out <- rep(NA_real_, length(v)); ch <- S$exceed_spec
  for (p in unique(pos)) { i <- which(pos == p)
    out[i] <- if (identical(unname(ch[p]), "Q")) dk_exceed_q(v[i], cut[i], pos[i], S$qgrid) else dk_exceed(v[i], cut[i], t[i], pos[i], S$exceed) }
  out
}
## outcome quantiles from quantile regression y ~ v per position (as 68 does for the other formats): q10 q25 q50 q75 q90
dk_quant <- function(v, pos, qr) {
  taus <- c(0.10, 0.25, 0.50, 0.75, 0.90)
  Q <- sapply(taus, \(tau) { r <- qr[qr$tau == tau, ]
    b0 <- r$coef[r$term == "(Intercept)"][match(pos, r$pos[r$term == "(Intercept)"])]
    b1 <- r$coef[r$term == "v"][match(pos, r$pos[r$term == "v"])]
    coalesce(b0, 0) + coalesce(b1, 1) * v })
  Q <- matrix(Q, ncol = 5); Q <- t(apply(Q, 1, sort)); Q <- matrix(Q, ncol = 5)
  setNames(as_tibble(as.data.frame(Q)), paste0("q", c(10, 25, 50, 75, 90)))
}

## ---- 4. D/ST: Yahoo projection, outcome odds from the Yahoo bundle's back-test residuals (kernel by projection) ----
## unc = bundle$unc (proj, resid, bw). D/ST scores are whole points, so "reach c" = proj + resid >= ceiling(c) - 0.5.
dst_odds <- function(proj, cuts, unc) {
  w_all <- sapply(proj, \(p) dnorm((unc$proj - p) / unc$bw))
  sapply(seq_along(proj), \(k) { w <- w_all[, k]; c0 <- ceiling(cuts[k] - 1e-9) - 0.5
    sum(w * (proj[k] + unc$resid >= c0)) / sum(w) })
}
dst_quant <- function(proj, unc) {
  wq <- function(x, w, p) { o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w); x[which(cw >= p)[1]] }
  out <- t(sapply(proj, \(p) { w <- dnorm((unc$proj - p) / unc$bw); s <- p + unc$resid
    c(wq(s, w, .10), wq(s, w, .25), wq(s, w, .50), wq(s, w, .75), wq(s, w, .90)) }))
  setNames(as_tibble(as.data.frame(matrix(out, ncol = 5))), paste0("q", c(10, 25, 50, 75, 90)))
}

## ---- 5. DraftKings salaries ----
## Public lobby + draftables JSON (no login). The main slate = the classic NFL draft group with no time suffix
## ("(Early Only)", "(Turbo)", "(Primetime)", ...) and the most games. Fallback: a DKSalaries.csv exported from the
## DK lobby and saved as data/dfs/DKSalaries_<season>_wk<ww>.csv (uploaded on GitHub).
DK_LOBBY <- "https://www.draftkings.com/lobby/getcontests?sport=NFL"
DK_DRAFTABLES <- "https://api.draftkings.com/draftgroups/v1/draftgroups/%s/draftables"
dk_team <- function(x) dplyr::recode(toupper(trimws(x)), JAC = "JAX", LAR = "LA", WSH = "WAS", ARZ = "ARI", OAK = "LV", SD = "LAC", STL = "LA")
dk_get_json <- function(url, mock = Sys.getenv("DK_MOCK_DIR")) {
  if (nzchar(mock)) {                                                   # tests: <dir>/lobby.json, <dir>/draftables_<id>.json
    f <- if (grepl("getcontests", url)) "lobby.json" else sprintf("draftables_%s.json", sub(".*/draftgroups/([0-9]+)/.*", "\\1", url))
    return(jsonlite::fromJSON(file.path(mock, f), simplifyVector = FALSE))
  }
  h <- curl::new_handle(); curl::handle_setheaders(h, `User-Agent` = "Mozilla/5.0 (dst-site DFS page)", Accept = "application/json")
  r <- curl::curl_fetch_memory(url, handle = h)
  if (r$status_code != 200) stop(sprintf("HTTP %d for %s", r$status_code, url))
  jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE)
}
## draft groups in the lobby. The lobby JSON used to carry a DraftGroups array; since 2026 it carries only Contests
## (fields: n = name, dg = draft group id, gameType, gameTypeId, po = prize pool, sd / sdstring = start). Both are read:
## from Contests, one row per Classic draft group with its contest count and total prize pool.
DK_NOT_MAIN <- "Early|Afternoon|Turbo|Primetime|Night|Sun-Mon|Thu-Mon|Mon-Thu|Late|Snake|Tiers|Best Ball|Showdown|Single Game"
dk_groups <- function(lobby) {
  g <- function(x, ...) { for (k in c(...)) if (!is.null(x[[k]])) return(x[[k]]); NA }
  dg <- lobby$DraftGroups %||% lobby$draftGroups %||% list()
  if (length(dg)) return(tibble(dg_id = map_chr(dg, \(x) as.character(g(x, "DraftGroupId", "draftGroupId"))),
         ctype = map_dbl(dg, \(x) as.numeric(g(x, "ContestTypeId", "contestTypeId"))),
         n_games = map_dbl(dg, \(x) as.numeric(g(x, "GameCount", "gameCount"))),
         start = map_chr(dg, \(x) as.character(g(x, "StartDateEst", "startDateEst", "StartDate"))),
         suffix = map_chr(dg, \(x) { s <- g(x, "ContestStartTimeSuffix", "contestStartTimeSuffix"); if (is.null(s) || is.na(s)) "" else as.character(s) }),
         tag = map_chr(dg, \(x) as.character(g(x, "DraftGroupTag", "draftGroupTag"))),
         game_type = map_dbl(dg, \(x) as.numeric(g(x, "GameTypeId", "gameTypeId"))), source = "draftgroups"))
  cs <- lobby$Contests %||% lobby$contests %||% list()
  if (!length(cs)) return(tibble())
  tibble(dg_id = map_chr(cs, \(x) as.character(format(as.numeric(g(x, "dg")), scientific = FALSE))),
         gtype = map_chr(cs, \(x) as.character(g(x, "gameType"))), name = map_chr(cs, \(x) as.character(g(x, "n"))),
         po = map_dbl(cs, \(x) as.numeric(g(x, "po"))), start = map_chr(cs, \(x) as.character(g(x, "sdstring", "sd")))) |>
    filter(gtype %in% "Classic", !is.na(dg_id), dg_id != "NA") |>
    group_by(dg_id) |>
    summarise(n_contests = n(), prize = sum(po, na.rm = TRUE), start = first(start),
              not_main = mean(grepl(DK_NOT_MAIN, name, ignore.case = TRUE)), example = name[which.max(po)], .groups = "drop") |>
    mutate(ctype = 21, n_games = NA_real_, suffix = ifelse(not_main > 0.5, "(not main)", ""), tag = "Featured", game_type = 1, source = "contests")
}
## the main slate: classic, no time suffix, featured; most games (DraftGroups) or most contests then prize (Contests)
dk_main_group <- function(groups) {
  if (!nrow(groups)) return(groups)
  x <- groups |> filter(ctype == 21 | is.na(ctype), !nzchar(trimws(suffix)))
  if (any(x$tag %in% "Featured")) x <- x |> filter(tag %in% "Featured")
  if (identical(x$source[1], "contests")) x |> arrange(desc(n_contests), desc(prize)) |> slice_head(n = 1) else
    x |> arrange(desc(n_games), start) |> slice_head(n = 1)
}
## draftables -> one row per player: name, DK position, team, salary, status, game (away @ home), start time
dk_parse_draftables <- function(j) {
  d <- j$draftables %||% list()
  if (!length(d)) return(tibble())
  g <- function(x, k) { v <- x[[k]]; if (is.null(v) || !length(v)) NA else v }
  out <- tibble(dk_id = map_chr(d, \(x) as.character(g(x, "playerId"))),
         name = map_chr(d, \(x) as.character(g(x, "displayName"))),
         first = map_chr(d, \(x) as.character(g(x, "firstName"))), last = map_chr(d, \(x) as.character(g(x, "lastName"))),
         dk_pos = map_chr(d, \(x) as.character(g(x, "position"))),
         team = dk_team(map_chr(d, \(x) as.character(g(x, "teamAbbreviation")))),
         salary = map_dbl(d, \(x) as.numeric(g(x, "salary"))),
         status = map_chr(d, \(x) as.character(g(x, "status"))),
         disabled = map_lgl(d, \(x) isTRUE(g(x, "isDisabled"))),
         game = map_chr(d, \(x) as.character(g(g(x, "competition"), "name"))),
         start = map_chr(d, \(x) as.character(g(g(x, "competition"), "startTime"))))
  ## each player is listed once per roster slot (e.g. RB and FLEX): keep one
  out |> filter(!is.na(salary)) |> distinct(dk_id, .keep_all = TRUE) |> mutate(status = ifelse(status %in% c("None", "", NA), "", status))
}
## the main slate's salaries, checked against the teams playing this week (the lobby can already show next week's slate).
## teams: teams with a game this week not yet started. Returns the draftables with pulled_at / dg_id, or stops with a reason.
## share of a salary list's games ("BUF @ MIA", "BUF@MIA ...") that are this week's matchups (games: away_team, home_team)
dk_game_share <- function(game, games) {
  g <- unique(sub("^\\s*([A-Za-z]+)\\s*@\\s*([A-Za-z]+).*$", "\\1@\\2", na.omit(game)))
  if (!length(g)) return(0)
  key <- paste0(dk_team(sub("@.*", "", g)), "@", dk_team(sub(".*@", "", g)))
  mean(key %in% c(paste0(games$away_team, "@", games$home_team), paste0(games$home_team, "@", games$away_team)))
}
## games: this week's games (away_team, home_team) not yet started
dk_pull_main <- function(games, now = Sys.time()) {
  mg <- dk_main_group(dk_groups(dk_get_json(DK_LOBBY)))
  if (!nrow(mg)) stop("no classic main-slate draft group in the lobby")
  x <- dk_parse_draftables(dk_get_json(sprintf(DK_DRAFTABLES, mg$dg_id)))
  share <- dk_game_share(x$game, games)
  if (!nrow(x) || share < 0.9) stop(sprintf("lobby main slate (draft group %s) isn't this week's (%.0f%% of its games are this week's matchups)", mg$dg_id, 100 * share))
  attr(x, "example") <- if (!is.null(mg$example)) mg$example else ""
  x |> mutate(pulled_at = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), dg_id = mg$dg_id)
}
## a readable reason for a failed pull (DK answers 403 to scripts on GitHub and on Stratus)
dk_err_text <- function(msg) if (grepl("HTTP 403", msg)) "DraftKings refuses automated salary requests (HTTP 403): export this week's DKSalaries.csv from the DK lobby and upload it" else msg
## DKSalaries.csv (lobby export): Position, Name + ID, Name, ID, Roster Position, Salary, Game Info, TeamAbbrev, AvgPointsPerGame
dk_parse_csv <- function(file) {
  x <- utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
  names(x) <- trimws(names(x))
  tibble(dk_id = as.character(x$ID), name = x$Name, first = NA_character_, last = NA_character_, dk_pos = x$Position,
         team = dk_team(x$TeamAbbrev), salary = as.numeric(x$Salary), status = "", disabled = FALSE,
         game = sub(" .*$", "", x$`Game Info`), start = NA_character_) |>
    distinct(dk_id, .keep_all = TRUE)
}

## ---- 6. Projected ownership (BETA: not yet fitted to real contest ownership) ----
## Within each position, a softmax ("which player would a typical entrant pick here?") over:
##   value = log(points per $1K), z-scored within the position (the strongest driver in public ownership models);
##   proj  = projection, z-scored (studs draw ownership on name / upside even at fair value);
##   form  = recent DK points, decay-weighted (x0.5 per game, so last week counts as much as all earlier games
##           together; Andrew's hypothesis: a big game last week raises ownership), z-scored; weight 0.20 (0.35 first;
##           Andrew 2026-10-04: it pushed one WR to 51%);
##   imp   = team implied total, z-scored (popular games).
## The shares are scaled so each position sums to its slots on a DK classic roster (QB 1, RB 2 + FLEX share,
## WR 3 + FLEX share, TE 1 + FLEX share, DST 1; 900% in all). How concentrated each position is (the softmax temperature)
## is set per slate so the "effective number of players" (1 / sum of squared shares) matches a typical main slate, scaled
## by the number of games: at 12 games QB 9, RB 14, WR 22, TE 9, D/ST 10 (top QB ~15-20%, top RB ~30-40%). The drivers
## decide WHO is popular; the targets only set HOW concentrated. Weights and targets are placeholders until 98 fits
## them to real DK ownership (contest standings).
## Budget: real ownership has to fit under the cap (the field's average lineup uses about $49.7K), which a per-position
## formula alone ignores (week 4's first version implied $62.5K lineups and 51% on a $9.1K WR). So every player's
## utility also carries -lambda x salary ($K), with lambda set per slate so that sum(ownership x salary) = $49.7K.
OWN_BETA <- list(
  w = c(value = 1.00, proj = 0.55, form = 0.20, imp = 0.20),
  neff12 = c(QB = 9, RB = 14, WR = 22, TE = 9, DST = 10),
  slots = c(QB = 100, RB = 235, WR = 350, TE = 115, DST = 100),
  avg_salary = 49700, cap = 0.60, fitted = FALSE)
own_project <- function(d, n_games = 12, spec = OWN_BETA) {
  ## d: pos (QB / RB / WR / TE / DST), salary, proj, form (may be NA), imp (may be NA), avail (FALSE = out)
  zs <- function(x) { m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE); if (!is.finite(s) || s == 0) s <- 1; z <- (x - m) / s; z[is.na(z)] <- 0; z }
  ok <- d$avail & !is.na(d$proj) & d$proj > 0 & !is.na(d$salary)
  u0 <- rep(NA_real_, nrow(d))
  for (p in unique(d$pos[ok])) { i <- which(ok & d$pos == p)
    val <- log(pmax(d$proj[i], 0.1) / (d$salary[i] / 1000))
    u0[i] <- spec$w[["value"]] * zs(val) + spec$w[["proj"]] * zs(d$proj[i]) + spec$w[["form"]] * zs(d$form[i]) + spec$w[["imp"]] * zs(d$imp[i]) }
  shares <- function(lambda) {
    own <- rep(0, nrow(d))
    for (p in unique(d$pos[ok])) { i <- which(ok & d$pos == p); u <- u0[i] - lambda * d$salary[i] / 1000
      target <- min((spec$neff12[[p]] %||% 10) * max(n_games, 2) / 12, length(i) * 0.8)
      soft <- function(tmp) { e <- exp((u - max(u)) / tmp); e / sum(e) }
      lo <- 0.02; hi <- 50
      for (it in 1:50) { mid <- sqrt(lo * hi); if (1 / sum(soft(mid)^2) < target) lo <- mid else hi <- mid }
      sh <- soft(sqrt(lo * hi)) * (spec$slots[[p]] %||% 100) / 100
      for (it in 1:5) { over <- sh > spec$cap; if (!any(over)) break           # cap one player's share; excess to the rest
        exc <- sum(sh[over] - spec$cap); sh[over] <- spec$cap; rest <- !over & sh > 0
        if (any(rest)) sh[rest] <- sh[rest] + exc * sh[rest] / sum(sh[rest]) }
      own[i] <- sh }
    own }
  avg_sal <- function(lambda) sum(shares(lambda) * coalesce(d$salary, 0))
  lambda <- 0
  if (avg_sal(0) > spec$avg_salary) { lo <- 0; hi <- 5
    for (it in 1:40) { mid <- (lo + hi) / 2; if (avg_sal(mid) > spec$avg_salary) lo <- mid else hi <- mid }
    lambda <- (lo + hi) / 2 }
  own <- shares(lambda); attr(own, "lambda") <- lambda; attr(own, "avg_salary") <- sum(own * coalesce(d$salary, 0))
  own
}
## hist: gsis_id, week, dk_pts (games played this season); returns one row per player: form, last_pts, last_wk, n_g
form_points <- function(hist, lambda = 0.5) {
  if (is.null(hist) || !nrow(hist)) return(tibble(gsis_id = character(), form = numeric(), last_pts = numeric(), last_wk = integer(), n_g = integer()))
  hist |> arrange(gsis_id, desc(week)) |> group_by(gsis_id) |>
    summarise(form = sum(dk_pts * lambda^(row_number() - 1)) / sum(lambda^(row_number() - 1)),
              last_pts = first(dk_pts), last_wk = first(week), n_g = n(), .groups = "drop")
}

## ---- 7. Game correlations (stacks) ----
## Roles inside a team-game by projection: QB (the top QB), RB1-RB2, WR1-WR3, TE1, DST; anyone else "OTH" (no correlation).
DFS_ROLE_N <- c(QB = 1, RB = 2, WR = 3, TE = 1)
dfs_roles <- function(game_id, team, pos, v) {
  d <- tibble(i = seq_along(pos), game_id, team, pos, v)
  d <- d |> group_by(game_id, team, pos) |> mutate(k = rank(-v, ties.method = "first")) |> ungroup()
  role <- ifelse(d$pos == "DST", "DST", ifelse(d$k <= coalesce(DFS_ROLE_N[d$pos], 0), paste0(d$pos, ifelse(d$pos %in% c("QB", "TE"), "", d$k)), "OTH"))
  role[d$pos %in% c("QB", "TE") & d$k == 1] <- ifelse(d$pos[d$pos %in% c("QB", "TE") & d$k == 1] == "QB", "QB", "TE1")
  role[order(d$i)]
}
## game environment: blowout = |spread| >= 7 (the favourite's side matters), shootout = total >= 49, else normal
dfs_env <- function(spread, total) ifelse(!is.na(spread) & abs(spread) >= 7, "blow", ifelse(!is.na(total) & total >= 49, "shoot", "norm"))
## correlation for a pair of players in the same game. cor: 94's table (type, a, b, env, fav, r).
## same team: a / b sorted; opposing: a = first player's role, fav = whether HIS team is favoured
dfs_pair_r <- function(cor, same, a, b, env, fav_a) {
  if (a == "OTH" || b == "OTH") return(0)
  if (same) { k <- sort(c(a, b)); x <- cor[cor$type == "same" & cor$a == k[1] & cor$b == k[2] & cor$env == env, ] } else
    x <- cor[cor$type == "opp" & cor$a == a & cor$b == b & cor$env == env & ((is.na(cor$fav) & is.na(fav_a)) | (!is.na(cor$fav) & cor$fav %in% fav_a)), ]
  if (nrow(x)) x$r[1] else 0
}
