# ==============================================================================
# 64_player_page.R — HTML for the Vegas-only player page (sourced by 65_player_refresh.R)
# player_page(P) returns the page: position tabs (QB / RB / WR / TE) × scoring format (Standard, Half PPR,
# PPR, FFPC), one sortable table each. Uses site_utils.R (tiers, tooltips, sticky columns, sparkline).
# ==============================================================================

pp_esc <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); x <- gsub(">", "&gt;", x); gsub('"', "&quot;", x) }
pp_f <- function(x, d = 1) ifelse(is.na(x), "", formatC(x, format = "f", digits = d))
pp_pct <- function(x) ifelse(is.na(x), "", paste0(round(100 * x), "%"))
pp_sg <- function(x, d = 1) ifelse(is.na(x) | abs(x) < 0.5 * 10^-d, "0", sprintf(paste0("%+.", d, "f"), x))
pp_et <- function(x, f) gsub(" 0", " ", format(x, f, tz = "America/New_York"))

PP_STAT_COLS <- list(
  QB = c(pass_att = "Pass att", pass_yds = "Pass yds", pass_td = "Pass TD", pass_int = "INT", rush_yds = "Rush yds"),
  RB = c(rush_att = "Rush att", rush_yds = "Rush yds", rec = "Rec", rec_yds = "Rec yds"),
  WR = c(rec = "Rec", rec_yds = "Rec yds", rush_yds = "Rush yds"),
  TE = c(rec = "Rec", rec_yds = "Rec yds"),
  FLEX = c(rush_yds = "Rush yds", rec = "Rec", rec_yds = "Rec yds"))
PP_PROP_LAB <- c(pass_yds = "Pass yds", pass_td = "Pass TD", pass_int = "INT", pass_att = "Pass att", rush_yds = "Rush yds",
                 rush_att = "Rush att", rec = "Receptions", rec_yds = "Rec yds")

PP_TIP <- c(
  Rank = "Rank at the position in this format.",
  Tier = "Natural-break tier of the projections (optimal 1-D grouping; 1 = best). Solid line = a gap of 1.5+ points between tiers; dashed = a smaller gap.",
  Player = "Vegas flags vs FantasyPros ECR, from our Vegas-only position rank: \U0001F525 love = at least 50% higher than ECR, \U0001F44D like = 25–50% higher, \U0001F914 dislike = 25–50% lower, \u2620\uFE0F fade = at least 50% lower (always at least 3 spots). The % is the gap divided by the better of the two ranks, so 2 spots matters more near the top (WR8 vs WR10 = 25%) than further down. Hover or tap a player for the exact difference.",
  Kickoff = "Kickoff (Eastern). \U0001F512 = game started: frozen at the last props pulled before kickoff.",
  Imp = "Team implied points from the latest pre-kickoff spread and total (median of sportsbooks, from the D/ST page's line pulls).",
  Proj = "Vegas-only projection: the sportsbook props converted to expected stats (yardage medians corrected for skew, counts via a Poisson fit to line and odds, anytime-TD price calibrated on 2023+ results) and scored in this format.",
  "\u0394" = "Change since the first pull that had props for this player this week.",
  Trend = "Projection at every props pull this week (open dot = first pull with props). Green = up, red = down. Hover a dot for the time.",
  "P(boom)" = "Chance of a boom week: half PPR QB 25+, RB / WR 20+, TE 15+ (other formats: the same share of player-games; FLEX uses each player's own position). Logistic fit per position on the Vegas projection and the anytime-TD price, 2023+: at the same projection a higher TD chance means more boom weeks, strongest for RBs, so RB P(boom) doesn't follow the rank exactly.",
  "P(bust)" = "Chance of a bust week: half PPR QB under 12, RB / WR under 6, TE under 4 (other formats: the same share). Logistic fit on the Vegas projection, 2023+.",
  "TD%" = "Anytime-TD price as a probability (median across books; includes the books' margin). Expected TDs in the projection are calibrated from it. On rows without props yet: the sportsbook price when one is posted (it then sets his expected TDs), otherwise italic* = the price implied by his expected TDs from history.",
  Books = "Sportsbooks behind the median lines (most for any one of this player's markets).",
  "Pass att" = "Expected pass attempts (volume; doesn't score).", "Rush att" = "Expected rushing attempts (volume; doesn't score).",
  Pos = "FLEX tab: the player's rank at his own position in this format.",
  "\u00b1" = "Model uncertainty of the projection, like the kicker and D/ST pages: half the width of its 90% bootstrap interval. Each of 60 draws resamples the sportsbooks behind the median lines (with replacement, per game) and uses one of 100 bootstrap refits of the props-to-stats calibration; the interval is the 5th to 95th percentile of the projections. Wide = books disagree, thin markets or lines far from typical. Hover for the interval. It measures how sure we are of the projection, not how much the player's score can swing (that is the range bar).",
  "Range bar" = "How much his score can swing: light band = 80% of outcomes (10th to 90th percentile), dark band = 50% (25th to 75th), line = projection; the small orange band around the line = the \u00b1 (90% interval of the projection itself). From quantile regression on 2023+ player-games with the same projection (adding the TD price, implied total or spread didn't help). Same scale within a table.",
  "Injury" = "(Q) Questionable, (D) Doubtful, (O) Out, IR / PUP / NFI / SUS after the name: the official NFL injury report's game status once it is out (Friday), before that Sleeper's status; IR / PUP / suspensions from either. Checked at every refresh and frozen at kickoff. Hover the player for practice participation and the injury. Red = ruled out or doubtful.",
  ECR = "FantasyPros expert consensus rank at the position (via DynastyProcess, updated about 10 am / 10 pm ET; the last one before kickoff). RB / WR / TE ranks are PPR in every format. Hover for the average rank, spread across experts and FantasyPros' projected PPR points. Amber when we rank the player a start but ECR has him as a sit: light = just outside the start line, dark = well outside (start lines: QB 12, RB 24, WR 36, TE 12).",
  "ESPN rank" = "ESPN's projected stats scored in this format, ranked at the position (last pull before kickoff); projected points in brackets. Amber as for ECR.",
  "Sleeper rank" = "Sleeper's projected stats (Rotowire) scored in this format, ranked at the position (last pull before kickoff); projected points in brackets. Amber as for ECR.")
PP_START <- c(QB = 12, RB = 24, WR = 36, TE = 12)
## amber when we have him inside the start line and the other source is outside it (1.5x = dark)
pp_flag <- function(ours, theirs, pos) { n <- unname(PP_START[pos])          # pos: one per row
  ifelse(is.na(theirs) | ours > n, "", ifelse(theirs > 1.5 * n, "fl2", ifelse(theirs > n, "fl1", ""))) }
PP_AMBER <- paste0("<p class='s legend'><span class='sw fl1'></span> <b>Light amber</b>: we rank him a start at his position ",
  "(inside QB 12 / RB 24 / WR 36 / TE 12) but that source ranks him just outside, a borderline sit (e.g. WR 37\u201354). ",
  "<span class='sw fl2'></span> <b>Dark amber</b>: that source ranks him well outside, beyond 1.5\u00d7 the start line (e.g. WR 55+). ",
  "Compared on position ranks, also on FLEX.</p>",
  "<p class='s legend'>Vegas vs FantasyPros ECR (our Vegas-only position rank; the gap \u00f7 the better of the two ranks, always at least 3 spots: ",
  "WR12 vs WR15 = 25%, WR10 vs WR15 = 50%): \U0001F525 <b>love</b> at least 50% higher \u00b7 \U0001F44D <b>like</b> 25\u201350% higher \u00b7 ",
  "\U0001F914 <b>dislike</b> 25\u201350% lower \u00b7 \u2620\uFE0F <b>fade</b> at least 50% lower. Hover a player for the exact difference.</p>")
## glossary text for rows without props (back-test numbers from 79 via the bundle)
pp_fb_note <- function(sk) {
  x <- ""
  if (!is.null(sk) && nrow(sk)) { w <- tidyr::pivot_wider(sk, names_from = spec, values_from = wk_spearman)
    fb <- setdiff(names(w), c("pos", "V"))[1]
    x <- paste0(" In the 2023\u201325 back-test it ranks players less well than the props (half PPR weekly rank correlation with results, same players: ",
                paste(sprintf("%s %.2f vs %.2f", w$pos, w[[fb]], w$V), collapse = ", "), ").") }
  paste0("Games whose props aren't posted yet (usually Tuesday–Wednesday). Every stat is the player's recent history ",
         "(recency-weighted, \u00d70.9 per game, \u00d70.5 per offseason, shrunk toward his position), scored in the format and adjusted ",
         "for this game's implied team total vs his team's recent scoring and for the spread (fit on 2019\u201325). Players shown: ",
         "active-roster players with enough recent production, the team's latest starting QB, and nobody ruled out or doubtful. ",
         "No \u00b1, love / fade or amber flags, since those are Vegas calls. The TD% is implied by his expected TDs. ",
         "Each player switches to props as soon as his game has them.", x)
}
pp_rank_pts <- function(rk, pts) ifelse(is.na(rk), "", ifelse(is.na(pts), as.character(rk), sprintf("%d (%.1f)", as.integer(rk), pts)))
PP_FILL_NOTE <- "Italic* = no prop for this stat: receptions from the receiving-yards prop and the player's yards per catch (or the reverse); otherwise his recency-weighted career average per game, shrunk toward players at his position without that prop."

pp_table <- function(df, raw, id, row_cls, cell_cls, stick = 3, row_key = NULL) {
  lft <- names(df) %in% c("Player", "Team", "Opp", "Kickoff")
  hdr <- paste0("<tr>", paste0(sprintf('<th title="%s" onclick="srt(this)"%s>%s</th>', pp_esc(coalesce(unname(PP_TIP[names(df)]), "")),
                                       ifelse(lft, ' style="text-align:left"', ""), pp_esc(names(df))), collapse = ""), "</tr>")
  M <- as.matrix(df)
  body <- vapply(seq_len(nrow(df)), function(i) { r <- M[i, ]
    cls <- ifelse(names(df) %in% c("Player", "Team", "Opp", "Kickoff"), "l", "")
    for (cn in intersect(names(cell_cls), names(df))) { k <- which(names(df) == cn); if (nzchar(cell_cls[[cn]][i])) cls[k] <- trimws(paste(cls[k], cell_cls[[cn]][i])) }
    paste0(sprintf("<tr%s%s>", if (nzchar(row_cls[i])) sprintf(' class="%s"', row_cls[i]) else "",
                   if (!is.null(row_key)) sprintf(' data-s="%s"', pp_esc(row_key[i])) else ""),
           paste0(ifelse(nzchar(cls), sprintf('<td class="%s">', cls), "<td>"), ifelse(names(df) %in% raw, r, pp_esc(r)), "</td>", collapse = ""), "</tr>") }, "")
  sprintf('<div class="tw"><table id="%s" data-stick="%d"><thead>%s</thead><tbody>%s</tbody></table></div>', id, stick, hdr, paste(body, collapse = ""))
}

pp_player_tip <- function(p, fmt) {
  lines <- c()
  for (s in names(PP_PROP_LAB)) { l <- p[[paste0("line.", s)]]; if (!is.null(l) && !is.na(l))
    lines <- c(lines, sprintf("%s %s (over %s)", PP_PROP_LAB[[s]], formatC(l, format = "fg"), pp_pct(p[[paste0("p_over.", s)]]))) }
  if (!is.na(p$p_td_raw)) lines <- c(lines, sprintf("Anytime TD %s", pp_pct(p$p_td_raw)))
  fills <- c()
  for (s in c(setdiff(names(PP_STAT_COLS[[p$pos]]), c("pass_att", "rush_att")), "tds")) {
    src <- p[[paste0("src.", s)]]; if (!is.null(src) && !is.na(src) && src != "prop")
      fills <- c(fills, sprintf("%s %s (%s)", if (s == "tds") "TDs" else PP_PROP_LAB[[s]], pp_f(p[[paste0("e.", s)]], if (s %in% c("rec", "pass_td", "pass_int", "tds")) 2 else 1), src)) }
  paste0("<b>", pp_esc(p$player_name), " (", p$team, ", ", p$pos, ") — ", PS_FORMATS[[fmt]], " ", pp_f(p[[paste0("vfp_", fmt)]], 2), "</b>",
         if (p$td_keep %in% TRUE) "\nTD-only: no yardage or receptions prop; kept because his TD price is at or above the position median." else "",
         "\n<b>Props</b> (median of books): ", if (length(lines)) pp_esc(paste(lines, collapse = " · ")) else "none",
         if (length(fills)) paste0("\n<b>No prop</b>: ", pp_esc(paste(fills, collapse = " · "))) else "")
}

## Vegas love / fade vs FantasyPros ECR (Andrew 2026-09-29): compare position ranks as a percentage, so a
## 2-spot gap inside the top 10 counts more than the same gap outside the top 25.
##   diff % = (ECR rank - our rank) / the better (smaller) of the two ranks   (symmetric: 8 vs 10 = +25%, 10 vs 8 = -25%)
##   flame when we rank him at least PP_LOVE_PCT higher, skull and crossbones at least that much lower,
##   and only when the gap is at least PP_LOVE_MIN spots (so 1–2 spot swaps at the very top are not flagged)
PP_LOVE_PCT <- 0.25; PP_LOVE_MIN <- 3   # Andrew 2026-09-29: 3 spots minimum
PP_LOVE_STRONG <- 0.50                  # Andrew 2026-10-03: four levels, love 🔥 >= 50%, like 👍 25-50%, dislike 🤔 -25 to -50%, fade ☠️ <= -50%
PP_SYM <- c(love = "\U0001F525", like = "\U0001F44D", dislike = "\U0001F914", fade = "\u2620\uFE0F")
pp_level <- function(pc, gap) ifelse(is.na(pc) | is.na(gap) | abs(gap) < PP_LOVE_MIN, "",
  ifelse(pc >= PP_LOVE_STRONG, "love", ifelse(pc >= PP_LOVE_PCT, "like", ifelse(pc <= -PP_LOVE_STRONG, "fade", ifelse(pc <= -PP_LOVE_PCT, "dislike", "")))))
pp_sym <- function(lev) unname(ifelse(lev %in% names(PP_SYM), PP_SYM[match(lev, names(PP_SYM))], ""))
PP_LF_LABEL <- c(love = "\U0001F525 love", like = "\U0001F44D like", dislike = "\U0001F914 dislike", fade = "\u2620\uFE0F fade")
## range bar: light = 80% of outcomes, dark = 50%, line = projection; orange band around the line = the ± (90% interval of
## the projection itself, Andrew 2026-09-29), drawn at least 2 px wide
pp_range_bar <- function(q10, q25, q75, q90, pj, lo = -2, hi = 35, ci_lo = NA, ci_hi = NA) {
  sc <- function(v) round(100 * (pmin(pmax(v, lo), hi) - lo) / (hi - lo), 1)
  ci_lo <- rep_len(ci_lo, length(pj)); ci_hi <- rep_len(ci_hi, length(pj)); has <- !is.na(ci_lo) & !is.na(ci_hi)
  ci <- ifelse(has, sprintf('<span class="ci" style="left:%s%%;width:max(2px,%s%%)"></span>', sc(ci_lo), sc(ci_hi) - sc(ci_lo)), "")
  ifelse(is.na(q10), "", sprintf('<div class="rb" title="80%%: %.1f to %.1f \u00b7 50%%: %.1f to %.1f \u00b7 proj %.2f%s"><span class="r80" style="left:%s%%;width:%s%%"></span><span class="r50" style="left:%s%%;width:%s%%"></span>%s<span class="pt" style="left:%s%%"></span><span class="zero" style="left:%s%%"></span></div>',
          q10, q90, q25, q75, pj, ifelse(has, sprintf(" (\u00b1 %.2f: %.2f to %.2f)", (ci_hi - ci_lo) / 2, ci_lo, ci_hi), ""),
          sc(q10), sc(q90) - sc(q10), sc(q25), sc(q75) - sc(q25), ci, sc(pj), sc(0)))
}
pp_player_cell <- function(d, prk) {
  ecr <- if ("ecr_rank" %in% names(d)) d$ecr_rank else rep(NA_real_, nrow(d))
  fb <- d$fallback %in% TRUE
  gap <- ecr - prk; pc <- gap / pmin(ecr, prk)
  lev <- ifelse(fb, "", pp_level(pc, gap)); sym <- ifelse(lev == "", "", paste0(" ", pp_sym(lev)))
  what <- ifelse(is.na(ecr), "No FantasyPros ECR for this player this week.",
    ifelse(gap == 0, "Same rank as ECR.",
      sprintf("%d spot%s %s than ECR (%+.0f%%)%s", as.integer(abs(gap)), ifelse(abs(gap) == 1, "", "s"),
              ifelse(gap > 0, "higher", "lower"), 100 * pc,
              ifelse(nzchar(sym), paste0(": Vegas ", lev, " ", pp_sym(lev)), ""))))
  st <- if ("inj_status" %in% names(d)) d$inj_status else rep(NA_character_, nrow(d))
  badge <- ifelse(is.na(st), "", sprintf(" <span class='inj%s'>(%s)</span>", ifelse(d$inj_out %in% TRUE, " out", ""), d$inj_badge))
  injl <- ifelse(is.na(st), "", paste0("\n<b>Injury: ", st, "</b>", ifelse(nzchar(coalesce(d$inj_detail, "")), paste0(" \u2014 ", pp_esc(d$inj_detail)), ""),
                                       ifelse(d$inj_out %in% TRUE, "\n\u26A0 Ruled out or unlikely to play: books usually pull his props, so this line is his last pre-report projection.", "")))
  what <- ifelse(fb, paste0("<b>No props posted yet for this game.</b> Projection = his recent history (every stat), adjusted for ",
                            "the team's implied total and the spread; no Vegas flags (love / like / dislike / fade) until props post.",
                            ifelse(is.na(ecr), "", sprintf("\nHistory rank %s%d vs ECR %s%d.", d$pos, as.integer(prk), d$pos, as.integer(ecr)))), what)
  tip <- paste0("<b>", pp_esc(d$player_name), " (", d$team, ", ", d$pos, ")</b>",
                ifelse(d$td_keep %in% TRUE, " TD-only", ""), injl,
                ifelse(fb, "", paste0("\nVegas ", d$pos, as.integer(prk), ifelse(is.na(ecr), "", sprintf(" · ECR %s%d (average expert rank %.1f)", d$pos, as.integer(ecr), d$ecr_avg)))),
                "\n", what)
  tip_span(paste0(pp_esc(d$player_name), badge, ifelse(d$td_keep, " <span class='s'>(TD)</span>", ""), ifelse(fb, " <span class='s nop'>no props yet</span>", ""), sym), tip)
}

## search bar (Andrew 2026-10-03): each player row carries a key with his name, team code, full team name and nicknames;
## lower case, accents and punctuation removed (the page's script normalises what is typed the same way)
PP_TEAM_NAMES <- c(ARI = "Arizona Cardinals", ATL = "Atlanta Falcons", BAL = "Baltimore Ravens", BUF = "Buffalo Bills", CAR = "Carolina Panthers",
  CHI = "Chicago Bears", CIN = "Cincinnati Bengals", CLE = "Cleveland Browns", DAL = "Dallas Cowboys", DEN = "Denver Broncos", DET = "Detroit Lions",
  GB = "Green Bay Packers", HOU = "Houston Texans", IND = "Indianapolis Colts", JAX = "Jacksonville Jaguars Jags", KC = "Kansas City Chiefs",
  LV = "Las Vegas Raiders", LAC = "Los Angeles Chargers LA Bolts", LA = "Los Angeles Rams LAR", MIA = "Miami Dolphins", MIN = "Minnesota Vikings",
  NE = "New England Patriots Pats", NO = "New Orleans Saints", NYG = "New York Giants NY", NYJ = "New York Jets NY", PHI = "Philadelphia Eagles",
  PIT = "Pittsburgh Steelers", SF = "San Francisco 49ers Niners", SEA = "Seattle Seahawks", TB = "Tampa Bay Buccaneers Bucs", TEN = "Tennessee Titans",
  WAS = "Washington Commanders")
pp_search_key <- function(name, team) {
  k <- tolower(paste(name, team, coalesce(unname(PP_TEAM_NAMES[team]), "")))
  k <- iconv(k, "UTF-8", "ASCII//TRANSLIT", sub = ""); k[is.na(k)] <- ""
  gsub("\\s+", " ", gsub("[^a-z0-9 ]", "", k))
}
pp_pos_table <- function(P, pos, fmt) {
  if (!nrow(P$cur)) return("<p class='s'>No props posted yet for this position.</p>")
  flex <- pos == "FLEX"
  d <- P$cur |> filter(include, if (flex) pos %in% c("RB", "WR", "TE") else pos == !!pos)
  if (!nrow(d)) return("<p class='s'>No props posted yet for this position.</p>")
  if (!"fallback" %in% names(d)) d$fallback <- FALSE
  v <- d[[paste0("vfp_", fmt)]]; o <- order(-v); d <- d[o, ]; v <- v[o]; fb <- d$fallback %in% TRUE
  tdp <- if ("td_prop" %in% names(d)) d$td_prop %in% TRUE else rep(FALSE, nrow(d))   # no-props row with a posted TD price
  prk <- ave(-v, d$pos, FUN = \(x) rank(x, ties.method = "first"))          # rank within the player's position
  k <- max(5, min(10, ceiling(nrow(d) / 10)))
  tr <- tiers(v, k = min(k, nrow(d)), clear = 1.5)
  ## trend + change since the first pull with props for this player
  hh <- if (nrow(P$hist)) P$hist else tibble(gsis_id = character(), game_id = character(), t = as.POSIXct(character(), tz = "UTC"), !!paste0("vfp_", fmt) := numeric())
  h <- hh |> filter(gsis_id %in% d$gsis_id) |> select(gsis_id, game_id, t, proj = !!paste0("vfp_", fmt)) |> arrange(t)
  first <- h |> group_by(gsis_id) |> slice_min(t, n = 1, with_ties = FALSE) |> ungroup()
  spark <- vapply(d$gsis_id, \(g) { x <- h[h$gsis_id == g, ]
    if (nrow(x) < 1) return("")
    sparkline(x |> mutate(kind = c("weekly", rep("refresh", nrow(x) - 1)), lab = pp_et(t, "%a %b %d %I:%M %p"))) }, "")
  scols <- PP_STAT_COLS[[pos]]
  stat_cells <- map(names(scols), \(s) {
    val <- pp_f(d[[paste0("e.", s)]], if (s %in% c("rec", "pass_td", "pass_int")) 2 else 1)
    ifelse(d[[paste0("src.", s)]] %in% "prop", val, paste0("<i class='fill'>", val, "*</i>")) }) |> setNames(scols)
  ko <- paste0(ifelse(d$locked, "\U0001F512 ", ""), pp_et(d$ko, "%a %I:%M %p"))
  t <- tibble(Rank = seq_len(nrow(d)), Tier = tr$tier,
              Player = pp_player_cell(d, prk),
              Team = d$team, Opp = paste0(ifelse(d$home == 1, "vs ", "@ "), d$opp), Kickoff = ko, Imp = pp_f(d$implied),
              Proj = pp_f(v, 2), "\u0394" = ifelse(fb, "", pp_sg(v - first$proj[match(d$gsis_id, first$gsis_id)])),
              Trend = spark, "P(boom)" = pp_pct(d[[paste0("p_boom_", fmt)]]), "P(bust)" = pp_pct(d[[paste0("p_bust_", fmt)]]))
  if (paste0("q10_", fmt) %in% names(d)) {
    q <- function(k) d[[paste0("q", k, "_", fmt)]]
    hi <- max(35, ceiling(max(q(90), na.rm = TRUE) / 5) * 5)
    cl <- if (paste0("ci_lo_", fmt) %in% names(d)) d[[paste0("ci_lo_", fmt)]] else NA; ch <- if (paste0("ci_hi_", fmt) %in% names(d)) d[[paste0("ci_hi_", fmt)]] else NA
    t <- t |> mutate("Range bar" = pp_range_bar(q(10), q(25), q(75), q(90), v, lo = min(-2, floor(min(q(10), na.rm = TRUE))), hi = hi, ci_lo = cl, ci_hi = ch), .after = Trend)
  }
  if (paste0("ci_lo_", fmt) %in% names(d)) {                      # model uncertainty (bootstrap), as on the kicker page
    lo <- d[[paste0("ci_lo_", fmt)]]; hi <- d[[paste0("ci_hi_", fmt)]]
    t <- t |> mutate("\u00b1" = ifelse(is.na(lo), "", tip_span(pp_f((hi - lo) / 2, 2),
                       sprintf("90%% interval of the projection: %.2f to %.2f (resampled sportsbooks + calibration refits)", lo, hi))), .after = Proj)
  }
  if (flex) t <- t |> mutate(Pos = paste0(d$pos, prk), .after = Player)
  lab <- function(r) if (flex) ifelse(is.na(r), NA_character_, paste0(d$pos, as.integer(r))) else as.character(as.integer(r))
  ## other rankings this week (ext_player_utils.R via 65); flags compare position ranks, also on FLEX
  ext_c <- list()
  if ("ecr_rank" %in% names(d) && any(!is.na(d$ecr_rank))) {
    ecr_tip <- ifelse(is.na(d$ecr_rank), "", sprintf("<b>FantasyPros ECR %s%d</b>\nAverage expert rank %.1f (± %.1f); best %s, worst %s%s\nUpdated %s%s",
      d$pos, as.integer(d$ecr_rank), d$ecr_avg, d$ecr_sd, d$ecr_best, d$ecr_worst,
      ifelse(is.na(d$ecr_pts), "", sprintf("\nFantasyPros projection %.1f PPR points", d$ecr_pts)), d$ecr_date,
      ifelse(d$pos == "QB", "", "\nPPR ranks (the same in every format)")))
    t$ECR <- ifelse(is.na(d$ecr_rank), "", tip_span(lab(d$ecr_rank), ecr_tip))
    ext_c$ECR <- ifelse(fb, "", pp_flag(prk, d$ecr_rank, d$pos))
  }
  for (src in c("ESPN", "Sleeper")) { lo <- tolower(src); rk <- d[[paste0(lo, "_rk_", fmt)]]
    if (!is.null(rk) && any(!is.na(rk))) { cn <- paste(src, "rank")
      pts <- d[[paste0(lo, "_pts_", fmt)]]
      t[[cn]] <- ifelse(is.na(rk), "", ifelse(is.na(pts), lab(rk), sprintf("%s (%.1f)", lab(rk), pts)))
      ext_c[[cn]] <- ifelse(fb, "", pp_flag(prk, rk, d$pos)) } }
  if (length(ext_c)) t <- t |> relocate(any_of(c("ECR", "ESPN rank", "Sleeper rank")), .after = any_of(c("Proj", "\u00b1")))
  t <- bind_cols(t, as_tibble(stat_cells), tibble("TD%" = ifelse(fb & !tdp & !is.na(d$p_td_raw), paste0("<i class='fill'>", pp_pct(d$p_td_raw), "*</i>"), pp_pct(d$p_td_raw)),
    Books = ifelse(fb & !tdp, "", do.call(pmax, c(map(grep("^n_books\\.", names(d), value = TRUE), \(c) coalesce(d[[c]], 0)), na.rm = TRUE)))))
  brk <- c(FALSE, tr$tier[-1] != tr$tier[-length(tr$tier)])
  row_cls <- trimws(paste(ifelse(tr$tier %% 2 == 1, "tier-odd", ""), ifelse(brk, ifelse(tr$clear[tr$tier] %in% TRUE, "tb-clear", "tb-soft"), ""),
                          ifelse(fb, "nop", "")))
  ng <- c(QB = 2, RB = 3, WR = 3, TE = 2, FLEX = 4)[[pos]]                 # green tiers (Andrew 2026-10-01: RB / WR 3, FLEX 4)
  tc <- if (ng <= 2) tier_cls(tr$tier, k = max(tr$tier)) else
    ifelse(tr$tier <= ng, paste0("g", ng, "_", tr$tier), tier_cls(tr$tier, k = max(tr$tier)))
  pos_cls <- if (flex) list(Pos = paste0("pz ", tolower(d$pos))) else list()        # FLEX: colour-coded position (Andrew 2026-10-03): RB purple (not green: the tiers are green), WR blue, TE orange
  pp_table(t, raw = c("Player", "Trend", "ECR", "Range bar", "\u00b1", "TD%", unname(scols)), id = paste0("t_", pos, "_", fmt), row_cls = row_cls,
           cell_cls = c(list(Rank = tc, Player = tc, Proj = tc), ext_c, pos_cls), row_key = pp_search_key(d$player_name, d$team))
}

## ---- Track record tab (69_player_track.R -> output/players_site/track_players.rds) ----
pp_simple <- function(df, bold = NULL, raw = character()) {         # plain table; bold: logical matrix like df
  hdr <- paste0("<tr>", paste0(sprintf("<th>%s</th>", pp_esc(names(df))), collapse = ""), "</tr>")
  M <- as.matrix(df)
  body <- vapply(seq_len(nrow(df)), \(i) paste0("<tr>", paste0(vapply(seq_along(df), \(j) {
    v <- if (names(df)[j] %in% raw) M[i, j] else pp_esc(M[i, j]); cls <- if (j <= 2) " class='l'" else ""
    if (!is.null(bold) && isTRUE(bold[i, j])) v <- paste0("<b>", v, "</b>"); sprintf("<td%s>%s</td>", cls, v) }, ""), collapse = ""), "</tr>"), "")
  sprintf("<div class='tw'><table>%s%s</table></div>", hdr, paste(body, collapse = ""))
}
pp_track <- function(TR, fmt) {
  if (is.null(TR)) return("<p class='s'>No track record yet: run 69_player_track.R (89 does it each Tuesday).</p>")
  m <- TR$metrics[TR$metrics$fmt == fmt, ]; ss <- TR$startsit[TR$startsit$fmt == fmt, ]; lf <- TR$lovefade[TR$lovefade$fmt == fmt, ]
  POSO <- c("QB", "RB", "WR", "TE"); SRCO <- c("Vegas-only", "ECR", "ESPN", "Sleeper")
  out <- c(sprintf("<p class='s'>How each ranking did once the games were played, on the <b>same players</b> each week (those with a Vegas projection that every source ranked; each source re-ranked within them). Vegas-only = our projection from the props 60 minutes before kickoff (calibrations fit on other seasons). ECR = FantasyPros expert consensus, the last version before kickoff (PPR ranks in every format). ESPN / Sleeper = their projected stats scored in this format%s. Seasons %d\u2013%d, through %d week %d.</p>",
                       if (isTRUE(TR$notes$ext_after > 0)) " (weeks before the site started: their projections pulled after the week)" else "",
                       TR$seasons[1], TR$seasons[2], TR$last_week$season, TR$last_week$week),
           "<p class='s'><b>Rank corr</b> = Spearman correlation with actual points. <b>NDCG</b> = top-weighted ranking score (1 = perfect order; mistakes near the top cost most). <b>Top-N pts</b> = average actual points of the source's top N (QB 12, RB 24, WR 36, TE 12). <b>RMSE</b> = projection error in points (ECR has none except FantasyPros' projection in PPR). Bold = best at the position.</p>")
  for (per in unique(m$period)) {
    d <- m[m$period == per, ]; d <- d[order(match(d$pos, POSO), match(d$source, SRCO)), ]
    tb <- tibble(Position = d$pos, Source = d$source, Weeks = d$weeks, Players = sprintf("%.0f", d$players),
                 `Rank corr` = sprintf("%.3f", d$rank_corr), NDCG = sprintf("%.3f", d$ndcg),
                 `Top-N pts` = sprintf("%.2f (N %d)", d$top_n, as.integer(d$n_top)), RMSE = ifelse(is.na(d$rmse), "\u2014", sprintf("%.2f", d$rmse)))
    best <- function(v, hi = TRUE) ave(v, d$pos, FUN = \(x) if (all(is.na(x))) FALSE else (if (hi) x == max(x, na.rm = TRUE) else x == min(x, na.rm = TRUE)))
    B <- matrix(FALSE, nrow(tb), ncol(tb)); B[, 5] <- best(d$rank_corr) == 1; B[, 6] <- best(d$ndcg) == 1; B[, 7] <- best(d$top_n) == 1; B[, 8] <- best(d$rmse, FALSE) == 1
    out <- c(out, sprintf("<h3>%s</h3>", pp_esc(per)), pp_simple(tb, B))
  }
  if (nrow(ss)) {
    ss <- ss[order(ss$period, match(ss$pos, POSO), match(ss$source, SRCO)), ]
    out <- c(out, "<h3>Start / sit calls: Vegas-only vs each ranking</h3>",
             "<p class='s'>Every pair of players (both inside the top 2N of either ranking) that the two rankings order differently is one call. <b>Vegas right</b> = share of calls where Vegas-only's pick scored more; <b>pts per call</b> = average points gained by following Vegas-only (+ = Vegas better).</p>",
             pp_simple(tibble(Period = ss$period, Position = ss$pos, `vs` = ss$source, Calls = format(ss$calls, big.mark = ","),
                              `Vegas right` = sprintf("%.1f%%", 100 * ss$vegas_right), `Pts per call` = sprintf("%+.2f", ss$pts_per_call))))
  }
  if (nrow(lf)) {
    lf <- lf[order(lf$period, match(lf$pos, POSO), match(lf$flag, names(PP_SYM))), ]
    out <- c(out, "<h3>Vegas flags vs ECR: love \U0001F525 \u00b7 like \U0001F44D \u00b7 dislike \U0001F914 \u00b7 fade \u2620\uFE0F</h3>",
             "<p class='s'>The page's flags (position ranks \u2265 25% and \u2265 3 spots apart, both ranked within the same players). A love says \u201cstart him over the players ECR ranks between our rank and theirs\u201d; a fade says the reverse. <b>Right</b> = share of flags where he outscored (love) or was outscored by (fade) the average of those players; <b>pts per flag</b> = the average margin (+ = the flag paid off).</p>",
             pp_simple(tibble(Period = lf$period, Position = lf$pos, Flag = unname(PP_LF_LABEL[lf$flag]), Flags = lf$n,
                              Right = sprintf("%.0f%%", 100 * lf$right), `Pts per flag` = sprintf("%+.2f", lf$margin), `Avg pts` = sprintf("%.1f", lf$pts),
                              `Median ranks: Vegas / ECR / finish` = sprintf("%g / %g / %g", lf$vegas_rank, lf$ecr_rank, lf$finish))))
    fc <- TR$flags_current[TR$flags_current$fmt == fmt, ]
    if (nrow(fc)) {
      fc <- fc[order(-fc$week, match(fc$pos, POSO), fc$vrank), ]
      out <- c(out, sprintf("<details><summary class='s'>This season's flags, player by player (%d)</summary>%s</details>", nrow(fc),
        pp_simple(tibble(Week = fc$week, Pos = fc$pos, Player = paste0(pp_esc(fc$player_name), " (", fc$team, ")"),
                         Flag = pp_sym(fc$flag), `Vegas rank` = fc$vrank, `ECR rank` = fc$ecr_rank,
                         Finish = fc$finish, Pts = sprintf("%.1f", fc$pts), `vs others` = ifelse(is.na(fc$margin), "", sprintf("%+.1f", fc$margin))), raw = "Player")))
    }
  }
  paste(out, collapse = "")
}

player_page <- function(P) {
  pos_l <- c(PS_POS, "FLEX"); fmts <- names(PS_FORMATS)
  fpos <- function(p) if (p == "QB") PS_FMT_POS$QB else PS_FMT_POS$other      # QB: Standard / FFPC / 6-pt pass TD
  panes <- unlist(lapply(pos_l, \(p) lapply(fpos(p), \(f) sprintf('<div class="pane" data-pos="%s" data-fmt="%s">%s</div>', p, f, pp_pos_table(P, p, f)))))
  gl <- tibble(Term = names(PP_TIP), Definition = unname(PP_TIP)) |>
    bind_rows(tibble(Term = c("Italic*", "Italic row: no props yet", "(TD)", "Formats"), Definition = c(PP_FILL_NOTE, pp_fb_note(P$fb_skill),
      "TD-only player: no yardage or receptions prop, shown because his anytime-TD price is at or above the median of players at his position with full props.",
      "Standard / Half PPR / PPR: ESPN defaults (0.04 per pass yard, pass TD 4, INT −2, fumble lost −2, 0 / 0.5 / 1 per catch). FFPC: 0.05 per pass yard, INT −1, fumble lost −1, 1 per catch, 1.5 per TE catch. 6-pt pass TD (QB tab): Standard with 6 points per passing TD. The QB tab shows Standard / FFPC / 6-pt pass TD only: Half PPR and PPR score QBs the same as Standard.")))
  gl_html <- paste0('<div class="tw"><table><thead><tr><th>Term</th><th>Definition</th></tr></thead><tbody>',
                    paste0("<tr><td class='l'><b>", pp_esc(gl$Term), "</b></td><td class='l wrap'>", pp_esc(gl$Definition), "</td></tr>", collapse = ""), "</tbody></table></div>")
  nfb <- if (is.null(P$n_fallback)) 0L else P$n_fallback
  fb_note <- if (nfb > 0) sprintf(" <b>Italic rows</b> (%d game%s without props yet): recent history adjusted for the implied total and spread, until that game's props post; see the Glossary.",
                                   nfb, if (nfb == 1) "" else "s") else ""
  status <- if (is.na(P$last_pull)) paste0("<b>No props pulled yet this week.</b> Sportsbooks usually post player props from Tuesday–Thursday; the page updates with each refresh.", fb_note) else
    sprintf("<b>Props updated %s ET</b> (median of up to %.0f sportsbooks; %d of %d games with props%s) · weekly bundle %s.",
            pp_et(P$last_pull, "%a %b %d %I:%M %p"), P$books, P$n_priced, P$n_games,
            if (P$n_locked > 0) sprintf(", %d started and locked", P$n_locked) else "", format(P$bundle_time, "%a %b %d")) |> paste0(fb_note)
  b <- P$site_base
  paste0('<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  sprintf("<title>Player projections %d wk %d</title>", P$season, P$week),
  '<style>:root{--bg:#fff;--fg:#1d1d1f;--mut:#666;--bd:#ddd;--th:#f3f3f5;--acc:#2b6cb0}
@media (prefers-color-scheme:dark){:root{--bg:#141416;--fg:#e8e8ea;--mut:#9a9aa0;--bd:#333;--th:#1f1f23;--acc:#7fb0ef}}
body{background:var(--bg);color:var(--fg);font-family:system-ui,sans-serif;max-width:1800px;margin:1.5rem auto;padding:0 16px}
h1{font-size:1.4rem;margin:.2rem 0}.s{color:var(--mut);font-size:13px}.tw{overflow-x:auto}a{color:var(--acc)}
table{border-collapse:collapse;font-size:13px;margin:.6rem 0;white-space:nowrap}th,td{border:1px solid var(--bd);padding:3px 7px;text-align:right}
th{background:var(--th);cursor:pointer;position:sticky;top:0}td.l{text-align:left}td.wrap{white-space:normal;max-width:760px}
.bar{display:flex;flex-wrap:wrap;gap:6px 18px;align-items:center;margin:.6rem 0}
.bar button{background:none;border:1px solid var(--bd);color:var(--fg);padding:6px 12px;border-radius:6px;cursor:pointer;min-height:36px}
.bar button.on{background:var(--acc);color:#fff;border-color:var(--acc)}.pane{display:none}.pane.on{display:block}
i.fill{color:var(--mut)}', RB_CSS, '.sw{display:inline-block;width:12px;height:12px;border:1px solid var(--bd);vertical-align:middle;border-radius:2px}
td.g3_1,td.g4_1{font-weight:700}td.g3_2,td.g4_2{font-weight:600}
:root{--g3_1:#8fd0a3;--g3_2:#c1e7cc;--g3_3:#e6f5ea;--g4_1:#7cc795;--g4_2:#a8ddb8;--g4_3:#cdecd6;--g4_4:#ebf7ee}
@media (prefers-color-scheme:dark){:root{--g3_1:#1f6b3a;--g3_2:#1a5230;--g3_3:#153a23;--g4_1:#1f6b3a;--g4_2:#1b5a33;--g4_3:#17472a;--g4_4:#123320}}
td.g3_1{background:var(--g3_1)!important}td.g3_2{background:var(--g3_2)!important}td.g3_3{background:var(--g3_3)!important}
td.g4_1{background:var(--g4_1)!important}td.g4_2{background:var(--g4_2)!important}td.g4_3{background:var(--g4_3)!important}td.g4_4{background:var(--g4_4)!important}
tr.nop td{font-style:italic}span.nop{font-style:normal;border:1px solid var(--bd);border-radius:4px;padding:0 4px;font-size:11px}
span.inj{color:#c05621;font-weight:700;font-size:12px}span.inj.out{color:#c53030}
@media (prefers-color-scheme:dark){span.inj{color:#f6ad55}span.inj.out{color:#fc8181}}
.sw.fl1{background:var(--fl1)}.sw.fl2{background:var(--fl2)}p.legend{margin:.2rem 0}
td.pz{font-weight:700;text-align:center}td.pz.rb{background:rgba(140,90,220,.24)!important}td.pz.wr{background:rgba(52,120,230,.22)!important}td.pz.te{background:rgba(236,130,40,.26)!important}td.pz.qb{background:rgba(214,64,96,.22)!important}
#srch{display:flex;align-items:center;gap:8px}#psearch{padding:7px 10px;border:1px solid var(--bd);border-radius:6px;background:var(--bg);color:var(--fg);min-height:36px;width:240px;font-size:14px}
@media (max-width:560px){#srch{width:100%}#psearch{flex:1;width:auto}}', SITE_CSS, '</style></head><body>',
  if (exists("site_nav")) site_nav("players", b) else sprintf("<p class='s'><a href='%s'>D/ST</a> · <a href='%sk/'>Kickers</a> · <b>Players</b></p>", b, b),
  sprintf("<h1>Player projections — %d week %d</h1><p class='s'>%s Vegas-only: sportsbook player props (pass / rush / receiving yards, attempts, receptions, pass TDs, INTs, anytime TD) converted to expected stats and scored in your format. In back-tests (2024–26) no model or extra stats beat these at kickoff. Hover a column header for its definition; click to sort.</p>",
          P$season, P$week, status),
  '<div class="bar"><div id="posb">', paste0(sprintf('<button data-pos="%s">%s</button>', c(pos_l, "TR", "GL"), c(pos_l, "Track record", "Glossary")), collapse = ""), '</div>',
  '<div id="fmtb">', paste0(sprintf('<button data-fmt="%s">%s</button>', fmts, unname(PS_FORMATS)), collapse = ""), '</div>',
  '<div id="srch"><input type="search" id="psearch" placeholder="Search player or team" aria-label="Search a player by name, or a team by city or nickname" autocomplete="off"><span id="pscount" class="s"></span></div></div>',
  PP_AMBER, paste0(panes, collapse = ""), paste0(sprintf('<div class="pane" data-pos="TR" data-fmt="%s">%s</div>', PS_FMT_POS$other, vapply(PS_FMT_POS$other, \(f) pp_track(P$track, f), "")), collapse = ""),
  sprintf('<div class="pane" data-pos="GL" data-fmt="*">%s<p class="s">%s</p></div>', gl_html, PP_FILL_NOTE),
  sprintf("<p class='s'>%s</p>", PP_FILL_NOTE),
  '<script>', SITE_JS, '
let st={pos:"QB",fmt:"half"};try{const s=JSON.parse(localStorage.getItem("pp_state")||"{}");if(s.pos)st.pos=s.pos;if(s.fmt)st.fmt=s.fmt}catch(e){}
const FQB=["std","ffpc","pt6"],FOT=["std","half","ppr","ffpc"];
function eff(){const a=st.pos=="QB"?FQB:FOT;return a.includes(st.fmt)?st.fmt:"std"}   /* QB: Half / PPR = Standard; others: 6-pt = Standard */
function show(){const f=eff(),a=st.pos=="QB"?FQB:(st.pos=="GL"?[]:FOT);
document.querySelectorAll(".pane").forEach(p=>p.classList.toggle("on",p.dataset.pos==st.pos&&(p.dataset.fmt==f||p.dataset.fmt=="*")));
document.querySelectorAll("#posb button").forEach(b=>b.classList.toggle("on",b.dataset.pos==st.pos));
document.querySelectorAll("#fmtb button").forEach(b=>{b.classList.toggle("on",b.dataset.fmt==f);b.style.display=a.includes(b.dataset.fmt)?"":"none"});
document.querySelectorAll("p.legend").forEach(l=>l.style.display=(st.pos=="TR"||st.pos=="GL")?"none":"");
document.getElementById("srch").style.display=(st.pos=="TR"||st.pos=="GL")?"none":"";applySearch();
try{localStorage.setItem("pp_state",JSON.stringify(st))}catch(e){};stickCols()}
document.querySelectorAll("#posb button").forEach(b=>b.onclick=()=>{st.pos=b.dataset.pos;show()});
document.querySelectorAll("#fmtb button").forEach(b=>b.onclick=()=>{st.fmt=b.dataset.fmt;show()});show();
function normq(x){return x.toLowerCase().normalize("NFD").replace(/[\\u0300-\\u036f]/g,"").replace(/[^a-z0-9 ]/g,"").trim().split(/\\s+/).filter(Boolean)}
function applySearch(){const q=document.getElementById("psearch");if(!q)return;const toks=normq(q.value);
document.querySelectorAll(".pane tr[data-s]").forEach(r=>{r.style.display=toks.every(t=>r.dataset.s.includes(t))?"":"none"});
const v=document.querySelector(".pane.on"),n=v?[...v.querySelectorAll("tr[data-s]")].filter(r=>r.style.display!="none").length:0;
let msg="";if(toks.length){msg=n?n+" player"+(n==1?"":"s"):"none on this tab";
if(!n){const o=["QB","RB","WR","TE"].map(p=>{const a=p=="QB"?FQB:FOT,f=a.includes(st.fmt)?st.fmt:"std",pn=document.querySelector(".pane[data-pos="+p+"][data-fmt="+f+"]");
const k=pn?[...pn.querySelectorAll("tr[data-s]")].filter(r=>r.style.display!="none").length:0;return k?p+" "+k:""}).filter(Boolean);if(o.length)msg+=" \u00b7 "+o.join(", ")}}
document.getElementById("pscount").textContent=msg}
document.getElementById("psearch").addEventListener("input",applySearch);
function srt(th){const t=th.closest("table"),b=t.tBodies[0],i=[...th.parentNode.children].indexOf(th),d=th.dataset.d=th.dataset.d=="a"?"d":"a";
const v=r=>{const s=r.children[i].innerText.replace(/[%+*\\u{1F512}]/gu,"").trim();const n=parseFloat(s);return isNaN(n)?s:n};
[...b.rows].sort((x,y)=>{const a=v(x),c=v(y);return (a>c?1:a<c?-1:0)*(d=="a"?1:-1)}).forEach(r=>b.appendChild(r))}</script></body></html>')
}
