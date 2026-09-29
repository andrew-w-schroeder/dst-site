# ==============================================================================
# 64_player_page.R — HTML for the Vegas-only player page (sourced by 65_player_refresh.R)
# player_page(P) returns the page: position tabs (QB / RB / WR / TE) × scoring format (Standard, Half PPR,
# PPR, FFPC), one sortable table each. Uses site_utils.R (tiers, tooltips, sticky columns, sparkline).
# ==============================================================================

pp_esc <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); x <- gsub(">", "&gt;", x); gsub('"', "&quot;", x) }
pp_f <- function(x, d = 1) ifelse(is.na(x), "", formatC(x, format = "f", digits = d))
pp_pct <- function(x) ifelse(is.na(x), "", paste0(round(100 * x), "%"))
pp_sg <- function(x, d = 1) ifelse(is.na(x) | abs(x) < 0.5 * 10^-d, "0", sprintf(paste0("%+.", d, "f"), x))
pp_et <- function(x, f) sub(" 0", " ", format(x, f, tz = "America/New_York"))

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
  Player = "Hover or tap for the sportsbook lines behind the projection and which stats had no prop.",
  Kickoff = "Kickoff (Eastern). \U0001F512 = game started: frozen at the last props pulled before kickoff.",
  Imp = "Team implied points from the latest pre-kickoff spread and total (median of sportsbooks, from the D/ST page's line pulls).",
  Proj = "Vegas-only projection: the sportsbook props converted to expected stats (yardage medians corrected for skew, counts via a Poisson fit to line and odds, anytime-TD price calibrated on 2023+ results) and scored in this format.",
  "\u0394" = "Change since the first pull that had props for this player this week.",
  Trend = "Projection at every props pull this week (open dot = first pull with props). Green = up, red = down. Hover a dot for the time.",
  "P(boom)" = "Chance of a boom week: half PPR QB 25+, RB / WR 20+, TE 15+ (other formats: the same share of player-games; FLEX uses each player's own position). Logistic fit per position on the Vegas projection and the anytime-TD price, 2023+: at the same projection a higher TD chance means more boom weeks, strongest for RBs, so RB P(boom) doesn't follow the rank exactly.",
  "P(bust)" = "Chance of a bust week: half PPR QB under 12, RB / WR under 6, TE under 4 (other formats: the same share). Logistic fit on the Vegas projection, 2023+.",
  "TD%" = "Anytime-TD price as a probability (median across books; includes the books' margin). Expected TDs in the projection are calibrated from it.",
  Books = "Sportsbooks behind the median lines (most for any one of this player's markets).",
  "Pass att" = "Expected pass attempts (volume; doesn't score).", "Rush att" = "Expected rushing attempts (volume; doesn't score).",
  Pos = "FLEX tab: the player's rank at his own position in this format.",
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
  "Compared on position ranks, also on FLEX.</p>")
pp_rank_pts <- function(rk, pts) ifelse(is.na(rk), "", ifelse(is.na(pts), as.character(rk), sprintf("%d (%.1f)", as.integer(rk), pts)))
PP_FILL_NOTE <- "Italic* = no prop for this stat: receptions from the receiving-yards prop and the player's yards per catch (or the reverse); otherwise his recency-weighted career average per game, shrunk toward players at his position without that prop."

pp_table <- function(df, raw, id, row_cls, cell_cls, stick = 3) {
  lft <- names(df) %in% c("Player", "Team", "Opp", "Kickoff")
  hdr <- paste0("<tr>", paste0(sprintf('<th title="%s" onclick="srt(this)"%s>%s</th>', pp_esc(coalesce(unname(PP_TIP[names(df)]), "")),
                                       ifelse(lft, ' style="text-align:left"', ""), pp_esc(names(df))), collapse = ""), "</tr>")
  M <- as.matrix(df)
  body <- vapply(seq_len(nrow(df)), function(i) { r <- M[i, ]
    cls <- ifelse(names(df) %in% c("Player", "Team", "Opp", "Kickoff"), "l", "")
    for (cn in intersect(names(cell_cls), names(df))) { k <- which(names(df) == cn); if (nzchar(cell_cls[[cn]][i])) cls[k] <- trimws(paste(cls[k], cell_cls[[cn]][i])) }
    paste0(if (nzchar(row_cls[i])) sprintf('<tr class="%s">', row_cls[i]) else "<tr>",
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

pp_pos_table <- function(P, pos, fmt) {
  if (!nrow(P$cur)) return("<p class='s'>No props posted yet for this position.</p>")
  flex <- pos == "FLEX"
  d <- P$cur |> filter(include, if (flex) pos %in% c("RB", "WR", "TE") else pos == !!pos)
  if (!nrow(d)) return("<p class='s'>No props posted yet for this position.</p>")
  v <- d[[paste0("vfp_", fmt)]]; o <- order(-v); d <- d[o, ]; v <- v[o]
  prk <- ave(-v, d$pos, FUN = \(x) rank(x, ties.method = "first"))          # rank within the player's position
  k <- max(5, min(10, ceiling(nrow(d) / 10)))
  tr <- tiers(v, k = min(k, nrow(d)), clear = 1.5)
  ## trend + change since the first pull with props for this player
  h <- P$hist |> filter(gsis_id %in% d$gsis_id) |> select(gsis_id, game_id, t, proj = !!paste0("vfp_", fmt)) |> arrange(t)
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
              Player = tip_span(paste0(pp_esc(d$player_name), ifelse(d$td_keep, " <span class='s'>(TD)</span>", "")),
                                vapply(seq_len(nrow(d)), \(i) pp_player_tip(d[i, ], fmt), "")),
              Team = d$team, Opp = paste0(ifelse(d$home == 1, "vs ", "@ "), d$opp), Kickoff = ko, Imp = pp_f(d$implied),
              Proj = pp_f(v, 2), "\u0394" = pp_sg(v - first$proj[match(d$gsis_id, first$gsis_id)]),
              Trend = spark, "P(boom)" = pp_pct(d[[paste0("p_boom_", fmt)]]), "P(bust)" = pp_pct(d[[paste0("p_bust_", fmt)]]))
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
    ext_c$ECR <- pp_flag(prk, d$ecr_rank, d$pos)
  }
  for (src in c("ESPN", "Sleeper")) { lo <- tolower(src); rk <- d[[paste0(lo, "_rk_", fmt)]]
    if (!is.null(rk) && any(!is.na(rk))) { cn <- paste(src, "rank")
      pts <- d[[paste0(lo, "_pts_", fmt)]]
      t[[cn]] <- ifelse(is.na(rk), "", ifelse(is.na(pts), lab(rk), sprintf("%s (%.1f)", lab(rk), pts)))
      ext_c[[cn]] <- pp_flag(prk, rk, d$pos) } }
  if (length(ext_c)) t <- t |> relocate(any_of(c("ECR", "ESPN rank", "Sleeper rank")), .after = Proj)
  t <- bind_cols(t, as_tibble(stat_cells), tibble("TD%" = pp_pct(d$p_td_raw),
    Books = do.call(pmax, c(map(grep("^n_books\\.", names(d), value = TRUE), \(c) coalesce(d[[c]], 0)), na.rm = TRUE))))
  brk <- c(FALSE, tr$tier[-1] != tr$tier[-length(tr$tier)])
  row_cls <- trimws(paste(ifelse(tr$tier %% 2 == 1, "tier-odd", ""), ifelse(brk, ifelse(tr$clear[tr$tier] %in% TRUE, "tb-clear", "tb-soft"), "")))
  tc <- tier_cls(tr$tier, k = max(tr$tier))
  pp_table(t, raw = c("Player", "Trend", "ECR", unname(scols)), id = paste0("t_", pos, "_", fmt), row_cls = row_cls,
           cell_cls = c(list(Rank = tc, Player = tc, Proj = tc), ext_c))
}

player_page <- function(P) {
  pos_l <- c(PS_POS, "FLEX"); fmts <- names(PS_FORMATS)
  panes <- unlist(lapply(pos_l, \(p) lapply(fmts, \(f) sprintf('<div class="pane" data-pos="%s" data-fmt="%s">%s</div>', p, f, pp_pos_table(P, p, f)))))
  gl <- tibble(Term = names(PP_TIP), Definition = unname(PP_TIP)) |>
    bind_rows(tibble(Term = c("Italic*", "(TD)", "Formats"), Definition = c(PP_FILL_NOTE,
      "TD-only player: no yardage or receptions prop, shown because his anytime-TD price is at or above the median of players at his position with full props.",
      "Standard / Half PPR / PPR: ESPN defaults (0.04 per pass yard, pass TD 4, INT −2, fumble lost −2, 0 / 0.5 / 1 per catch). FFPC: 0.05 per pass yard, INT −1, fumble lost −1, 1 per catch, 1.5 per TE catch.")))
  gl_html <- paste0('<div class="tw"><table><thead><tr><th>Term</th><th>Definition</th></tr></thead><tbody>',
                    paste0("<tr><td class='l'><b>", pp_esc(gl$Term), "</b></td><td class='l wrap'>", pp_esc(gl$Definition), "</td></tr>", collapse = ""), "</tbody></table></div>")
  status <- if (is.na(P$last_pull)) "<b>No props pulled yet this week.</b> Sportsbooks usually post player props from Tuesday–Thursday; the page updates with each refresh." else
    sprintf("<b>Props updated %s ET</b> (median of up to %.0f sportsbooks; %d of %d games with props%s) · weekly bundle %s.",
            pp_et(P$last_pull, "%a %b %d %I:%M %p"), P$books, P$n_priced, P$n_games,
            if (P$n_locked > 0) sprintf(", %d started and locked", P$n_locked) else "", format(P$bundle_time, "%a %b %d"))
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
i.fill{color:var(--mut)}.sw{display:inline-block;width:12px;height:12px;border:1px solid var(--bd);vertical-align:middle;border-radius:2px}
.sw.fl1{background:var(--fl1)}.sw.fl2{background:var(--fl2)}p.legend{margin:.2rem 0}', SITE_CSS, '</style></head><body>',
  sprintf("<p class='s'><a href='%s'>D/ST</a> · <a href='%sk/'>Kickers</a> · <b>Players</b> · <a href='%splayers/archive/'>past weeks</a></p>", b, b, b),
  sprintf("<h1>Player projections — %d week %d</h1><p class='s'>%s Vegas-only: sportsbook player props (pass / rush / receiving yards, attempts, receptions, pass TDs, INTs, anytime TD) converted to expected stats and scored in your format. In back-tests (2024–26) no model or extra stats beat these at kickoff. Hover a column header for its definition; click to sort.</p>",
          P$season, P$week, status),
  '<div class="bar"><div id="posb">', paste0(sprintf('<button data-pos="%s">%s</button>', c(pos_l, "GL"), c(pos_l, "Glossary")), collapse = ""), '</div>',
  '<div id="fmtb">', paste0(sprintf('<button data-fmt="%s">%s</button>', fmts, unname(PS_FORMATS)), collapse = ""), '</div></div>',
  PP_AMBER, paste0(panes, collapse = ""), sprintf('<div class="pane" data-pos="GL" data-fmt="*">%s<p class="s">%s</p></div>', gl_html, PP_FILL_NOTE),
  sprintf("<p class='s'>%s</p>", PP_FILL_NOTE),
  '<script>', SITE_JS, '
let st={pos:"QB",fmt:"half"};try{const s=JSON.parse(localStorage.getItem("pp_state")||"{}");if(s.pos)st.pos=s.pos;if(s.fmt)st.fmt=s.fmt}catch(e){}
function show(){document.querySelectorAll(".pane").forEach(p=>p.classList.toggle("on",p.dataset.pos==st.pos&&(p.dataset.fmt==st.fmt||p.dataset.fmt=="*")));
document.querySelectorAll("#posb button").forEach(b=>b.classList.toggle("on",b.dataset.pos==st.pos));
document.querySelectorAll("#fmtb button").forEach(b=>b.classList.toggle("on",b.dataset.fmt==st.fmt));
try{localStorage.setItem("pp_state",JSON.stringify(st))}catch(e){};stickCols()}
document.querySelectorAll("#posb button").forEach(b=>b.onclick=()=>{st.pos=b.dataset.pos;show()});
document.querySelectorAll("#fmtb button").forEach(b=>b.onclick=()=>{st.fmt=b.dataset.fmt;show()});show();
function srt(th){const t=th.closest("table"),b=t.tBodies[0],i=[...th.parentNode.children].indexOf(th),d=th.dataset.d=th.dataset.d=="a"?"d":"a";
const v=r=>{const s=r.children[i].innerText.replace(/[%+*\\u{1F512}]/gu,"").trim();const n=parseFloat(s);return isNaN(n)?s:n};
[...b.rows].sort((x,y)=>{const a=v(x),c=v(y);return (a>c?1:a<c?-1:0)*(d=="a"?1:-1)}).forEach(r=>b.appendChild(r))}</script></body></html>')
}
