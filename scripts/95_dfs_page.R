# ==============================================================================
# 95_dfs_page.R — HTML for the DraftKings DFS page (sourced by 96_dfs_refresh.R)
# dfs_page(P) returns the page: QB / RB / WR / TE / FLEX / DST tabs for the DK main slate, one sortable table each.
# Reuses the Players page's table, range-bar and search helpers (64_player_page.R) and site_utils.R; changes neither.
# ==============================================================================

DFS_TIP <- c(
  Player = "Injury badge after the name as on the Players page (official report from Friday, else Sleeper). DK's own status (Q / O / IR) is shown when it differs. \U0001F512 in Kickoff = game started (late swap: locked at the last pre-kickoff projection).",
  Pos = "FLEX tab: the player's rank at his own position by DK projection.",
  Kickoff = "Kickoff (Eastern). \U0001F512 = game started: frozen at its last pre-kickoff props.",
  Salary = "DraftKings salary on the main slate (classic, $50K cap).",
  Proj = "Vegas-only projection in DK scoring: the Players page's expected stats from sportsbook props, scored 4 per pass TD, 0.04 per pass yard, -1 per INT, 0.1 per rush / receiving yard, 1 per catch, 6 per TD, -1 per fumble lost, plus 3 x the chance of each yardage bonus (300+ pass, 100+ rush, 100+ receiving yards). DST (model) tab: your Yahoo D/ST model (DK's D/ST scoring is Yahoo's); DST (Vegas) tab: the Vegas-only D/ST fit (spread, total, home field).",
  "±" = "Model uncertainty of the projection (half of its 90% interval), from the Players page's PPR bootstrap (resampled sportsbooks + calibration refits); DK differs from PPR only by INT / fumble -1 and the bonuses.",
  "Pts/$1K" = "Projection per $1,000 of salary (the usual DFS value number). 4.0 = 4x salary. Cheap players always look good here; see Value.",
  Value = "Projection minus what a typical player at his position and salary on this slate projects for (a straight-line fit of projection on salary per position). + = more points than his price suggests. Unlike Pts/$1K this doesn't favour minimum-price players.",
  "Own%" = "PROJECTED ownership in large DK tournaments (BETA: a placeholder formula, not yet fitted to real contest ownership). Within each position, a share driven by value (points per $1K, strongest), projection, recent DK points (decay-weighted: last game counts as much as all earlier ones together) and team implied total, scaled so the positions add up to DK's 9 roster spots (QB 100%, RB ~235%, WR ~350%, TE ~115%, D/ST 100%).",
  Lev = "Leverage = P(4x) minus projected ownership, in points of %. + = he reaches a tournament-winning score more often than the field plays him (under-owned upside); - = the field is over-exposed to him.",
  "P(boom)" = "Chance of a DK boom week, cut-offs set so they happen as often as the Players page's half-PPR boom (see Glossary). From the outcome distribution at his projection (2023+ results).",
  "P(4x)" = "Chance of scoring at least 4x his salary in DK points (e.g. $7,000 -> 28 points), the usual tournament target. Same outcome distribution as P(boom).",
  Ceiling = "90th percentile of his DK score (1 week in 10 he scores at least this).",
  "Range bar" = "How much his DK score can swing: light band = 80% of outcomes, dark = 50%, line = projection, orange = the ±. Same scale within a tab.",
  Bonus = "Chance of DK's +3 yardage bonus: QB 300+ passing yards; RB 100+ rushing (and 100+ receiving when higher); WR / TE 100+ receiving. Logistic on the expected yards (tighter when a sportsbook line exists), fit on 2023+ results.",
  Imp = "Team implied points from the latest pre-kickoff spread and total. D/ST: the OPPONENT's implied points (lower = better for a defense).",
  Form = "Recent DK points, decay-weighted (x0.5 per game back, this season); used by the ownership model.",
  Last = "His DK points in his most recent game this season (week in brackets).",
  Vegas = "D/ST tab: the Vegas-only D/ST projection (spread, total and home field only, the Yahoo bundle's Vegas fit), for comparison with the model's Proj.",
  Model = "DST Vegas tab: the D/ST model's projection (the D/ST page's Yahoo model), for comparison with this tab's Vegas-only Proj.",
  "Model − Vegas" = "The D/ST model's projection minus the Vegas-only projection. + = the model likes this defense more than the betting lines alone do.")

dfs_pct <- function(x, d = 0) ifelse(is.na(x), "", paste0(formatC(100 * x, format = "f", digits = d), "%"))
dfs_money <- function(x) ifelse(is.na(x), "", paste0("$", formatC(x, format = "d", big.mark = ",")))
## green / red shading for value-type columns: top fifth strong green, next fifth light green, bottom fifth light red
dfs_shade <- function(x, rev = FALSE) { if (all(is.na(x))) return(rep("", length(x))); r <- rank(if (rev) -x else x, na.last = "keep") / sum(!is.na(x))
  ifelse(is.na(r), "", ifelse(r > 0.8, "vg2", ifelse(r > 0.6, "vg1", ifelse(r <= 0.2, "vb1", "")))) }
dfs_lev_cls <- function(x) ifelse(is.na(x), "", ifelse(x >= 0.05, "vg2", ifelse(x >= 0.02, "vg1", ifelse(x <= -0.05, "vb2", ifelse(x <= -0.02, "vb1", "")))))
dfs_own_cls <- function(x) ifelse(is.na(x), "", ifelse(x >= 0.20, "ow3", ifelse(x >= 0.10, "ow2", ifelse(x >= 0.05, "ow1", ""))))

dfs_player_cell <- function(d) {
  st <- if ("inj_status" %in% names(d)) d$inj_status else rep(NA_character_, nrow(d))
  badge <- ifelse(is.na(st) | !nzchar(coalesce(d$inj_badge, "")), "", sprintf(" <span class='inj%s'>(%s)</span>", ifelse(d$inj_out %in% TRUE, " out", ""), d$inj_badge))
  dks <- coalesce(d$dk_status, "")
  dkb <- ifelse(nzchar(dks) & (is.na(st) | dks != coalesce(d$inj_badge, "")), sprintf(" <span class='dks' title='DraftKings status'>DK %s</span>", pp_esc(dks)), "")
  nop <- ifelse(d$fallback %in% TRUE, " <span class='nop'>no props yet</span>", "")
  tip <- paste0("<b>", pp_esc(d$player_name), " (", d$team, ", ", d$pos, ")</b>",
    ifelse(is.na(d$salary), "", sprintf("\nDK %s · projection %.2f = %.2f from the expected stats + %.2f yardage bonus odds", dfs_money(d$salary), d$dk_proj, d$dk_base, d$dk_bonus)),
    ifelse(is.na(d$exp_line) | !nzchar(d$exp_line), "", paste0("\nExpected: ", pp_esc(d$exp_line))),
    ifelse(is.na(st), "", paste0("\n<b>Injury: ", st, "</b>", ifelse(nzchar(coalesce(d$inj_detail, "")), paste0(" — ", pp_esc(d$inj_detail)), ""))),
    ifelse(d$fallback %in% TRUE, "\n<b>No props posted yet for this game:</b> projection from his recent history adjusted for the implied total and spread (Players page).", ""))
  paste0(tip_span(pp_esc(d$player_name), tip), badge, dkb, nop)
}

DFS_STAT_COLS <- list(
  QB = c(pass_yds = "Pass yds", pass_td = "Pass TD", pass_int = "INT", rush_yds = "Rush yds"),
  RB = c(rush_yds = "Rush yds", rec = "Rec", rec_yds = "Rec yds", tds = "TDs"),
  WR = c(rec = "Rec", rec_yds = "Rec yds", tds = "TDs"),
  TE = c(rec = "Rec", rec_yds = "Rec yds", tds = "TDs"),
  FLEX = c(rush_yds = "Rush yds", rec = "Rec", rec_yds = "Rec yds", tds = "TDs"),
  DST = character(), DSTV = character())

dfs_pos_table <- function(P, pos) {
  R <- P$rows
  if (is.null(R) || !nrow(R)) return("<p class='s'>No players yet.</p>")
  flex <- pos == "FLEX"
  d <- R |> filter(if (flex) pos %in% c("RB", "WR", "TE") else pos == !!pos)
  if (!nrow(d)) return("<p class='s'>No players on the slate at this position yet.</p>")
  d <- d |> arrange(desc(dk_proj)); v <- d$dk_proj
  prk <- ave(-v, d$pos, FUN = \(x) rank(x, ties.method = "first"))
  ko <- paste0(ifelse(d$locked %in% TRUE, "\U0001F512 ", ""), pp_et(d$ko, "%a %I:%M %p"))
  is_dst <- d$pos == "DST"
  t <- tibble(Player = dfs_player_cell(d), Team = d$team, Opp = paste0(ifelse(d$home == 1, "vs ", "@ "), d$opp), Kickoff = ko,
              Salary = dfs_money(d$salary), Proj = pp_f(v, 2),
              "±" = ifelse(is.na(d$ci_half), "", pp_f(d$ci_half, 2)),
              "Pts/$1K" = pp_f(d$ppk, 2), Value = ifelse(is.na(d$value), "", pp_sg(d$value, 2)),
              "Own%" = dfs_pct(d$own, 1), Lev = ifelse(is.na(d$lev), "", sprintf("%+.1f", 100 * d$lev)),
              "P(boom)" = dfs_pct(d$p_boom), "P(4x)" = dfs_pct(d$p_4x), Ceiling = pp_f(d$q90, 1))
  hi <- max(35, ceiling(max(d$q90, na.rm = TRUE) / 5) * 5)
  t$`Range bar` <- pp_range_bar(d$q10, d$q25, d$q75, d$q90, v, lo = min(-2, floor(min(d$q10, na.rm = TRUE))), hi = hi,
                                ci_lo = v - d$ci_half, ci_hi = v + d$ci_half)
  if (pos %in% c("DST", "DSTV") && "alt_proj" %in% names(d)) {         # the other D/ST projection, and model - Vegas
    mv <- if (pos == "DST") v - d$alt_proj else d$alt_proj - v
    t <- t |> mutate(!!(if (pos == "DST") "Vegas" else "Model") := pp_f(d$alt_proj, 2), "Model − Vegas" = ifelse(is.na(mv), "", pp_sg(mv, 2)), .after = Proj)
  }
  if (!pos %in% c("DST", "DSTV")) t$Bonus <- dfs_pct(d$p_bonus)
  t$Imp <- pp_f(d$implied)
  scols <- DFS_STAT_COLS[[pos]]
  for (s in names(scols)) {
    x <- d[[paste0("e.", s)]]; x[!is.na(x) & abs(x) < 0.05] <- 0                  # no "-0.0"
    val <- pp_f(x, if (s %in% c("rec", "pass_td", "pass_int", "tds")) 2 else 1)
    t[[scols[[s]]]] <- ifelse(is.na(d[[paste0("e.", s)]]), "", ifelse(d[[paste0("src.", s)]] %in% "prop" | s == "tds", val, paste0("<i class='fill'>", val, "*</i>")))
  }
  if (!pos %in% c("DST", "DSTV")) { t$Form <- pp_f(d$form, 1); t$Last <- ifelse(is.na(d$last_pts), "", sprintf("%.1f (wk %d)", d$last_pts, as.integer(d$last_wk))) }
  if (flex) t <- t |> mutate(Pos = paste0(d$pos, prk), .after = Player)
  cell_cls <- list(`Pts/$1K` = dfs_shade(d$ppk), Value = dfs_shade(d$value), Lev = dfs_lev_cls(d$lev), `Own%` = dfs_own_cls(d$own),
                   `P(4x)` = dfs_shade(d$p_4x))
  if (flex) cell_cls$Pos <- paste0("pz ", tolower(d$pos))
  row_cls <- ifelse(d$fallback %in% TRUE, "nop", "")
  html <- pp_table(t, raw = c("Player", "Range bar", unname(scols)), id = paste0("d_", pos), row_cls = row_cls, cell_cls = cell_cls,
           stick = if (flex) 2 else 1, row_key = pp_search_key(d$player_name, d$team))
  dfs_header_tips(html, names(t))
}
## column headers carry this page's definitions (pp_table puts the Players page's in title=): data-tip, shown by the
## page's own pop-up on hover or tap (Andrew 2026-10-04); stat columns get a short generic definition
dfs_header_tips <- function(html, cols) {
  for (cn in cols) {
    tip <- DFS_TIP[cn]
    st <- c("Pass yds" = "passing yards", "Pass TD" = "passing TDs", INT = "interceptions", "Rush yds" = "rushing yards",
            Rec = "receptions", "Rec yds" = "receiving yards", TDs = "rushing + receiving TDs (from the anytime-TD price)")
    if (is.na(tip)) tip <- if (cn %in% names(st))
      paste0("Expected ", st[[cn]], " from the sportsbook props (italic* = no line for this stat: his career average).") else
      if (cn == "Team") "His team." else if (cn == "Opp") "Opponent (vs = home, @ = away)." else ""
    esc <- gsub("([][(){}.*+?^$|\\\\])", "\\\\\\1", pp_esc(cn))
    html <- sub(sprintf('<th title="[^"]*"( onclick="srt\\(this\\)")?([^>]*)>%s</th>', esc),
                sprintf('<th data-tip="%s"\\1\\2>%s</th>', gsub("\\\\", "\\\\\\\\", pp_esc(tip)), pp_esc(cn)), html)
  }
  html
}

dfs_glossary <- function(P) {
  S <- P$spec; bt <- S$backtest
  cuts <- if (!is.null(S$dk_cut)) paste(sprintf("%s %.1f+", names(S$dk_cut), S$dk_cut), collapse = ", ") else "—"
  acc <- if (!is.null(bt$accuracy)) paste(sprintf("%s RMSE %.2f vs %.2f without the bonus odds", bt$accuracy$pos, bt$accuracy$rmse, bt$accuracy$rmse_no_bonus), collapse = "; ") else ""
  extra <- c(
    "DK scoring" = "Classic: pass TD 4, 0.04 per pass yard, +3 at 300+ pass yards, INT -1; 0.1 per rush / receiving yard, +3 at 100+ rushing and at 100+ receiving; 1 per catch; rush / receiving / return TD 6; 2-pt 2; fumble lost -1. D/ST: sacks 1, INT 2, fumble recovery 2, TD 6, safety 2, blocked kick 2, points allowed 0: 10, 1-6: 7, 7-13: 4, 14-20: 1, 21-27: 0, 28-34: -1, 35+: -4 (the same as Yahoo's).",
    "Main slate" = "The DraftKings classic draft group with no time suffix (not Early / Turbo / Primetime / Showdown), normally the Sunday 1 pm + 4 pm games. Salaries are pulled at every refresh until the slate locks.",
    "DK boom cut-offs" = paste0("The DK score reached as often as the Players page's half-PPR boom (QB 25+, RB / WR 20+, TE 15+): ", cuts, ". D/ST: 10+ (as Yahoo)."),
    "Back-test" = paste0("Leave-one-season-out, ", paste(range(bt$seasons %||% NA), collapse = "-"), ", core players: ", acc, "."),
    "Italic row: no props yet" = "His game's player props aren't posted yet; the projection is the Players page's history-based fallback, in DK scoring.",
    "Italic*" = "No sportsbook line for this stat: from his career average (as on the Players page).",
    "Ownership (BETA)" = "The ownership formula is a placeholder until it is fitted to real DraftKings ownership (contest standings). Treat it as a ranking of who the field is likely to play, not an exact percentage.")
  gl <- tibble(Term = c(names(DFS_TIP), names(extra)), Definition = c(unname(DFS_TIP), unname(extra)))
  paste0('<div class="tw"><table><thead><tr><th>Term</th><th>Definition</th></tr></thead><tbody>',
         paste0("<tr><td class='l'><b>", pp_esc(gl$Term), "</b></td><td class='l wrap'>", pp_esc(gl$Definition), "</td></tr>", collapse = ""), "</tbody></table></div>")
}

dfs_page <- function(P) {
  tabs <- c("QB", "RB", "WR", "TE", "FLEX", "DST", "DSTV")
  tab_lab <- c(QB = "QB", RB = "RB", WR = "WR", TE = "TE", FLEX = "FLEX", DST = "DST (model)", DSTV = "DST (Vegas)")
  panes <- vapply(tabs, \(p) sprintf('<div class="pane" data-pos="%s">%s</div>', p, dfs_pos_table(P, p)), "")
  b <- P$site_base
  slate <- if (is.null(P$slate) || !nrow(P$slate)) paste0("<b>DraftKings main slate not found yet</b> (DK usually posts the next week's salaries Sunday night / Monday); projections below are for every game this week, without salaries.",
                    if (!is.null(P$dk_err) && !is.na(P$dk_err)) sprintf(" <span class='s'>(DK pull: %s)</span>", pp_esc(P$dk_err)) else "") else
    sprintf("<b>DK main slate: %d games</b> (%s) · salaries pulled %s ET%s.", nrow(P$slate), paste(P$slate$label, collapse = ", "),
            pp_et(P$sal_time, "%a %b %d %I:%M %p"), if (identical(P$sal_src, "csv")) " from the uploaded DKSalaries.csv" else "")
  props <- if (is.na(P$last_pull)) "No props pulled yet this week: rows are history-based until they post." else
    sprintf("Props updated %s ET; %d of %d slate games locked.", pp_et(P$last_pull, "%a %b %d %I:%M %p"), P$n_locked, P$n_slate)
  unm <- if (P$n_unmatched > 0) sprintf(" %d DK player%s not shown: ruled out on the injury report, or backups with no props and too little history.", P$n_unmatched, if (P$n_unmatched == 1) "" else "s") else ""
  paste0('<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  sprintf("<title>DraftKings DFS %d wk %d</title>", P$season, P$week),
  '<style>:root{--bg:#fff;--fg:#1d1d1f;--mut:#666;--bd:#ddd;--th:#f3f3f5;--acc:#2b6cb0;--vg2:#8fd0a3;--vg1:#d3eedb;--vb1:#f6d5d5;--vb2:#eba8a8;--ow1:#fdf0d9;--ow2:#f9d9a6;--ow3:#f2b766}
@media (prefers-color-scheme:dark){:root{--bg:#141416;--fg:#e8e8ea;--mut:#9a9aa0;--bd:#333;--th:#1f1f23;--acc:#7fb0ef;--vg2:#1f6b3a;--vg1:#163d25;--vb1:#4a2323;--vb2:#6b2a2a;--ow1:#3b3020;--ow2:#5a4320;--ow3:#7a5418}}
body{background:var(--bg);color:var(--fg);font-family:system-ui,sans-serif;max-width:1800px;margin:1.5rem auto;padding:0 16px}
h1{font-size:1.4rem;margin:.2rem 0}.s{color:var(--mut);font-size:13px}.tw{overflow-x:auto}a{color:var(--acc)}
table{border-collapse:collapse;font-size:13px;margin:.6rem 0;white-space:nowrap}th,td{border:1px solid var(--bd);padding:3px 7px;text-align:right}
th{background:var(--th);cursor:pointer;position:sticky;top:0}td.l{text-align:left}td.wrap{white-space:normal;max-width:760px}
.bar{display:flex;flex-wrap:wrap;gap:6px 18px;align-items:center;margin:.6rem 0}
.bar button{background:none;border:1px solid var(--bd);color:var(--fg);padding:6px 12px;border-radius:6px;cursor:pointer;min-height:36px}
.bar button.on{background:var(--acc);color:#fff;border-color:var(--acc)}.pane{display:none}.pane.on{display:block}
i.fill{color:var(--mut)}td.vg2{background:var(--vg2)!important;font-weight:600}td.vg1{background:var(--vg1)!important}td.vb1{background:var(--vb1)!important}td.vb2{background:var(--vb2)!important}
td.ow1{background:var(--ow1)!important}td.ow2{background:var(--ow2)!important}td.ow3{background:var(--ow3)!important;font-weight:600}
.beta{font-size:11px;font-weight:700;border:1px solid #c05621;color:#c05621;border-radius:4px;padding:0 4px;vertical-align:middle}
tr.nop td{font-style:italic}span.nop{font-style:normal;border:1px solid var(--bd);border-radius:4px;padding:0 4px;font-size:11px}
span.dks{font-style:normal;font-size:11px;color:var(--mut)}
span.inj{color:#c05621;font-weight:700;font-size:12px}span.inj.out{color:#c53030}
@media (prefers-color-scheme:dark){span.inj{color:#f6ad55}span.inj.out{color:#fc8181}.beta{color:#f6ad55;border-color:#f6ad55}}
td.pz{font-weight:700;text-align:center;width:1%;padding:3px 4px;font-size:12px}
#hdrtip{position:absolute;z-index:50;display:none;max-width:340px;padding:8px 10px;border-radius:6px;font-size:12.5px;line-height:1.4;white-space:normal;text-align:left;font-weight:400;
  background:var(--fg);color:var(--bg);box-shadow:0 4px 14px rgba(0,0,0,.25);pointer-events:none}
th[data-tip]{text-decoration:underline dotted;text-underline-offset:3px;text-decoration-color:var(--mut)}td.pz.rb{background:rgba(140,90,220,.24)!important}td.pz.wr{background:rgba(0,170,200,.26)!important}td.pz.te{background:rgba(236,130,40,.26)!important}
@media (prefers-color-scheme:dark){td.pz.rb{background:rgba(140,90,220,.42)!important}td.pz.wr{background:rgba(0,170,200,.42)!important}td.pz.te{background:rgba(236,130,40,.45)!important}}
#srch{display:flex;align-items:center;gap:8px}#psearch{padding:7px 10px;border:1px solid var(--bd);border-radius:6px;background:var(--bg);color:var(--fg);min-height:36px;width:240px;font-size:14px}
.sw{display:inline-block;width:12px;height:12px;border:1px solid var(--bd);vertical-align:middle;border-radius:2px}.sw.vg2{background:var(--vg2)}.sw.vb1{background:var(--vb1)}.sw.ow3{background:var(--ow3)}
p.legend{margin:.2rem 0}
@media (max-width:560px){#srch{width:100%}#psearch{flex:1;width:auto}}', RB_CSS, SITE_CSS, '</style></head><body>',
  site_nav("dfs", b),
  sprintf("<h1>DraftKings DFS — %d week %d</h1>", P$season, P$week),
  sprintf("<p class='s'>%s %s%s Vegas-only projections in DK scoring (the Players page's props, plus the odds of DK's yardage bonuses) next to DK salaries. Hover a column header for its definition; click to sort.</p>", slate, props, unm),
  "<p class='s legend'><span class='sw vg2'></span> green = among the best fifth on the tab (Pts/$1K, Value, P(4x)); <span class='sw vb1'></span> red = worst fifth. ",
  "<b>Own%</b> <span class='beta'>BETA</span> is projected tournament ownership from a placeholder formula (shaded <span class='sw ow3'></span> 20%+), not yet fitted to real DK ownership. ",
  "<b>Lev</b> = P(4x) minus Own%: + = under-owned upside.</p>",
  '<div class="bar"><div id="posb">', paste0(sprintf('<button data-pos="%s">%s</button>', c(tabs, "GL"), c(unname(tab_lab[tabs]), "Glossary")), collapse = ""), '</div>',
  '<div id="srch"><input type="search" id="psearch" placeholder="Search player or team" aria-label="Search a player by name, or a team by city or nickname" autocomplete="off"><span id="pscount" class="s"></span></div></div>',
  paste0(panes, collapse = ""), sprintf('<div class="pane" data-pos="GL">%s</div>', dfs_glossary(P)),
  '<script>', SITE_JS, '
let st={pos:"RB"};try{const s=JSON.parse(localStorage.getItem("dfs_state")||"{}");if(s.pos)st.pos=s.pos}catch(e){}
function show(){document.querySelectorAll(".pane").forEach(p=>p.classList.toggle("on",p.dataset.pos==st.pos));
document.querySelectorAll("#posb button").forEach(b=>b.classList.toggle("on",b.dataset.pos==st.pos));
document.querySelectorAll("p.legend").forEach(l=>l.style.display=st.pos=="GL"?"none":"");
document.getElementById("srch").style.display=st.pos=="GL"?"none":"";applySearch();
try{localStorage.setItem("dfs_state",JSON.stringify(st))}catch(e){};stickCols()}
document.querySelectorAll("#posb button").forEach(b=>b.onclick=()=>{st.pos=b.dataset.pos;show()});
function normq(x){return x.toLowerCase().normalize("NFD").replace(/[\\u0300-\\u036f]/g,"").replace(/[^a-z0-9 ]/g,"").trim().split(/\\s+/).filter(Boolean)}
function applySearch(){const q=document.getElementById("psearch");if(!q)return;const toks=normq(q.value);
document.querySelectorAll(".pane tr[data-s]").forEach(r=>{r.style.display=toks.every(t=>r.dataset.s.includes(t))?"":"none"});
const v=document.querySelector(".pane.on"),n=v?[...v.querySelectorAll("tr[data-s]")].filter(r=>r.style.display!="none").length:0;
let msg="";if(toks.length){msg=n?n+" player"+(n==1?"":"s"):"none on this tab";
if(!n){const o=["QB","RB","WR","TE","DST"].map(p=>{const pn=document.querySelector(".pane[data-pos="+p+"]");
const k=pn?[...pn.querySelectorAll("tr[data-s]")].filter(r=>r.style.display!="none").length:0;return k?p+" "+k:""}).filter(Boolean);if(o.length)msg+=" \\u00b7 "+o.join(", ")}}
document.getElementById("pscount").textContent=msg}
document.getElementById("psearch").addEventListener("input",applySearch);show();
const tipEl=document.createElement("div");tipEl.id="hdrtip";document.body.appendChild(tipEl);let tipT=null;
function tipShow(th){const t=th.dataset.tip;if(!t)return;tipEl.textContent=t;tipEl.style.display="block";const r=th.getBoundingClientRect(),w=tipEl.offsetWidth,
vw=document.documentElement.clientWidth;tipEl.style.left=(window.scrollX+Math.max(8,Math.min(r.left,vw-w-8)))+"px";tipEl.style.top=(window.scrollY+r.bottom+6)+"px"}
function tipHide(){tipEl.style.display="none"}
document.querySelectorAll("th[data-tip]").forEach(th=>{th.addEventListener("mouseenter",()=>tipShow(th));th.addEventListener("mouseleave",tipHide);
th.addEventListener("touchstart",()=>{tipShow(th);clearTimeout(tipT);tipT=setTimeout(tipHide,4000)},{passive:true})});
window.addEventListener("scroll",tipHide,{passive:true});
function srt(th){const t=th.closest("table"),b=t.tBodies[0],i=[...th.parentNode.children].indexOf(th),d=th.dataset.d=th.dataset.d=="a"?"d":"a";
const v=r=>{const s=r.children[i].innerText.replace(/[$,%+*\\u{1F512}]/gu,"").trim();const n=parseFloat(s);return isNaN(n)?s:n};
[...b.rows].sort((x,y)=>{const a=v(x),c=v(y);return (a>c?1:a<c?-1:0)*(d=="a"?1:-1)}).forEach(r=>b.appendChild(r))}</script></body></html>')
}
