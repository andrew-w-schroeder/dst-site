# ==============================================================================
# 54_k_report.R — kicker projections report (ESPN + decimal) from report parts
#
# Reads output/k/report_parts_<season>_wk<ww>.rds (written by 50_k_model.R, or the re-scored copy that
# 55_k_refresh.R writes under work/) and writes output/k/k_proj_<season>_wk<ww>.html.
# Tabs: Compare · ESPN · Decimal · Back-test · Glossary. Refreshed parts add kickoff / lock / Δ Proj columns
# and a "lines updated" status line. Only dplyr / tidyr / purrr / tibble, so it runs on the GitHub runner.
# Usage: Rscript 54_k_report.R [season] [week]      (defaults: newest report parts)
# ==============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(purrr); library(tibble) })
if (!isTRUE(l10n_info()$`UTF-8`)) invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")))
PROJ_DIR <- Sys.getenv("FF_PROJ_DIR", path.expand("~/ML/ff"))
K_DIR    <- file.path(PROJ_DIR, "output/k")
a <- commandArgs(trailingOnly = TRUE)
if (length(a) >= 2) { SEASON <- as.integer(a[1]); WEEK <- as.integer(a[2]) } else {
  f <- list.files(K_DIR, pattern = "^report_parts_\\d{4}_wk\\d{2}\\.rds$")
  if (!length(f)) stop("no kicker report parts: run 50_k_model.R first")
  key <- max(sub("report_parts_(\\d{4})_wk(\\d{2}).*", "\\1\\2", f)); SEASON <- as.integer(substr(key, 1, 4)); WEEK <- as.integer(substr(key, 5, 6))
}
P <- readRDS(file.path(K_DIR, sprintf("report_parts_%d_wk%02d.rds", SEASON, WEEK)))
pred <- P$pred; SC <- P$SCORING; glossary <- P$glossary
refreshed <- !is.null(P$refresh)
SCRIPT_DIR <- local({ f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)); if (length(f)) dirname(normalizePath(f[1])) else file.path(PROJ_DIR, "scripts") })
source(file.path(SCRIPT_DIR, "site_utils.R"))
fit_time <- if (refreshed && !is.null(P$refresh$model_fit)) P$refresh$model_fit else P$generated
DLAB <- sprintf("Δ since %s run", format(fit_time, "%a", tz = "America/New_York"))       # always vs the weekly run
DLAB_IMP <- sprintf("Δ Imp since %s run", format(fit_time, "%a", tz = "America/New_York"))

## ---- helpers ----
esc <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); x <- gsub(">", "&gt;", x); gsub('"', "&quot;", x) }
tip <- setNames(glossary$definition, glossary$term)
f1 <- function(x, d = 1) formatC(x, format = "f", digits = d)
pct <- function(x) ifelse(is.na(x), "", paste0(round(100 * x), "%"))
signed <- function(x, d = 2) ifelse(is.na(x) | abs(x) < 0.5 * 10^-d, "0", sprintf(paste0("%+.", d, "f"), x))
et <- function(x, f) ifelse(is.na(x), NA_character_, sub(" 0", " ", format(x, f, tz = "America/New_York")))
range_bar <- function(q10, q25, q75, q90, pj, lo = -2, hi = 22) {
  sc <- function(v) round(100 * (pmin(pmax(v, lo), hi) - lo) / (hi - lo), 1)
  sprintf('<div class="rb" title="80%%: %.1f to %.1f · 50%%: %.1f to %.1f · proj %.2f"><span class="r80" style="left:%s%%;width:%s%%"></span><span class="r50" style="left:%s%%;width:%s%%"></span><span class="pj" style="left:%s%%"></span></div>',
          q10, q90, q25, q75, pj, sc(q10), sc(q90) - sc(q10), sc(q25), sc(q75) - sc(q25), sc(pj))
}
html_table <- function(df, raw = character(), id = "", row_cls = NULL, cell_cls = list()) {
  hdr <- paste0("<tr>", paste0(sprintf('<th title="%s" onclick="srt(this)">%s</th>', esc(coalesce(tip[names(df)], "")), esc(names(df))), collapse = ""), "</tr>")
  M <- as.matrix(df)
  body <- vapply(seq_len(nrow(df)), function(i) { r <- M[i, ]
    txt <- !grepl("^[-+−0-9.,%–/ NA]*$", r) & !(names(df) %in% raw) | names(df) %in% c("Kicker")
    cls <- ifelse(txt, "l", "")
    for (cn in intersect(names(cell_cls), names(df))) { k <- which(names(df) == cn); if (nzchar(cell_cls[[cn]][i])) cls[k] <- trimws(paste(cls[k], cell_cls[[cn]][i])) }
    paste0(if (!is.null(row_cls) && nzchar(row_cls[i])) sprintf('<tr class="%s">', row_cls[i]) else "<tr>",
           paste0(ifelse(nzchar(cls), sprintf('<td class="%s">', cls), "<td>"), ifelse(names(df) %in% raw, r, esc(r)), "</td>", collapse = ""), "</tr>") }, "")
  sprintf('<div class="tw"><table id="%s"><thead>%s</thead><tbody>%s</tbody></table></div>', id, hdr, paste(body, collapse = ""))
}
kickoff <- function(p) {
  ko <- if ("ko" %in% names(p)) p$ko else as.POSIXct(NA)
  sched <- as.POSIXct(paste(p$gameday, p$gametime), tz = "America/New_York")
  t <- ifelse(is.na(ko), format(sched, "%a %H:%M"), et(ko, "%a %H:%M"))
  paste0(ifelse(coalesce(p$locked, FALSE), "\U0001F512 ", ""), t)
}
if (!"locked" %in% names(pred)) pred$locked <- FALSE
wind_lab <- ifelse(pred$indoor == 1, "indoor", paste0(round(pred$wind), ifelse(pred$wind_known == 1, "", "*")))
gust_lab <- if ("wx_gust" %in% names(pred)) ifelse(pred$indoor == 1 | is.na(pred$wx_gust), "", as.character(round(pred$wx_gust))) else rep("", nrow(pred))
rain_lab <- if ("wx_precip_prob" %in% names(pred)) ifelse(pred$indoor == 1 | is.na(pred$wx_precip_prob), "",
                paste0(round(pred$wx_precip_prob), "%", if ("wx_precip_in" %in% names(pred)) ifelse(is.na(pred$wx_precip_in), "", paste0(" · ", rain_in(pred$wx_precip_in))) else "")) else rep("", nrow(pred))
# injury status from the refresh (starters.R): (Q) / (D) / (O) after the name; Out → ⚠ + likely replacement in the hover
K_ABBR <- c(Out = "O", Doubtful = "D", Questionable = "Q", Suspended = "SUS", IR = "IR", PUP = "PUP", NFI = "NFI")
k_badge <- function(p) if (!"k_status" %in% names(p)) "" else
  paste0(ifelse(is.na(p$k_status), "", paste0(" (", dplyr::coalesce(unname(K_ABBR[p$k_status]), p$k_status), ")")), ifelse(p$k_out %in% TRUE, " \u26A0", ""))
k_note <- function(p) if (!"k_status" %in% names(p)) "" else
  paste0(ifelse(is.na(p$k_status), "", paste0("\n<b>Status: ", p$k_status, "</b> (", esc(p$k_status_detail), ")")),
         ifelse(p$k_out %in% TRUE, paste0("\n\u26A0 Listed kicker ruled out", ifelse(is.na(p$k_alt), "", paste0("; likely replacement: ", esc(p$k_alt))),
                                          ". The projection still uses the listed kicker's skill (team, Vegas and weather terms carry over)."), ""))
why_tip <- function(p, sy) { w <- p[[paste0("why_", sy)]]; if (is.null(w)) return(paste0(esc(p$kicker), k_badge(p)))
  tip_span(paste0(esc(p$kicker), k_badge(p)), paste0("<b>", esc(p$kicker), " (", p$team, ") — ", SC[[sy]]$label, " ", sprintf("%.2f", p[[paste0("proj_", sy)]]),
                                 "</b>", k_note(p), "\nWhat moves this projection vs an average kicker this week (points):\n", esc(w),
                                 "\n<i>Vegas lines excluded; each group set to this week's league average in turn.</i>")) }
rank_cls <- function(r) ifelse(r <= 8, "top", ifelse(r >= 25, "bot", ""))
opp_lab <- paste0(ifelse(pred$home == 1, "vs ", "@ "), pred$opp)

## ---- tabs ----
sys_table <- function(sy) {
  o <- order(pred[[paste0("rank_", sy)]]); p <- pred[o, ]
  tr <- tiers(p[[paste0("proj_", sy)]], clear = median(p[[paste0("pm_", sy)]], na.rm = TRUE))
  t <- tibble(Rank = p[[paste0("rank_", sy)]], Tier = tr$tier, Kicker = why_tip(p, sy), Team = p$team, Opp = opp_lab[o], Kickoff = kickoff(p),
              Wind = wind_lab[o], Gust = gust_lab[o], Rain = rain_lab[o], Imp = f1(p$implied_own), Proj = f1(p[[paste0("proj_", sy)]], 2))
  if (refreshed) t[[DLAB]] <- signed(p[[paste0("proj_", sy)]] - p[[paste0("proj_base_", sy)]])
  if (paste0("trend_svg_", sy) %in% names(p)) t$Trend <- p[[paste0("trend_svg_", sy)]]
  brk <- c(FALSE, tr$tier[-1] != tr$tier[-length(tr$tier)])
  attr(t, "row_cls") <- trimws(paste(ifelse(tr$tier %% 2 == 1, "tier-odd", ""), ifelse(brk, ifelse(tr$clear[tr$tier] %in% TRUE, "tb-clear", "tb-soft"), "")))
  attr(t, "cell_cls") <- list(Rank = rank_cls(t$Rank), Kicker = rank_cls(t$Rank))
  t %>% mutate(
    `±` = f1(p[[paste0("pm_", sy)]], 2), `90% CI` = paste0(f1(p[[paste0("ci_lo_", sy)]]), "–", f1(p[[paste0("ci_hi_", sy)]])),
    `Range bar` = range_bar(p[[paste0("q10_", sy)]], p[[paste0("q25_", sy)]], p[[paste0("q75_", sy)]], p[[paste0("q90_", sy)]], p[[paste0("proj_", sy)]]),
    `80% range` = paste0(f1(p[[paste0("q10_", sy)]]), "–", f1(p[[paste0("q90_", sy)]])),
    `P(boom)` = pct(p[[paste0("p_boom_", sy)]]), `P(bust)` = pct(p[[paste0("p_bust_", sy)]]), `P(top N)` = pct(p[[paste0("p_top_", sy)]]),
    `E[FGA]` = f1(p$e_fga, 2), `E[50+]` = f1(p$e_a_50p, 2), `E[XP]` = f1(p$e_xp, 2), `P(make) 40s / 50+` = paste0(pct(p$p_40s), " / ", pct(p$p_50p)),
    `Career FG%` = pct(p$k_fg_pct), `Career 50+%` = pct(p$k_fg50_pct), `Career XP%` = pct(p$k_xp_pct), `Career FGA` = p$k_career_fga,
    `Coach GROE` = if ("c_groe" %in% names(p)) sprintf("%+.1f%%", 100 * p$c_groe) else sprintf("%+.1f%%", 100 * p$c_go_oe), Inj = coalesce(p$injury, ""))
}
tip[c("Career FG%", "Career 50+%", "Career XP%", "Career FGA", "Coach GROE")] <- c(tip["k_fg_pct"], tip["k_fg50_pct"], tip["k_xp_pct"],
  "Career FG attempts before this game (nflverse pbp since 2004).", if (!is.na(tip["c_groe"])) tip["c_groe"] else tip["c_go_oe"])
tip[c("ESPN proj", "Dec proj", "Avg rank", "Rank spread", "Kickoff", DLAB, DLAB_IMP, "Tier", "ESPN tier", "Dec tier", "Trend", "Gust", "Rain", "Kicker")] <- c(
  "ESPN projection.", "Decimal projection.", "Mean of the two ranks.",
  "|ESPN rank − Decimal rank|.", "Kickoff (Eastern). \U0001F512 = game started: frozen at the last pre-kickoff line.",
  "Change in the projection since the weekly (Tuesday) model run — not since the previous refresh — from Vegas line and weather-forecast moves only.",
  "Change in the team's implied points since the weekly model run.",
  "Natural-break tier (optimal 1-D grouping into 6 tiers; 1 = best). Solid line above a tier = the drop is bigger than the model's typical ± (clear break); dashed = softer break.",
  "ESPN tier (see Tier).", "Decimal tier (see Tier).",
  "Projection over time: open dot = weekly model run, filled dots = one per day the page was refreshed (last value that day). Green = up since the weekly run, red = down. Hover a dot for date and value.",
  "Forecast wind gust (mph, max over kickoff + 2 h; Open-Meteo). Display only: the model uses sustained wind.",
  "Forecast chance of rain (%, max hourly over kickoff + 2 h) and expected rain (inches, total over those 3 hours); Open-Meteo. Display only: not a model input.",
  "Hover or tap a kicker for what moves his projection vs an average kicker this week (Vegas lines excluded). Green = top 8, red = bottom 8. (Q) / (D) / (O) = injury status checked at each refresh (official report, Sleeper, Ourlads); \u26A0 = ruled out, hover for the likely replacement.")
cmp <- pred %>% mutate(avg_rank = (rank_espn + rank_dec) / 2, opp_lab = opp_lab, ko_lab = kickoff(pred), wl = wind_lab) %>% arrange(avg_rank)
oc <- match(cmp$team, pred$team)
cmp_tbl <- cmp %>% transmute(Kicker = vapply(seq_len(nrow(cmp)), function(i) why_tip(cmp[i, ], "espn"), ""), Team = team, Opp = opp_lab, Kickoff = ko_lab, Wind = wl,
                             Gust = gust_lab[oc], Rain = rain_lab[oc], Imp = f1(implied_own))
if (refreshed) cmp_tbl[[DLAB_IMP]] <- signed(cmp$implied_own - cmp$implied_own_base, 1)
cmp_tbl <- cmp_tbl %>% mutate(`ESPN proj` = f1(cmp$proj_espn, 2), `ESPN rank` = cmp$rank_espn, `ESPN tier` = tiers(cmp$proj_espn)$tier,
  `Dec proj` = f1(cmp$proj_dec, 2), `Dec rank` = cmp$rank_dec, `Dec tier` = tiers(cmp$proj_dec)$tier,
  `Avg rank` = f1(cmp$avg_rank), `Rank spread` = abs(cmp$rank_espn - cmp$rank_dec), `ESPN P(top N)` = pct(cmp$p_top_espn),
  `Dec P(top N)` = pct(cmp$p_top_dec), `E[50+]` = f1(cmp$e_a_50p, 2), Inj = coalesce(cmp$injury, ""))
tabs <- c("Compare", SC$espn$label, SC$dec$label, "Back-test", "Glossary")
pos_cls <- rank_cls(ifelse(seq_len(nrow(cmp)) > nrow(cmp) - 8, 25, seq_len(nrow(cmp))))          # by average rank
sys_html <- function(sy) { t <- sys_table(sy)
  paste0("<p class='s'>", esc(SC[[sy]]$long), " · Tiers: natural breaks (solid line = clear drop, dashed = softer). Hover or tap a kicker for what drives the projection.</p>",
         html_table(t, raw = c("Range bar", "Trend", "Kicker"), id = paste0("t_", sy), row_cls = attr(t, "row_cls"), cell_cls = attr(t, "cell_cls"))) }
panes <- c(paste0("<p class='s'>Green = top 8 / red = bottom 8 (Kicker: by average rank). Hover or tap a kicker for what drives the ESPN projection.</p>",
                  html_table(cmp_tbl, id = "t_cmp", raw = "Kicker", cell_cls = list(Kicker = pos_cls, `ESPN rank` = rank_cls(cmp$rank_espn), `Dec rank` = rank_cls(cmp$rank_dec)))),
           sys_html("espn"), sys_html("dec"),
           P$backtest_html, html_table(glossary))

## ---- page ----
status <- if (!refreshed) sprintf("Model run %s.", format(P$generated, "%a %b %d %H:%M")) else {
  r <- P$refresh
  sprintf("<b>Lines / weather updated %s ET</b> (%s; weather: %s) · model run %s.", et(r$time, "%a %b %d %I:%M %p"),
          if (r$n_priced > 0) sprintf("median of %.0f sportsbooks, %d of %d games priced%s", r$books, r$n_priced, r$n_games,
                                     if (r$n_locked > 0) sprintf(", %d started", r$n_locked) else "") else "no sportsbook lines yet: weekly-run lines",
          r$weather, format(fit_time, "%a %b %d"))
}
html <- paste0('<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  sprintf("<title>Kicker projections %d wk %d</title>", SEASON, WEEK),
  '<style>:root{--bg:#fff;--fg:#1d1d1f;--mut:#666;--bd:#ddd;--th:#f3f3f5;--acc:#2b6cb0;--r80:#c9dcf2;--r50:#6f9fd8}
@media (prefers-color-scheme:dark){:root{--bg:#141416;--fg:#e8e8ea;--mut:#9a9aa0;--bd:#333;--th:#1f1f23;--acc:#7fb0ef;--r80:#2a3f5c;--r50:#4f78ad}}
body{background:var(--bg);color:var(--fg);font-family:system-ui,sans-serif;max-width:1400px;margin:1.5rem auto;padding:0 16px}
h1{font-size:1.4rem;margin:.2rem 0}.s{color:var(--mut);font-size:13px}.tw{overflow-x:auto}a{color:var(--acc)}
table{border-collapse:collapse;font-size:13px;margin:.6rem 0;white-space:nowrap}th,td{border:1px solid var(--bd);padding:3px 7px;text-align:right}
th{background:var(--th);cursor:pointer;position:sticky;top:0}td.l{text-align:left}
.tabs button{background:none;border:1px solid var(--bd);color:var(--fg);padding:6px 12px;margin:0 4px 4px 0;border-radius:6px 6px 0 0;cursor:pointer}
.tabs button.on{background:var(--acc);color:#fff;border-color:var(--acc)}.pane{display:none}.pane.on{display:block}
.rb{position:relative;width:150px;height:12px}.rb span{position:absolute;top:0;height:12px}.r80{background:var(--r80)}.r50{background:var(--r50)}
.pj{width:2px;background:var(--fg)}
td.top{background:#e3f4e8;font-weight:600}td.bot{background:#fbe6e6}
@media (prefers-color-scheme:dark){td.top{background:#17351f}td.bot{background:#3a1a1a}}', SITE_CSS, '</style></head><body>',
  if (!is.null(P$nav)) P$nav else "",
  sprintf("<h1>Kicker projections — %d week %d</h1><p class='s'>%s Blend of elastic net + component model, trained 2015 → last completed week. Kicker = depth-chart PK1 (fallback: last kicker used). Hover a column header for its definition; click to sort. Wind * = no forecast, outdoor median used.</p>",
          SEASON, WEEK, status),
  '<div class="tabs">', paste0(sprintf('<button onclick="tab(%d)"%s>%s</button>', seq_along(tabs) - 1, ifelse(seq_along(tabs) == 1, ' class="on"', ""), tabs), collapse = ""), "</div>",
  paste0(sprintf('<div class="pane%s">%s</div>', ifelse(seq_along(panes) == 1, " on", ""), panes), collapse = ""),
  '<script>function tab(i){document.querySelectorAll(".pane").forEach((p,j)=>p.classList.toggle("on",i==j));document.querySelectorAll(".tabs button").forEach((b,j)=>b.classList.toggle("on",i==j))}
function srt(th){const t=th.closest("table"),b=t.tBodies[0],i=[...th.parentNode.children].indexOf(th),d=th.dataset.d=th.dataset.d=="a"?"d":"a";
const v=r=>{const s=r.children[i].innerText.replace(/[%+±*\\u{1F512}]/gu,"").trim();const n=parseFloat(s);return isNaN(n)?s:n};
[...b.rows].sort((x,y)=>{const a=v(x),c=v(y);return (a>c?1:a<c?-1:0)*(d=="a"?1:-1)}).forEach(r=>b.appendChild(r))}</script></body></html>')
out <- file.path(K_DIR, sprintf("k_proj_%d_wk%02d.html", SEASON, WEEK))
writeLines(html, out)
message("wrote ", out)
