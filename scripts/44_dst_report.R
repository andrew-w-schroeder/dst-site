# ==============================================================================
# 44_dst_report.R — one HTML (+ markdown) report with ESPN, Yahoo and FFPC D/ST projections
#
# Reads output/dst/<system>/report_parts_<season>_wk<ww>.rds written by 40_dst_model.R and writes
#   output/dst/dst_proj_<season>_wk<ww>_all.{html,md}
# Tabs: Compare (all three side by side, sortable) · one tab per scoring system · Glossary.
# Usage: Rscript 44_dst_report.R [season] [week]   (defaults: the newest report parts found)
# ==============================================================================

## ---- 1. Setup ----
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(purrr); library(stringr); library(tibble) })
if (!l10n_info()$`UTF-8`) invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))     # e.g. a bare CI shell: keep –, Δ and 🔒 intact
PROJ_DIR <- Sys.getenv("FF_PROJ_DIR", path.expand("~/ML/ff"))
SCRIPT_DIR <- local({ f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)); if (length(f)) dirname(normalizePath(f[1])) else file.path(PROJ_DIR, "scripts") })
if (file.exists(file.path(SCRIPT_DIR, "site_utils.R"))) source(file.path(SCRIPT_DIR, "site_utils.R")) else stop("site_utils.R not found next to 44_dst_report.R")
DST_DIR  <- file.path(PROJ_DIR, "output/dst")
SYSTEMS  <- c("espn", "yahoo", "ffpc")
a <- commandArgs(trailingOnly = TRUE)
if (length(a) >= 2) { SEASON <- as.integer(a[1]); WEEK <- as.integer(a[2]) } else {
  f <- list.files(file.path(DST_DIR, SYSTEMS), pattern = "^report_parts_\\d{4}_wk\\d{2}\\.rds$", full.names = TRUE)
  if (!length(f)) stop("no report parts found: run 40_dst_model.R first")
  key <- max(sub(".*report_parts_(\\d{4})_wk(\\d{2}).*", "\\1\\2", f))
  SEASON <- as.integer(substr(key, 1, 4)); WEEK <- as.integer(substr(key, 5, 6))
}
parts <- set_names(SYSTEMS) %>% map(~ {
  f <- file.path(DST_DIR, .x, sprintf("report_parts_%d_wk%02d.rds", SEASON, WEEK)); if (file.exists(f)) readRDS(f) else NULL
}) %>% compact()
if (!length(parts)) stop(sprintf("no report parts for %d week %d", SEASON, WEEK))
missing <- setdiff(SYSTEMS, names(parts)); if (length(missing)) warning("missing systems (not run yet): ", paste(missing, collapse = ", "))
message("building combined report for ", SEASON, " week ", WEEK, ": ", paste(map_chr(parts, ~ .x$SC$label), collapse = ", "))

## ---- 2. Comparison table ----
# Refreshed weeks (45_dst_refresh.R) carry kickoff times, lock flags and the weekly-run baseline in `pred`.
refreshed <- !is.null(parts[[1]]$refresh)
# Δ columns are always measured from the weekly (Tuesday) model run, not from the previous refresh
DLAB <- if (refreshed) sprintf("Δ since %s run", format(parts[[1]]$refresh$model_fit, "%a", tz = "America/New_York")) else "Δ Proj"
DLAB_OPP <- if (refreshed) sprintf("Δ Opp implied since %s run", format(parts[[1]]$refresh$model_fit, "%a", tz = "America/New_York")) else "Δ Opp implied"
esc <- function(x) { x <- gsub("&", "&amp;", as.character(x), fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub('"', "&quot;", gsub(">", "&gt;", x, fixed = TRUE), fixed = TRUE) }
why_tip <- function(team, proj, why, label) ifelse(is.na(why), esc(team), tip_span(esc(team),
  paste0("<b>", esc(team), " D/ST — ", label, " ", sprintf("%.2f", proj), "</b>\nWhat moves this projection vs an average D/ST this week (points):\n",
         esc(why), "\n<i>Vegas lines excluded; each group set to this week's league average in turn.</i>")))
et <- function(x, f) ifelse(is.na(x), NA_character_, sub(" 0", " ", format(x, f, tz = "America/New_York")))
kickoff <- function(p) ifelse(is.na(p$ko), "", paste0(ifelse(p$locked, "\U0001F512 ", ""), et(p$ko, "%a %I:%M %p")))
signed <- function(x, d = 1) ifelse(is.na(x) | abs(x) < 0.5 * 10^-d, "0", sprintf(paste0("%+.", d, "f"), x))
make_proj_tbl <- function(pred, html = FALSE, label = "") {       # same columns as 40_dst_model.R section 11, from (possibly refreshed) pred
  fmt0 <- function(x) as.character(as.integer(round(x)))
  t <- pred %>% transmute(Rank = rank, Team = team, Opp = paste0(ifelse(home == 1, "vs ", "@ "), opp), `Opp QB` = opp_qb_name,
                          Spread = spread, `Opp implied` = round(implied_opp, 1), Proj = round(proj, 2),
                          `±` = sprintf("±%.1f", (ci_hi - ci_lo) / 2), `90% CI` = sprintf("%.1f–%.1f", ci_lo, ci_hi), `80% range` = paste(fmt0(q10), "to", fmt0(q90)),
                          `P(10+)` = sprintf("%.0f%%", 100 * p_boom), `P(<3)` = sprintf("%.0f%%", 100 * p_bust), `P(top 8)` = sprintf("%.0f%%", 100 * p_top8),
                          `E[sacks]` = round(e_sacks, 2), `E[TO]` = round(e_to, 2), `PA pts` = round(e_pa, 2),
                          `YA pts` = round(e_ya, 2), `P(TD)` = round(p_td, 2), Venue = ifelse(indoor == 1, "indoor", "outdoor"),
                          `Opp QB hist %` = round(100 * o_qb_cont), `Opp caller hist %` = round(100 * o_pc_cont))
  if ("proj_base" %in% names(pred)) t <- t %>% mutate(!!DLAB := signed(pred$proj - pred$proj_base, 2), .after = Proj) %>%
    mutate(Kickoff = kickoff(pred), .after = Opp)
  if (html && "trend_svg" %in% names(pred)) t <- t %>% mutate(Trend = pred$trend_svg, .after = all_of(DLAB))
  if ("wx_wind" %in% names(pred)) t <- t %>% mutate(Weather = wx_label(pred$indoor, pred$wx_temp, pred$wx_wind, pred$wx_gust, pred$wx_precip_prob, pred$wx_precip_in), .after = Venue)
  t <- t %>% mutate(Tier = tiers(pred$proj)$tier, .after = Rank)
  if (html && "why" %in% names(pred)) t$Team <- why_tip(pred$team, pred$proj, pred$why, label)
  t
}
base <- parts[[1]]$pred %>% transmute(team, Opp = paste0(ifelse(home == 1, "vs ", "@ "), opp), `Opp QB` = opp_qb_name,
                                      Spread = spread, `Opp implied` = round(implied_opp, 1))
if (refreshed) base <- base %>% mutate(Kickoff = kickoff(parts[[1]]$pred), .after = Opp) %>%
  mutate(`Δ Opp implied` = signed(parts[[1]]$pred$implied_opp - parts[[1]]$pred$implied_opp_base), .after = `Opp implied`)
if ("wx_wind" %in% names(parts[[1]]$pred)) base <- base %>% mutate(Weather = wx_label(parts[[1]]$pred$indoor, parts[[1]]$pred$wx_temp, parts[[1]]$pred$wx_wind,
                                                                                  parts[[1]]$pred$wx_gust, parts[[1]]$pred$wx_precip_prob, parts[[1]]$pred$wx_precip_in), .after = `Opp implied`)
if (refreshed) base <- base %>% rename(!!DLAB_OPP := `Δ Opp implied`)
cmp <- reduce(imap(parts, function(p, s) p$pred %>% transmute(team, !!paste(p$SC$label, "proj") := round(proj, 2), !!paste(p$SC$label, "rank") := as.integer(rank),
                                                                !!paste(p$SC$label, "tier") := tiers(proj)$tier,
                                                                !!paste(p$SC$label, "P(top 8)") := if ("p_top8" %in% names(p$pred)) sprintf("%.0f%%", 100 * p_top8) else NA_character_)),
              left_join, by = "team", .init = base)
rank_cols <- grep(" rank$", names(cmp), value = TRUE)
cmp <- cmp %>% mutate(`Avg rank` = round(rowMeans(across(all_of(rank_cols))), 1),
                      `Rank spread` = do.call(pmax, across(all_of(rank_cols))) - do.call(pmin, across(all_of(rank_cols)))) %>%
  arrange(`Avg rank`) %>% rename(Team = team)
cmp_cls <- ifelse(seq_len(nrow(cmp)) <= 8, "top", ifelse(seq_len(nrow(cmp)) > nrow(cmp) - 8, "bot", ""))     # by average rank
p1 <- parts[[1]]$pred[match(cmp$Team, parts[[1]]$pred$team), ]
cmp_html <- cmp %>% mutate(Team = if ("why" %in% names(p1)) why_tip(Team, p1$proj, p1$why, parts[[1]]$SC$label) else esc(Team))

## ---- 3. Glossary (shared) ----
g0 <- parts[[1]]
compare_gloss <- tribble(~term, ~definition,
  "<System> proj", "projected D/ST fantasy points under that scoring system (each system has its own model, tuning and feature selection)",
  "<System> rank", "rank of that projection among this week's 32 D/STs (1 = best)",
  "<System> P(top 8)", "chance the D/ST finishes as a top-8 scorer this week under that scoring system (4,000 simulated weeks using back-test errors)",
  "Range", "bar: light = 80% range of the actual score, dark = 50% range, tick = projection; axis −5 to 25 points",
  "±", "half-width of the projection's 90% CI: how sure the model is about the projection itself. The one uncertainty measure that differs by team beyond the projection level; widest when the opponent has a new QB or the inputs are unusual",
  "Kickoff", "kickoff time (Eastern); \U0001F512 = game has started, so its projection is frozen at the last pre-kickoff line",
  "Tier", "natural-break tier of the projection (optimal 1-D grouping into 6 tiers; 1 = best). A solid line above a tier = the drop into it is larger than the model's typical ± (a clear break); dashed = a softer break",
  "<System> tier", "tier of that system's projection (see Tier)",
  "Trend", "the projection over time: open dot = the weekly model run, filled dots = one per day the page was refreshed (that day's last value); green = up since the weekly run, red = down. Hover a dot for date and value",
  "Weather", "Open-Meteo forecast for kickoff + 2 h: temperature, sustained wind (max gust), chance of rain (max hourly) and expected rain in inches (total over the 3 hours). Display only: the D/ST model's weather inputs come from the weekly run",
  "Team (hover)", "hover or tap a team to see what moves its projection vs an average D/ST this week (Vegas lines excluded)",
  "Δ Opp implied", "change in the opponent's Vegas implied points since the weekly (Tuesday) model run (negative = good for this D/ST)",
  "Δ Proj", "change in the projection since the weekly (Tuesday) model run — not since the previous refresh — driven only by Vegas line moves",
  "Avg rank", "average of the three ranks; the table is sorted by it",
  "Rank spread", "largest minus smallest rank across systems; big spreads mean the scoring rules change the pick (usually shutout / points-allowed upside vs yards allowed or sacks)")
col_gloss <- g0$col_glossary %>% mutate(definition = case_when(
  term == "Proj"   ~ "projected fantasy points for the tab's scoring system: average of elastic net + component model + ridge",
  term == "PA pts" ~ "expected points from the tab's points-allowed tiers",
  term == "YA pts" ~ "expected points from the yards-allowed tiers (ESPN only; 0 for Yahoo and FFPC)",
  TRUE ~ definition))
gloss_lookup <- c(setNames(compare_gloss$definition, compare_gloss$term), setNames(col_gloss$definition, col_gloss$term),
                  setNames(g0$model_glossary$definition, g0$model_glossary$term), setNames(g0$metric_glossary$definition, g0$metric_glossary$term),
                  setNames(g0$glossary_all$definition, g0$glossary_all$term))
describe <- function(x) {
  x2 <- sub("^(ESPN|Yahoo|FFPC) (proj|rank|tier|P\\(top 8\\))$", "<System> \\2", x)
  x2 <- sub("^Δ since .* run$", "Δ Proj", sub("^Δ Opp implied since .* run$", "Δ Opp implied", x2))
  unname(coalesce(gloss_lookup[x2], ""))
}
feature_gloss <- map(parts, ~ .x$glossary_all %>% filter(str_starts(section, "Features"))) %>% bind_rows() %>%
  distinct(section, term, .keep_all = TRUE) %>%
  mutate(definition = sub(" — computed, not in the current model \\(see feature selection\\)$", "", definition),
         used_in = map_chr(term, function(t) paste(map_chr(parts, ~ if (t %in% .x$imp_tbl$feature || any(.x$glossary_all$term == t & !grepl("not in the current model", .x$glossary_all$definition))) .x$SC$label else NA_character_) %>% discard(is.na), collapse = ", ")))

## ---- 4. HTML helpers ----
esc <- function(x) { x <- gsub("&", "&amp;", as.character(x), fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub('"', "&quot;", gsub(">", "&gt;", x, fixed = TRUE), fixed = TRUE) }
html_table <- function(df, id = NULL, sortable = FALSE, left = 1, rank_cols = character(), tips = TRUE, raw_cols = character(),
                       row_cls = NULL, cell_cls = list()) {
  th <- map_chr(seq_along(df), function(k) { n <- names(df)[k]; d <- if (tips) describe(n) else ""
    sprintf('<th%s%s>%s%s</th>', if (sortable) sprintf(' onclick="sortTable(this,%d)"', k - 1) else "",
            if (nzchar(d)) sprintf(' title="%s"', esc(d)) else "", esc(n), if (nzchar(d)) "<sup>?</sup>" else "") })
  rows <- map_chr(seq_len(nrow(df)), function(i) paste0(if (!is.null(row_cls) && nzchar(row_cls[i])) sprintf('<tr class="%s">', row_cls[i]) else "<tr>", paste0(map_chr(seq_along(df), function(k) {
    v <- df[[k]][i]; cls <- if (names(df)[k] %in% rank_cols && !is.na(v)) (if (v <= 8) ' class="top"' else if (v >= 25) ' class="bot"' else "") else ""
    if (names(df)[k] %in% names(cell_cls) && nzchar(cell_cls[[names(df)[k]]][i])) cls <- sprintf(' class="%s"', cell_cls[[names(df)[k]]][i])
    sprintf("<td%s>%s</td>", cls, if (names(df)[k] %in% raw_cols) v else esc(ifelse(is.na(v), "", v))) }), collapse = ""), "</tr>"))
  sprintf('<table%s class="l%d%s"><thead><tr>%s</tr></thead><tbody>%s</tbody></table>', if (is.null(id)) "" else sprintf(' id="%s"', id),
          left, if (sortable) " sortable" else "", paste(th, collapse = ""), paste(rows, collapse = ""))
}
gloss_table <- function(df) html_table(df %>% select(Term = term, Definition = definition), left = 99, tips = FALSE)

## ---- 5. Tabs ----
range_bar <- function(proj, q10, q25, q75, q90, ci_lo, ci_hi, lo = -5, hi = 25) {
  pos <- function(x) round(100 * (min(max(x, lo), hi) - lo) / (hi - lo), 1)
  sprintf('<div class="rb" title="80%%: %.0f to %.0f · 50%%: %.0f to %.0f · proj %.1f (90%% CI %.1f–%.1f)"><span class="r80" style="left:%s%%;width:%s%%"></span><span class="r50" style="left:%s%%;width:%s%%"></span><span class="pt" style="left:%s%%"></span><span class="zero" style="left:%s%%"></span></div>',
          round(q10) + 0, round(q90) + 0, round(q25) + 0, round(q75) + 0, proj, ci_lo, ci_hi, pos(q10), pos(q90) - pos(q10), pos(q25), pos(q75) - pos(q25), pos(proj), pos(0))
}
sys_tab <- function(p) {
  p$pred <- p$pred[order(p$pred$rank), ]
  pt <- make_proj_tbl(p$pred, html = TRUE, label = p$SC$label); ac <- if ("Trend" %in% names(pt)) "Trend" else if (DLAB %in% names(pt)) DLAB else "Proj"
  tr <- tiers(p$pred$proj, clear = median((p$pred$ci_hi - p$pred$ci_lo) / 2, na.rm = TRUE)); tt <- tr$tier
  brk <- c(FALSE, tt[-1] != tt[-length(tt)])
  row_cls <- trimws(paste(ifelse(tt %% 2 == 1, "tier-odd", ""), ifelse(brk, ifelse(tr$clear[tt] %in% TRUE, "tb-clear", "tb-soft"), "")))
  team_cls <- ifelse(p$pred$rank <= 8, "top", ifelse(p$pred$rank >= 25, "bot", ""))
  if (all(c("q10", "q90") %in% names(p$pred))) pt <- pt %>% mutate(Range = pmap_chr(p$pred[c("proj", "q10", "q25", "q75", "q90", "ci_lo", "ci_hi")], range_bar), .after = all_of(ac))
  cvt <- p$cv_tbl %>% select(any_of(c("model", "rmse", "mae", "spearman", "top8_avg", "bot8_avg", "edge_top8", "vs_vegas_top8", "t_stat", "what")))
  paste0(sprintf("<p class='rules'><b>%s scoring:</b> %s</p>", esc(p$SC$label), esc(p$SC$rules)),
         sprintf("<p class='note'>Feature families in this model: %s.</p>", esc(paste(p$families, collapse = ", "))),
         "<p class='note'>Range bar: light = where the actual score lands 8 times in 10, dark = 5 times in 10, tick = projection, thin line = 0 points (axis −5 to 25). 90% CI = uncertainty of the projection itself.</p>",
         "<p class='note'>Tiers: natural breaks in the projections. A solid line = a clear drop (bigger than the model's typical ±), dashed = a softer break. Hover or tap a team for what drives its projection.</p>",
         html_table(pt, id = paste0("t_", p$system), sortable = TRUE, left = if (refreshed) 6 else 5, rank_cols = "Rank", raw_cols = c("Range", "Trend", "Team"),
                    row_cls = row_cls, cell_cls = list(Team = team_cls)),
         sprintf("<h3>Back-test: %s (trained on 2018–%d)</h3><p class='note'>The same season is used to choose features and settings, so these numbers are somewhat optimistic.</p>",
                 paste(p$cv_season, collapse = ", "), min(p$cv_season) - 1),
         html_table(cvt, left = 1),
         "<h3>Top 25 GBM features</h3>", html_table(p$imp_tbl %>% select(feature, rel_inf, meaning), left = 1))
}
tabs <- c(list(Compare = paste0(
  "<p class='note'>One model per scoring system (same data, features and method; each tuned and feature-selected on its own 2025 back-test). ",
  "Click a column header to sort. Green = top 8 in that system (Team column: top 8 by average rank), red = bottom 8. Hover or tap a team for what drives its projection. P(top 8) = chance of actually finishing top 8 this week; see each format's tab for score ranges.</p>",
  html_table(cmp_html, id = "t_compare", sortable = TRUE, left = if (refreshed) 4 else 3, rank_cols = rank_cols, raw_cols = "Team", cell_cls = list(Team = cmp_cls)),
  "<h3>Scoring rules</h3>", paste0(map_chr(parts, ~ sprintf("<p class='rules'><b>%s:</b> %s</p>", esc(.x$SC$label), esc(.x$SC$rules))), collapse = ""))),
  set_names(map(parts, sys_tab), map_chr(parts, ~ .x$SC$label)),
  list(Glossary = paste0(
    "<h3>Comparison columns</h3>", gloss_table(compare_gloss),
    "<h3>Projection table columns</h3>", gloss_table(col_gloss),
    "<h3>Models</h3>", gloss_table(g0$model_glossary), "<h3>Back-test metrics</h3>", gloss_table(g0$metric_glossary),
    sprintf("<p class='note'>%s</p>", esc(g0$rate_note)),
    paste0(imap_chr(split(feature_gloss, feature_gloss$section), ~ sprintf("<details><summary>%s</summary>%s</details>", esc(.y),
      html_table(.x %>% transmute(Feature = term, Definition = definition, `In model for` = ifelse(nzchar(used_in), used_in, "—")), left = 99, tips = FALSE))), collapse = ""))))

css <- ':root{--bg:#fff;--fg:#1d1d1f;--muted:#666;--line:#ddd;--head:#f3f3f3;--top:#e3f4e8;--bot:#fbe6e6;--accent:#1f5fbf;--rng80:#c9dcf5;--rng50:#6f9ee0}
@media (prefers-color-scheme: dark){:root{--bg:#141414;--fg:#e8e8e8;--muted:#9a9a9a;--line:#333;--head:#222;--top:#17351f;--bot:#3a1a1a;--accent:#7fb0ff;--rng80:#26395a;--rng50:#4f7fc4}}
body{font-family:system-ui,sans-serif;max-width:1250px;margin:1.5rem auto;padding:0 16px;color:var(--fg);background:var(--bg)}
h1{margin:.2rem 0}.sub{color:var(--muted);font-size:14px}
.tabs{display:flex;gap:4px;flex-wrap:wrap;border-bottom:2px solid var(--line);margin:1rem 0}
.tabs button{border:0;background:none;padding:8px 14px;font-size:15px;color:var(--muted);cursor:pointer;border-bottom:3px solid transparent;margin-bottom:-2px}
.tabs button.on{color:var(--fg);border-bottom-color:var(--accent);font-weight:600}
.panel{display:none}.panel.on{display:block}
table{border-collapse:collapse;font-size:13.5px;margin:.6rem 0 1.2rem;display:block;overflow-x:auto;max-width:100%}
th,td{border:1px solid var(--line);padding:4px 8px;text-align:right;white-space:nowrap}
th{background:var(--head);position:sticky;top:0}th[title]{cursor:help}th sup{color:var(--muted);font-size:10px;margin-left:2px}
table.sortable th{cursor:pointer}
table.l1 td:nth-child(-n+1),table.l3 td:nth-child(-n+3),table.l4 td:nth-child(-n+4),table.l5 td:nth-child(-n+5),table.l6 td:nth-child(-n+6){text-align:left}
table.l99 td{text-align:left;white-space:normal}table.l99 td:first-child{font-family:ui-monospace,monospace;white-space:nowrap}
td.top{background:var(--top);font-weight:600}td.bot{background:var(--bot)}
.rb{position:relative;width:170px;height:14px}.rb span{position:absolute;top:0;height:14px}
.rb .r80{background:var(--rng80);border-radius:3px}.rb .r50{background:var(--rng50);border-radius:3px}
.rb .pt{width:2px;margin-left:-1px;background:var(--fg);top:-2px;height:18px}.rb .zero{width:1px;background:var(--muted);opacity:.6}
.rules,.note{color:var(--muted);font-size:13.5px;max-width:1000px}details{margin:.4rem 0}summary{cursor:pointer;font-weight:600}'
css <- paste0(css, SITE_CSS)
js <- 'function show(id){document.querySelectorAll(".panel").forEach(p=>p.classList.toggle("on",p.id===id));
document.querySelectorAll(".tabs button").forEach(b=>b.classList.toggle("on",b.dataset.t===id));}
function sortTable(th,col){const t=th.closest("table"),b=t.tBodies[0],rows=[...b.rows],asc=th.dataset.asc!=="1";
rows.sort((x,y)=>{let a=x.cells[col].innerText,c=y.cells[col].innerText,na=parseFloat(a),nc=parseFloat(c);
if(!isNaN(na)&&!isNaN(nc))return asc?na-nc:nc-na;return asc?a.localeCompare(c):c.localeCompare(a)});
rows.forEach(r=>b.appendChild(r));t.querySelectorAll("th").forEach(h=>h.dataset.asc="");th.dataset.asc=asc?"1":"0";}'
ids <- paste0("p", seq_along(tabs))
fit_time <- max(do.call(c, map(parts, "generated")))
status_line <- if (!refreshed) sprintf("model run %s", et(fit_time, "%a %b %d, %I:%M %p ET")) else {
  r <- parts[[1]]$refresh
  sprintf("<b>lines updated %s</b> (%s) · model run %s", et(r$time, "%a %b %d, %I:%M %p ET"),
          if (r$n_priced > 0) sprintf("median of %.0f sportsbooks via The Odds API; %d of %d games priced%s", r$books, r$n_priced, nrow(parts[[1]]$pred) / 2,
                                     if (r$n_locked > 0) sprintf(", %d started", r$n_locked) else "") else "no sportsbook lines yet: weekly-run lines",
          et(fit_time, "%a %b %d"))
}
if (refreshed) status_line <- paste0(status_line, sprintf(" · Δ columns = change since the weekly model run (%s)", et(parts[[1]]$refresh$model_fit, "%a %b %d")))
html <- paste0('<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  sprintf("<title>D/ST projections %d wk %d</title><style>%s</style></head><body>", SEASON, WEEK, css),
  if (refreshed) sprintf("<div class='sub'><b>D/ST</b> · <a href='%sk/'>Kickers</a> · <a href='%sarchive/'>past weeks</a></div>",
                         Sys.getenv("SITE_BASE", "/dst-site/"), Sys.getenv("SITE_BASE", "/dst-site/")) else "",
  sprintf("<h1>D/ST projections — %d week %d</h1><div class='sub'>%s · %s · hover a column header (<sup>?</sup>) for its definition</div>",
          SEASON, WEEK, paste(map_chr(parts, ~ .x$SC$label), collapse = " · "), status_line),
  "<div class='tabs'>", paste0(sprintf('<button data-t="%s" onclick="show(\'%s\')">%s</button>', ids, ids, esc(names(tabs))), collapse = ""), "</div>",
  paste0(sprintf('<div class="panel" id="%s">%s</div>', ids, unlist(tabs)), collapse = ""),
  sprintf("<script>%s show('%s');</script></body></html>", js, ids[1]))
out_html <- file.path(DST_DIR, sprintf("dst_proj_%d_wk%02d_all.html", SEASON, WEEK))
writeLines(html, out_html)

## ---- 6. Markdown twin ----
md_table <- function(df) { cell <- function(x) gsub("|", "\\|", ifelse(is.na(x), "", as.character(x)), fixed = TRUE)
  c(paste0("| ", paste(cell(names(df)), collapse = " | "), " |"), paste0("|", strrep("---|", ncol(df))),
    apply(df, 1, function(r) paste0("| ", paste(cell(r), collapse = " | "), " |"))) }
md <- c(sprintf("# D/ST projections — %d week %d", SEASON, WEEK), "", "## Compare", "", md_table(cmp), "",
        unlist(map(parts, ~ c(sprintf("## %s", .x$SC$label), "", paste("Scoring:", .x$SC$rules), "", md_table(make_proj_tbl(.x$pred)), ""))),
        "## Glossary", "", md_table(compare_gloss %>% rename(Term = term, Definition = definition)), "",
        md_table(col_gloss %>% rename(Term = term, Definition = definition)), "",
        "Feature definitions: see dst_glossary.md in each system's folder.")
writeLines(md, sub("html$", "md", out_html))
message("wrote ", out_html, " (+ .md)")
