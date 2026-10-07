## ---- 49_ros_page.R: the rest-of-season (ROS) page for D/ST and kickers ----
## Andrew 2026-10-03. Every remaining week of every team, scored with the SAME weekly models as the D/ST and kicker pages
## (the published bundles' blends), from each team's current form (ros_rows_*.rds, written by the 40 / 50 hooks):
##   lines   : this week's and next week's sportsbook lines (the D/ST refresh already pulls both: data/lines/line_history.csv;
##             else nflverse's), later weeks projected from market power ratings fitted to every line posted so far
##             (implied = league + offense + opponent defense + home field; 47's back-test: 2.1 pts error 1 week out,
##             2.6 at 4 weeks, 3.1 at 8, vs 3.7 with no team information)
##   weather : this week's Open-Meteo forecast where the refresh has one, else the climate normal for that stadium, date
##             and kickoff hour (48); domes and retractable roofs indoor
##   this week's column = the D/ST / kicker page's latest projection (data/lines/proj_history.csv), so the two pages agree
## Runs on Stratus (preview, from 43: output/ros/ros_<season>_wk<ww>.html) and in the daily GitHub refresh after 55
## (site/ros/index.html). Base R + dplyr / tidyr / purrr / jsonlite. No API calls.
## Usage: Rscript scripts/49_ros_page.R     (FF_PROJ_DIR = repo root on GitHub, ~/ML/ff on Stratus; REFRESH_NOW for tests)
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(purrr) })
invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")))   # the page text has · ° – (GitHub sets LANG=C.UTF-8 already)
PROJ_DIR <- Sys.getenv("FF_PROJ_DIR", getwd())
SITE_REPO_DIR <- path.expand(Sys.getenv("DST_SITE_DIR", "~/ML/dst-site"))           # Stratus: the site repo's line history
SITE_BASE <- Sys.getenv("SITE_BASE", "/dst-site/")
source(file.path(PROJ_DIR, "scripts", "ros_utils.R"))
source(file.path(PROJ_DIR, "scripts", "site_utils.R"))
DST_E <- new.env(); sys.source(file.path(PROJ_DIR, "scripts", "dst_blend_utils.R"), DST_E)   # separate environments: the two
K_E   <- new.env(); sys.source(file.path(PROJ_DIR, "scripts", "k_blend_utils.R"), K_E)       # utility files share function names
utc <- function(x) as.POSIXct(sub("Z$", "", x), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
NOW <- if (nzchar(Sys.getenv("REFRESH_NOW"))) utc(Sys.getenv("REFRESH_NOW")) else as.POSIXct(format(Sys.time(), tz = "UTC"), tz = "UTC")
first_file <- function(rel) { f <- c(file.path(PROJ_DIR, rel), file.path(SITE_REPO_DIR, rel)); f <- f[file.exists(f)]; if (length(f)) f[1] else NA_character_ }
say <- function(...) message(sprintf(...))

## ---- 1. Inputs: 48's file, the weekly bundles and the ROS rows of the same week ----
inf <- list.files(file.path(PROJ_DIR, "output/ros"), pattern = "^ros_inputs_\\d{4}_wk\\d{2}\\.rds$")
if (!length(inf)) { message("ROS: no ros_inputs file yet (48_ros_inputs.R) - page not built"); quit(save = "no") }
key <- max(sub("ros_inputs_(\\d{4})_wk(\\d{2}).*", "\\1\\2", inf)); SEASON <- as.integer(substr(key, 1, 4)); WEEK <- as.integer(substr(key, 5, 6))
wk <- sprintf("%d_wk%02d", SEASON, WEEK)
INP <- readRDS(file.path(PROJ_DIR, "output/ros", paste0("ros_inputs_", wk, ".rds")))
load_pair <- function(dir) {
  b <- file.path(dir, paste0("bundle_", wk, ".rds")); r <- file.path(dir, paste0("ros_rows_", wk, ".rds"))
  if (file.exists(b) && file.exists(r)) list(B = readRDS(b), R = readRDS(r)) else NULL }
DST <- Filter(Negate(is.null), setNames(lapply(c("espn", "yahoo", "ffpc"), function(s) load_pair(file.path(PROJ_DIR, "output/dst", s))), c("espn", "yahoo", "ffpc")))
KK <- load_pair(file.path(PROJ_DIR, "output/k"))
if (!length(DST) && is.null(KK)) { message("ROS: no ros_rows for ", wk, " (40 / 50 hooks) - page not built"); quit(save = "no") }
say("ROS %d week %d: D/ST formats %s; kickers %s", SEASON, WEEK, paste(names(DST), collapse = ", "), if (is.null(KK)) "no" else "yes")
games <- INP$games
WEEKS <- sort(unique(games$week))

## ---- 2. Lines: posted (site history, else nflverse) for this week / next; projected from power ratings after that ----
line_csv <- first_file("data/lines/line_history.csv")
posted <- tibble(game_id = character(), home_spread = numeric(), total = numeric(), n_books = integer())
if (!is.na(line_csv)) {
  h <- read.csv(line_csv, stringsAsFactors = FALSE) %>% as_tibble() %>% mutate(t = utc(pulled_at), ko = utc(commence_time)) %>% filter(t < ko, t <= NOW)
  m1 <- h %>% inner_join(games %>% select(game_id, home = home_team, away = away_team, gameday), by = c("home", "away")) %>% mutate(flip = FALSE)
  m2 <- h %>% inner_join(games %>% select(game_id, home = away_team, away = home_team, gameday), by = c("home", "away")) %>% mutate(flip = TRUE)
  posted <- bind_rows(m1, m2) %>% filter(abs(as.numeric(difftime(ko, as.POSIXct(gameday, tz = "UTC"), units = "days"))) <= 2) %>%
    group_by(game_id) %>% slice_max(t, n = 1, with_ties = FALSE) %>% ungroup() %>%
    transmute(game_id, home_spread = ifelse(flip, -home_spread, home_spread), total, n_books)
}
nflv <- INP$lines_nflv %>% inner_join(games %>% select(game_id, home_team), by = "game_id") %>% filter(team == home_team) %>%
  transmute(game_id, nv_spread = spread, nv_total = total)
gl <- games %>% select(game_id, week, home_team, away_team, neutral) %>% left_join(posted, by = "game_id") %>% left_join(nflv, by = "game_id") %>%
  mutate(src = ifelse(!is.na(home_spread), "books", ifelse(!is.na(nv_spread), "nflverse", "projected")),
         home_spread = coalesce(home_spread, nv_spread), total = coalesce(total, nv_total))
# ratings: every closing line so far + the posted lines of the remaining games
pos_of <- function(w) INP$pos$pos[match(w, INP$pos$week)]
known <- gl %>% filter(!is.na(home_spread)) %>%
  { bind_rows(transmute(., game_id, week, team = home_team, opp = away_team, loc = ifelse(neutral, 0, 1), implied = (total + home_spread) / 2),
              transmute(., game_id, week, team = away_team, opp = home_team, loc = ifelse(neutral, 0, -1), implied = (total - home_spread) / 2)) } %>%
  mutate(season = SEASON, pos = pos_of(week))
obs <- bind_rows(INP$lines_past %>% select(season, pos, team, opp, loc, implied), known %>% select(season, pos, team, opp, loc, implied)) %>% filter(!is.na(pos))
pos_now <- max(c(pos_of(WEEK), known$pos), na.rm = TRUE)
RT <- ros_fit_ratings(obs, SEASON, pos_now, INP$params)
gl <- gl %>% mutate(loc_h = ifelse(neutral, 0, 1),
                    p_home = ros_implied(RT, home_team, away_team, loc_h), p_away = ros_implied(RT, away_team, home_team, -loc_h),
                    home_spread = ifelse(src == "projected", p_home - p_away, home_spread), total = ifelse(src == "projected", p_home + p_away, total),
                    line_src = ifelse(src == "projected", "projected", ifelse(week == WEEK, "live", "lookahead")))
say("ROS lines: %d games with sportsbook lines, %d from nflverse, %d projected (ratings from %d team-lines)",
    sum(gl$src == "books"), sum(gl$src == "nflverse"), sum(gl$src == "projected"), RT$n)
team_lines <- bind_rows(gl %>% transmute(game_id, team = home_team, spread_new = home_spread, total_new = total, line_src),
                        gl %>% transmute(game_id, team = away_team, spread_new = -home_spread, total_new = total, line_src))

## ---- 3. Weather: forecast (this week / next, where the refresh fetched one), else the climate normal ----
wx_csv <- first_file("data/lines/weather_history.csv")
fc <- if (!is.na(wx_csv)) wx_latest(read_wx_hist(wx_csv)) else tibble(game_id = character())
gw <- games %>% select(game_id, week, indoor, retract) %>% left_join(INP$climate, by = "game_id") %>%
  left_join(fc %>% select(game_id, f_temp = temp, f_wind = wind, f_gust = gust, f_rain = precip_prob, f_rmax = precip_max), by = "game_id") %>%
  mutate(wx_src = ifelse(indoor, "indoor", ifelse(!is.na(f_temp) & !is.na(f_wind), "forecast", "climate")),
         temp_new = ifelse(indoor, 70, ifelse(wx_src == "forecast", f_temp, c_temp)),
         wind_new = ifelse(indoor, 0, ifelse(wx_src == "forecast", f_wind, c_wind)),
         cold35 = ifelse(indoor, 0, ifelse(wx_src == "forecast", as.numeric(f_temp <= 35), p_cold35)),
         cold40 = ifelse(indoor, 0, ifelse(wx_src == "forecast", as.numeric(f_temp <= 40), p_cold40)),
         windhi = ifelse(indoor, 0, ifelse(wx_src == "forecast", as.numeric(f_wind >= 15), p_windhi)))
say("ROS weather: %d forecasts, %d climate normals, %d indoor", sum(gw$wx_src == "forecast"), sum(gw$wx_src == "climate"), sum(gw$wx_src == "indoor"))

## ---- 4. Score every remaining team-game with the weekly models ----
ph_csv <- first_file("data/lines/proj_history.csv")
ph <- if (!is.na(ph_csv)) read.csv(ph_csv, stringsAsFactors = FALSE) %>% as_tibble() %>% filter(season == SEASON, week == WEEK) else tibble()
ph_latest <- function(model, sy) {
  if (!nrow(ph)) return(tibble(team = character(), ph_proj = numeric()))
  ph %>% filter(model == !!model, system == sy) %>% group_by(team) %>% slice_max(time, n = 1, with_ties = FALSE) %>% ungroup() %>% select(team, ph_proj = proj)
}
week_drivers <- function(te, f, grp) {           # per week: what moves each projection vs an average team that week (site_utils)
  out <- rep(NA_character_, nrow(te))
  for (w in unique(te$week)) { i <- which(te$week == w); if (length(i) > 2) out[i] <- drivers(te[i, ], f, grp, top = 3)$text }
  out
}
fill_lines_wx <- function(rows) rows %>% left_join(team_lines, by = c("game_id", "team")) %>% left_join(gw %>% select(-week), by = "game_id")
res <- list()
for (sy in names(DST)) {
  B <- DST[[sy]]$B; te <- fill_lines_wx(DST[[sy]]$R$rows) %>%
    mutate(spread = spread_new, total_line = total_new, implied_own = (total_new + spread_new) / 2, implied_opp = (total_new - spread_new) / 2,
           indoor = as.integer(indoor.y), temp = temp_new, wind = wind_new, wind_hi = windhi, cold = cold35)
  f <- function(t) DST_E$blend_pred(B$main, t, B$SC, B$specs, B$final_models)
  te$proj <- f(te); te$proj_model <- te$proj
  te <- te %>% left_join(ph_latest("dst", sy), by = "team") %>%
    mutate(proj = ifelse(week == WEEK & !is.na(ph_proj), ph_proj, proj))
  te$why <- week_drivers(te, f, dst_groups(DST[[sy]]$R$vars))
  res[[paste0("dst_", sy)]] <- te %>% transmute(kind = "dst", system = sy, label = B$SC$label, week, game_id, team, opp, home, proj, proj_model, ph_proj, why,
                                                who = opp_qb_name, implied_ref = implied_opp, spread, total_line)
  if (anyNA(te$proj)) warning("ROS ", sy, ": ", sum(is.na(te$proj)), " projections missing")
}
if (!is.null(KK)) {
  B <- KK$B; te <- fill_lines_wx(KK$R$rows) %>%
    mutate(spread = spread_new, total_line = total_new, indoor = as.integer(indoor.y), temp = temp_new, wind = wind_new,
           wind_known = as.integer(indoor == 1 | wx_src == "forecast")) %>%
    K_E$derive_vegas() %>% K_E$derive_env() %>%
    mutate(cold = ifelse(wx_src == "climate", cold40, cold), wind_hi = ifelse(wx_src == "climate", windhi, wind_hi))   # climate: chances
  comp <- K_E$comp_score(B$comp, te, B$SCORING)
  lab <- c(espn = "ESPN", dec = "Decimal")
  for (sy in names(B$SCORING)) {
    f <- function(t) K_E$blend_score(B, t, sy)
    te$proj <- K_E$blend_score(B, te, sy, comp = comp)
    te$proj_model <- te$proj
    te2 <- te %>% left_join(ph_latest("k", sy), by = "team") %>% mutate(proj = ifelse(week == WEEK & !is.na(ph_proj), ph_proj, proj))
    te2$why <- week_drivers(te2, f, k_groups(KK$R$vars))
    res[[paste0("k_", sy)]] <- te2 %>% transmute(kind = "k", system = sy, label = coalesce(unname(lab[sy]), sy), week, game_id, team, opp, home, proj, proj_model, ph_proj, why,
                                                 who = kicker, implied_ref = implied_own, spread, total_line)
    if (anyNA(te2$proj)) warning("ROS kickers ", sy, ": ", sum(is.na(te2$proj)), " projections missing")
  }
}
res <- bind_rows(res)
wk_chk <- res %>% filter(week == WEEK, !is.na(ph_proj)) %>% group_by(kind, system) %>% summarise(d = max(abs(proj_model - ph_proj)), .groups = "drop")
if (nrow(wk_chk)) say("ROS check: week %d scored here vs the weekly page's latest (differences = newer lines / QB swaps there): %s", WEEK,
                      paste(sprintf("%s %s %.2f", wk_chk$kind, wk_chk$system, wk_chk$d), collapse = ", "))
if (nzchar(Sys.getenv("ROS_DEBUG"))) saveRDS(list(res = res, gl = gl, gw = gw, ratings = RT), file.path(PROJ_DIR, "output/ros/ros_debug.rds"))
say("ROS: scored %d team-games across %d formats", n_distinct(paste(res$game_id, res$team)), n_distinct(paste(res$kind, res$system)))

## ---- 5. Hover text per team-game (shared by the formats of a section) ----
fmt_ko <- function(d, t) { x <- as.POSIXct(paste(d, t), tz = "America/New_York"); sub(" 0", " ", format(x, "%a %b %d %I:%M %p")) }
ginfo <- games %>% left_join(gl %>% select(game_id, line_src, n_books), by = "game_id") %>% left_join(gw %>% select(-week, -indoor, -retract), by = "game_id") %>%
  mutate(ko_txt = fmt_ko(gameday, gametime),
         wx_txt = case_when(
           roof == "dome" ~ "Indoor (dome)",
           indoor & retract ~ "Retractable roof (usually closed; assumed closed)",
           !indoor & retract & wx_src == "forecast" ~ paste0("Roof open as recorded · forecast: ", round(f_temp), "° · ", round(f_wind), " mph"),
           indoor ~ "Indoor",
           wx_src == "forecast" ~ paste0("Forecast: ", round(f_temp), "° · ", round(f_wind), " mph", ifelse(is.na(f_gust), "", paste0(" (gusts ", round(f_gust), ")")),
                                        ifelse(is.na(f_rain) | f_rain < 20, "", paste0(" · ", round(f_rain), "% rain"))),
           TRUE ~ paste0("Typical for this date and kickoff: ", round(c_temp), "° · ", round(c_wind), " mph",
                         ifelse(p_cold35 >= 0.1, paste0(" · ", ros_pct(p_cold35), " chance of 35° or colder"), ""),
                         ifelse(!is.na(p_snow) & p_snow >= 0.05, paste0(" · ", ros_pct(p_snow), " snow"), ""),
                         ifelse(!is.na(p_rain) & p_rain >= 0.15, paste0(" · ", ros_pct(p_rain), " rain"), ""))),
         venue = ifelse(neutral, paste0(" · ", stadium), ""))
LSRC <- c(live = "this week's sportsbook line", lookahead = "next week's posted line", projected = "projected from team power ratings")
res <- res %>% left_join(ginfo %>% select(game_id, ko_txt, wx_txt, venue, line_src, n_books), by = "game_id") %>%
  mutate(spread_txt = ifelse(spread > 0, sprintf("-%.1f", spread), ifelse(spread < 0, sprintf("+%.1f", -spread), "PK")),
         ha = ifelse(home == 1, "vs", "@"))

## ---- 6. Page ----
sections <- list()
for (kd in unique(res$kind)) {
  r <- res %>% filter(kind == kd); teams <- sort(unique(r$team)); systems <- unique(r$system)
  base <- r %>% filter(system == systems[1]) %>%
    transmute(team, week, opp_lab = paste(ha, opp), tip = paste0(
      sprintf("Wk %d · %s %s%s · %s", week, ha, opp, venue, ko_txt), "\n",
      if (kd == "dst") sprintf("Opp. implied total %.1f · spread %s %s · total %.1f", implied_ref, team, spread_txt, total_line)
      else sprintf("Own implied total %.1f · spread %s %s · total %.1f", implied_ref, team, spread_txt, total_line),
      " (", LSRC[line_src], ")\n",
      if (kd == "dst") paste0("Opp. QB: ", coalesce(who, "?")) else paste0("Kicker: ", coalesce(who, "?")), "\n", wx_txt),
      src = line_src)
  cell <- function(tm, w, col) { x <- base[[col]][base$team == tm & base$week == w]; if (length(x)) x[[1]] else NULL }
  vals <- lapply(setNames(systems, systems), function(sy) {
    rs <- r %>% filter(system == sy)
    lapply(setNames(teams, teams), function(tm) lapply(WEEKS, function(w) { x <- rs[rs$team == tm & rs$week == w, ]
      if (!nrow(x)) NA else list(v = round(x$proj[1], 2), d = x$why[1]) })) })      # NA -> JSON null (a bye)
  sections[[kd]] <- list(
    title = if (kd == "dst") "D/ST" else "Kickers",
    systems = lapply(setNames(systems, systems), function(sy) unique(r$label[r$system == sy])),
    teams = teams, who = setNames(lapply(teams, function(tm) { x <- r$who[r$team == tm & r$system == systems[1]]; if (kd == "k" && length(x)) x[1] else "" }), teams),
    cells = lapply(setNames(teams, teams), function(tm) lapply(WEEKS, function(w) { o <- cell(tm, w, "opp_lab"); if (is.null(o)) NA else
      list(o = o, t = cell(tm, w, "tip"), s = cell(tm, w, "src")) })),
    vals = vals)
}
wsrc <- sapply(WEEKS, function(w) { s <- gl$line_src[gl$week == w]; if (all(s == "live")) "live" else if (all(s %in% c("live", "lookahead"))) "lookahead" else if (all(s == "projected")) "projected" else "mixed" })
rel <- INP$reliab
rel_txt <- if (!is.null(rel)) {
  rr <- function(kd, h) { x <- rel$retained$retained[rel$retained$kind == kd & rel$retained$h == h]; if (length(x)) ros_pct(x) else "?" }
  sprintf(paste0("How far ahead it can see (back-test 2021–25, ranking accuracy kept vs knowing the real lines): D/ST %s one week out, %s at four weeks, %s at eight; ",
                 "kickers %s, %s, %s. Sorting by points scored so far keeps much less (D/ST weekly rank correlation ≈ 0.08 vs ≈ 0.28 for this page). ",
                 "The Weeks 15–17 column is a rough guide this early and firms up from about week 10."),
          rr("dst", 1), rr("dst", 4), rr("dst", 8), rr("k", 1), rr("k", 4), rr("k", 8))
} else ""
DATA <- list(season = SEASON, week = WEEK, weeks = WEEKS, wsrc = wsrc, playoff = INP$playoff_weeks %||% 15:17, sections = sections)
DATA <- rapply(DATA, function(x) { x <- enc2utf8(x); Encoding(x) <- "UTF-8"; x }, classes = "character", how = "replace")
json <- jsonlite::toJSON(DATA, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
updated <- format(NOW, "%a %b %d %I:%M %p ET", tz = "America/New_York")
css <- paste0(':root{--bg:#fff;--fg:#1d1d1f;--muted:#666;--line:#ddd;--head:#f3f3f3;--accent:#1f5fbf;--pos:46,160,67;--neg:214,57,57}
@media (prefers-color-scheme: dark){:root{--bg:#141414;--fg:#e8e8e8;--muted:#9a9a9a;--line:#333;--head:#222;--accent:#7fb0ff;--pos:63,185,90;--neg:235,90,90}}
body{font-family:system-ui,sans-serif;max-width:1800px;margin:1.5rem auto;padding:0 16px;color:var(--fg);background:var(--bg)}
h1{margin:.2rem 0}.sub{color:var(--muted);font-size:14px}.note{font-size:13.5px;max-width:1000px;line-height:1.45}
.bar{display:flex;flex-wrap:wrap;gap:14px;align-items:center;margin:.8rem 0}.grp{display:flex;gap:4px;align-items:center;font-size:13px;color:var(--muted)}
.grp button{border:1px solid var(--line);background:var(--head);color:var(--fg);padding:6px 12px;border-radius:6px;cursor:pointer;font-size:14px}
.grp button.on{background:var(--accent);border-color:var(--accent);color:#fff;font-weight:600}
.tabs{display:flex;gap:4px;border-bottom:2px solid var(--line);margin:1rem 0 .4rem}
.tabs button{border:0;background:none;padding:8px 16px;font-size:16px;color:var(--muted);cursor:pointer;border-bottom:3px solid transparent;margin-bottom:-2px}
.tabs button.on{color:var(--fg);border-bottom-color:var(--accent);font-weight:600}
.wrap{overflow-x:auto;max-width:100%}
table{border-collapse:collapse;font-size:13px}
th,td{border:1px solid var(--line);padding:4px 7px;text-align:center;white-space:nowrap}
th{background:var(--head);position:sticky;top:0;cursor:pointer;z-index:2;font-weight:600}
th small{display:block;font-weight:400;color:var(--muted);font-size:10.5px}
td.tm,th.tm{position:sticky;left:0;text-align:left;z-index:3}td.tm{background:var(--bg);font-weight:600}th.tm{z-index:4}
td.tm small{font-weight:400;color:var(--muted);margin-left:4px}
td.c{min-width:46px;cursor:help;line-height:1.15}td.c .o{display:block;font-size:10px;color:var(--muted)}
td.pj{font-style:italic}td.bye{color:var(--muted);font-size:11px;background:repeating-linear-gradient(45deg,transparent,transparent 4px,rgba(127,127,127,.08) 4px,rgba(127,127,127,.08) 8px)}
td.sum{font-weight:600}th.sumh{background:var(--head)}td.gap,th.gap{border:0;background:none;padding:0 3px}
th.sorted{box-shadow:inset 0 -3px 0 var(--accent)}
#tip{position:fixed;z-index:50;display:none;max-width:420px;background:var(--bg);color:var(--fg);border:1px solid var(--line);border-radius:8px;
  padding:8px 10px;font-size:12.5px;line-height:1.4;box-shadow:0 4px 18px rgba(0,0,0,.25);white-space:pre-line;pointer-events:none}
#tip b{font-size:13.5px}.leg{font-size:12.5px;color:var(--muted)}.leg span{display:inline-block;padding:1px 6px;border-radius:4px;margin:0 2px}
details{margin:.6rem 0;font-size:13.5px;max-width:1000px}details summary{cursor:pointer;color:var(--accent)}
#pairs .chips{display:flex;flex-wrap:wrap;gap:4px;align-items:center;font-size:12.5px;color:var(--muted);margin:.2rem 0 .6rem}#pairs .chips span{margin-right:4px}
#pairs .chips button{border:1px solid var(--line);background:none;color:var(--fg);padding:2px 7px;border-radius:999px;font-size:12px;cursor:pointer}
#pairs .chips button.off{text-decoration:line-through;opacity:.45}
#pairs select{font-size:14px;padding:5px 8px;border-radius:6px;border:1px solid var(--line);background:var(--bg);color:var(--fg)}
#pairs th{cursor:default}td.s{min-width:40px;cursor:help;line-height:1.15}td.s small{display:block;font-size:9.5px;color:var(--muted)}
td.s.pa,.sw.pa{background:rgba(127,127,127,.12)}td.s.pb,.sw.pb{background:rgba(var(--pos),.32)}td.s.bb,.sw.bb{background:rgba(var(--neg),.38)}td.s.pj{font-style:italic}
.sw{display:inline-block;width:12px;height:12px;border-radius:3px;vertical-align:-1px;border:1px solid var(--line)}
tr.solo td{font-style:italic;color:var(--muted)}
table.hm td,table.hm th{padding:3px 3px;min-width:30px;font-size:11px}table.hm td.hc{cursor:pointer}table.hm td.dg{color:var(--muted);font-style:italic}', NAV_CSS)
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>Rest of season</title><style>', css, '</style></head><body>', site_nav("ros", SITE_BASE),
  '<h1>Rest of season: D/ST &amp; kickers</h1>',
  sprintf('<p class="sub">Updated %s · %d season, weeks %d–%d · same models as the weekly D/ST and kicker pages</p>', updated, SEASON, min(WEEKS), max(WEEKS)),
  '<p class="note">Projected points for every remaining week. Week ', WEEK, ' = the weekly page; next week uses the lines books have posted; ',
  '<i>italic</i> weeks use projected lines and typical weather. Hover or tap a cell for the matchup, lines, weather and what moves it.</p>',
  '<div class="tabs" id="sec"></div>',
  '<div class="bar"><div class="grp" id="fmt"></div><div class="grp" id="view"></div>',
  '<span class="leg">colour: <span style="background:rgba(var(--pos),.45)">better</span> / <span style="background:rgba(var(--neg),.45)">worse</span> than that week&#39;s average · click a column to sort</span></div>',
  '<div class="wrap"><table id="grid"></table></div><div id="pairs" style="display:none"></div><div id="tip"></div>',
  # D/ST pairs tab (Andrew 2026-10-07): built in the page from the D/ST projections above (ros_utils.R, ROS_JS)
  '<div id="pairs_help" hidden><details><summary>How pairs are scored</summary><ul>',
  '<li><b>Rule:</b> with two D/STs, each week you start the one projected higher. On a bye the other one plays; if both are on bye that week scores 0 (marked ⚠).</li>',
  '<li><b>Pair pts/wk:</b> the points that rule gives, averaged over the weeks in the window (rest of season = this week through week ', max(INP$playoff_weeks %||% 15:17), '; playoffs = weeks ',
  paste(range(INP$playoff_weeks %||% 15:17), collapse = "–"), '). Use it to choose a pair.</li>',
  '<li><b>Gain:</b> pair points minus your D/ST on its own (or, for overall pairs and the heatmap&#39;s <i>Pairing gain</i>, minus the better of the two on its own). It measures how well the schedules cover each other: byes and weak matchups that fall in different weeks.</li>',
  '<li><b>Uncertainty:</b> weeks after next use projected lines (<i>italic</i>), so they are rougher; the projections already pull far-off weeks toward average, which keeps distant gains small. The page rebuilds daily as lines post.</li>',
  '<li><b>Not available:</b> click teams in the row of buttons to hide D/STs that are rostered in your league; the page remembers them on this device.</li></ul></details></div>',
  sprintf('<p class="note">%s</p>', rel_txt),
  '<details><summary>How this works</summary><ul>',
  '<li><b>Models:</b> the weekly D/ST (ESPN, Yahoo, FFPC) and kicker (ESPN, decimal) blends, fed each future matchup. Team, opponent, QB and kicker rates are as of the last game played, so they don&#39;t change between now and a future week.</li>',
  '<li><b>Lines:</b> this week = the latest sportsbook consensus; next week = the lookahead lines books have posted (or nflverse&#39;s); later weeks = projected. Every posted line gives two implied team totals; a ridge fit of implied total = league average + team offense + opponent defense + home field (recent weeks count most) projects any future game. In the 2021–25 back-test the projected implied totals missed the eventual closing line by 2.1 points one week out, 2.6 at four weeks and 3.1 at eight (3.7 with no team information).</li>',
  '<li><b>Weather:</b> this week&#39;s Open-Meteo forecast where available, otherwise the 2016–25 typical weather for that stadium, date (±10 days) and kickoff hour (chances of cold and strong wind enter the models as probabilities). Domes and retractable roofs count as indoor.</li>',
  '<li><b>Columns:</b> Next 3 / Next 4 = total points over the next 3 / 4 weeks including this one (a bye counts 0); ROS avg = average over the remaining games; Wk 15–17 = fantasy playoff total.</li>',
  '<li><b>Not included:</b> future injuries, QB or kicker changes, coaching changes. The page rebuilds every day with new lines; the models refit every Tuesday.</li></ul></details>',
  '<script>const D=', json, ';', ROS_JS(), '</script></body></html>')
out_dir <- if (dir.exists(file.path(PROJ_DIR, "site"))) file.path(PROJ_DIR, "site", "ros") else file.path(PROJ_DIR, "output", "ros")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out <- if (basename(dirname(out_dir)) == "site") file.path(out_dir, "index.html") else file.path(out_dir, paste0("ros_", wk, ".html"))
writeLines(html, out, useBytes = TRUE)
say("ROS page: %s (%.0f KB)", out, file.size(out) / 1024)
