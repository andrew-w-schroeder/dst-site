# ==============================================================================
# 43_dst_run_all.R — weekly driver: D/ST projections for ESPN, Yahoo and FFPC + combined report
#
# 1. (optional, run first) 41_nflpro_harvest.R with LAST_COMPLETED_WEEK bumped, so NGS data is current
# 2. set WEEK below and run this script (RStudio: Source). For each scoring system it runs
#    40_dst_model.R in a fresh R process; a system that has never been tuned also gets the one-off
#    tune → 42_dst_feature_selection.R step. Then 44_dst_report.R builds
#    output/dst/dst_proj_<season>_wk<ww>_all.html (tabs: Compare · ESPN · Yahoo · FFPC · Glossary).
# 3. 46_dst_publish.R pushes this week's scoring bundles to the dst-site GitHub repo; from then on a GitHub
#    Action re-scores with the latest sportsbook lines every day until kickoff (PUBLISH <- FALSE to skip).
# Per-system logs: output/dst/<system>/run_wk<ww>.log
# QB / weather overrides: edit qb_override / weather_override in 40_dst_model.R before running.
# ==============================================================================

PROJ_DIR <- Sys.getenv("FF_PROJ_DIR", path.expand("~/ML/ff"))
SEASON   <- 2026
WEEK     <- 3
SYSTEMS  <- c("espn", "yahoo", "ffpc")
RSCRIPT  <- file.path(R.home("bin"), "Rscript")

run_step <- function(script, sys, log) {
  message(sprintf("[%s] %s %s ...", format(Sys.time(), "%H:%M:%S"), script, sys))
  st <- system2(RSCRIPT, c(shQuote(file.path(PROJ_DIR, "scripts", script)), sys, WEEK), stdout = log, stderr = log)
  if (!identical(as.integer(st), 0L)) stop(sprintf("%s (%s) failed: see %s", script, sys, log), call. = FALSE)
}
for (sys in SYSTEMS) {
  dir.create(file.path(PROJ_DIR, "output/dst", sys), recursive = TRUE, showWarnings = FALSE)
  log <- file.path(PROJ_DIR, "output/dst", sys, sprintf("run_wk%02d.log", WEEK))
  if (!file.exists(file.path(PROJ_DIR, "output/dst", sys, "selected_groups.rds"))) {      # first time only (~15 min)
    run_step("40_dst_model.R", sys, log); run_step("42_dst_feature_selection.R", sys, sub("\\.log$", "_select.log", log))
  }
  run_step("40_dst_model.R", sys, log)
}
st <- system2(RSCRIPT, c(shQuote(file.path(PROJ_DIR, "scripts", "44_dst_report.R")), SEASON, WEEK))
if (!identical(as.integer(st), 0L)) stop("44_dst_report.R failed")
message("done: ", file.path(PROJ_DIR, "output/dst", sprintf("dst_proj_%d_wk%02d_all.html", SEASON, WEEK)))

# Website: push this week's model to GitHub; the daily line refresh takes it from there.
PUBLISH <- TRUE
if (PUBLISH) {
  st <- system2(RSCRIPT, c(shQuote(file.path(PROJ_DIR, "scripts", "46_dst_publish.R")), SEASON, WEEK))
  if (!identical(as.integer(st), 0L)) warning("publishing to the website failed (projections above are fine): see the messages above")
}
