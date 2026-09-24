# ==============================================================================
# 46_dst_publish.R — send this week's model to the website repo (dst-site) on GitHub
#
# Copies ONLY what the daily refresh needs — the scoring bundles + report parts for the newest week and
# the three scripts 44 / 45 / dst_blend_utils — into the site folder, commits and pushes. The push starts
# the GitHub Action, which pulls current lines, re-scores and publishes the page within ~2 minutes.
# Raw data, caches, logs and the pro.nfl.com token are never copied (and are .gitignored in the site repo).
#
# First run (one time): creates the site folder, runs `git init` and connects it to the empty GitHub repo
# in DST_SITE_REPO. Every run also rewrites the GitHub workflow, .gitignore and README from the templates below. Called at the end of 43_dst_run_all.R.
# Usage: Rscript 46_dst_publish.R [season] [week]      (defaults: newest bundle week)
# ==============================================================================

PROJ_DIR  <- Sys.getenv("FF_PROJ_DIR", path.expand("~/ML/ff"))
SITE_DIR  <- Sys.getenv("DST_SITE_DIR", path.expand("~/ML/dst-site"))
SITE_REPO <- Sys.getenv("DST_SITE_REPO", "")      # e.g. https://github.com/<your-username>/dst-site.git (first run only)
SYSTEMS   <- c("espn", "yahoo", "ffpc")
DST_DIR   <- file.path(PROJ_DIR, "output/dst")

git <- function(..., ok_fail = FALSE) {
  out <- suppressWarnings(system2("git", c("-C", shQuote(SITE_DIR), ...), stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status"); if (!is.null(st) && st != 0 && !ok_fail) stop("git ", paste(c(...), collapse = " "), " failed:\n", paste(out, collapse = "\n"), call. = FALSE)
  invisible(out)
}
if (Sys.which("git") == "") stop("git is not installed: see the setup guide (claude/dst-website-setup.md)")

## ---- 1. Which week ----
a <- commandArgs(trailingOnly = TRUE)
if (length(a) >= 2) { SEASON <- as.integer(a[1]); WEEK <- as.integer(a[2]) } else {
  f <- list.files(file.path(DST_DIR, SYSTEMS), pattern = "^bundle_\\d{4}_wk\\d{2}\\.rds$")
  if (!length(f)) stop("no scoring bundles: run 43_dst_run_all.R first")
  key <- max(sub("bundle_(\\d{4})_wk(\\d{2}).*", "\\1\\2", f)); SEASON <- as.integer(substr(key, 1, 4)); WEEK <- as.integer(substr(key, 5, 6))
}
wk <- sprintf("%d_wk%02d", SEASON, WEEK)

## ---- 2. Site repo: create + connect on the first run; (re)write the workflow, .gitignore and README ----
TEMPLATES <- list(
  ".github/workflows/dst_refresh.yml" = r"---[# Re-scores this week's D/ST projections with the latest sportsbook lines and publishes the page.
# Times are UTC (GitHub cron has no time zones). EDT = UTC-4 until Nov 1 2026, then EST = UTC-5.
# GitHub may start a scheduled run 5-30 min late; games that have kicked off are locked at their
# last pre-kickoff line, so a late run never uses in-game odds.
name: DST refresh

on:
  schedule:
    - cron: "0 14 * * 2-6"   # Tue-Sat 14:00 UTC = 10:00 am EDT / 9:00 am EST
    - cron: "30 22 * * 4"    # Thu 22:30 UTC = 6:30 pm EDT, before Thursday Night Football
    - cron: "0 13 * * 0"     # Sun 13:00 UTC = 9:00 am EDT (before London games)
    - cron: "45 15 * * 0"    # Sun 15:45 UTC = 11:45 am EDT, after inactives
    - cron: "35 16 * * 0"    # Sun 16:35 UTC = 12:35 pm EDT / 11:35 am EST, last look before 1 pm kickoffs
  push:                      # a new weekly model was published from RStudio
    branches: [main]
    paths: ["output/dst/**"]
  workflow_dispatch:         # "Run workflow" button on the Actions tab

permissions:
  contents: write            # commit line history + archive
  pages: write               # publish the site
  id-token: write

concurrency:
  group: dst-refresh
  cancel-in-progress: false

jobs:
  refresh:
    runs-on: ubuntu-24.04    # pinned: ubuntu-latest moves to 26.04 in Oct 2026
    environment:
      name: github-pages
      url: ${{ steps.deploy.outputs.page_url }}
    steps:
      - uses: actions/checkout@v5

      - uses: r-lib/actions/setup-r@v2
        with:
          use-public-rspm: true   # prebuilt Linux binaries: packages install in seconds

      - name: Install R packages
        run: Rscript -e 'install.packages(c("dplyr", "tidyr", "purrr", "stringr", "tibble", "jsonlite", "curl"))'

      - name: Re-score with the latest lines
        env:
          ODDS_API_KEY: ${{ secrets.ODDS_API_KEY }}
          FF_PROJ_DIR: ${{ github.workspace }}
          LANG: C.UTF-8
        run: Rscript scripts/45_dst_refresh.R

      - name: Save line history and archive
        run: |
          git config user.name "github-actions[bot]"
          git config user.email "41898282+github-actions[bot]@users.noreply.github.com"
          git add -A            # data/lines may not exist yet (no odds pulled); work/ is gitignored
          if ! git diff --cached --quiet; then
            git commit -m "Line refresh $(date -u +'%Y-%m-%d %H:%M UTC')"
            git pull --rebase origin main && git push origin HEAD:main
          fi

      - uses: actions/configure-pages@v5
      - uses: actions/upload-pages-artifact@v3
        with:
          path: site
      - id: deploy
        uses: actions/deploy-pages@v4
]---",
  ".gitignore" = r"---[.Rhistory
.RData
.Rproj.user/
*.log
# never publish credentials or raw data
*token*
*.env
.Renviron
work/
data/nflpro/
data/dst/
]---",
  "README.md" = r"---[# D/ST projections site

Weekly fantasy D/ST projections (ESPN, Yahoo, FFPC) re-scored with the latest sportsbook lines.
The page lives at `https://<your-username>.github.io/dst-site/`; past weeks are under `/archive/`.

How it works

* **Tuesday, on my computer:** `43_dst_run_all.R` fits the models and `46_dst_publish.R` pushes this week's
  scoring bundles (`output/dst/<system>/bundle_*.rds`, coefficients only, no raw data) to this repo.
* **Every day until kickoff:** `.github/workflows/dst_refresh.yml` runs `scripts/45_dst_refresh.R`, which pulls
  consensus spreads/totals from The Odds API (secret `ODDS_API_KEY`), re-scores all three formats, appends the
  lines to `data/lines/line_history.csv` and publishes `site/` to GitHub Pages. Started games are locked at their
  last pre-kickoff line.
* **Run it now:** Actions tab → DST refresh → Run workflow.

Files in this repo are written by the scripts in `~/ML/ff`; edit them there, not here.
]---")
first <- !dir.exists(file.path(SITE_DIR, ".git"))
if (first) {
  if (!nzchar(SITE_REPO)) stop("first publish: set DST_SITE_REPO to your empty GitHub repo URL, e.g.\n",
                               "  Sys.setenv(DST_SITE_REPO = \"https://github.com/<your-username>/dst-site.git\")", call. = FALSE)
  message("creating the site repo in ", SITE_DIR)
  dir.create(SITE_DIR, recursive = TRUE, showWarnings = FALSE)
  git("init", "-q"); git("symbolic-ref", "HEAD", "refs/heads/main"); git("remote", "add", "origin", SITE_REPO)
}
for (f in names(TEMPLATES)) {
  dir.create(dirname(file.path(SITE_DIR, f)), recursive = TRUE, showWarnings = FALSE)
  writeLines(TEMPLATES[[f]], file.path(SITE_DIR, f), sep = "")
}

## ---- 3. Copy this week's files (and drop older weeks' bundles; the HTML archive keeps past weeks) ----
dir.create(file.path(SITE_DIR, "scripts"), recursive = TRUE, showWarnings = FALSE)
for (s in c("44_dst_report.R", "45_dst_refresh.R", "dst_blend_utils.R"))
  file.copy(file.path(PROJ_DIR, "scripts", s), file.path(SITE_DIR, "scripts", s), overwrite = TRUE)
n <- 0
for (sys in SYSTEMS) {
  src <- file.path(DST_DIR, sys, paste0(c("bundle_", "report_parts_"), wk, ".rds"))
  if (!all(file.exists(src))) { warning(sys, ": no bundle / report parts for ", wk, " — not published"); next }
  dst <- file.path(SITE_DIR, "output/dst", sys); dir.create(dst, recursive = TRUE, showWarnings = FALSE)
  old <- setdiff(list.files(dst, pattern = "^(bundle|report_parts)_.*\\.rds$"), basename(src))
  if (length(old)) { git("rm", "-q", "--cached", "--ignore-unmatch", file.path("output/dst", sys, old), ok_fail = TRUE); file.remove(file.path(dst, old)) }
  file.copy(src, dst, overwrite = TRUE); n <- n + 1
}
if (!n) stop("nothing to publish")

## ---- 4. Commit + push ----
if (!length(git("config", "user.name", ok_fail = TRUE)))
  stop("git doesn't know who you are yet: run the usethis::use_git_config() step in the setup guide", call. = FALSE)
git("add", "-A")
if (length(git("status", "--porcelain"))) git("commit", "-q", "-m", shQuote(sprintf("Weekly model %s week %d", SEASON, WEEK)))
git("pull", "--rebase", "-q", "-X", "theirs", "origin", "main", ok_fail = TRUE)   # pick up the bot's line-history commits
out <- git("push", "-u", "origin", "main", ok_fail = TRUE)
if (!is.null(attr(out, "status"))) stop("git push failed:\n", paste(out, collapse = "\n"),
                                        "\nIf it mentions authentication, redo the gitcreds::gitcreds_set() step in the setup guide.", call. = FALSE)
message(sprintf("published %s week %d (%d systems). The GitHub Action now pulls current lines and updates the site (~2 min).", SEASON, WEEK, n))
