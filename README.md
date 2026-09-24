# D/ST projections site

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
