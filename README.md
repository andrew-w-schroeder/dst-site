# D/ST projections site

Weekly fantasy D/ST projections (ESPN, Yahoo, FFPC) and kicker projections (ESPN, decimal) re-scored with the
latest sportsbook lines (kickers also with the latest weather forecast).
D/ST: `https://<your-username>.github.io/dst-site/` (past weeks `/archive/`) · kickers: `/k/` (past weeks `/k/archive/`).

How it works

* **Tuesday, on my computer:** `43_dst_run_all.R` fits the models and `46_dst_publish.R` pushes this week's
  scoring bundles (`output/dst/<system>/bundle_*.rds`, coefficients only, no raw data) to this repo.
* **Every day until kickoff:** `.github/workflows/dst_refresh.yml` runs `scripts/45_dst_refresh.R`, which pulls
  consensus spreads/totals from The Odds API (secret `ODDS_API_KEY`), re-scores all three formats, appends the
  lines to `data/lines/line_history.csv` and publishes `site/` to GitHub Pages. Started games are locked at their
  last pre-kickoff line. Then `scripts/55_k_refresh.R` re-scores the kickers with the same lines plus Open-Meteo
  forecasts (`data/lines/weather_history.csv`; no key, no extra Odds API credits) and writes `site/k/`.
  Both pages get tiers, forecast gusts / rain, a dotted trend line per team (`data/lines/proj_history.csv`)
  and hover text with what drives each projection (`scripts/site_utils.R`).
* **Starting QBs:** every refresh re-checks each offense's starter (`scripts/starters.R`: official injury report,
  Sleeper and Ourlads depth charts, nflverse schedule) and re-scores a new starter exactly; kickers get an
  injury-status flag. Choices are logged to `data/lines/starter_history.csv`.
  **To force a starter:** edit `data/lines/qb_override.csv` on GitHub (pencil icon) and add a line such as
  `2026,3,WAS,Marcus Mariota,Daniels out (hamstring)` — saving it starts a refresh. Rows only apply to their week.
* **Run it now:** Actions tab → DST refresh → Run workflow.

Files in this repo are written by the scripts in `~/ML/ff`; edit them there, not here.
