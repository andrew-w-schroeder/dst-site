# Fantasy projections site (D/ST · Kickers · ROS · Players · DFS)

`https://<your-username>.github.io/dst-site/` — every page refreshes together on one schedule.

How it works

* **Tuesday, on my computer:** `weekly_run.R` runs the weekly models (D/ST, kickers, rest of season, track record, players,
  DFS fit) and then `site_publish.R`, which copies this week's model files (coefficients and bundles, no raw data) and the
  page scripts here in **one commit and one push**.
* **One workflow, `.github/workflows/site_refresh.yml`** ("Site refresh"), runs on a schedule (Eastern times written by
  site_publish.R), on every push from site_publish.R, and from the Run workflow button. In one job, in order:
  D/ST (`45`, sportsbook lines via secret `ODDS_API_KEY`, starting QBs) → kickers (`55`, + weather forecasts) →
  rest of season (`49`) → players (`65`, live props via secret `PROPS_API_KEY`) → DFS (`96`) → save the history files
  (`data/lines`, `data/players`, `data/dfs`) → publish `site/` to GitHub Pages once. Started games are locked at their
  last pre-kickoff lines and props.
* **Mid-week:** save `DKSalaries.csv` from the DK lobby in `~/ML/ff/data/dfs/` (or edit a script) and run `site_publish.R`.
* **To force a starting QB:** edit `data/lines/qb_override.csv` here on GitHub (pencil icon), e.g.
  `2026,3,WAS,Marcus Mariota,Daniels out (hamstring)` — saving it starts a refresh. Rows only apply to their week.
* **Run it now:** Actions tab → Site refresh → Run workflow.

Files in this repo are written by the scripts in `~/ML/ff`; edit them there, not here.
