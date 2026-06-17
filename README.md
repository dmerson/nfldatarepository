# NFL Data Analysis

An interactive R Shiny app for exploring NFL team performance from 1999 through 2025. Browse all-time records, season-by-season breakdowns, head-to-head history, team trends, home field advantage rankings, and regression analyses that test how well prior-year performance and schedule difficulty predict future win percentage.

**Live app:** https://dmerson.shinyapps.io/nfldataanalysis/

---

## Running Locally

**Prerequisites:** R (≥ 4.1) and RStudio.

```r
# 1. Install dependencies (run once)
source("install_packages.R")

# 2. Launch the app
shiny::runApp()
```

On first launch the app downloads ~15 MB of game data from the [nflverse](https://nflverse.nflverse.com/) public repository and caches it to `data/nfl_games_cache.rds`. Subsequent launches load from the cache and start instantly.

To refresh the data mid-season, delete the cache file:

```r
file.remove("data/nfl_games_cache.rds")
```

## Deploying to shinyapps.io

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "YOUR_NAME", token = "YOUR_TOKEN", secret = "YOUR_SECRET")
rsconnect::deployApp()
```

Credentials are found under **Account → Tokens** on your shinyapps.io dashboard.

---

## Tabs

### All-Time Records
A sortable, filterable table of every franchise's cumulative stats across any season range you choose. Columns include:

| Column | Description |
|--------|-------------|
| W / L / T | Wins, losses, ties |
| Win% | `(W + 0.5×T) / G` — standard NFL convention |
| PF / PA | Total points for and against |
| Avg PM | Average point differential per game |
| SOS Adj PM | `Avg PM + avg(opponent Avg PM)` — rewards teams that beat good opponents by large margins |
| Opp Win% | Combined win% of all opponents faced — a raw schedule-strength measure |

Filters: season range slider, game type checkboxes (regular season, wild card, divisional, conference championship, Super Bowl). Data is downloadable as CSV or Excel.

---

### Home Field Advantage
Ranks every franchise by how much better they perform at home versus on the road. Two tables are shown — one sorted by HFA Win%, one by HFA Avg Point Margin — so you can compare whether both metrics agree on who has the strongest home field.

Games are split into three buckets:

| Bucket | Definition |
|--------|-----------|
| **Home** | True home games only |
| **Away** | Road games |
| **Neutral** | Neutral-site games (Super Bowls, international games) — tallied separately and excluded from the HFA calculation since neither team has a home advantage |

**HFA columns:**

| Column | Description |
|--------|-------------|
| `HFA_Win_Pct` | Home Win% − Away Win% |
| `HFA_Avg_PM` | Home Avg Point Margin − Away Avg Point Margin |

Filters: season range slider, game type checkboxes.

---

### Season Breakdown
The same stat columns as All-Time Records, scoped to a single season selected from a dropdown. Useful for comparing every team in a given year side by side.

---

### Head-to-Head
Pick any two franchises and see their full game history against each other. The sidebar shows a summary (record, total games, average margin) and the main panel shows every individual game from Team A's perspective. Filterable by season range and game type.

---

### Team Detail
Drill into one franchise across any season range:

- **Season-by-Season Trends chart** — line chart of Win%, Avg PM, and SOS Adj PM by year, making it easy to spot dynasty runs and down periods.
- **Game Log table** — every game the team played, sortable and downloadable.

---

### Regression
Tests whether a team's upcoming schedule difficulty — measured by how their future opponents performed *last* year — predicts their win percentage in the coming season. Uses a single predictor.

**How the predictive score is built:**
1. For team T in season A, look at every opponent on T's season A+1 schedule.
2. For each of those opponents, look up their chosen quality metric from season A.
3. Average those values → **Predictive Score** for team T in season A+1.

**Two metric options (radio button):**

| Metric | What it measures |
|--------|-----------------|
| **Win%** | Average win% of upcoming opponents from the prior season |
| **SOS Adj PM** | Average SOS-adjusted point differential of upcoming opponents from the prior season |

The tab displays R², adjusted R², slope, and p-value in summary boxes, a scatter plot of predictive score vs actual next-season win%, and the full underlying dataset.

---

### Multi Regression
A two-predictor model that combines the team's own prior-year performance with a forward-looking schedule difficulty estimate.

**Model:** `Win%(A+1) ~ Team Win%(A) + Avg Opp Win%(A) for A+1 schedule`

| Predictor | How it's built |
|-----------|---------------|
| **Team Win%(A)** | Team's own win% in season A |
| **Avg Opp Win%(A) for A+1 schedule** | For each opponent on the team's A+1 schedule, take that opponent's win% from season A, then average across the full schedule. This is a pre-season estimate of how hard next year will be. |
| **Outcome** | Team's actual Win%(A+1) |

**Visualizations:**

| Panel | What it tells you |
|-------|------------------|
| **R² / Adj R²** | How much of next-season win% variance the two predictors jointly explain |
| **Coefficient table** | Estimate, std error, t-stat, and p-value for each predictor |
| **Actual vs Predicted** | Model-fitted win% (x) vs actual win% (y); the dashed diagonal is perfect prediction |
| **Partial: Team Win%(A)** | Marginal effect of prior-year performance after controlling for schedule difficulty |
| **Partial: Avg Opp Win%(A) for A+1 Schedule** | Marginal effect of schedule difficulty after controlling for prior-year performance |

---

## File Structure

```
nfldataanalysis/
│
├── app.R                   # Main Shiny application — UI layout and server logic
├── install_packages.R      # Run once to install all R package dependencies
│
├── R/
│   ├── data_load.R         # Downloads and caches game data via nflreadr;
│   │                       # converts one-row-per-game to two-rows-per-game
│   │                       # (one per team perspective)
│   │
│   ├── stats_calc.R        # All statistics functions:
│   │                       #   calc_records()                — W-L-T, Win%, points
│   │                       #   calc_sos_adjusted_pm()        — SOS Adj PM
│   │                       #   calc_opponent_record()        — opponent W-L-T
│   │                       #   calc_full_summary()           — all three joined
│   │                       #   calc_home_field_advantage()   — home/away/neutral splits + HFA
│   │                       #   calc_head_to_head()           — game log for two teams
│   │
│   ├── regression.R        # All regression functions:
│   │                       #   compute_season_metric()         — Win% or SOS Adj PM per team-season
│   │                       #   build_regression_data()         — single-predictor dataset
│   │                       #   fit_regression()                — lm(), R²/slope/p-value
│   │                       #   plot_regression()               — scatter with regression line
│   │                       #   build_multi_regression_data()   — two-predictor dataset
│   │                       #   fit_multi_regression()          — lm(), coefficient table
│   │                       #   plot_actual_vs_predicted()      — fitted vs actual scatter
│   │                       #   plot_partial_regression()       — partial regression plots
│   │
│   └── team_mappings.R     # Handles relocated franchises (OAK→LV, SD→LAC, STL→LAR);
│                           # maps abbreviations to full team names
│
└── data/                   # Auto-created at runtime; holds the cached RDS file
                            # (excluded from git via .gitignore)
```

---

## Data Source

Game data is sourced from [nflverse](https://nflverse.nflverse.com/) via the [`nflreadr`](https://nflreadr.nflverse.com/) R package. No API key is required. Coverage begins with the 1999 season.
