# Team relocation mapping: some franchises moved cities and got new abbreviations.
# This table lets us group all seasons of a franchise together under one label,
# rather than treating "OAK" and "LV" as different teams.

# Maps each historical abbreviation to its current/canonical abbreviation
TEAM_RELOCATIONS <- c(
  "OAK" = "LV",   # Oakland Raiders → Las Vegas Raiders (moved 2020)
  "SD"  = "LAC",  # San Diego Chargers → LA Chargers (moved 2017)
  "STL" = "LAR"   # St. Louis Rams → LA Rams (moved 2016)
)

# Apply relocation mapping to a vector of team abbreviations.
# Teams that haven't moved are returned unchanged.
normalize_team <- function(team_abbr) {
  ifelse(team_abbr %in% names(TEAM_RELOCATIONS),
         TEAM_RELOCATIONS[team_abbr],
         team_abbr)
}

# Full team name lookup (keyed by current abbreviation)
TEAM_NAMES <- c(
  "ARI" = "Arizona Cardinals",
  "ATL" = "Atlanta Falcons",
  "BAL" = "Baltimore Ravens",
  "BUF" = "Buffalo Bills",
  "CAR" = "Carolina Panthers",
  "CHI" = "Chicago Bears",
  "CIN" = "Cincinnati Bengals",
  "CLE" = "Cleveland Browns",
  "DAL" = "Dallas Cowboys",
  "DEN" = "Denver Broncos",
  "DET" = "Detroit Lions",
  "GB"  = "Green Bay Packers",
  "HOU" = "Houston Texans",
  "IND" = "Indianapolis Colts",
  "JAX" = "Jacksonville Jaguars",
  "KC"  = "Kansas City Chiefs",
  "LAC" = "Los Angeles Chargers",
  "LAR" = "Los Angeles Rams",
  "LV"  = "Las Vegas Raiders",
  "MIA" = "Miami Dolphins",
  "MIN" = "Minnesota Vikings",
  "NE"  = "New England Patriots",
  "NO"  = "New Orleans Saints",
  "NYG" = "New York Giants",
  "NYJ" = "New York Jets",
  "PHI" = "Philadelphia Eagles",
  "PIT" = "Pittsburgh Steelers",
  "SEA" = "Seattle Seahawks",
  "SF"  = "San Francisco 49ers",
  "TB"  = "Tampa Bay Buccaneers",
  "TEN" = "Tennessee Titans",
  "WAS" = "Washington Commanders"
)

# Given an abbreviation (current or historical), return the full franchise name.
get_team_name <- function(abbr) {
  canonical <- normalize_team(abbr)
  name <- TEAM_NAMES[canonical]
  ifelse(is.na(name), abbr, name)  # Fall back to abbr if not found
}

# Current home stadium coordinates for each franchise.
# LAC/LAR share SoFi Stadium and NYG/NYJ share MetLife — offset slightly so
# both circles are visible on the map.
STADIUM_COORDS <- data.frame(
  team = c(
    "ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
    "DAL", "DEN", "DET", "GB",  "HOU", "IND", "JAX", "KC",
    "LAC", "LAR", "LV",  "MIA", "MIN", "NE",  "NO",  "NYG",
    "NYJ", "PHI", "PIT", "SEA", "SF",  "TB",  "TEN", "WAS"
  ),
  lat = c(
    33.5277, 33.7553, 39.2779, 42.7738, 35.2258, 41.8623, 39.0955, 41.5061,
    32.7480, 39.7439, 42.3400, 44.5013, 29.6847, 39.7601, 30.3239, 39.0489,
    33.9528, 33.9542, 36.0909, 25.9580, 44.9736, 42.0909, 29.9511, 40.8128,
    40.8142, 39.9008, 40.4468, 47.5952, 37.4033, 27.9759, 36.1665, 38.9078
  ),
  lon = c(
    -112.2626, -84.4006, -76.6227, -78.7870, -80.8528, -87.6167, -84.5161, -81.6995,
     -97.0928,-105.0201, -83.0456, -88.0622, -95.4107, -86.1639, -81.6373, -94.4839,
    -118.3398,-118.3386,-115.1833, -80.2389, -93.2575, -71.2643, -90.0812, -74.0752,
     -74.0738, -75.1675, -80.0158,-122.3316,-121.9694, -82.5033, -86.7713, -76.8645
  ),
  stadium = c(
    "State Farm Stadium",       "Mercedes-Benz Stadium",  "M&T Bank Stadium",
    "Highmark Stadium",         "Bank of America Stadium","Soldier Field",
    "Paycor Stadium",           "Huntington Bank Field",  "AT&T Stadium",
    "Empower Field at Mile High","Ford Field",             "Lambeau Field",
    "NRG Stadium",              "Lucas Oil Stadium",      "EverBank Stadium",
    "Arrowhead Stadium",        "SoFi Stadium (LAC)",     "SoFi Stadium (LAR)",
    "Allegiant Stadium",        "Hard Rock Stadium",      "U.S. Bank Stadium",
    "Gillette Stadium",         "Caesars Superdome",      "MetLife Stadium (NYG)",
    "MetLife Stadium (NYJ)",    "Lincoln Financial Field","Acrisure Stadium",
    "Lumen Field",              "Levi's Stadium",         "Raymond James Stadium",
    "Nissan Stadium",           "Northwest Stadium"
  ),
  stringsAsFactors = FALSE
)
