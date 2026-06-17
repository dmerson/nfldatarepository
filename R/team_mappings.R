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
