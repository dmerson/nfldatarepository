# Data loading functions.
# nflreadr fetches from the nflverse public GitHub repo — no API key needed.
# The first load downloads ~10-15 MB; subsequent runs use the local cache.

library(nflreadr)
library(dplyr)

source("R/team_mappings.R")

# Load game schedules for all seasons and cache them locally.
# Set use_cache = FALSE to force a fresh download (e.g., mid-season updates).
load_nfl_games <- function(seasons = 1999:2025, use_cache = TRUE) {
  cache_file <- "data/nfl_games_cache.rds"

  if (use_cache && file.exists(cache_file)) {
    message("Loading from cache: ", cache_file)
    return(readRDS(cache_file))
  }

  message("Downloading NFL schedule data from nflverse (seasons ", min(seasons), "-", max(seasons), ")...")
  raw <- nflreadr::load_schedules(seasons = seasons)

  # Keep only games that have been played (both scores exist)
  games <- raw %>%
    filter(!is.na(away_score), !is.na(home_score)) %>%
    select(
      game_id, season, week, game_type, gameday,
      away_team, away_score,
      home_team, home_score,
      location, stadium, overtime, div_game
    ) %>%
    # Normalize team abbreviations so relocated franchises are consistent
    mutate(
      away_team = normalize_team(away_team),
      home_team = normalize_team(home_team)
    )

  dir.create("data", showWarnings = FALSE)
  saveRDS(games, cache_file)
  message("Cached to ", cache_file)

  games
}

# Convert the one-row-per-game format into two-rows-per-game
# (one from each team's perspective). This makes per-team stats trivial to compute.
games_to_team_rows <- function(games) {
  # Away team's view of each game
  away <- games %>%
    transmute(
      game_id, season, week, game_type, gameday, stadium, overtime, div_game,
      team      = away_team,
      opponent  = home_team,
      team_score = away_score,
      opp_score  = home_score,
      home_away  = "away",
      point_diff = away_score - home_score,
      result = case_when(
        away_score > home_score ~ "W",
        away_score < home_score ~ "L",
        TRUE ~ "T"
      )
    )

  # Home team's view of each game
  home <- games %>%
    transmute(
      game_id, season, week, game_type, gameday, stadium, overtime, div_game,
      team      = home_team,
      opponent  = away_team,
      team_score = home_score,
      opp_score  = away_score,
      home_away  = "home",
      point_diff = home_score - away_score,
      result = case_when(
        home_score > away_score ~ "W",
        home_score < away_score ~ "L",
        TRUE ~ "T"
      )
    )

  bind_rows(away, home)
}

# Convenience: get sorted list of seasons present in the data
available_seasons <- function(games) sort(unique(games$season))

# Convenience: get sorted list of all team abbreviations
available_teams <- function(team_rows) sort(unique(team_rows$team))
