# Statistics calculation functions.
# All functions accept a `team_rows` dataframe (two rows per game, one per team)
# and optional filters for season range and game type.

library(dplyr)

# Valid game_type values in the data:
#   "REG" = regular season
#   "WC"  = wild card
#   "DIV" = divisional round
#   "CON" = conference championship
#   "SB"  = Super Bowl

# ─────────────────────────────────────────────────────────────
# HELPER: apply season + game type filters
# ─────────────────────────────────────────────────────────────
filter_games <- function(team_rows,
                         season_min = NULL, season_max = NULL,
                         game_types = c("REG", "WC", "DIV", "CON", "SB")) {
  data <- team_rows %>% filter(game_type %in% game_types)
  if (!is.null(season_min)) data <- data %>% filter(season >= season_min)
  if (!is.null(season_max)) data <- data %>% filter(season <= season_max)
  data
}

# ─────────────────────────────────────────────────────────────
# 1. Win-loss-tie records + basic point stats
# ─────────────────────────────────────────────────────────────
calc_records <- function(team_rows,
                         season_min = NULL, season_max = NULL,
                         game_types = c("REG", "WC", "DIV", "CON", "SB"),
                         group_by_season = FALSE) {

  data <- filter_games(team_rows, season_min, season_max, game_types)
  grp  <- if (group_by_season) c("team", "season") else "team"

  data %>%
    group_by(across(all_of(grp))) %>%
    summarise(
      G        = n(),
      W        = sum(result == "W"),
      L        = sum(result == "L"),
      T        = sum(result == "T"),
      # Win% counts ties as half a win (standard NFL convention)
      Win_Pct  = round((W + 0.5 * T) / G, 3),
      PF       = sum(team_score),     # Points For
      PA       = sum(opp_score),      # Points Against
      # Avg point differential per game (positive = outscoring opponents on avg)
      Avg_PM   = round(mean(point_diff), 2),
      .groups  = "drop"
    )
}

# ─────────────────────────────────────────────────────────────
# 2. Strength-of-schedule adjusted plus-minus
#    Formula: team avg point diff + avg(avg point diff of each opponent)
#    Interpretation: teams that beat good opponents by big margins rank highest
# ─────────────────────────────────────────────────────────────
calc_sos_adjusted_pm <- function(team_rows,
                                 season_min = NULL, season_max = NULL,
                                 game_types = c("REG", "WC", "DIV", "CON", "SB"),
                                 group_by_season = FALSE) {

  data <- filter_games(team_rows, season_min, season_max, game_types)
  grp  <- if (group_by_season) c("team", "season") else "team"

  # Step 1: compute each team's own average point differential in this window
  opp_quality <- data %>%
    group_by(team) %>%
    summarise(avg_pm = mean(point_diff), .groups = "drop")

  # Step 2: join opponent quality onto each game row, then average it per team
  data %>%
    left_join(opp_quality, by = c("opponent" = "team")) %>%
    rename(opp_avg_pm = avg_pm) %>%
    group_by(across(all_of(grp))) %>%
    summarise(
      Own_Avg_PM    = round(mean(point_diff), 2),
      # Average of the opponents' own avg point differential
      Opp_Avg_PM    = round(mean(opp_avg_pm, na.rm = TRUE), 2),
      # The combined adjusted metric
      SOS_Adj_PM    = round(Own_Avg_PM + Opp_Avg_PM, 2),
      .groups = "drop"
    )
}

# ─────────────────────────────────────────────────────────────
# 3. Opponent total record (combined W-L-T of all teams faced)
#    A measure of schedule strength based on wins, not point margins
# ─────────────────────────────────────────────────────────────
calc_opponent_record <- function(team_rows,
                                 season_min = NULL, season_max = NULL,
                                 game_types = c("REG", "WC", "DIV", "CON", "SB"),
                                 group_by_season = FALSE) {

  data <- filter_games(team_rows, season_min, season_max, game_types)
  grp  <- if (group_by_season) c("team", "season") else "team"

  # Step 1: each team's record in the filtered window
  all_records <- data %>%
    group_by(team) %>%
    summarise(
      opp_W = sum(result == "W"),
      opp_L = sum(result == "L"),
      opp_T = sum(result == "T"),
      .groups = "drop"
    )

  # Step 2: for each team, sum the records of every opponent they faced
  data %>%
    left_join(all_records, by = c("opponent" = "team")) %>%
    group_by(across(all_of(grp))) %>%
    summarise(
      Opp_W      = sum(opp_W, na.rm = TRUE),
      Opp_L      = sum(opp_L, na.rm = TRUE),
      Opp_T      = sum(opp_T, na.rm = TRUE),
      Opp_GP     = Opp_W + Opp_L + Opp_T,
      # Win% of the combined opponent pool
      Opp_Win_Pct = round((Opp_W + 0.5 * Opp_T) / Opp_GP, 3),
      .groups = "drop"
    )
}

# ─────────────────────────────────────────────────────────────
# 4. Combined summary table (all three stat groups joined)
# ─────────────────────────────────────────────────────────────
calc_full_summary <- function(team_rows,
                              season_min = NULL, season_max = NULL,
                              game_types = c("REG", "WC", "DIV", "CON", "SB"),
                              group_by_season = FALSE) {

  records  <- calc_records(team_rows, season_min, season_max, game_types, group_by_season)
  sos      <- calc_sos_adjusted_pm(team_rows, season_min, season_max, game_types, group_by_season)
  opp_rec  <- calc_opponent_record(team_rows, season_min, season_max, game_types, group_by_season)

  join_keys <- if (group_by_season) c("team", "season") else "team"

  records %>%
    left_join(sos,     by = join_keys) %>%
    left_join(opp_rec, by = join_keys)
}

# ─────────────────────────────────────────────────────────────
# 5. Home field advantage
#    Splits each team's record and avg point differential into
#    true home games vs away+neutral games, then computes the difference.
#    venue_type column must exist in team_rows (added in data_load.R).
# ─────────────────────────────────────────────────────────────
calc_home_field_advantage <- function(team_rows,
                                      season_min = NULL, season_max = NULL,
                                      game_types = c("REG", "WC", "DIV", "CON", "SB")) {

  data <- filter_games(team_rows, season_min, season_max, game_types)

  # Helper: summarise W-L-T, Win%, and Avg PM for a filtered slice
  summarise_slice <- function(df, prefix) {
    df %>%
      group_by(team) %>%
      summarise(
        G      = n(),
        W      = sum(result == "W"),
        L      = sum(result == "L"),
        T      = sum(result == "T"),
        Win_Pct = round((W + 0.5 * T) / G, 3),
        Avg_PM  = round(mean(point_diff), 2),
        .groups = "drop"
      ) %>%
      rename_with(~ paste0(prefix, "_", .), -team)
  }

  home_stats    <- summarise_slice(data %>% filter(venue_type == "home"),    "Home")
  away_stats    <- summarise_slice(data %>% filter(venue_type != "home"),    "Away")  # away + neutral
  neutral_stats <- summarise_slice(data %>% filter(venue_type == "neutral"), "Neut")

  home_stats %>%
    left_join(away_stats,    by = "team") %>%
    left_join(neutral_stats, by = "team") %>%
    mutate(
      # Home field advantage = home minus away+neutral
      HFA_Win_Pct = round(Home_Win_Pct - Away_Win_Pct, 3),
      HFA_Avg_PM  = round(Home_Avg_PM  - Away_Avg_PM,  2),
      Team        = sapply(team, get_team_name)
    ) %>%
    select(
      Team, team,
      Home_G, Home_W, Home_L, Home_T, Home_Win_Pct, Home_Avg_PM,
      Away_G, Away_W, Away_L, Away_T, Away_Win_Pct, Away_Avg_PM,
      Neut_G, Neut_W, Neut_L, Neut_T, Neut_Win_Pct, Neut_Avg_PM,
      HFA_Win_Pct, HFA_Avg_PM
    ) %>%
    arrange(desc(HFA_Win_Pct))
}

# ─────────────────────────────────────────────────────────────
# 6. Head-to-head game log between two specific teams
# ─────────────────────────────────────────────────────────────
calc_head_to_head <- function(team_rows, team_a, team_b,
                              season_min = NULL, season_max = NULL,
                              game_types = c("REG", "WC", "DIV", "CON", "SB")) {

  data <- filter_games(team_rows, season_min, season_max, game_types)

  # Keep only games where team_a played team_b, from team_a's perspective
  data %>%
    filter(team == team_a, opponent == team_b) %>%
    arrange(gameday) %>%
    select(season, week, game_type, gameday, home_away, stadium,
           team, team_score, opp_score, opponent, point_diff, result, overtime)
}
