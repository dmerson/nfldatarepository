# Regression analysis: does last year's opponent quality predict next year's win%?
#
# Predictive score for team T in season A+1:
#   = average SOS_Adj_PM (from season A) across all of T's season A+1 opponents
#
# Higher pred_score → tougher upcoming schedule → expected negative correlation with Win_Pct

library(dplyr)

# ─────────────────────────────────────────────────────────────
# Build the regression dataset
# Returns one row per (team, season A+1) with pred_score and Win_Pct
# ─────────────────────────────────────────────────────────────
build_regression_data <- function(team_rows,
                                  game_types = c("REG"),
                                  season_min = 1999,
                                  season_max = 2025) {

  filtered <- team_rows %>%
    filter(game_type %in% game_types,
           season >= season_min,
           season <= season_max)

  # Step 1: per-season SOS_Adj_PM for every team (this is "season A quality")
  season_sos <- filtered %>%
    group_by(team) %>%
    # Need per-season values, so re-compute grouped by both team and season
    { . } %>%
    # Compute avg point diff per team per season first
    group_by(team, season) %>%
    summarise(own_avg_pm = mean(point_diff), .groups = "drop") %>%
    # Then join opponent avg pm onto each game to get SOS_Adj_PM per season
    {
      opp_pm <- .
      filtered %>%
        left_join(opp_pm, by = c("opponent" = "team", "season" = "season")) %>%
        rename(opp_avg_pm = own_avg_pm) %>%
        group_by(team, season) %>%
        summarise(
          own_avg_pm = mean(point_diff),
          opp_avg_pm = mean(opp_avg_pm, na.rm = TRUE),
          SOS_Adj_PM = own_avg_pm + opp_avg_pm,
          .groups = "drop"
        )
    }

  # Step 2: get each team's schedule (opponents) for every season A+1
  # We join "next season opponents" against "this season SOS_Adj_PM"
  schedules <- filtered %>%
    select(team, season, opponent) %>%
    # For each game in season A+1, we want the opponent's SOS_Adj_PM from season A
    mutate(prev_season = season - 1) %>%
    left_join(
      season_sos %>% select(team, season, SOS_Adj_PM) %>%
        rename(opp_sos_prev = SOS_Adj_PM),
      by = c("opponent" = "team", "prev_season" = "season")
    )

  # Step 3: average across the full schedule → one pred_score per (team, season)
  pred_scores <- schedules %>%
    group_by(team, season) %>%
    summarise(
      pred_score   = mean(opp_sos_prev, na.rm = TRUE),
      n_with_data  = sum(!is.na(opp_sos_prev)),   # how many opponents had prior-year data
      .groups = "drop"
    ) %>%
    # Drop rows where we couldn't look up enough opponents (e.g. expansion team season 1)
    filter(n_with_data >= 10)

  # Step 4: get actual Win_Pct for season A+1
  win_pcts <- filtered %>%
    group_by(team, season) %>%
    summarise(
      G       = n(),
      W       = sum(result == "W"),
      L       = sum(result == "L"),
      T       = sum(result == "T"),
      Win_Pct = (W + 0.5 * T) / G,
      .groups = "drop"
    )

  # Step 5: join pred_score with the Win_Pct it's predicting
  pred_scores %>%
    inner_join(win_pcts, by = c("team", "season")) %>%
    filter(!is.na(pred_score), !is.na(Win_Pct)) %>%
    mutate(
      pred_score = round(pred_score, 3),
      Win_Pct    = round(Win_Pct, 3),
      Team       = sapply(team, get_team_name)
    ) %>%
    arrange(team, season)
}

# ─────────────────────────────────────────────────────────────
# Fit the linear model and extract key stats
# Returns a named list: model, r_squared, p_value, coef_intercept, coef_slope
# ─────────────────────────────────────────────────────────────
fit_regression <- function(reg_data) {
  model <- lm(Win_Pct ~ pred_score, data = reg_data)
  s     <- summary(model)

  list(
    model          = model,
    r_squared      = round(s$r.squared, 4),
    adj_r_squared  = round(s$adj.r.squared, 4),
    coef_intercept = round(coef(model)[["(Intercept)"]], 4),
    coef_slope     = round(coef(model)[["pred_score"]], 4),
    p_value        = round(s$coefficients["pred_score", "Pr(>|t|)"], 6),
    n_obs          = nrow(reg_data)
  )
}

# ─────────────────────────────────────────────────────────────
# Scatter plot: pred_score vs Win_Pct with regression line
# ─────────────────────────────────────────────────────────────
plot_regression <- function(reg_data, model_stats) {
  slope_dir <- ifelse(model_stats$coef_slope < 0, "negative", "positive")
  sig_label <- ifelse(model_stats$p_value < 0.001, "p < 0.001",
                      paste0("p = ", model_stats$p_value))

  subtitle <- glue::glue(
    "R² = {model_stats$r_squared}  |  ",
    "slope = {model_stats$coef_slope}  |  ",
    "{sig_label}  |  ",
    "n = {model_stats$n_obs} team-seasons"
  )

  ggplot(reg_data, aes(x = pred_score, y = Win_Pct)) +
    geom_point(alpha = 0.35, color = "#1a6fb5", size = 1.8) +
    geom_smooth(method = "lm", se = TRUE,
                color = "#e87722", fill = "#e87722", alpha = 0.15, linewidth = 1.2) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1)) +
    labs(
      title    = "Opponent Quality (Prior Year) vs Next-Season Win%",
      subtitle = subtitle,
      x        = "Predictive Score (avg opponent SOS Adj PM from prior season)",
      y        = "Actual Win% (season A+1)"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.subtitle = element_text(color = "grey50", size = 10))
}
