# Regression analysis: does last year's opponent quality predict next year's win%?
#
# Predictive score for team T in season A+1:
#   = average of [each opponent's chosen metric from season A] across T's A+1 schedule
#
# Two supported metrics for opponent quality:
#   "SOS_Adj_PM" — opponent's strength-of-schedule adjusted point differential
#   "Win_Pct"    — opponent's win percentage
#
# Higher pred_score → tougher upcoming schedule → expected negative correlation with Win_Pct

library(dplyr)

# ─────────────────────────────────────────────────────────────
# Compute per-season metric for every team
# Returns a two-column dataframe: (team, season, metric_value)
# ─────────────────────────────────────────────────────────────
compute_season_metric <- function(filtered, metric) {

  if (metric == "Win_Pct") {
    filtered %>%
      group_by(team, season) %>%
      summarise(
        G           = n(),
        W           = sum(result == "W"),
        T           = sum(result == "T"),
        metric_value = (W + 0.5 * T) / G,
        .groups = "drop"
      ) %>%
      select(team, season, metric_value)

  } else if (metric == "SOS_Adj_PM") {
    # Step 1: each team's own avg point diff per season
    own_pm <- filtered %>%
      group_by(team, season) %>%
      summarise(own_avg_pm = mean(point_diff), .groups = "drop")

    # Step 2: join opponent avg pm onto every game row, then summarise
    filtered %>%
      left_join(own_pm, by = c("opponent" = "team", "season" = "season")) %>%
      rename(opp_avg_pm = own_avg_pm) %>%
      group_by(team, season) %>%
      summarise(
        own_avg_pm   = mean(point_diff),
        opp_avg_pm   = mean(opp_avg_pm, na.rm = TRUE),
        metric_value = own_avg_pm + opp_avg_pm,
        .groups = "drop"
      ) %>%
      select(team, season, metric_value)

  } else {
    stop("metric must be 'Win_Pct' or 'SOS_Adj_PM'")
  }
}

# ─────────────────────────────────────────────────────────────
# Build the regression dataset
# Returns one row per (team, season A+1) with pred_score and Win_Pct
# ─────────────────────────────────────────────────────────────
build_regression_data <- function(team_rows,
                                  metric     = "SOS_Adj_PM",
                                  game_types = "REG",
                                  season_min = 1999,
                                  season_max = 2025) {

  filtered <- team_rows %>%
    filter(game_type %in% game_types,
           season    >= season_min,
           season    <= season_max)

  # Step 1: per-season opponent-quality metric for every team (season A)
  season_metric <- compute_season_metric(filtered, metric)

  # Step 2: for each game in season A+1, look up opponent's metric from season A
  pred_scores <- filtered %>%
    select(team, season, opponent) %>%
    mutate(prev_season = season - 1) %>%
    left_join(
      season_metric %>% rename(opp_metric_prev = metric_value),
      by = c("opponent" = "team", "prev_season" = "season")
    ) %>%
    # Step 3: average across the full A+1 schedule → one pred_score per (team, season)
    group_by(team, season) %>%
    summarise(
      pred_score  = mean(opp_metric_prev, na.rm = TRUE),
      n_with_data = sum(!is.na(opp_metric_prev)),
      .groups = "drop"
    ) %>%
    # Drop rows without enough opponent lookups (expansion teams, first seasons)
    filter(n_with_data >= 10)

  # Step 4: actual Win_Pct for season A+1 (always the outcome variable)
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

  # Step 5: join pred_score with the Win_Pct it is predicting
  pred_scores %>%
    inner_join(win_pcts, by = c("team", "season")) %>%
    filter(!is.na(pred_score), !is.na(Win_Pct)) %>%
    mutate(
      pred_score = round(pred_score, 4),
      Win_Pct    = round(Win_Pct, 3),
      Team       = sapply(team, get_team_name)
    ) %>%
    arrange(team, season)
}

# ─────────────────────────────────────────────────────────────
# Fit the linear model and extract key stats
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
plot_regression <- function(reg_data, model_stats, metric) {
  sig_label <- ifelse(model_stats$p_value < 0.001, "p < 0.001",
                      paste0("p = ", model_stats$p_value))

  subtitle <- glue::glue(
    "R² = {model_stats$r_squared}  |  ",
    "slope = {model_stats$coef_slope}  |  ",
    "{sig_label}  |  ",
    "n = {model_stats$n_obs} team-seasons"
  )

  x_label <- if (metric == "Win_Pct") {
    "Predictive Score (avg opponent Win% from prior season)"
  } else {
    "Predictive Score (avg opponent SOS Adj PM from prior season)"
  }

  # Win_Pct scores should be formatted as percentages on x-axis too
  x_scale <- if (metric == "Win_Pct") {
    scale_x_continuous(labels = scales::percent_format(accuracy = 1))
  } else {
    scale_x_continuous()
  }

  ggplot(reg_data, aes(x = pred_score, y = Win_Pct)) +
    geom_point(alpha = 0.35, color = "#1a6fb5", size = 1.8) +
    geom_smooth(method = "lm", se = TRUE,
                color = "#e87722", fill = "#e87722", alpha = 0.15, linewidth = 1.2) +
    x_scale +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1)) +
    labs(
      title    = "Opponent Quality (Prior Year) vs Next-Season Win%",
      subtitle = subtitle,
      x        = x_label,
      y        = "Actual Win% (season A+1)"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.subtitle = element_text(color = "grey50", size = 10))
}
