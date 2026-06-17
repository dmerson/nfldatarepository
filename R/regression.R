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

# ═════════════════════════════════════════════════════════════
# MULTIPLE REGRESSION
# Model: Win_Pct(A+1) ~ Win_Pct(A) + Avg_Opp_Win_Pct(A+1 schedule, from season A)
# ═════════════════════════════════════════════════════════════

# Build the dataset for multiple regression.
# Each row is one (team, season A+1) with three values:
#   own_win_pct_A    — team's own Win% in season A (prior year performance)
#   avg_opp_win_pct  — avg Win%(A) of every opponent on the A+1 schedule (schedule difficulty)
#   Win_Pct          — team's actual Win% in season A+1 (outcome)
build_multi_regression_data <- function(team_rows,
                                        game_types = "REG",
                                        season_min = 1999,
                                        season_max = 2025) {

  filtered <- team_rows %>%
    filter(game_type %in% game_types,
           season    >= season_min,
           season    <= season_max)

  # Per-season Win% for every team (used as both own predictor and opponent lookup)
  season_winpct <- filtered %>%
    group_by(team, season) %>%
    summarise(
      G        = n(),
      W        = sum(result == "W"),
      T        = sum(result == "T"),
      win_pct  = (W + 0.5 * T) / G,
      .groups  = "drop"
    ) %>%
    select(team, season, win_pct)

  # For each game in season A+1, look up opponent's Win% from season A, then average
  avg_opp_winpct <- filtered %>%
    select(team, season, opponent) %>%
    mutate(prev_season = season - 1) %>%
    left_join(
      season_winpct %>% rename(opp_win_pct_prev = win_pct),
      by = c("opponent" = "team", "prev_season" = "season")
    ) %>%
    group_by(team, season) %>%
    summarise(
      avg_opp_win_pct = mean(opp_win_pct_prev, na.rm = TRUE),
      n_with_data     = sum(!is.na(opp_win_pct_prev)),
      .groups = "drop"
    ) %>%
    filter(n_with_data >= 10)

  # Join own Win%(A), avg opp Win%(A), and actual Win%(A+1)
  avg_opp_winpct %>%
    # own Win% in season A  (season - 1 relative to A+1)
    mutate(prev_season = season - 1) %>%
    left_join(
      season_winpct %>% rename(own_win_pct_A = win_pct),
      by = c("team" = "team", "prev_season" = "season")
    ) %>%
    # actual Win% in season A+1
    left_join(
      season_winpct %>% rename(Win_Pct = win_pct),
      by = c("team" = "team", "season" = "season")
    ) %>%
    filter(!is.na(own_win_pct_A), !is.na(avg_opp_win_pct), !is.na(Win_Pct)) %>%
    mutate(
      own_win_pct_A   = round(own_win_pct_A, 3),
      avg_opp_win_pct = round(avg_opp_win_pct, 3),
      Win_Pct         = round(Win_Pct, 3),
      Team            = sapply(team, get_team_name)
    ) %>%
    select(Team, team, season, own_win_pct_A, avg_opp_win_pct, Win_Pct) %>%
    arrange(team, season)
}

# Fit the multiple regression and return model stats
fit_multi_regression <- function(data) {
  model <- lm(Win_Pct ~ own_win_pct_A + avg_opp_win_pct, data = data)
  s     <- summary(model)
  coefs <- as.data.frame(s$coefficients)
  coefs <- round(coefs, 4)
  coefs$term <- rownames(coefs)
  rownames(coefs) <- NULL
  coefs <- coefs[, c("term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
  names(coefs) <- c("Term", "Estimate", "Std Error", "t", "p-value")

  list(
    model         = model,
    r_squared     = round(s$r.squared, 4),
    adj_r_squared = round(s$adj.r.squared, 4),
    n_obs         = nrow(data),
    coef_table    = coefs
  )
}

# Actual vs Predicted scatter plot
# Shows how well the combined model reproduces observed Win%
plot_actual_vs_predicted <- function(data, model_stats) {
  fitted_vals <- fitted(model_stats$model)

  plot_data <- data %>%
    mutate(predicted = round(fitted_vals, 3))

  subtitle <- glue::glue(
    "R² = {model_stats$r_squared}  |  ",
    "Adj R² = {model_stats$adj_r_squared}  |  ",
    "n = {model_stats$n_obs} team-seasons"
  )

  ggplot(plot_data, aes(x = predicted, y = Win_Pct)) +
    geom_point(alpha = 0.35, color = "#1a6fb5", size = 1.8) +
    # 45-degree reference line — perfect predictions fall on this
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "grey50", linewidth = 0.8) +
    geom_smooth(method = "lm", se = TRUE,
                color = "#e87722", fill = "#e87722", alpha = 0.15, linewidth = 1.1) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title    = "Actual vs Predicted Win% (A+1)",
      subtitle = subtitle,
      x        = "Predicted Win% — model output",
      y        = "Actual Win%"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.subtitle = element_text(color = "grey50", size = 10))
}

# Partial regression plots — one panel per predictor
# Shows the marginal effect of each variable after controlling for the other
plot_partial_regression <- function(data, model_stats, predictor) {

  other <- if (predictor == "own_win_pct_A") "avg_opp_win_pct" else "own_win_pct_A"

  # Residuals of Y ~ other predictor
  res_y <- residuals(lm(as.formula(paste("Win_Pct ~", other)), data = data))
  # Residuals of focal predictor ~ other predictor
  res_x <- residuals(lm(as.formula(paste(predictor, "~", other)), data = data))

  plot_data <- data.frame(res_x = res_x, res_y = res_y)

  x_label <- if (predictor == "own_win_pct_A") {
    "Team Win% in Season A  (residualized)"
  } else {
    "Avg Opponent Win% from Season A  (residualized)"
  }

  ggplot(plot_data, aes(x = res_x, y = res_y)) +
    geom_point(alpha = 0.35, color = "#1a6fb5", size = 1.8) +
    geom_smooth(method = "lm", se = TRUE,
                color = "#e87722", fill = "#e87722", alpha = 0.15, linewidth = 1.1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    labs(
      title = x_label,
      x     = x_label,
      y     = "Win%(A+1)  (residualized)"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(size = 11))
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
