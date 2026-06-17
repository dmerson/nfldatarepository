# ─────────────────────────────────────────────────────────────────────────────
# NFL Data Analysis Shiny App
# Tabs: All-Time Records | Season Breakdown | Head-to-Head | Team Detail
#
# To run locally:  source("install_packages.R") once, then shiny::runApp()
# To deploy:       rsconnect::deployApp() after connecting your shinyapps.io account
# ─────────────────────────────────────────────────────────────────────────────

library(shiny)
library(bslib)   # Modern Bootstrap 5 theming
library(DT)      # Interactive data tables
library(dplyr)
library(ggplot2)
library(scales)
library(glue)

source("R/team_mappings.R")
source("R/data_load.R")
source("R/stats_calc.R")

# ── Load data once at startup ────────────────────────────────────────────────
# This runs when the app starts. Uses local cache if available.
games     <- load_nfl_games(seasons = 1999:2024)
team_rows <- games_to_team_rows(games)
seasons   <- available_seasons(games)
teams     <- available_teams(team_rows)

# Build a named vector for dropdowns: "Arizona Cardinals" → "ARI"
team_choices <- setNames(teams, sapply(teams, get_team_name))

# ── Shared UI helpers ────────────────────────────────────────────────────────

# A standard set of filters used on most tabs
season_range_ui <- function(id_prefix, min_val = min(seasons), max_val = max(seasons)) {
  tagList(
    sliderInput(
      inputId = paste0(id_prefix, "_season_range"),
      label   = "Season range",
      min     = min(seasons), max = max(seasons),
      value   = c(min_val, max_val),
      step    = 1, sep = ""
    )
  )
}

game_type_ui <- function(id_prefix) {
  checkboxGroupInput(
    inputId  = paste0(id_prefix, "_game_types"),
    label    = "Include game types",
    choices  = c("Regular season" = "REG",
                 "Wild Card"      = "WC",
                 "Divisional"     = "DIV",
                 "Conf. Champ."   = "CON",
                 "Super Bowl"     = "SB"),
    selected = c("REG", "WC", "DIV", "CON", "SB"),
    inline   = TRUE
  )
}

# Render a DT table with shared formatting defaults
nfl_datatable <- function(data, ...) {
  datatable(
    data,
    rownames  = FALSE,
    filter    = "top",           # Column-level search boxes
    extensions = "Buttons",
    options   = list(
      pageLength = 32,
      dom        = "Bfrtip",     # Show Buttons + search + table + info + paging
      buttons    = list("csv", "excel"),  # Download buttons
      scrollX    = TRUE
    ),
    ...
  )
}

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = "NFL Stats (1999–2024)",
  theme = bs_theme(bootswatch = "darkly", primary = "#1a6fb5"),

  # ── Tab 1: All-Time Records ───────────────────────────────────────────────
  nav_panel(
    title = "All-Time Records",
    layout_sidebar(
      sidebar = sidebar(
        season_range_ui("alltime"),
        hr(),
        game_type_ui("alltime"),
        hr(),
        helpText("Avg PM = average point differential per game.",
                 "SOS Adj PM = Avg PM + avg Avg PM of opponents (strength-of-schedule adjusted).",
                 "Opp Win% = combined win% of all opponents faced.")
      ),
      # Main panel: table
      DTOutput("alltime_table")
    )
  ),

  # ── Tab 2: Season Breakdown ───────────────────────────────────────────────
  nav_panel(
    title = "Season Breakdown",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("season_select", "Season",
                    choices = rev(seasons), selected = max(seasons)),
        hr(),
        game_type_ui("season"),
        hr(),
        helpText("Shows all teams' stats for the selected season only.")
      ),
      DTOutput("season_table")
    )
  ),

  # ── Tab 3: Head-to-Head ───────────────────────────────────────────────────
  nav_panel(
    title = "Head-to-Head",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("h2h_team_a", "Team A", choices = team_choices, selected = "KC"),
        selectInput("h2h_team_b", "Team B", choices = team_choices, selected = "NE"),
        hr(),
        season_range_ui("h2h"),
        hr(),
        game_type_ui("h2h"),
        hr(),
        # Summary stats box
        uiOutput("h2h_summary")
      ),
      DTOutput("h2h_table")
    )
  ),

  # ── Tab 4: Team Detail ────────────────────────────────────────────────────
  nav_panel(
    title = "Team Detail",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("detail_team", "Team", choices = team_choices, selected = "KC"),
        hr(),
        season_range_ui("detail"),
        hr(),
        game_type_ui("detail")
      ),
      # Two rows: season-trend chart on top, game log on bottom
      layout_columns(
        col_widths = 12,
        card(
          card_header("Season-by-Season Trends"),
          plotOutput("detail_trend_chart", height = "300px")
        ),
        card(
          card_header("Game Log"),
          DTOutput("detail_game_log")
        )
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Tab 1: All-Time Records ───────────────────────────────────────────────
  alltime_data <- reactive({
    calc_full_summary(
      team_rows,
      season_min  = input$alltime_season_range[1],
      season_max  = input$alltime_season_range[2],
      game_types  = input$alltime_game_types
    ) %>%
      # Add full team names for readability
      mutate(Team = sapply(team, get_team_name), .before = team) %>%
      rename(Abbr = team) %>%
      arrange(desc(Win_Pct))
  })

  output$alltime_table <- renderDT({
    nfl_datatable(alltime_data(),
                  caption = "All-Time Team Records (1999–2024)")
  })

  # ── Tab 2: Season Breakdown ───────────────────────────────────────────────
  season_data <- reactive({
    calc_full_summary(
      team_rows,
      season_min  = as.integer(input$season_select),
      season_max  = as.integer(input$season_select),
      game_types  = input$season_game_types,
      group_by_season = TRUE
    ) %>%
      mutate(Team = sapply(team, get_team_name), .before = team) %>%
      rename(Abbr = team) %>%
      arrange(desc(Win_Pct))
  })

  output$season_table <- renderDT({
    nfl_datatable(season_data(),
                  caption = glue("Team Stats — {input$season_select} Season"))
  })

  # ── Tab 3: Head-to-Head ───────────────────────────────────────────────────
  h2h_data <- reactive({
    req(input$h2h_team_a, input$h2h_team_b)
    req(input$h2h_team_a != input$h2h_team_b)

    calc_head_to_head(
      team_rows,
      team_a      = input$h2h_team_a,
      team_b      = input$h2h_team_b,
      season_min  = input$h2h_season_range[1],
      season_max  = input$h2h_season_range[2],
      game_types  = input$h2h_game_types
    )
  })

  # Text summary box above the H2H table
  output$h2h_summary <- renderUI({
    data <- h2h_data()
    if (nrow(data) == 0) return(helpText("No games found for this matchup."))

    w <- sum(data$result == "W")
    l <- sum(data$result == "L")
    t <- sum(data$result == "T")
    name_a <- get_team_name(input$h2h_team_a)
    name_b <- get_team_name(input$h2h_team_b)

    tagList(
      tags$b(glue("{name_a} vs {name_b}")),
      tags$p(glue("{name_a} record: {w}W-{l}L-{t}T")),
      tags$p(glue("Total games: {nrow(data)}")),
      tags$p(glue("Avg margin: {round(mean(data$point_diff), 1)} pts"))
    )
  })

  output$h2h_table <- renderDT({
    data <- h2h_data()
    req(nrow(data) > 0)
    name_a <- get_team_name(input$h2h_team_a)
    name_b <- get_team_name(input$h2h_team_b)
    nfl_datatable(data,
                  caption = glue("{name_a} vs {name_b} — {input$h2h_team_a} perspective"))
  })

  # ── Tab 4: Team Detail ────────────────────────────────────────────────────

  # Per-season summary for the selected team
  detail_season_summary <- reactive({
    calc_full_summary(
      team_rows,
      season_min  = input$detail_season_range[1],
      season_max  = input$detail_season_range[2],
      game_types  = input$detail_game_types,
      group_by_season = TRUE
    ) %>%
      filter(team == input$detail_team)
  })

  # Full game log for the selected team
  detail_game_log <- reactive({
    team_rows %>%
      filter(
        team       == input$detail_team,
        season     >= input$detail_season_range[1],
        season     <= input$detail_season_range[2],
        game_type  %in% input$detail_game_types
      ) %>%
      mutate(Opponent = sapply(opponent, get_team_name)) %>%
      select(season, week, game_type, gameday, home_away, stadium,
             team_score, opp_score, Opponent, point_diff, result, overtime) %>%
      arrange(gameday)
  })

  # Line chart: Win%, Avg PM, and SOS Adj PM over seasons
  output$detail_trend_chart <- renderPlot({
    data <- detail_season_summary()
    req(nrow(data) > 0)

    team_name <- get_team_name(input$detail_team)

    # Pivot longer so ggplot can use color for each metric
    plot_data <- data %>%
      select(season, Win_Pct, Avg_PM, SOS_Adj_PM) %>%
      tidyr::pivot_longer(-season, names_to = "metric", values_to = "value")

    ggplot(plot_data, aes(x = season, y = value, color = metric)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
      scale_x_continuous(breaks = scales::breaks_pretty()) +
      scale_color_manual(
        values  = c("Win_Pct" = "#1a6fb5", "Avg_PM" = "#e87722", "SOS_Adj_PM" = "#2ca02c"),
        labels  = c("Win_Pct" = "Win%", "Avg_PM" = "Avg +/-", "SOS_Adj_PM" = "SOS Adj +/-")
      ) +
      labs(title = team_name, x = "Season", y = "Value", color = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  output$detail_game_log <- renderDT({
    data <- detail_game_log()
    req(nrow(data) > 0)
    team_name <- get_team_name(input$detail_team)
    nfl_datatable(data, caption = glue("{team_name} — Game Log"))
  })
}

# ── Launch ───────────────────────────────────────────────────────────────────
shinyApp(ui, server)
