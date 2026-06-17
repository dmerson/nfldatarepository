# Run this script ONCE before launching the app to install all dependencies.
# In RStudio: open this file and click "Source", or run: source("install_packages.R")

install.packages(c(
  "shiny",       # The web app framework
  "bslib",       # Modern Bootstrap-based theming for Shiny
  "DT",          # Interactive sortable/filterable data tables
  "dplyr",       # Data manipulation (filter, group_by, summarise, etc.)
  "tidyr",       # Data reshaping (pivot_longer/wider)
  "ggplot2",     # Charts and visualizations
  "nflreadr",    # Pulls free NFL data from the nflverse public repo
  "scales",      # Formatting helpers (percentages, commas) used in ggplot2
  "glue",        # String interpolation for building readable labels
  "leaflet"      # Interactive maps with circle markers
))




