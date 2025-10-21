# app.R — multi-variable, MT time, separate plots per variable
# Reads multiple CSVs (header row 1, units row 2, data start row 3),
# merges them by timestamp, and plots selected variables from DEFAULT_COMPONENTS.
# Landing tab is a Leaflet map. ASCII-only strings; balanced braces.

suppressPackageStartupMessages({
  library(shiny)
  library(readr)
  library(vroom)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(DT)
  library(zoo)
  library(leaflet)
})

# ----------------------------- CONFIG ----------------------------------------
DATA_PATHS <- c(
  "CH4 + CO2"   = "https://raw.githubusercontent.com/meredithfranklin/UOGD-source-contributions/refs/heads/main/data/1-min-data/Boulder_AIR_LNM_ch4_co2_finalized_Oct30_1min.csv",
  "CO"          = "https://raw.githubusercontent.com/meredithfranklin/UOGD-source-contributions/refs/heads/main/data/1-min-data/Boulder_AIR_LNM_co_finalized_1min.csv",
  "H2S + SO2"   = "https://raw.githubusercontent.com/meredithfranklin/UOGD-source-contributions/refs/heads/main/data/1-min-data/Boulder_AIR_LNM_h2s_so2_finalized_1min.csv",
  "Meteorology" = "https://raw.githubusercontent.com/meredithfranklin/UOGD-source-contributions/refs/heads/main/data/1-min-data/Boulder_AIR_LNM_met_finalized_1min_w_wvmx.csv",
  "NOx"         = "https://raw.githubusercontent.com/meredithfranklin/UOGD-source-contributions/refs/heads/main/data/1-min-data/Boulder_AIR_LNM_nox_finalized_1min.csv",
  "O3"          = "https://raw.githubusercontent.com/meredithfranklin/UOGD-source-contributions/refs/heads/main/data/1-min-data/Boulder_AIR_LNM_o3_finalized_1min.csv"
)

# Datetime column in files (string). Data are in UTC; we convert to Mountain time.
DATETIME_COLUMN <- "time_utc"

# Site for the map
SITE_NAME <- "LNM"
SITE_LAT  <- 32.2974
SITE_LON  <- -104.1095

# Variables offered (subset of columns that actually exist)
DEFAULT_COMPONENTS <- c(
  "ch4", "co2_ppm", "co", "h2s", "so2", "wdr_deg", "wsp_ms", "temp_f", "relh_percent",
  "solr", "pressure_altcorr", "rain", "water_vapor_mr", "nox", "no", "no2", "o3"
)

# Units for labels (ASCII only)
unit_map <- c(
  ethane = "ppb", propane = "ppb", `i-butane` = "ppb", `n-butane` = "ppb",
  `i-pentane` = "ppb", `n-pentane` = "ppb", `n-hexane` = "ppb", cyclopentane = "ppb",
  `n-heptane` = "ppb", `n-octane` = "ppb", ethene = "ppb", propene = "ppb",
  `1_3-butadiene` = "ppb", isoprene = "ppb", acetylene = "ppb",
  benzene = "ppb", toluene = "ppb", `ethyl-benzene` = "ppb", `o-xylene` = "ppb",
  `m&p-xylene` = "ppb", co = "ppm", co2 = "ppm", co2_ppm = "ppm",
  nox = "ppb", no = "ppb", no2 = "ppb", h2s = "ppb", so2 = "ppb",
  o3 = "ppb", ch4 = "ppm",
  wdr_deg = "deg", wsp_ms = "m s^-1", temp_f = "F", relh_percent = "%",
  solr = "W m^-2", pressure_altcorr = "hPa", rain = "mm", water_vapor_mr = "g kg^-1"
)

norm_name <- function(x) { x <- tolower(x); x <- gsub("[^a-z0-9]+", "_", x); gsub("(^_|_$)", "", x) }
label_with_unit <- function(var) { u <- unit_map[[var]]; if (!is.null(u)) paste0(var, " (", u, ")") else var }
roll_mean <- function(x, k) {
  if (k <= 1) return(x)
  zoo::rollapply(x, width = k, FUN = function(y) mean(y, na.rm = TRUE),
                 align = "right", partial = TRUE, fill = NA)
}

# ----------------------------- UI --------------------------------------------
ui <- fluidPage(
  titlePanel("Loving, New Mexico (LNM) Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      h4("Dataset"),
      selectizeInput("which_file", "Select file(s)", choices = names(DATA_PATHS),
                     selected = names(DATA_PATHS)[1], multiple = TRUE,
                     options = list(maxItems = 6)),
      helpText("Select one or several files; selections are merged by timestamp."),
      tags$hr(),
      
      h4("Controls"),
      uiOutput("vars_ui"),
      uiOutput("date_ui"),
      selectInput("agg", "Aggregation",
                  choices = c("None" = "none", "5-minute" = "5min",
                              "Hourly" = "hour", "Daily" = "day"), selected = "none"),
      selectInput("smooth", "Smoothing",
                  choices = c("None" = "none", "8-pt rolling" = "r8",
                              "30-pt rolling" = "r30", "LOESS (span 0.2)" = "loess"),
                  selected = "none"),
      checkboxInput("points", "Show points", FALSE),
      width = 4
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map", height = "600px")),
        tabPanel("Plots", uiOutput("plots_ui")),
        tabPanel("Table", DTOutput("table"))
      )
    )
  )
)

# ----------------------------- SERVER ----------------------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(raw = NULL, dt_col = NULL, comp_cols = character())
  
  # Force-parse datetime as UTC and convert to Mountain Time
  ingest <- function(df, dt_override = NULL) {
    validate(need(nrow(df) > 0, "Empty dataset"))
    
    # Pick datetime column
    dt_col <- NULL
    if (!is.null(dt_override) && dt_override %in% names(df)) {
      dt_col <- dt_override
    } else {
      candidates <- c("time_utc","datetime","timestamp","date_time","date","time","start_time")
      hit <- intersect(candidates, names(df))
      if (length(hit)) dt_col <- hit[[1]]
    }
    validate(need(!is.null(dt_col), "No datetime-like column found."))
    
    # Parse as UTC, then convert to America/Denver (Mountain Time)
    if (!inherits(df[[dt_col]], "POSIXct") && !inherits(df[[dt_col]], "Date")) {
      parsed <- suppressWarnings(lubridate::ymd_hms(df[[dt_col]], tz = "UTC", quiet = TRUE))
      if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::ymd_hm(df[[dt_col]], tz = "UTC", quiet = TRUE))
      if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::ymd(df[[dt_col]], quiet = TRUE))
      if (!all(is.na(parsed))) df[[dt_col]] <- parsed
    }
    if (inherits(df[[dt_col]], "Date")) df[[dt_col]] <- as.POSIXct(df[[dt_col]], tz = "UTC")
    if (!inherits(df[[dt_col]], "POSIXct")) df[[dt_col]] <- as.POSIXct(df[[dt_col]], tz = "UTC")
    df[[dt_col]] <- lubridate::with_tz(lubridate::force_tz(df[[dt_col]], "UTC"), "America/Denver")
    
    # Components = numeric columns excluding datetime
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    comp_cols <- setdiff(num_cols, dt_col)
    
    list(df = as.data.frame(df), dt_col = dt_col, comp_cols = comp_cols)
  }
  
  # Read local path or GitHub RAW, using header row 1; skip units row 2 (data start row 3)
  read_data_source <- function(path) {
    file_path <- path
    if (grepl("^https?://", path)) {
      ext <- tools::file_ext(path)
      file_path <- tempfile(fileext = ifelse(nchar(ext), paste0(".", ext), ".csv"))
      utils::download.file(path, file_path, mode = "wb", quiet = TRUE)
    }
    hdr_line <- readr::read_lines(file_path, n_max = 1)
    if (length(hdr_line) == 0) stop("File has no header line: ", path)
    delim <- if (grepl("	", hdr_line)) "	" else if (grepl(";", hdr_line)) ";" else ","
    col_names <- strsplit(hdr_line, delim, fixed = TRUE)[[1]]
    col_names <- trimws(col_names)
    vroom::vroom(file_path, delim = delim, col_names = col_names, skip = 2,
                 altrep = FALSE, show_col_types = FALSE)
  }
  
  # Merge selected files by timestamp (full join), then build UI
  load_and_setup <- function(paths) {
    if (length(paths) == 0) return(invisible(NULL))
    
    cleaned <- lapply(paths, function(p) {
      raw_df <- read_data_source(p)
      ing <- ingest(as.data.frame(raw_df), dt_override = DATETIME_COLUMN)
      keep <- c(ing$dt_col, ing$comp_cols)
      df <- ing$df[, intersect(keep, names(ing$df)), drop = FALSE]
      names(df)[names(df) == ing$dt_col] <- "time"
      df
    })
    
    merged <- cleaned[[1]]
    if (length(cleaned) > 1) {
      for (i in 2:length(cleaned)) merged <- dplyr::full_join(merged, cleaned[[i]], by = "time")
    }
    
    rv$raw <- merged
    rv$dt_col <- "time"
    rv$comp_cols <- setdiff(names(merged)[vapply(merged, is.numeric, logical(1))], "time")
    
    present_vars <- intersect(DEFAULT_COMPONENTS, rv$comp_cols)
    validate(need(length(present_vars) > 0, "None of the DEFAULT_COMPONENTS are present in the merged data."))
    
    rng <- range(rv$raw[[rv$dt_col]], na.rm = TRUE)
    
    output$vars_ui <- renderUI({
      selectizeInput("variables", "Variables",
                     choices = present_vars,
                     selected = head(present_vars, min(4, length(present_vars))),
                     multiple = TRUE,
                     options = list(plugins = list("remove_button")))
    })
    output$date_ui <- renderUI({
      start_def <- as.Date("2023-05-01")
      end_def   <- as.Date("2024-05-31")
      start_val <- max(as.Date(rng[1]), start_def, na.rm = TRUE)
      end_val   <- min(as.Date(rng[2]), end_def, na.rm = TRUE)
      dateRangeInput("daterange", "Date range",
                     start = start_val, end = end_val,
                     min = as.Date(rng[1]), max = as.Date(rng[2]))
    })
    
    invisible(TRUE)
  }
  
  # Initial load and on selection changes
  observeEvent(input$which_file, {
    paths <- unname(DATA_PATHS[input$which_file])
    load_and_setup(paths)
  }, ignoreInit = FALSE)
  
  # Filter to selected series; optional aggregation and smoothing
  d_filt <- reactive({
    req(rv$raw, rv$dt_col, input$variables)
    df <- rv$raw
    
    # Filter by date
    if (!is.null(input$daterange)) {
      df <- dplyr::filter(df, .data[[rv$dt_col]] >= as.POSIXct(input$daterange[1]) &
                            .data[[rv$dt_col]] <= as.POSIXct(input$daterange[2] + 1))
    }
    
    # Keep only time + selected variables, then pivot longer
    comps <- intersect(input$variables, names(df))
    validate(need(length(comps) > 0, "No selected variables found in data."))
    df_long <- df[, c("time", comps), drop = FALSE]
    df_long <- tidyr::pivot_longer(df_long, cols = tidyselect::all_of(comps),
                                   names_to = "variable", values_to = "value")
    
    # Aggregate (by bucket and variable)
    if (input$agg != "none") {
      bucket <- if (input$agg == "5min") {
        lubridate::floor_date(df_long$time, unit = "5 minutes")
      } else if (input$agg == "hour") {
        lubridate::floor_date(df_long$time, unit = "hour")
      } else if (input$agg == "day") {
        lubridate::floor_date(df_long$time, unit = "day")
      } else df_long$time
      df_long <- df_long %>%
        dplyr::mutate(bucket = bucket) %>%
        dplyr::group_by(bucket, variable) %>%
        dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::rename(time = bucket)
    }
    
    # Label with units for legend/strips
    df_long <- df_long %>% dplyr::mutate(var_label = {
      v <- variable; u <- vapply(v, function(z) unit_map[[z]], character(1));
      u[is.na(u)] <- "";
      ifelse(nzchar(u), paste0(v, " (", u, ")"), v)
    })
    
    # Smoothing per variable
    df_long <- df_long %>% dplyr::arrange(variable, time) %>%
      dplyr::group_by(variable) %>%
      dplyr::mutate(smooth = {
        if (input$smooth == "r8")      roll_mean(value, 8) else
          if (input$smooth == "r30")     roll_mean(value, 30) else
            if (input$smooth == "loess") {
              y <- value; x <- as.numeric(time);
              fit <- try(loess(y ~ x, span = 0.2, na.action = na.exclude), silent = TRUE)
              if (inherits(fit, "try-error")) rep(NA_real_, length(y)) else as.numeric(predict(fit, x))
            } else rep(NA_real_, length(value))
      }) %>% dplyr::ungroup()
    
    df_long
  })
  
  # Dynamic plots: one ggplot per variable
  output$plots_ui <- renderUI({
    req(input$variables)
    ids <- paste0("plot_", make.names(input$variables))
    heights <- rep("360px", length(ids))
    tagList(mapply(function(id, lbl, h) {
      box <- div(style = "margin-bottom: 24px;",
                 h4(lbl),
                 plotOutput(id, height = h))
      box
    }, ids, input$variables, heights, SIMPLIFY = FALSE))
  })
  
  # Render each plot separately
  observe({
    df_all <- d_filt()
    req(nrow(df_all) > 0)
    vars <- unique(df_all$variable)
    for (v in vars) {
      local({
        var <- v
        plot_id <- paste0("plot_", make.names(var))
        output[[plot_id]] <- renderPlot({
          df <- df_all[df_all$variable == var, , drop = FALSE]
          validate(need(nrow(df) > 0, "No data to plot."))
          ylab <- label_with_unit(var)
          p <- ggplot(df, aes(x = time, y = value)) +
            geom_line(alpha = 0.6, na.rm = TRUE)
          if (isTRUE(input$points)) p <- p + geom_point(alpha = 0.5, size = 0.6, na.rm = TRUE)
          if (!all(is.na(df$smooth))) p <- p + geom_line(aes(y = smooth), color = "dodgerblue", linewidth = 1.0, na.rm = TRUE)
          p + labs(x = NULL, y = ylab) +
            theme_minimal(base_size = 14) + theme(panel.grid.minor = element_blank())
        })
      })
    }
  })
  
  # Table (long form across variables)
  output$table <- renderDT({
    df <- d_filt()
    validate(need(nrow(df) > 0, "No data to show."))
    DT::datatable(dplyr::arrange(df, time, var_label), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Map (landing tab)
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      addAwesomeMarkers(lng = SITE_LON, lat = SITE_LAT, label = SITE_NAME,
                        icon = awesomeIcons(icon = "map-marker", library = "fa",
                                            markerColor = "lightgreen", iconColor = "green"),
                        popup = SITE_NAME) %>%
      addScaleBar(position = "bottomleft")
  })
}

shinyApp(ui, server)
