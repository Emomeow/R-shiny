# K-M Survival Curve Shiny App

# 1. LOAD LIBRARIES
# -----------------
library(shiny)
library(survival)
library(survminer)
library(ggplot2)
library(ggpubr)
library(shinycssloaders) # For the loading spinner

# 2. DEFINE USER INTERFACE (UI)
# ----------------------------
ui <- fluidPage(
  # App title
  titlePanel("Kaplan-Meier Survival Plotter"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: File upload
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Select time variable
      selectInput("time_col", "Select Time to Event Column",
                  choices = NULL, selected = NULL),
      
      # Input: Select status variable
      selectInput("status_col", "Select Censor/Status Column",
                  choices = NULL, selected = NULL),
      
      # Action button
      actionButton("run", "Run Analysis", icon = icon("play")),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Select group variable(s) - NOW MULTI-SELECT
      h4("Grouping (Interactive)"),
      selectInput("group_cols", "Select Group Column(s) (Optional)",
                  choices = NULL, selected = NULL, multiple = TRUE),
      
      # Horizontal line
      tags$hr(),
      
      # --- UPDATED: MULTI-FILTERING SECTION ---
      h4("Data Filtering (Interactive)"),
      
      # Numeric filter (now multi-select)
      selectInput("filter_num_cols", "Filter by Numeric Column(s):",
                  choices = c("None" = "", NULL), multiple = TRUE),
      uiOutput("filter_num_sliders_ui"), # Dynamic sliders (plural)
      
      # Categorical filter (now multi-select)
      selectInput("filter_cat_cols", "Filter by Categorical Column(s):",
                  choices = c("None" = "", NULL), multiple = TRUE),
      uiOutput("filter_cat_checkboxes_ui"), # Dynamic checkboxes (plural)
      
      width = 3
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Kaplan-Meier Plot", 
                 # Add a spinner while plot is loading
                 shinycssloaders::withSpinner(
                   plotOutput("km_plot", height = "500px"),
                   type = 6
                 )
        ),
        tabPanel("Model Summary", 
                 verbatimTextOutput("fit_summary")
        )
      )
    )
  )
)

# 3. DEFINE SERVER LOGIC
# ----------------------
server <- function(input, output, session) {
  
  # Reactive: Read the uploaded file
  file_data <- reactive({
    req(input$file1)
    tryCatch({
      df <- read.csv(input$file1$datapath, header = TRUE, stringsAsFactors = TRUE)
      df
    }, error = function(e) {
      # Return a safe error message
      stop(safeError(e))
    })
  })
  
  # Observer: Update select inputs based on file columns
  observeEvent(file_data(), {
    df <- file_data()
    cols <- colnames(df)
    
    # Get numeric and factor/character columns
    numeric_cols <- cols[sapply(df, is.numeric)]
    
    # For status, we want numeric (0/1) or logical
    status_cols <- cols[sapply(df, function(x) is.numeric(x) || is.logical(x))]
    
    # For group, we want factor, character, or logical
    # Also, let's allow numeric columns with few unique values
    discrete_cols <- cols[sapply(df, function(col) {
      is.factor(col) || is.character(col) || is.logical(col) ||
        (is.numeric(col) && length(unique(col)) <= 10)
    })]
    
    # Update inputs
    updateSelectInput(session, "time_col",
                      choices = numeric_cols,
                      selected = character(0))
    
    updateSelectInput(session, "status_col",
                      choices = status_cols,
                      selected = character(0))
    
    updateSelectInput(session, "group_cols",
                      choices = discrete_cols,
                      selected = character(0))
    
    # Update filter inputs (now plural)
    updateSelectInput(session, "filter_num_cols",
                      choices = numeric_cols,
                      selected = character(0))
    
    updateSelectInput(session, "filter_cat_cols",
                      choices = discrete_cols,
                      selected = character(0))
  })
  
  # --- UPDATED: Dynamic UI for MULTIPLE Numeric Filters ---
  output$filter_num_sliders_ui <- renderUI({
    df <- file_data()
    req(df, input$filter_num_cols)
    
    # Use lapply to create a list of slider inputs
    lapply(input$filter_num_cols, function(col_name) {
      col_vals <- na.omit(df[[col_name]])
      min_val <- floor(min(col_vals, na.rm = TRUE))
      max_val <- ceiling(max(col_vals, na.rm = TRUE))
      
      sliderInput(inputId = paste0("filter_num_", col_name), # Unique ID
                  label = paste("Select Range for", col_name, ":"),
                  min = min_val, max = max_val, 
                  value = c(min_val, max_val))
    })
  })
  
  # --- UPDATED: Dynamic UI for MULTIPLE Categorical Filters ---
  output$filter_cat_checkboxes_ui <- renderUI({
    df <- file_data()
    req(df, input$filter_cat_cols)
    
    # Use lapply to create a list of checkbox group inputs
    lapply(input$filter_cat_cols, function(col_name) {
      choices <- levels(as.factor(df[[col_name]]))
      
      checkboxGroupInput(inputId = paste0("filter_cat_", col_name), # Unique ID
                         label = paste("Select Values for", col_name, ":"),
                         choices = choices, 
                         selected = choices, 
                         inline = TRUE)
    })
  })
  
  
  # --- UPDATED: Reactive values to store the "locked-in" choices
  # These are set *only* when "Run Analysis" is pressed
  locked_in_data <- reactiveValues(
    df = NULL,
    time_col = NULL,
    status_col = NULL
  )
  
  # --- UPDATED: Observer to "lock in" data when Run is pressed
  observeEvent(input$run, {
    req(file_data(), input$time_col, input$status_col)
    
    df <- file_data() # Get the raw data
    
    # --- VALIDATION LOGIC ---
    # 1. Check Time column
    time_vals <- df[[input$time_col]]
    validate(
      need(is.numeric(time_vals), "Validation Error: The selected 'Time' column must be numeric."),
      need(all(time_vals >= 0, na.rm = TRUE), "Validation Error: The selected 'Time' column contains negative values. Please check your data.")
    )
    
    # 2. Check Status column
    status_vals <- df[[input$status_col]]
    unique_status_vals <- unique(na.omit(status_vals))
    
    is_logical_status <- is.logical(status_vals)
    is_numeric_status <- is.numeric(status_vals) && all(unique_status_vals %in% c(0, 1))
    
    validate(
      need(is_logical_status || is_numeric_status, 
           paste("Validation Error: The selected 'Status' column is not valid.",
                 "It must be numeric (containing only 0s and 1s) or logical (TRUE/FALSE).",
                 "\nFound values:", paste(head(unique_status_vals, 5), collapse = ", ")))
    )
    # --- END VALIDATION ---
    
    # Lock in the raw data and column names
    locked_in_data$df <- df
    locked_in_data$time_col <- input$time_col
    locked_in_data$status_col <- input$status_col
  })
  
  
  # --- UPDATED: Reactive for Filtering and Grouping Data ---
  # This now depends on locked_in_data, filter inputs, AND group inputs
  processed_data <- reactive({
    # Wait for "Run" to be pressed at least once
    req(locked_in_data$df) 
    
    df <- locked_in_data$df
    
    # Apply Numeric Filters (now loops through all selected)
    if (!is.null(input$filter_num_cols)) {
      for (col_name in input$filter_num_cols) {
        input_id <- paste0("filter_num_", col_name)
        range_val <- input[[input_id]]
        
        # Check if the slider UI has rendered and has a value
        if (!is.null(range_val)) {
          df <- df[df[[col_name]] >= range_val[1] & df[[col_name]] <= range_val[2] & !is.na(df[[col_name]]), ]
        }
      }
    }
    
    # Apply Categorical Filters (now loops through all selected)
    if (!is.null(input$filter_cat_cols)) {
      for (col_name in input$filter_cat_cols) {
        input_id <- paste0("filter_cat_", col_name)
        values_to_keep <- input[[input_id]]
        
        # Check if the checkbox UI has rendered and has a value
        if (!is.null(values_to_keep)) {
          df <- df[as.factor(df[[col_name]]) %in% values_to_keep, ]
        }
      }
    }
    
    # Check if filtering resulted in 0 rows
    validate(
      need(nrow(df) > 0, "Validation Error: The current filters result in zero rows of data.")
    )
    
    # Apply Group Combination (now reads from input$group_cols)
    group_cols <- input$group_cols
    if (!is.null(group_cols) && length(group_cols) > 0) {
      
      # Convert any numeric group columns to factor
      for (col in group_cols) {
        if(is.numeric(df[[col]])) {
          df[[col]] <- as.factor(df[[col]])
        }
      }
      
      if (length(group_cols) == 1) {
        df$..group_col.. <- as.factor(df[[group_cols[1]]])
      } else {
        df$..group_col.. <- apply(df[, group_cols, drop = FALSE], 1, paste, collapse = " - ")
        df$..group_col.. <- as.factor(df$..group_col..)
      }
      
      # --- NEW VALIDATION: Check for n=1 in any group ---
      group_counts <- table(df$..group_col..)
      validate(
        need(all(group_counts > 1), 
             paste("Validation Error: The current filters/groups result in at least one group having only one subject (n=1),",
                   "which is not enough to plot. Please broaden your filters or change grouping.",
                   "\nGroups with n<2:", paste(names(group_counts[group_counts < 2]), collapse=", "))
        )
      )
      
    }
    
    df # Return the processed data
  })
  
  
  # --- UPDATED: Reactive: Fit the survival model ---
  # This will re-run whenever processed_data() OR input$group_cols changes
  fit <- reactive({
    # This will run validation checks first
    df <- processed_data() 
    
    time_sym <- as.symbol(locked_in_data$time_col)
    status_sym <- as.symbol(locked_in_data$status_col)
    
    # Check for groups based on the *interactive* input
    has_group <- !is.null(input$group_cols) && length(input$group_cols) > 0
    
    if (has_group) {
      group_sym <- as.symbol("..group_col..")
      call <- substitute(
        survfit(Surv(time, status) ~ group, data = df),
        list(time = time_sym, status = status_sym, group = group_sym)
      )
    } else {
      call <- substitute(
        survfit(Surv(time, status) ~ 1, data = df),
        list(time = time_sym, status = status_sym)
      )
    }
    
    eval(call)
  })
  
  
  # --- UPDATED: Reactive: Generate the plot object ---
  # This will re-run whenever fit() or processed_data() changes
  plot_obj <- reactive({
    df <- processed_data()
    f <- fit()
    
    # Check for groups based on the *interactive* input
    has_group <- !is.null(input$group_cols) && length(input$group_cols) > 0
    
    if (has_group) {
      # --- Logic for grouped analysis ---
      legend_title <- paste(input$group_cols, collapse = " + ")
      
      ggsurvplot(
        f,
        data = df,
        pval = TRUE,
        conf.int = TRUE,
        risk.table = TRUE,
        legend.labs = NULL,
        legend.title = legend_title,
        palette = "default", # Use default palette for groups
        ggtheme = theme_minimal(),
        risk.table.y.text = FALSE,
        risk.table.y.text.col = TRUE,
        ncensor.plot = TRUE
      )
      
    } else {
      # --- Logic for non-grouped analysis ( ~ 1) ---
      
      ggsurvplot(
        f,
        data = df,
        pval = FALSE,
        conf.int = TRUE,
        conf.int.style = "ribbon", # Explicitly set CI style
        color = "black",            # FIX: Set line color directly
        fill = "grey",              # FIX: Set ribbon fill directly
        risk.table = TRUE,
        legend = "none",            # No legend for a single curve
        ggtheme = theme_minimal(),
        risk.table.y.text = FALSE,
        risk.table.y.text.col = TRUE,
        ncensor.plot = TRUE
      )
    }
  })
  
  # Output: Render the Kaplan-Meier plot
  output$km_plot <- renderPlot({
    print(plot_obj())
  })
  
  # Output: Render the model summary
  output$fit_summary <- renderPrint({
    f <- fit()
    summary(f)
  })
  
}

# 4. RUN THE APPLICATION
# ----------------------
shinyApp(ui = ui, server = server)
