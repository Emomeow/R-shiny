# K-M Survival Curve Shiny App

# 1. LOAD LIBRARIES
# -----------------
# Make sure these are installed: 
# install.packages(c("shiny", "survival", "survminer", "ggplot2", "ggpubr", "shinycssloaders", "data.table", "magrittr", "ggrepel"))
library(shiny)
library(survival)
library(survminer)
library(ggplot2)
library(ggpubr)
library(shinycssloaders) # For the loading spinner
library(data.table)      # For fast fread()
library(magrittr)        # For the %>% pipe (used in debounce)
library(ggrepel)         # For non-overlapping plot labels

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
      
      # --- Demo Data Button ---
      p("Or, use a pre-loaded dataset:", style = "text-align: center;"),
      actionButton("load_demo", "Load Demo Data", icon = icon("table"), width = "100%"),
      
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
      
      h4("Interactive Controls"),
      
      # Input: Select group variable(s)
      selectInput("group_cols", "Select Group Column(s) (Optional)",
                  choices = NULL, selected = NULL, multiple = TRUE),
      
      # --- New Input: Specific Time Point ---
      numericInput("time_point", "Show Details at Time Point (Optional):", 
                   value = NULL, min = 0),
      
      # Horizontal line
      tags$hr(),
      
      # --- Multi-filtering section ---
      h4("Data Filtering (Interactive)"),
      
      # Numeric filter
      selectInput("filter_num_cols", "Filter by Numeric Column(s):",
                  choices = c("None" = "", NULL), multiple = TRUE),
      uiOutput("filter_num_sliders_ui"),
      
      # Categorical filter
      selectInput("filter_cat_cols", "Filter by Categorical Column(s):",
                  choices = c("None" = "", NULL), multiple = TRUE),
      uiOutput("filter_cat_checkboxes_ui"),
      
      width = 3
    ),
    
    # Main panel for displaying outputs
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Kaplan-Meier Plot", 
                 shinycssloaders::withSpinner(
                   plotOutput("km_plot", height = "500px"),
                   type = 6
                 ),
                 
                 # --- MOVED FROM OTHER TAB ---
                 tags$hr(), # Add a nice separator
                 h4("Summary at Specific Time Point"),
                 verbatimTextOutput("time_point_summary")
                 
        ),
        tabPanel("Model Summary", 
                 # --- Time point summary was moved to the plot tab ---
                 h4("Overall Model Summary"),
                 verbatimTextOutput("fit_summary")
        )
      )
    )
  )
)

# 3. DEFINE SERVER LOGIC
# ----------------------
server <- function(input, output, session) {
  
  # --- Define the demo dataset ---
  demo_data <- data.frame(
    stringsAsFactors = TRUE,
    Survival_Time = c(12L, 24L, 8L, 15L, 44L, 30L, 22L, 18L, 35L, 29L, 40L, 5L,
                      10L, 50L, 33L, 21L, 19L, 28L, 37L, 45L, 9L, 14L, 25L,
                      32L, 42L, 6L, 11L, 20L, 27L, 38L),
    Event_Status = c(1L, 0L, 1L, 1L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 0L,
                     1L, 1L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 0L, 1L),
    Treatment_Group = c("Group A", "Group A", "Group A", "Group B", "Group B",
                        "Group A", "Group B", "Group A", "Group B", "Group A",
                        "Group B", "Group A", "Group A", "Group B", "Group A",
                        "Group B", "Group A", "Group B", "Group A", "Group B",
                        "Group A", "Group B", "Group A", "Group B", "Group A",
                        "Group B", "Group A", "Group B", "Group A", "Group B"),
    Sex = c("Male", "Female", "Male", "Male", "Female", "Female", "Male",
            "Female", "Male", "Female", "Male", "Female", "Male", "Female",
            "Male", "Female", "Male", "Female", "Male", "Female", "Male",
            "Female", "Male", "Female", "Male", "Female", "Male", "Female",
            "Male", "Female"),
    Age = c(65L, 58L, 71L, 55L, 62L, 59L, 68L, 63L, 70L, 61L, 57L, 72L, 60L,
            66L, 54L, 69L, 64L, 56L, 67L, 73L, 53L, 74L, 52L, 75L, 61L, 76L,
            59L, 77L, 60L, 78L),
    Stage = c("Stage II", "Stage I", "Stage III", "Stage II", "Stage I",
              "Stage II", "Stage III", "Stage I", "Stage II", "Stage I",
              "Stage III", "Stage I", "Stage II", "Stage I", "Stage III",
              "Stage II", "Stage I", "Stage III", "Stage II", "Stage I",
              "Stage III", "Stage II", "Stage I", "Stage III", "Stage II",
              "Stage I", "Stage III", "Stage II", "Stage I", "Stage II")
  )
  
  # Use reactiveValues for single data source
  rv <- reactiveValues(
    raw_df = NULL,
    time_col = NULL,
    status_col = NULL,
    run_pressed = FALSE
  )
  
  # Observer: Read the uploaded file
  observeEvent(input$file1, {
    req(input$file1)
    tryCatch({
      df <- data.table::fread(input$file1$datapath, 
                              header = TRUE, 
                              stringsAsFactors = TRUE, 
                              data.table = FALSE)
      rv$raw_df <- df
      updateActionButton(session, "load_demo", label = "Load Demo Data")
    }, error = function(e) {
      stop(safeError(e))
    })
  })
  
  # --- Observer for Demo Data Button ---
  observeEvent(input$load_demo, {
    rv$raw_df <- demo_data
    # reset("file1") # Removed shinyjs dependency
    updateActionButton(session, "load_demo", label = "Demo Data Loaded!")
  })
  
  # Observer: Update select inputs based on file columns
  observeEvent(rv$raw_df, {
    df <- rv$raw_df
    cols <- colnames(df)
    
    numeric_cols <- cols[sapply(df, is.numeric)]
    status_cols <- cols[sapply(df, function(x) is.numeric(x) || is.logical(x))]
    discrete_cols <- cols[sapply(df, function(col) {
      is.factor(col) || is.character(col) || is.logical(col) ||
        (is.numeric(col) && length(unique(col)) <= 10)
    })]
    
    updateSelectInput(session, "time_col", choices = numeric_cols, selected = character(0))
    updateSelectInput(session, "status_col", choices = status_cols, selected = character(0))
    updateSelectInput(session, "group_cols", choices = discrete_cols, selected = character(0))
    updateSelectInput(session, "filter_num_cols", choices = numeric_cols, selected = character(0))
    updateSelectInput(session, "filter_cat_cols", choices = discrete_cols, selected = character(0))
    
    if (identical(df, demo_data)) {
      updateSelectInput(session, "time_col", selected = "Survival_Time")
      updateSelectInput(session, "status_col", selected = "Event_Status")
    }
  })
  
  # --- Dynamic UI for Numeric Filters ---
  output$filter_num_sliders_ui <- renderUI({
    df <- rv$raw_df
    req(df, input$filter_num_cols)
    
    lapply(input$filter_num_cols, function(col_name) {
      col_vals <- na.omit(df[[col_name]])
      min_val <- floor(min(col_vals, na.rm = TRUE))
      max_val <- ceiling(max(col_vals, na.rm = TRUE))
      
      sliderInput(inputId = paste0("filter_num_", col_name),
                  label = paste("Select Range for", col_name, ":"),
                  min = min_val, max = max_val, 
                  value = c(min_val, max_val))
    })
  })
  
  # --- Dynamic UI for Categorical Filters ---
  output$filter_cat_checkboxes_ui <- renderUI({
    df <- rv$raw_df
    req(df, input$filter_cat_cols)
    
    lapply(input$filter_cat_cols, function(col_name) {
      choices <- levels(as.factor(df[[col_name]]))
      
      checkboxGroupInput(inputId = paste0("filter_cat_", col_name),
                         label = paste("Select Values for", col_name, ":"),
                         choices = choices, 
                         selected = choices, 
                         inline = TRUE)
    })
  })
  
  
  # --- Observer to "lock in" data when Run is pressed ---
  observeEvent(input$run, {
    req(rv$raw_df, input$time_col, input$status_col)
    
    df <- rv$raw_df
    
    time_vals <- df[[input$time_col]]
    validate(
      need(is.numeric(time_vals), "Validation Error: The selected 'Time' column must be numeric."),
      need(all(time_vals >= 0, na.rm = TRUE), "Validation Error: The selected 'Time' column contains negative values. Please check your data.")
    )
    
    status_vals <- df[[input$status_col]]
    unique_status_vals <- unique(na.omit(status_vals))
    is_logical_status <- is.logical(status_vals)
    is_numeric_status <- is.numeric(status_vals) && all(unique_status_vals %in% c(0, 1))
    
    validate(
      need(is_logical_status || is_numeric_status, 
           paste("Validation Error: The selected 'Status' column is not valid.",
                 "It must be numeric (containing only 0s and 1s) or logical (TRUE/FALSE)."))
    )
    
    rv$time_col <- input$time_col
    rv$status_col <- input$status_col
    rv$run_pressed <- TRUE
  })
  
  
  # --- Reactive for Filtering and Grouping Data (Debounced) ---
  processed_data <- reactive({
    req(rv$raw_df, rv$run_pressed) 
    
    df <- rv$raw_df
    
    if (!is.null(input$filter_num_cols)) {
      for (col_name in input$filter_num_cols) {
        input_id <- paste0("filter_num_", col_name)
        range_val <- input[[input_id]]
        if (!is.null(range_val)) {
          df <- df[df[[col_name]] >= range_val[1] & df[[col_name]] <= range_val[2] & !is.na(df[[col_name]]), ]
        }
      }
    }
    
    if (!is.null(input$filter_cat_cols)) {
      for (col_name in input$filter_cat_cols) {
        input_id <- paste0("filter_cat_", col_name)
        values_to_keep <- input[[input_id]]
        if (!is.null(values_to_keep)) {
          df <- df[as.factor(df[[col_name]]) %in% values_to_keep, ]
        }
      }
    }
    
    validate(need(nrow(df) > 0, "Validation Error: The current filters result in zero rows of data."))
    
    group_cols <- input$group_cols
    if (!is.null(group_cols) && length(group_cols) > 0) {
      
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
      
      group_counts <- table(df$..group_col..)
      validate(
        need(all(group_counts > 1), 
             paste("Validation Error: The current filters/groups result in at least one group having only one subject (n=1)."))
      )
    }
    
    df
    
  }) %>% debounce(250)
  
  
  # --- Reactive to determine if plot should be grouped ---
  # This is the single source of truth for grouping
  should_be_grouped <- reactive({
    df <- processed_data()
    
    # Check if ..group_col.. exists AND has more than one level
    "..group_col.." %in% colnames(df) &&
      length(unique(df$..group_col..)) > 1
  })
  
  
  # --- Reactive: Fit the survival model ---
  fit <- reactive({
    df <- processed_data()
    
    time_sym <- as.symbol(rv$time_col)
    status_sym <- as.symbol(rv$status_col)
    
    if (should_be_grouped()) {
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
  
  
  # --- NEW Reactive: Get Summary Data for a Specific Time Point ---
  time_point_data <- reactive({
    req(input$time_point, is.numeric(input$time_point), input$time_point >= 0)
    
    f <- fit()
    
    # Use summary() to get survival probabilities at the specified time
    # 'times' argument is the key here
    summary_at_time <- summary(f, times = input$time_point)
    
    max_time <- max(f$time)
    if (input$time_point > max_time) {
      return(paste0("Note: Entered time (", input$time_point, ") is beyond the maximum observed event/censor time (", max_time, "). Showing estimates at last known point before or at this time."))
    }
    
    if (should_be_grouped()) {
      data.frame(
        Group = summary_at_time$strata,
        Time = summary_at_time$time,
        n.risk = summary_at_time$n.risk,
        Prob.Survival = round(summary_at_time$surv, 3),
        Lower.CI = round(summary_at_time$lower, 3),
        Upper.CI = round(summary_at_time$upper, 3)
      )
    } else {
      data.frame(
        Group = "All Subjects",
        Time = summary_at_time$time,
        n.risk = summary_at_time$n.risk,
        Prob.Survival = round(summary_at_time$surv, 3),
        Lower.CI = round(summary_at_time$lower, 3),
        Upper.CI = round(summary_at_time$upper, 3)
      )
    }
  })
  
  
  # --- Reactive: Generate the BASE plot object (no annotations) ---
  base_plot_obj <- reactive({
    df <- processed_data()
    f <- fit()
    
    # Base plot
    if (should_be_grouped()) {
      legend_title <- paste(input$group_cols, collapse = " + ")
      
      p <- ggsurvplot(
        f, data = df, pval = TRUE, conf.int = TRUE, risk.table = TRUE,
        legend.labs = NULL, legend.title = legend_title, palette = "default",
        ggtheme = theme_minimal(), risk.table.y.text = FALSE,
        risk.table.y.text.col = TRUE, ncensor.plot = TRUE
      )
      
    } else {
      p <- ggsurvplot(
        f, data = df, pval = FALSE, conf.int = TRUE, conf.int.style = "ribbon",
        color = "black", conf.int.fill = "grey",
        risk.table = TRUE, legend = "none",
        ggtheme = theme_minimal(), risk.table.y.text = FALSE,
        risk.table.y.text.col = TRUE, ncensor.plot = TRUE
      )
    }
    
    return(p)
  })
  
  # Output: Render the Kaplan-Meier plot
  output$km_plot <- renderPlot({
    
    # 1. Get the cached base plot (this is fast)
    p <- base_plot_obj()
    
    # 2. Get the fit object (also cached) for max time calculation
    f <- fit() 
    
    # 3. Add annotations if time_point is provided (this is fast)
    if (!is.null(input$time_point) && is.numeric(input$time_point) && input$time_point > 0) {
      
      summary_data <- time_point_data()
      
      # Check if it returned a note (e.g., time out of bounds)
      if (is.data.frame(summary_data)) {
        
        # Add vertical line
        p$plot <- p$plot + 
          geom_vline(xintercept = input$time_point, linetype = "dashed", color = "red")
        
        # Create labels
        summary_data$label <- paste0(
          "S(t) = ", summary_data$Prob.Survival,
          "\n95% CI: (", summary_data$Lower.CI, ", ", summary_data$Upper.CI, ")"
        )
        
        # Add text labels using ggrepel
        p$plot <- p$plot +
          ggrepel::geom_text_repel(
            data = summary_data,
            aes(x = Time, y = Prob.Survival, label = label),
            nudge_y = 0.05, # Push label up
            nudge_x = (max(f$time) - min(f$time)) * 0.05, # Push label right
            segment.color = "grey50",
            size = 3.5,
            inherit.aes = FALSE # Our previous fix
          ) +
          # Add points at the intersection
          geom_point(
            data = summary_data, 
            aes(x = Time, y = Prob.Survival), 
            color = "red", 
            size = 3,
            inherit.aes = FALSE # Our previous fix
          )
      }
    }
    
    # 4. Print the final, annotated plot
    print(p)
    
  })
  
  # --- NEW Output: Render the time point summary ---
  output$time_point_summary <- renderPrint({
    if (is.null(input$time_point) || !is.numeric(input$time_point) || input$time_point <= 0) {
      cat("Enter a positive time value in the sidebar to see a detailed summary at that point.")
    } else {
      print(time_point_data())
    }
  })
  
  # Output: Render the model summary
  output$fit_summary <- renderPrint({
    req(fit())
    
    if (should_be_grouped()) {
      f <- fit()
      print(summary(f))
      cat("\n----------------------------------\n")
      cat("Log-Rank Test p-value:\n")
      print(surv_pvalue(f, data = processed_data()))
    } else {
      print(summary(fit()))
    }
  })
  
}

# 4. RUN THE APPLICATION
# ----------------------
shinyApp(ui = ui, server = server)



