# K-M Survival Curve Shiny App

# 1. LOAD LIBRARIES
# -----------------
# Make sure these are installed: install.packages(c("shiny", "survival", "survminer"))
library(shiny)
library(survival)
library(survminer)

# 2. DEFINE USER INTERFACE (UI)
# ----------------------------
ui <- fluidPage(
  # Set a theme for a cleaner look
  theme = shinythemes::shinytheme("flatly"),
  
  # Application title
  titlePanel("Kaplan-Meier Survival Curve Generator"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # --- Sidebar Panel for Inputs ---
    sidebarPanel(
      width = 3,
      
      # Input: File upload
      fileInput("file1", "Upload CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Checkbox for header
      checkboxInput("header", "File contains header", TRUE),
      
      # Input: Select separator
      radioButtons("sep", "Column Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Horizontal line
      tags$hr(),
      
      # --- Dynamic UI for Column Selection ---
      # These inputs are generated in the server
      
      # Input: Select time column
      uiOutput("time_col_ui"),
      
      # Input: Select status column
      uiOutput("status_col_ui"),
      
      # Input: Select group column (optional)
      uiOutput("group_col_ui"),
      
      # Horizontal line
      tags$hr(),
      
      # Action button to trigger the analysis
      actionButton("run_analysis", "Run Analysis", icon = icon("chart-line"), class = "btn-primary btn-lg")
    ),
    
    # --- Main Panel for Outputs ---
    mainPanel(
      width = 9,
      # Use tabs for a clean output
      tabsetPanel(type = "tabs",
                  tabPanel("K-M Plot", 
                           h4("Kaplan-Meier Survival Plot"),
                           p("The plot shows the survival probability over time. If a group is selected, it will display separate curves and a log-rank p-value."),
                           # Output: K-M Plot
                           plotOutput("km_plot", height = "500px")
                  ),
                  tabPanel("Survival Summary", 
                           h4("Survival Fit Summary"),
                           p("This table provides the survival probabilities, standard errors, and confidence intervals at different time points."),
                           # Output: Summary table
                           verbatimTextOutput("surv_summary")
                  ),
                  tabPanel("Data Preview",
                           h4("Uploaded Data (First 10 Rows)"),
                           p("A quick preview of your data to ensure it loaded correctly."),
                           # Output: Data table preview
                           tableOutput("data_preview")
                  )
      )
    )
  )
)

# 3. DEFINE SERVER LOGIC
# ----------------------
server <- function(input, output, session) {
  
  # --- Reactive: Read Data ---
  # Reactively reads the uploaded file
  data <- reactive({
    # Require the file to be uploaded
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep)
      },
      error = function(e) {
        # Return a safe error if file is bad
        stop(safeError(e))
      }
    )
    
    return(df)
  })
  
  # --- Dynamic UI Generation ---
  
  # Get column names once data is loaded
  column_names <- reactive({
    req(data())
    names(data())
  })
  
  # Render the Time Column selector
  output$time_col_ui <- renderUI({
    req(column_names())
    selectInput("time_col", "Select Time to Event Column:", 
                choices = column_names(), selected = column_names()[1])
  })
  
  # Render the Status Column selector
  output$status_col_ui <- renderUI({
    req(column_names())
    # Try to guess the second column as status
    selected_status <- if (length(column_names()) > 1) column_names()[2] else column_names()[1]
    selectInput("status_col", "Select Censor/Status Column:", 
                choices = column_names(), selected = selected_status)
  })
  
  # Render the Group Column selector
  output$group_col_ui <- renderUI({
    req(column_names())
    # Add "None" as the first option
    selectInput("group_col", "Select Group Column (Optional):", 
                choices = c("None", column_names()), selected = "None")
  })
  
  # --- Data Preview Output ---
  output$data_preview <- renderTable({
    req(data())
    head(data(), 10)
  })
  
  # --- Reactive: Run Analysis ---
  # This block runs ONLY when the action button is pressed
  analysis_results <- eventReactive(input$run_analysis, {
    
    # Require all necessary inputs
    req(data(), input$time_col, input$status_col, input$group_col)
    
    # Create a local copy of the data
    df <- data()
    
    # --- Data Preparation & Formula Creation ---
    # We must wrap column names in ` ` in case they have spaces or special chars
    # And ensure they are the correct data type
    tryCatch({
      time_var <- `[[`(df, input$time_col)
      if (!is.numeric(time_var)) {
        time_var <- as.numeric(as.character(time_var))
      }
      
      status_var <- `[[`(df, input$status_col)
      if (!is.integer(status_var)) {
        status_var <- as.integer(as.character(status_var))
      }
      
      # Build the survival formula
      if (input$group_col == "None") {
        # No grouping variable
        formula_str <- "Surv(time_var, status_var) ~ 1"
        form <- as.formula(formula_str)
      } else {
        # With grouping variable
        group_var <- as.factor(`[[`(df, input$group_col))
        df$group_var <- group_var # Add to data frame for survfit
        
        formula_str <- "Surv(time_var, status_var) ~ group_var"
        form <- as.formula(formula_str)
      }
      
      # --- Model Fitting ---
      fit <- survfit(form, data = df)
      
      # Return a list of results
      list(
        fit = fit,
        data = df, # Data now includes a 'group_var' if one was selected
        formula = form,
        group_var_name = input$group_col
      )
    }, error = function(e) {
      # Return an error object if anything fails (e.g., bad column type)
      list(error = paste("Error during analysis:", e$message,
                         "\nPlease check that Time is numeric and Status is integer (e.g., 0/1)."))
    })
  })
  
  # --- Output: Render K-M Plot ---
  output$km_plot <- renderPlot({
    
    results <- analysis_results()
    
    # Check if the analysis returned an error
    if (!is.null(results$error)) {
      # Plot a blank canvas with the error message
      plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Analysis Error", 
           type="n", xlab="", ylab="", xaxt="n", yaxt="n")
      text(0.5, 0.5, results$error, col = "red", cex = 1.2)
      return()
    }
    
    # Determine if we should show p-value
    show_pval <- (results$group_var_name != "None")
    
    # Create the plot
    p <- ggsurvplot(
      results$fit,
      data = results$data,
      pval = show_pval,             # Show p-value if groups exist
      conf.int = TRUE,             # Show confidence intervals
      risk.table = TRUE,           # Add risk table
      risk.table.col = "strata",   # Color risk table by group
      legend.title = results$group_var_name, # Use column name for legend
      surv.median.line = "hv",     # Add median survival lines
      ggtheme = theme_minimal()    # Use a clean theme
    )
    
    # Print the plot
    print(p)
  })
  
  # --- Output: Render Summary Table ---
  output$surv_summary <- renderPrint({
    
    results <- analysis_results()
    
    # Check for error
    if (!is.null(results$error)) {
      cat(results$error)
      return()
    }
    
    # Show the summary of the fit
    summary(results$fit)
  })
  
}

# 4. RUN THE APPLICATION
# ----------------------
shinyApp(ui = ui, server = server)
