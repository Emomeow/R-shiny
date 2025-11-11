# Load necessary libraries
library(shiny)
library(dplyr)
library(tidyr)     # For pivot_longer/wider
library(tibble)    # For rownames_to_column
library(broom)     # For tidy()
library(stringr)   # For string matching
library(ggplot2)
library(ggrepel)   # For non-overlapping text labels
library(DT)        # For interactive data tables
library(purrr)     # For possibly()

# --- Create a realistic demo dataset in the new format ---
# (Rows = Samples, Cols = Metadata + Genes)
set.seed(42)
n_genes <- 1000
# Create expression data
control_samples <- data.frame(matrix(rnorm(n_genes * 3, mean = 8, sd = 1.5), n_genes))
names(control_samples) <- paste0("Control_", 1:3)

# Create two different treatment groups
treated_A_samples <- data.frame(matrix(rnorm(n_genes * 3, mean = 8, sd = 1.5), n_genes))
names(treated_A_samples) <- paste0("Treated_A_", 1:3)

treated_B_samples <- data.frame(matrix(rnorm(n_genes * 3, mean = 8, sd = 1.5), n_genes))
names(treated_B_samples) <- paste0("Treated_B_", 1:3)

gene_ids <- paste0("Gene_", 1:n_genes)

# Add differential expression
n_up <- 50
n_down <- 40
# Treatment A vs Control
treated_A_samples[1:n_up, ] <- treated_A_samples[1:n_up, ] + rnorm(n_up * 3, mean = 2, sd = 0.5)
treated_A_samples[(n_up + 1):(n_up + n_down), ] <- treated_A_samples[(n_up + 1):(n_up + n_down), ] - rnorm(n_down * 3, mean = 2, sd = 0.5)

# Treatment B vs Control (different set of genes)
n_up_B <- 30
n_down_B <- 30
treated_B_samples[101:(100 + n_up_B), ] <- treated_B_samples[101:(100 + n_up_B), ] + rnorm(n_up_B * 3, mean = -2, sd = 0.5)
treated_B_samples[(100 + n_up_B + 1):(100 + n_up_B + n_down_B), ] <- treated_B_samples[(100 + n_up_B + 1):(100 + n_up_B + n_down_B), ] - rnorm(n_down_B * 3, mean = 1.5, sd = 0.5)


# Combine into a (rows=genes, cols=samples) format first
demo_expression_data <- data.frame(GeneID = gene_ids) %>%
  bind_cols(control_samples, treated_A_samples, treated_B_samples)

# Create metadata
demo_metadata <- data.frame(
  Sample = c(
    paste0("Control_", 1:3),
    paste0("Treated_A_", 1:3),
    paste0("Treated_B_", 1:3)
  ),
  Condition = c(
    rep("Control", 3),
    rep("Treated_A", 3),
    rep("Treated_B", 3)
  ),
  Age = c(30, 32, 31, 29, 30, 31, 25, 26, 27) # Example other metadata
)

# Transpose expression data and join with metadata
demo_expr_t <- as.data.frame(t(demo_expression_data[, -1]))
names(demo_expr_t) <- demo_expression_data$GeneID
demo_expr_t$Sample <- rownames(demo_expr_t)

# This is the final single-file demo data
demo_single_file <- demo_metadata %>%
  left_join(demo_expr_t, by = "Sample")
# --- End of demo data creation ---


# Define the UI (User Interface)
ui <- fluidPage(
  titlePanel("Volcano Plot Generator (T-Test / ANOVA)"),
  
  sidebarLayout(
    sidebarPanel(
      p("Upload a single CSV (rows=samples, cols=metadata+genes). Select 2 groups for a t-test or 3+ for ANOVA."),
      
      # Checkbox to use demo data
      checkboxInput("demo_data", "Use demo data", value = TRUE),
      
      # File Upload
      fileInput("raw_file", "Upload Data (CSV)"),
      
      hr(),
      
      # Dynamic UI for column selection
      uiOutput("group_col_select"),
      uiOutput("groups_to_compare_select"),
      uiOutput("first_gene_col_select"),
      
      # Action button
      actionButton("run_analysis", "Run Analysis", class = "btn-primary"),
      
      hr(),
      
      h4("Volcano Plot Controls"),
      # This new UI lets you pick which pair to plot
      uiOutput("contrast_select"),
      
      # Threshold inputs
      numericInput("pval_thresh", "P-value threshold:", 0.05, min = 0.001, max = 1, step = 0.01),
      numericInput("fc_thresh", "Log2 Fold Change threshold (absolute):", 1, min = 0, max = 10, step = 0.5),
      
      # Labeling input
      numericInput("num_labels", "Number of top genes to label:", 10, min = 0, max = 50, step = 1)
      
    ),
    
    mainPanel(
      h4("Volcano Plot"),
      plotOutput("volcano_plot"),
      
      hr(),
      
      h4("Top Significant Features"),
      DT::dataTableOutput("top_genes_table")
    )
  )
)

# Define the Server logic
server <- function(input, output, session) {
  
  # 1. Reactive expression to read data
  raw_data <- reactive({
    if (input$demo_data) {
      return(demo_single_file)
    }
    req(input$raw_file)
    tryCatch({
      read.csv(input$raw_file$datapath, check.names = FALSE)
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # 2. Render dynamic UI selectors
  
  output$group_col_select <- renderUI({
    df <- raw_data()
    if (is.null(df)) return(NULL)
    # Guess the grouping column (e.g., "Condition")
    group_guess <- grep("condition|group|type", names(df), value = TRUE, ignore.case = TRUE)[1]
    if (is.na(group_guess)) {
      group_guess <- names(df)[2] # Default to 2nd col if no match
    }
    selectizeInput("group_col", "Select Grouping Column:", choices = names(df), selected = group_guess)
  })
  
  output$groups_to_compare_select <- renderUI({
    df <- raw_data()
    col <- input$group_col
    if (is.null(df) || is.null(col) || !col %in% names(df)) return(NULL)
    
    choices <- unique(df[[col]])
    # Select default based on demo data for convenience
    selected_choices <- if (input$demo_data) c("Control", "Treated_A") else choices
    selectizeInput("groups_to_compare", "Select Groups to Analyze (2 or more):",
                   choices = choices, selected = selected_choices, multiple = TRUE)
  })
  
  output$first_gene_col_select <- renderUI({
    df <- raw_data()
    if (is.null(df)) return(NULL)
    req(input$group_col)
    
    group_col_index <- which(names(df) == input$group_col)
    if (length(group_col_index) == 0) return(NULL)
    
    first_numeric_guess <- NA
    if (group_col_index < ncol(df)) {
      for (i in (group_col_index + 1):ncol(df)) {
        if (is.numeric(df[[i]])) {
          first_numeric_guess <- names(df)[i]
          break
        }
      }
    }
    if (is.na(first_numeric_guess)) {
      first_numeric_guess <- names(df)[group_col_index + 1]
    }
    
    selectizeInput("first_gene_col", "Select *First* Gene Column:", choices = names(df), selected = first_numeric_guess)
  })
  
  
  # 3. EXPENSIVE ANALYSIS: Run T-test OR ANOVA
  # This eventReactive block only runs when 'run_analysis' is clicked.
  analysis_results <- eventReactive(input$run_analysis, {
    
    req(raw_data(), input$group_col, input$groups_to_compare, input$first_gene_col)
    
    num_groups <- length(input$groups_to_compare)
    
    if (num_groups < 2) {
      showNotification("Please select at least 2 groups to compare.", type = "error")
      return(NULL)
    }
    
    df <- raw_data()
    
    # --- Data Processing (The "Reshape") ---
    
    # 1. Find all gene columns
    gene_start_index <- which(names(df) == input$first_gene_col)
    if (length(gene_start_index) == 0) {
      showNotification("Could not find the 'first gene' column.", type = "error")
      return(NULL)
    }
    gene_cols <- names(df)[gene_start_index:ncol(df)]
    
    # 2. Filter to only the groups of interest
    data_filtered <- df %>%
      filter(.data[[input$group_col]] %in% input$groups_to_compare) %>%
      # Important: Make the group column a factor
      mutate(!!input$group_col := factor(.data[[input$group_col]], levels = input$groups_to_compare))
    
    if (nrow(data_filtered) < 2) {
      showNotification("Not enough data after filtering for selected groups.", type = "error")
      return(NULL)
    }
    
    # 3. Pivot to long format
    joined_data <- data_filtered %>%
      select(all_of(c(input$group_col, gene_cols))) %>%
      pivot_longer(
        cols = all_of(gene_cols),
        names_to = "GeneID",
        values_to = "Expression"
      )
    
    # 4. *** NEW CONDITIONAL LOGIC ***
    
    final_results <- NULL
    
    if (num_groups == 2) {
      # --- Run Welch's T-test ---
      showNotification("Running Welch's t-test for all genes...", type = "message")
      
      # R's t.test(formula) calculates mean(level1) - mean(level2)
      # We set the levels in data_filtered, so level1 = input$groups_to_compare[1]
      # We want the contrast to be level2 - level1.
      
      safe_ttest <- possibly(
        ~ tidy(t.test(as.formula(paste("Expression ~", input$group_col)), data = .x)),
        otherwise = NULL
      )
      
      final_results <- joined_data %>%
        group_by(GeneID) %>%
        do(safe_ttest(.)) %>%
        ungroup() %>%
        filter(!is.null(p.value)) %>% # Filter out failed tests
        mutate(
          # Create the contrast string
          contrast = paste(input$groups_to_compare[2], input$groups_to_compare[1], sep = "-"),
          # Invert the estimate to get (level2 - level1)
          log2FoldChange = -estimate
        ) %>%
        select(
          GeneID,
          contrast,
          log2FoldChange,
          p_value = p.value  # This is the unadjusted p-value
        )
      
    } else {
      # --- Run ANOVA + Tukey's HSD ---
      showNotification("Running ANOVA and Tukey's HSD for all genes...", type = "message")
      
      safe_tukey <- possibly(
        ~ tidy(TukeyHSD(aov(as.formula(paste("Expression ~", input$group_col)), data = .x))),
        otherwise = NULL
      )
      
      final_results <- joined_data %>%
        group_by(GeneID) %>%
        do(safe_tukey(.)) %>%
        ungroup() %>%
        filter(!is.null(contrast)) %>% # Filter out failed tests
        select(
          GeneID,
          contrast,
          log2FoldChange = estimate,  # 'estimate' is the mean difference
          p_value = adj.p.value       # 'adj.p.value' is the Tukey-adjusted p-value
        )
    }
    
    if (is.null(final_results) || nrow(final_results) == 0) {
      showNotification("Analysis failed. Check if data is numeric and has variance.", type = "error")
      return(NULL)
    }
    
    showNotification("Analysis complete! Plot is now interactive.", type = "message")
    
    return(final_results)
  })
  
  # 4. Render the NEW UI element for contrast selection
  output$contrast_select <- renderUI({
    de_results <- analysis_results()
    req(de_results)
    contrast_choices <- unique(de_results$contrast)
    
    selectInput("selected_contrast", "Select Contrast to Plot:",
                choices = contrast_choices, selected = contrast_choices[1])
  })
  
  
  # 5. CHEAP PLOTTING: Reactive for processing and plotting
  processed_results <- reactive({
    
    de_results <- analysis_results()
    req(de_results, input$selected_contrast)
    req(input$pval_thresh, input$fc_thresh, input$num_labels)
    
    de_results_filtered <- de_results %>%
      filter(contrast == input$selected_contrast)
    
    # 6. Add significance categories
    df_processed <- de_results_filtered %>%
      mutate(
        negLog10P = -log10(p_value),
        Significance = case_when(
          log2FoldChange > input$fc_thresh & p_value < input$pval_thresh ~ "Upregulated",
          log2FoldChange < -input$fc_thresh & p_value < input$pval_thresh ~ "Downregulated",
          TRUE ~ "Not Significant"
        ),
        Significance = factor(Significance, levels = c("Upregulated", "Downregulated", "Not Significant"))
      )
    
    # --- Generate Plot ---
    y_line <- -log10(input$pval_thresh)
    plot_colors <- c("Upregulated" = "red", "Downregulated" = "blue", "Not Significant" = "grey")
    
    plot_title <- paste("Volcano Plot:", input$selected_contrast)
    
    base_plot <- ggplot(df_processed, aes(x = log2FoldChange, y = negLog10P, color = Significance)) +
      geom_point(alpha = 0.5, size = 1.5) +
      scale_color_manual(values = plot_colors) +
      geom_vline(xintercept = c(-input$fc_thresh, input$fc_thresh), linetype = "dashed", col = "black") +
      geom_hline(yintercept = y_line, linetype = "dashed", col = "black") +
      labs(title = plot_title, x = "log2(Fold Change)", y = "-log10(P-value)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
    
    # --- Add labels ---
    data_to_label <- df_processed %>%
      filter(Significance != "Not Significant") %>%
      arrange(p_value) %>%
      head(input$num_labels)
    
    final_plot <- base_plot +
      geom_text_repel(
        data = data_to_label,
        aes(label = GeneID),
        box.padding = 0.5, point.padding = 0.5,
        segment.color = 'grey50', size = 3.5, max.overlaps = Inf
      )
    
    # --- Get top genes for table ---
    top_genes_table <- df_processed %>%
      filter(Significance != "Not Significant") %>%
      arrange(p_value) %>%
      mutate(
        p_value = scales::scientific(p_value, digits = 3),
        log2FoldChange = round(log2FoldChange, 3)
      ) %>%
      select(GeneID, log2FoldChange, p_value, Significance)
    
    return(list(plot = final_plot, table = top_genes_table))
  })
  
  
  # 6. Render the plot
  output$volcano_plot <- renderPlot({
    res <- processed_results()
    if (is.null(res)) return()
    print(res$plot)
  })
  
  # 7. Render the data table
  output$top_genes_table <- DT::renderDataTable({
    res <- processed_results()
    if (is.null(res)) {
      if (is.null(analysis_results())) {
        return(data.frame(Info = "Select groups and click 'Run Analysis' to see results."))
      } else {
        return(data.frame(Info = "Select a contrast to view results."))
      }
    }
    DT::datatable(res$table, options = list(pageLength = 10), rownames = FALSE)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)