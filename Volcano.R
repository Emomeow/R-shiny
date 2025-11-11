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
library(emmeans)   # For covariate adjustment and pairwise comparisons

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
  titlePanel("Volcano Plot Generator (Linear Model)"),
  
  sidebarLayout(
    sidebarPanel(
      p("Upload a single CSV (rows=samples, cols=metadata+genes). Select a control and treatment group to compare."),
      
      # Checkbox to use demo data
      checkboxInput("demo_data", "Use demo data", value = TRUE),
      
      # File Upload
      fileInput("raw_file", "Upload Data (CSV)"),
      
      hr(),
      
      # Dynamic UI for column selection
      uiOutput("group_col_select"),
      uiOutput("control_group_select"),     # <-- CHANGED
      uiOutput("treatment_group_select"),   # <-- CHANGED
      uiOutput("covariate_select"),
      uiOutput("first_gene_col_select"),
      
      # Action button
      actionButton("run_analysis", "Run Analysis", class = "btn-primary"),
      
      hr(),
      
      h4("Volcano Plot Controls"),
      
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
  
  # --- NEW: Control Group Selector ---
  output$control_group_select <- renderUI({
    df <- raw_data()
    col <- input$group_col
    if (is.null(df) || is.null(col) || !col %in% names(df)) return(NULL)
    
    choices <- unique(df[[col]])
    # Guess "Control"
    control_guess <- grep("control|ctrl|base", choices, value = TRUE, ignore.case = TRUE)[1]
    if (is.na(control_guess)) {
      control_guess <- choices[1]
    }
    selectizeInput("control_group", "Select Control Group:",
                   choices = choices, selected = control_guess)
  })
  
  # --- NEW: Treatment Group Selector ---
  output$treatment_group_select <- renderUI({
    df <- raw_data()
    col <- input$group_col
    req(input$control_group)
    if (is.null(df) || is.null(col) || !col %in% names(df)) return(NULL)
    
    # Choices are all groups *except* the selected control group
    choices <- setdiff(unique(df[[col]]), input$control_group)
    
    # Guess the first available treatment
    treatment_guess <- choices[1]
    if (input$demo_data) {
      treatment_guess <- "Treated_A"
    }
    
    selectizeInput("treatment_group", "Select Treatment Group:",
                   choices = choices, selected = treatment_guess)
  })
  
  output$covariate_select <- renderUI({
    df <- raw_data()
    req(input$group_col, input$first_gene_col)
    if (is.null(df)) return(NULL)
    
    gene_start_index <- which(names(df) == input$first_gene_col)
    if (length(gene_start_index) == 0) return(NULL)
    
    meta_cols <- names(df)[1:(gene_start_index - 1)]
    potential_covariates <- setdiff(meta_cols, input$group_col)
    
    covariate_choices <- character(0)
    if (length(potential_covariates) > 0) {
      is_numeric_cov <- sapply(df[potential_covariates], is.numeric)
      covariate_choices <- potential_covariates[is_numeric_cov]
    }
    
    if (length(covariate_choices) == 0) {
      return(p("No numeric covariates available."))
    }
    
    cov_guess <- grep("age|sex|batch", covariate_choices, value = TRUE, ignore.case = TRUE)
    
    selectizeInput("covariates", "(Optional) Select Covariate(s):",
                   choices = covariate_choices, selected = cov_guess, multiple = TRUE)
  })
  
  
  output$first_gene_col_select <- renderUI({
    df <- raw_data()
    if (is.null(df)) return(NULL)
    req(input$group_col)
    
    group_col_index <- which(names(df) == input$group_col)
    if (length(group_col_index) == 0) return(NULL)
    
    gene_guess <- grep("gene_1|gene1", names(df), value = TRUE, ignore.case = TRUE)[1]
    
    if (!is.na(gene_guess)) {
      first_numeric_guess <- gene_guess
    } else {
      first_numeric_guess <- NA
      if (group_col_index < ncol(df)) {
        for (i in (group_col_index + 1):ncol(df)) {
          if (is.numeric(df[[i]])) {
            if (!tolower(names(df)[i]) %in% c("age", "batch", "sex", "id", "sample")) {
              first_numeric_guess <- names(df)[i]
              break
            }
          }
        }
      }
      
      if (is.na(first_numeric_guess) && group_col_index < ncol(df)) {
        for (i in (group_col_index + 1):ncol(df)) {
          if (is.numeric(df[[i]])) {
            first_numeric_guess <- names(df)[i]
            break
          }
        }
      }
      
      if(is.na(first_numeric_guess)) {
        first_numeric_guess <- names(df)[group_col_index + 1]
      }
    }
    
    selectizeInput("first_gene_col", "Select *First* Gene Column:", choices = names(df), selected = first_numeric_guess)
  })
  
  
  # 3. EXPENSIVE ANALYSIS: Run Linear Model + emmeans
  analysis_results <- eventReactive(input$run_analysis, {
    
    # --- UPDATED: Require control and treatment groups ---
    req(raw_data(), input$group_col, input$control_group, input$treatment_group, 
        input$first_gene_col)
    
    # Store selected groups
    selected_groups <- c(input$control_group, input$treatment_group)
    
    df <- raw_data()
    
    # 1. Find gene columns
    gene_start_index <- which(names(df) == input$first_gene_col)
    if (length(gene_start_index) == 0) {
      showNotification("Could not find the 'first gene' column.", type = "error")
      return(NULL)
    }
    gene_cols <- names(df)[gene_start_index:ncol(df)]
    
    # 2. Get covariates
    covariate_cols <- input$covariates
    
    # 3. Filter data for ONLY the two selected groups
    data_filtered <- df %>%
      filter(.data[[input$group_col]] %in% selected_groups) %>%
      # Set factor levels with control as the reference (first level)
      mutate(!!input$group_col := factor(.data[[input$group_col]], levels = selected_groups))
    
    if (nrow(data_filtered) < 2) {
      showNotification("Not enough data after filtering.", type = "error")
      return(NULL)
    }
    
    # 4. Pivot to long format
    cols_to_keep <- c(input$group_col, covariate_cols)
    
    joined_data <- data_filtered %>%
      select(all_of(cols_to_keep), all_of(gene_cols)) %>%
      pivot_longer(
        cols = all_of(gene_cols),
        names_to = "GeneID",
        values_to = "Expression"
      )
    
    # 5. Build model formula
    group_var <- input$group_col
    
    formula_rhs <- group_var
    if (!is.null(covariate_cols) && length(covariate_cols) > 0) {
      formula_rhs <- paste(formula_rhs, "+", paste(covariate_cols, collapse = " + "))
    }
    
    model_formula <- as.formula(paste("Expression ~", formula_rhs))
    
    showNotification(paste("Running model:", deparse(model_formula)), type = "message")
    
    # 6. Define safe analysis function
    safe_lm_emmeans <- possibly(
      # --- FIX: Remove the 'data' argument ---
      function(group_var, formula) {
        
        fit <- tryCatch({
          # --- FIX: Use cur_data() directly ---
          # This gets the data frame for the current group
          lm(formula, data = cur_data())
        }, error = function(e) {
          message("lm() failed: ", e$message)
          return(list(error = e$message))
        })
        
        if (is.list(fit) && !is.null(fit$error)) {
          return(data.frame(contrast = "MODEL_FAILED", 
                            estimate = 0, 
                            p.value = 1, 
                            error_msg = fit$error))
        }
        
        # Since we only have 2 groups, emmeans will find the single pair
        emm <- emmeans(fit, as.formula(paste("~", group_var)))
        # 'adjust' is irrelevant for one pair, but we leave it
        pairs <- pairs(emm, adjust = "tukey") 
        return(summary(pairs, infer = TRUE))
      },
      otherwise = NULL
    )
    
    # 7. Run analysis for all genes
    final_results <- joined_data %>%
      group_by(GeneID) %>%
      # --- FIX: Don't pass .data ---
      summarise(results = list(safe_lm_emmeans(group_var = input$group_col, formula = model_formula)), .groups = "drop") %>%
      tidyr::unnest(results)
    
    lm_errors <- final_results %>% 
      filter(contrast == "MODEL_FAILED")
    
    if (nrow(lm_errors) > 0) {
      first_error <- unique(lm_errors$error_msg)[1]
      showNotification(paste("Analysis failed for", nrow(lm_errors), 
                             "genes. First error:", first_error), 
                       type = "error", duration = 10)
      final_results <- final_results %>% filter(contrast != "MODEL_FAILED")
    }
    
    if (is.null(final_results) || nrow(final_results) == 0) {
      showNotification("Analysis failed for all genes. Check data for variance and missing values.", type = "error")
      return(NULL)
    }
    
    # 8. Format results
    final_results <- final_results %>%
      select(
        GeneID,
        contrast,
        log2FoldChange = estimate,
        p_value = p.value # P-value from the single pairwise comparison
      )
    
    showNotification("Analysis complete! Plot is ready.", type = "message")
    
    return(final_results)
  })
  
  # 4. REMOVED contrast_select UI
  
  
  # 5. CHEAP PLOTTING: Reactive for processing and plotting
  processed_results <- reactive({
    
    # --- UPDATED: No longer need input$selected_contrast ---
    de_results <- analysis_results()
    req(de_results)
    req(input$pval_thresh, input$fc_thresh, input$num_labels)
    
    # No filter needed, de_results only has one contrast
    
    # 6. Add significance categories
    df_processed <- de_results %>%
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
    
    # --- UPDATED: Get plot title from the single contrast ---
    plot_title <- paste("Volcano Plot:", df_processed$contrast[1])
    
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
      # --- UPDATED: Simplified message ---
      if (is.null(analysis_results())) {
        return(data.frame(Info = "Select groups and click 'Run Analysis' to see results."))
      } else {
        return(data.frame(Info = "Analysis running or no significant results found..."))
      }
    }
    DT::datatable(res$table, options = list(pageLength = 10), rownames = FALSE)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
