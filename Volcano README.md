# **Volcano Plot Generator (Linear Model)**

This Shiny application provides a complete workflow for generating volcano plots directly from a raw "wide" data file. It uses a robust linear model (lm) and emmeans for statistical analysis, allowing for optional covariate adjustment.

**shinyapps.io link: https://yuezhan.shinyapps.io/Volcano/**

## **Key Features**

* **Single File Upload:** No need to separate your data and metadata. The app accepts one "wide" CSV file.  
* **Covariate Adjustment:** Optionally select one or more covariates (e.g., "Age", "Sex") to adjust your analysis for. The app runs a linear model (e.g., Expression \~ Condition \+ Age).  
* **Smart Statistical Analysis:**  
  * Uses a unified lm \+ emmeans workflow for all comparisons.  
  * **2-Group Comparison:** Calculates p-values and fold changes (equivalent to a t-test from a linear model).  
  * **3+ Group Comparison:** Automatically performs pairwise comparisons with **Tukey's adjustment** for multiple comparisons.  
* **Interactive Plotting:**  
  * Select which pairwise contrast to visualize (e.g., "Treated\_A-Control" or "Treated\_B-Control").  
  * Interactively change p-value and Log2 Fold Change thresholds.  
  * Interactively set the number of top genes to label in the plot.  
* **Data Export:** View and search the top significant features in an interactive table.

## **How to Use the App**

1. **Load Data:** Upload your CSV file or check the "Use demo data" box to get started.  
2. **Select Columns (in Sidebar):**  
   * **Select Grouping Column:** Choose the column that contains your group information (e.g., "Condition", "Treatment\_Group").  
   * **Select Groups to Analyze:** Choose the specific groups you want to compare.  
   * **(Optional) Select Covariate(s):** Choose any metadata columns to include in the model as covariates.  
   * **Select *First* Gene Column:** This is the most important step. Select the *first* column in your file that contains gene expression data. The app will assume all columns from this one to the end of the file are genes.  
3. **Run Analysis:** Click the **"Run Analysis"** button. This performs the "expensive" statistical calculations for every gene.  
4. **Select Contrast & Interact:**  
   * After the analysis, a **"Select Contrast to Plot"** dropdown will appear. Choose the pairwise comparison you want to see.  
   * Use the "Volcano Plot Controls" (thresholds, labels) to adjust the plot in real-time. The plot will update instantly without re-running the analysis.

## **Data Input Format**

The app requires your data to be in a single **"wide" CSV file**.

* **Rows** must be **Samples**.  
* **Columns** must be **Metadata followed by Gene/Feature data**.

The app is designed to find the metadata columns first (like Sample and Condition) and then treat all subsequent columns (from the "First Gene Column" you select) as your numeric expression data.

### **Example Data Structure:**

Your CSV file should look like this:

| Sample | Condition | Age | Gene\_1 | Gene\_2 | Gene\_3 | ... |
| :---- | :---- | :---- | :---- | :---- | :---- | :---- |
| s1 | Control | 30 | 7.8 | 9.1 | 8.2 | ... |
| s2 | Control | 32 | 8.1 | 9.0 | 8.1 | ... |
| s3 | Treated\_A | 29 | 10.2 | 8.9 | 10.5 | ... |
| s4 | Treated\_A | 30 | 9.9 | 9.2 | 10.3 | ... |
| s5 | Treated\_B | 25 | 7.9 | 9.0 | 9.5 | ... |
| s6 | Treated\_B | 26 | 8.2 | 9.1 | 9.2 | ... |

* Sample, Condition, and Age are **Metadata columns**.  
* Gene\_1, Gene\_2, Gene\_3... are **Gene/Feature columns**.  
* In the app, you would set:  
  * Select Grouping Column: to **"Condition"**  
  * (Optional) Select Covariate(s): to **"Age"**  
  * Select \*First\* Gene Column: to **"Gene\_1"**

## **Required R Packages**

To run this app locally, you will need the following R libraries installed:

install.packages(c(  
  "shiny", "dplyr", "tidyr", "tibble", "broom",   
  "stringr", "ggplot2", "ggrepel", "DT", "purrr", "emmeans"  
))  
