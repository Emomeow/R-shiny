# Kaplan-Meier Survival Plotter

This is a R Shiny application for performing and visualizing Kaplan-Meier survival analysis. It allows a user to upload their own data (or use a built-in demo) to generate interactive and publication-ready survival plots using the `survival` and `survminer` packages.

*(This is a placeholder; you can insert your `image_cae5ff.png` screenshot here)*

-----

## Features

  * **Upload Data:** Easily upload your own dataset in CSV format.
  * **Demo Data:** Includes a pre-loaded demo dataset to explore the app's functionality immediately.
  * **Interactive Plotting:** Generates a Kaplan-Meier plot using `ggsurvplot`, complete with:
      * Confidence Interval (ribbon or step)
      * Log-Rank Test p-value (for grouped analysis)
      * Risk Table (Number at risk)
      * Censoring Plot
  * **Flexible Grouping:** Stratify the analysis and plot by one or even multiple grouping variables (e.t., `Treatment` + `Sex`).
  * **Dynamic Filtering:** Interactively filter the source data using sliders for numeric columns (e.g., `Age`) and checkboxes for categorical columns (e.g., `Stage`). The plot and summaries update instantly.
  * **Time Point Analysis:** Specify a single time point (e.g., `365` days) to get a detailed summary table (Survival Probability, 95% CI, n.risk) for that specific time. This point is also annotated directly on the plot.
  * **Model Summaries:** View the complete `survfit` model output and log-rank test details in a separate tab.

-----

## How to Use

Follow these steps to perform an analysis:

1.  **Get Data:**

      * Click **"Browse..."** to upload your own CSV file. The data must have:
          * A numeric "time-to-event" column.
          * A "status" column (coded as 0=censor, 1=event, or `FALSE`/`TRUE`).
      * ...Or, click **"Load Demo Data"** to use the built-in example.

2.  **Configure Analysis:**

      * Using the dropdowns, select the **"Time to Event Column"** (e.g., `Survival_Time`).
      * Select the **"Censor/Status Column"** (e.g., `Event_Status`).

3.  **Run Initial Analysis:**

      * Click the **"Run Analysis"** button. This validates your inputs and generates the initial plot for the full dataset.

4.  **Explore and Interact (Optional):**

      * **Add Groups:** Select one or more columns under **"Select Group Column(s)"** (e.g., `Treatment_Group`) to stratify the plot. The plot will update automatically.
      * **Filter Data:** Use the **"Data Filtering (Interactive)"** controls to dynamically subset your data. The plot and summaries will update as you change the filters.
      * **Get Specifics:** Enter a number in the **"Show Details at Time Point"** box. The plot will instantly update with an annotated point, and a summary table will appear below the plot.

5.  **Review Summaries:**

      * Review the **"Summary at Specific Time Point"** table directly below the plot.
      * Click the **"Model Summary"** tab to see the raw `survfit` output and log-rank test statistics.

-----

## Running the App

### Prerequisites

You must have R and, preferably, RStudio installed on your computer.

### Libraries

Before running, you must have the following R packages installed. You can install them all by running this command in your R console:

```r
install.packages(c("shiny", "survival", "survminer", "ggplot2", "ggpubr", "shinycssloaders", "data.table", "magrittr", "ggrepel"))
```

### Launching the App

1.  Save the application code as `KM.R`.
2.  Open the `KM.R` file in RStudio.
3.  Click the **"Run App"** button that appears at the top of the script editor.

*Alternatively, you can run the app from the R console by setting your working directory to the file's location and running:*

```r
shiny::runApp("KM.R")
```
