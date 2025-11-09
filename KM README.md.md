# **Kaplan-Meier Survival Plotter Shiny App**

## **Aim of the App**

This Shiny application provides a fully interactive, code-free web interface for performing and visualizing survival analysis. Its primary aim is to allow researchers, students, and analysts to quickly upload their own survival data (in .csv format), run a base analysis, and then interactively explore different groups and data filters in real-time.

## **Features**

* **Easy Upload:** Upload your survival data as a simple .csv file.  
* **Dynamic Column Mapping:** The app intelligently filters and populates the dropdown menus based on your CSV's column types.  
* **Fully Interactive Analysis:** After an initial analysis is run, you can:  
  * **Change Grouping Interactively:** Select one or more categorical columns (e.g., Sex and Stage) to automatically generate combined groups (e.g., "Male \- Stage I"). The plot will update instantly.  
  * **Change Filters Interactively:** Filter your dataset in real-time. The plot updates immediately as you adjust sliders or checkboxes.  
    * **Multi-Numeric Filter:** Select multiple numeric columns (like Age) and use sliders to analyze specific ranges.  
    * **Multi-Categorical Filter:** Select multiple categorical columns (like Stage) and use checkboxes to include or exclude values.  
* **Rich Visualization:** Creates a ggplot2-based Kaplan-Meier plot using the survminer package, which includes:  
  * The survival curve(s).  
  * Confidence intervals.  
  * A p-value from the log-rank test (for grouped analysis).  
  * A risk table showing the number of subjects at risk over time.  
  * A censor plot.  
* **Robust Input Validation:** Includes built-in data checks to ensure your 'Time', 'Status', and filtered group sizes are valid, providing user-friendly error messages.  
* **Model Summary:** View the raw output from the survfit model in a separate "Model Summary" tab, which also updates interactively.

## **Requirements**

Before running the app, you must have the following R packages installed. You can install them all by running this command in your R console:

install.packages(c("shiny", "survival", "survminer", "ggplot2", "ggpubr", "shinycssloaders"))

## **Running the App**

1. Save the application code as a single file named app.R.  
2. Open RStudio.  
3. Ensure your R working directory is set to the folder where you saved app.R.  
4. Run one of the following commands in the R console:

\# Option 1: Using the shiny package  
shiny::runApp()

\# Option 2: If RStudio is open with the app.R file,  
\# you can click the "Run App" button in the top-right  
\# corner of the script editor.

The application will launch in a new window or in your web browser.

## **Publishing to GitHub**

This app is fully self-contained in the app.R file, making it easy to share on GitHub. You can publish your project directory (containing app.R and this README.md) to a public GitHub repository.

Once it is on GitHub, others can run your app directly from the R console (after installing the required packages) by using the shiny::runGitHub function:

\# Example command to run from GitHub:  
\# Replace 'YourUsername/YourRepoName' with your actual GitHub repo path  
shiny::runGitHub("YourUsername/YourRepoName")

## **How to Use the App**

1. **Upload Data:** Click the "Choose CSV File" button and select your data file.  
2. **Map Core Columns:**  
   * **Select Time to Event Column:** Choose the numeric column from your data that represents time (e.g., Survival\_Time, Days\_to\_Event).  
   * **Select Censor/Status Column:** Choose the column representing the event status. This **must** be a numeric column with only 0 (censored) and 1 (event) or a logical TRUE/FALSE column.  
3. **Run Analysis:** Click the blue "Run Analysis" button. This "locks in" your time and status columns and generates the initial plot for the whole dataset.  
4. **Explore Interactively (After Running):**  
   * **Add/Change Groups:** Use the "Select Group Column(s)" dropdown to add or change groups. The plot will update automatically.  
   * **Filter Data:** Use the "Data Filtering" section to subset your data. The plot will update automatically as you adjust sliders or check boxes.  
5. **View Results:**  
   * The **"Kaplan-Meier Plot"** tab will show your interactive survival curve(s), risk table, and censor plot.  
   * The **"Model Summary"** tab will show the detailed output from the survival::survfit function, which also updates with your changes.

## **Input Data Format**

Your CSV file must contain, at a minimum, a numeric time column and a numeric/logical status column. Additional columns for grouping and filtering are optional. Column headers are required.

**Example (complex\_survival\_data.csv):**

Survival\_Time,Event\_Status,Treatment\_Group,Sex,Age,Stage  
12,1,Group A,Male,65,Stage II  
24,0,Group A,Female,58,Stage I  
8,1,Group A,Male,71,Stage III  
...  
15,1,Group B,Male,55,Stage II  
44,1,Group B,Female,62,Stage I  
...

* Survival\_Time: Numeric time to event.  
* Event\_Status: Numeric status, where 1 \= event occurred and 0 \= censored.  
* Treatment\_Group: A character/factor column (can be used for grouping).  
* Sex: A second character/factor column (can be used for grouping).  
* Age: A numeric column (can be used for a numeric filter).  
* Stage: A character/factor column (can of be used for grouping or a categorical filter).