# **Kaplan-Meier Survival Plotter Shiny App**

## **Aim of the App**

This Shiny application provides an interactive, code-free web interface for performing and visualizing survival analysis. Its primary aim is to allow researchers, students, and analysts to quickly upload their own survival data (in .csv format) and generate publication-ready Kaplan-Meier survival plots.

## **Features**

* **Easy Upload:** Upload your survival data as a simple .csv file.  
* **Dynamic Column Mapping:** The app intelligently filters and populates the dropdown menus based on your CSV's column types (e.g., only shows numeric columns for 'Time').  
* **Grouped or Ungrouped Analysis:** You can generate a single survival curve for your entire dataset or compare survival curves between multiple groups.  
* **Rich Visualization:** Creates a ggplot2-based Kaplan-Meier plot using the survminer package, which includes:  
  * The survival curve(s).  
  * Confidence intervals.  
  * A p-value from the log-rank test (for grouped analysis).  
  * A risk table showing the number of subjects at risk over time.  
  * A censor plot.  
* **Input Validation:** Includes built-in data checks to ensure your 'Time' and 'Status' columns are in the correct format, providing user-friendly error messages if they aren't.  
* **Model Summary:** View the raw output from the survfit model in a separate "Model Summary" tab.

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

## **How to Use the App**

1. **Upload Data:** Click the "Choose CSV File" button and select your data file.  
2. **Map Columns:**  
   * **Select Time to Event Column:** Choose the numeric column from your data that represents time (e.g., Survival\_Time, Days\_to\_Event).  
   * **Select Censor/Status Column:** Choose the column representing the event status. This **must** be a numeric column with only 0 (censored) and 1 (event) or a logical TRUE/FALSE column.  
   * **Select Group Column (Optional):** If you want to compare survival between different groups, select a categorical column (e.g., Treatment\_Group, Sex). If you want a single curve for the whole dataset, leave this set to "None".  
3. **Run Analysis:** Click the blue "Run Analysis" button.  
4. **View Results:**  
   * The **"Kaplan-Meier Plot"** tab will show your survival curve(s), risk table, and censor plot.  
   * The **"Model Summary"** tab will show the detailed output from the survival::survfit function, including median survival times and confidence intervals.

## **Input Data Format**

Your CSV file must contain, at a minimum, a numeric time column and a numeric/logical status column. A grouping column is optional. Column headers are required.

**Example (sample\_survival\_data.csv):**

Survival\_Time,Event\_Status,Treatment\_Group  
6,1,Group A  
13,1,Group A  
20,0,Group A  
25,1,Group A  
...  
8,1,Group B  
15,1,Group B  
22,1,Group B  
...

* Survival\_Time: Numeric time to event.  
* Event\_Status: Numeric status, where 1 \= event occurred and 0 \= censored.  
* Treatment\_Group: A character or factor column for comparison.
