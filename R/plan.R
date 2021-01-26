
# The input NIR files
Mian_NIR <- list.files(paste0(here(), "/Data/NIR_Exports"), full.names = TRUE) 
Mian_NIR <- Mian_NIR[which(file_ext(Mian_NIR) == "xlsx")]

the_plan <-
  drake_plan(

    ## Plan targets in here.
    NIR_Cleaned = clean_Mian_NIR(Mian_NIR), 
    
    
    # Check for outliers within each test
    Outliers_Checked = check_outliers(NIR_Cleaned),
    
    # Create a worknook from the NIR data with conditional formatting for the
    # different potential outliers
    OutputWorkbook = make_workbook(Outliers_Checked),
    
    # Create names for output distribution plots
    PlotNames = make_plot_names(Outliers_Checked),
    #PlotPaths = file_out(!!PlotNames),
    
    ExportWorkbook = saveWorkbook(OutputWorkbook, 
                                  file = file_out(!!paste0(here(), "/Exports/AllTables.xlsx")), 
                                  overwrite = TRUE)
    
    # General checks 
    # 1. What tests are in the data
    #     - How many entries are in each test
    #
    
    # Things to check (per test)
    # 1. Counts of entries in each test (are any measurements missing)
    # 2. Find potential outliers.
    #     - Present tabular and numeric summaries

)
