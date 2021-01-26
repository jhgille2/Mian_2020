
# The input NIR files
Mian_NIR <- list.files(paste0(here(), "/Data/NIR_Exports"), full.names = TRUE) 
Mian_NIR <- Mian_NIR[which(file_ext(Mian_NIR) == "xlsx")]

the_plan <-
  drake_plan(

    # Read in NIR exports and do some wrangling to 
    # "unwind" the NIR code and combine all files into a single dataframe
    NIR_Cleaned = clean_Mian_NIR(Mian_NIR), 
    
    # Check for outliers within each test
    Outliers_Checked = check_outliers(NIR_Cleaned),
    
    # Create a workbook from the NIR data with conditional formatting for the
    # different potential outliers
    OutputWorkbook = make_workbook(Outliers_Checked),
    
    ### 
    #  My idea was to make some distribution plots that could be added to the workbook
    #  I think right now they have to be first exported and then imported back into the 
    #  file. I'm not sure how I can work this into the drake workflow right now. 
    #  Things get a bit tricky with the environment locks.
    #
    # # Create names for output distribution plots
    # PlotNames = make_plot_names(Outliers_Checked),
    # PlotPaths = file_out(!!PlotNames),
    ###
    
    # Save the excel workbook
    ExportWorkbook = saveWorkbook(OutputWorkbook, 
                                  file = file_out(!!paste0(here(), "/Exports/AllTables.xlsx")), 
                                  overwrite = TRUE)

)
