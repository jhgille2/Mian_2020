##################################################
## Project: Mian 2020 test processing
## Script purpose: The plan for the data preparation
## workflow for the 2020 Mian data. I have however tried to
## make the steps fairly general in case of future use. 
## Date: 2021-01-26
## Author: Jay Gillenwater
##################################################


# The input NIR files
Mian_NIR <- list.files(paste0(here(), "/Data/NIR_Exports"), full.names = TRUE) 
Mian_NIR <- Mian_NIR[which(file_ext(Mian_NIR) == "xlsx")]

# The input field data
Mian_FieldFiles <- list.files(paste0(here(), "/Data/Field Data"), recursive = TRUE, full.names = TRUE)

# The input lead sheets
Mian_Leadsheets <- list.files(paste0(here(), "/Data/Leadsheets"), recursive = TRUE, full.names = TRUE)

the_plan <-
  drake_plan(
    
    ## Section: NIR Processing
    ##################################################

    # Read in NIR exports and do some data wrangling to 
    # "unwind" the NIR code and combine all files into a single dataframe
    NIR_Cleaned = clean_Mian_NIR(Mian_NIR), 
    
    # Check for outliers within each test and restructure
    # Tthe dataframe into a list of dataframes
    # separated by test type
    Outliers_Checked = check_outliers(NIR_Cleaned),
    
    # Check for possible measurement errors like repeats or unequal reps for each test
    # and keep the same structure as the Outliers_Checked
    Errors_checked = check_errors(Outliers_Checked),
    
    # "Clean up" the error check output to make two dataframes:
    # One for duplicated NIRs, and one for unusual replication numbers
    Errors_processes = clean_errors(Errors_checked),
    
    
    ## Section: Field Data Processing
    ##################################################
    
    # Read in and clean the agronomic notes
    FieldData = read_FieldData(Mian_FieldFiles),
    
    # Read in and clean the lead sheets
    LeadSheets = read_Leadsheets(Mian_Leadsheets),
    
    # Check that the field data read in match what is expected, and then
    # check for poential outliers due to errors in recording, or measurments
    FieldDataValidation = validate_FieldData(FieldData, ExpectedTraitPlotCount = LeadSheets$AllTraits_byPlot),
    
    # Get the field data's structure to one that better matched that of the NIR data
    ProcessedFieldData  = process_FieldData(FieldData),
    
    
    # Merge field data with NIR
    FullData = combine_Field_NIR(ProcessedFieldData, Outliers_Checked),
    
    
    
    ## Section: Export
    ##################################################
    
    # Create a workbook from the NIR data with conditional formatting for the
    # different potential outliers
    OutputWorkbook = make_workbook(Outliers_Checked),
    
    # Save the excel workbooks 
    # Need to look into if it's possible to do this programatically instead of manually defining the 
    # target files. Maybe something with branching?
    ExportWorkbook_HIF = saveWorkbook(OutputWorkbook$HIF,
                                      file = file_out(!!paste0(here(), "/Exports/Mian_2020_HIF.xlsx")),
                                      overwrite = TRUE), 
    
    ExportWorkbook_Jay = saveWorkbook(OutputWorkbook$Jay,
                                      file = file_out(!!paste0(here(), "/Exports/Mian_2020_Jay.xlsx")),
                                      overwrite = TRUE), 
    
    ExportWorkbook_LP = saveWorkbook(OutputWorkbook$LP,
                                      file = file_out(!!paste0(here(), "/Exports/Mian_2020_LP.xlsx")),
                                      overwrite = TRUE), 
    
    ExportWorkbook_LU = saveWorkbook(OutputWorkbook$LU,
                                      file = file_out(!!paste0(here(), "/Exports/Mian_2020_LU.xlsx")),
                                      overwrite = TRUE), 
    
    ExportWorkbook_LO_HP = saveWorkbook(OutputWorkbook$`LO-HP`,
                                      file = file_out(!!paste0(here(), "/Exports/Mian_2020_LO-HP.xlsx")),
                                      overwrite = TRUE),
    
    # Export the error summary workbook as well
    ErrorSummaryWorkbook = saveWorkbook(Errors_processes$SummaryWorkbook,
                                        file = file_out(!!paste0(here(), "/Exports/ErrorSummary.xlsx")),
                                        overwrite = TRUE)
    
    
)

# The dependency graph
vis_drake_graph(the_plan)

# Make the plan
# make(the_plan)
