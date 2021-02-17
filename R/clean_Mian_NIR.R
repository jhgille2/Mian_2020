##' Read in the files exported from the perten NIR machine and clean the data
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Mian_NIR
clean_Mian_NIR <- function(Mian_NIR, takeLatestMeasurement = TRUE) {
  
  # Read in each of the files in to a dataframe and store in a list
  NIRTables <- lapply(Mian_NIR, read_excel)
  
  # Define a function that can be applied to each list element to 
  # clean the data from the NIR export
  NIR_Table_Clean <- function(NIRTable){
    
    # The row where measurements start 
    MeasureRow <- which(NIRTable[, 1] == "Sample ID") + 1
    
    # The row where the header is kept
    HeaderRow <- NIRTable[which(NIRTable[, 1] == "Product:"), ]
    
    # Pull out just measurements
    Measurements <- NIRTable[MeasureRow:(nrow(NIRTable) - 1), ]
    
    # Bind the header to the measurements and then use the first row (the header) as column names.
    AllData <- bind_rows(HeaderRow, Measurements) %>% 
      row_to_names(1) %>%
      clean_names() %>%
      # Rename columns then only keep the code, date, protein, oil, and moisture measurements
      rename(NIR_Code = product,
             Date     = x2019_a_whole_small) %>%
      select(NIR_Code, 
             Date,
             moisture,
             protein_dry_basis,
             oil_dry_basis) %>%
      # Split the NIR code into its components
      separate(NIR_Code,
               into = c("Year", "Loc", "Test", "Genotype", "Code", "Plot", "Rep", "NIR_Number"),
               sep = "_") %>%
      # Convert the date column from an excel date number to an R date object
      mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
      mutate(Genotype = str_trim(Genotype)) %>%
      dplyr::select(Date, 
                     Test, 
                     Year, 
                     Loc, 
                     Genotype,
                     Code, 
                     Rep, 
                     Plot, 
                     NIR_Number, 
                     moisture, 
                     protein_dry_basis, 
                     oil_dry_basis)
    
    AllData
  }
  
  # Apply this function to each NIR export, merge all files, and return the result
  Cleaned_NIR <- lapply(NIRTables, NIR_Table_Clean)
  
  # Add file paths to each table in case we need to go back and inspect the original files
  for(i in 1:length(Cleaned_NIR)){
    Cleaned_NIR[[i]]$FilePath <- Mian_NIR[[i]]  
  }
  
  # The file stem unique to my system to be replaced in the filepath variable
  FileBase <- "C:/Users/Jay/Desktop/Documents/R/Mian_2020/Data/NIR_Exports/"
  
  # Bind all the dataframes together, conevrt columns to their proper formats,
  # rearrange columns, sort the data, and format the file name column
  AllData <- do.call(bind_rows, Cleaned_NIR) %>% 
    mutate(Year              = as.numeric(Year), 
           Rep               = as.numeric(Rep),
           Code              = as.numeric(Code),
           Plot              = as.numeric(Plot),
           moisture          = as.numeric(moisture),
           protein_dry_basis = as.numeric(protein_dry_basis),
           oil_dry_basis     = as.numeric(oil_dry_basis),
           protein_plus_oil  = protein_dry_basis + oil_dry_basis,
           Pro13             = round(protein_dry_basis * 0.87, 2),
           Oil13             = round(oil_dry_basis * 0.87, 2),
           PO13              = round(protein_plus_oil * 0.87, 2),
           Loc               = toupper(Loc),
           Test              = str_replace(Test, "LU 5E", "LU 5 Early"))  %>%
    dplyr::select(FilePath,
                  Date, 
                  Test, 
                  Year, 
                  Loc, 
                  Genotype,
                  Code, 
                  Rep, 
                  Plot, 
                  NIR_Number, 
                  Pro13,
                  Oil13,
                  PO13, 
                  protein_dry_basis, 
                  oil_dry_basis,
                  protein_plus_oil,
                  moisture) %>%
    arrange(Test, Loc, Code, Rep) %>%
    mutate(FilePath = str_replace(FilePath, FileBase, ""))
  
  # Exclude any rows where the test is missing and return the final data
  AllData <- distinct(AllData[!(is.na(AllData$Test)), ])
  
  # Some NIR samples were measured twice. A simple way of eliminating duplicated measurements is to 
  # just take the most recent sample.
  if(takeLatestMeasurement){
    AllData %>%
      group_by(NIR_Number) %>%
      top_n(1, Date) %>%
      ungroup() %>%
      group_by(NIR_Number) %>%
      sample_n(1) %>%
      ungroup()
  }else{
    AllData
  }
}
