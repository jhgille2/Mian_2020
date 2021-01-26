##' Read in the files exported from the perten NIR machine and clean the data
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Mian_NIR
clean_Mian_NIR <- function(Mian_NIR) {
  
  # Read in each of the files to a dataframe
  NIRTables <- lapply(Mian_NIR, read_excel)
  
  # "NIR files" is a list of the NIR exports. 
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
      # Rename columns then only keep the code, date, protein and oil measurements
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
      # Convert the date column to a proper date
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
  
  # Add file paths to each table (for checking)
  for(i in 1:length(Cleaned_NIR)){
    Cleaned_NIR[[i]]$FilePath <- Mian_NIR[[i]]  
  }
  
  # The fielstem to be replaced in the filepath variable
  FileBase <- "C:/Users/Jay/Desktop/Documents/R/Mian_2020/Data/NIR_Exports/"
  
  AllData <- do.call(bind_rows, Cleaned_NIR) %>% 
    mutate(Year              = as.numeric(Year), 
           Rep               = as.numeric(Rep),
           Code              = as.numeric(Code),
           Plot              = as.numeric(Plot),
           moisture          = as.numeric(moisture),
           protein_dry_basis = as.numeric(protein_dry_basis),
           oil_dry_basis     = as.numeric(oil_dry_basis),
           protein_plus_oil  = protein_dry_basis + oil_dry_basis,
           Loc               = toupper(Loc))  %>%
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
                  moisture, 
                  protein_dry_basis, 
                  oil_dry_basis,
                  protein_plus_oil) %>%
    arrange(Test, Loc, Code, Rep) %>%
    mutate(FilePath = str_replace(FilePath, FileBase, ""))
  
  AllData[!(is.na(AllData$Test)), ]

}
