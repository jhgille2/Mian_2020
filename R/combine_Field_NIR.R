##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param ProcessedFieldData
##' @param Outliers_Checked
combine_Field_NIR <- function(FieldData, Outliers_Checked) {

  # get a single dataframe with all the NIR data
  AllNIR <- do.call(bind_rows, lapply(Outliers_Checked, function(x) do.call(bind_rows, x)))
  AllNIR <- unique(AllNIR)
  
  # The field data for LP 5E - PLY was corrected so that the field data matches the NIR by plot number, 
  # Separate this data out first and then join by plot number
  LP5EPly_FieldData   <- FieldData %>% dplyr::filter(Loc == "PLY", Test == "LP 5E")
  LP5EPly_NIR         <- AllNIR %>%
                              dplyr::filter(Loc == "PLY", Test == "LP 5E") %>%
                              dplyr::select(-one_of(c("Genotype", "Loc", "Test", "Code", "Rep", "Year")))
  
  FieldData <- FieldData %>% dplyr::filter(!(Loc == "PLY" & Test == "LP 5E"))
  
  AllData <- left_join(FieldData, AllNIR, by = c("Genotype", "Loc", "Test", "Code", "Plot", "Rep", "Year"))
  
  LP5EData <- left_join(LP5EPly_FieldData, LP5EPly_NIR, by = "Plot")
  
  AllData <- bind_rows(AllData, LP5EData)
  
  AllData %>%
    dplyr::select(ID,
                  Genotype,
                  Loc,
                  Test, 
                  Year, 
                  Rep, 
                  Code, 
                  Plot, 
                  FC, 
                  PC, 
                  AgScore,
                  MD, 
                  LOD, 
                  HT, 
                  Yield, 
                  SDWT, 
                  SQ,
                  Pro13, 
                  Oil13,
                  PO13,
                  protein_dry_basis, 
                  oil_dry_basis,
                  protein_plus_oil,
                  Note1, 
                  Note2) %>%
    arrange(Test, Loc, Code, Rep) %>%
    mutate(across(everything(), ~replace_na(.x, "."))) %>%
    dplyr::filter(Genotype != "V14-3508 (4L)") -> AllData_reduced
  
  # SPlit this data following the Loc/Test
  SplitData <- split(AllData_reduced, list(AllData_reduced$Test))
  
  # Remove any list elements with zero rows
  SplitData <- SplitData[which(lapply(SplitData, nrow) > 0)]
  
  # This list will be split according to that "test type" the data belongs to
  # like "LP", "LU", "HIF"...
  # This part is a likely source of potential bugs though. I am getting this type from the test names
  # themselves. Specifically, I assume that the test type can be found by taking the part of the test name
  # that comes before the first space in the test name. This will fail if another character is used
  # or if inconsistent test names are used, like if an underscore is used in place of a space in some of
  # the names. For the current data, this does not seem to be an issue, but care should be taken if this
  # code is used for data in the future. 
  SplitNames     <- str_split(names(SplitData), " ") 
  ShortTestNames <- lapply(SplitNames, function(x) x[[1]]) %>% unlist()
  
  UniqueTestNames <- unique(ShortTestNames)
  
  ResultList        <- vector("list", length = length(UniqueTestNames))
  names(ResultList) <- UniqueTestNames
  for(i in 1:length(ResultList)){
    
    # The name of the current test type
    CurrentName <- names(ResultList)[[i]]
    
    # Get the elements of the split data list that belong to the current test type
    # and put in the matching list element
    ResultList[[CurrentName]] <- SplitData[which(ShortTestNames == CurrentName)]
    
  }

  return(list(FullData = AllData, ReducedData = AllData_reduced, ReducedList = ResultList))
}
