##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Mian_FieldFiles
read_FieldData <- function(Mian_FieldFiles) {
  
  ReducedFiles <- Mian_FieldFiles %>% 
    str_replace("~\\$", "") %>% 
    unique() 
  
  # Clean up the file names
  SimpleFileNames <- ReducedFiles %>%
    basename() %>%
    file_path_sans_ext()
  
  # Make a dataframe to lookup tests
  NoteLookup <- data.frame(LongName  = ReducedFiles,
                           ShortName = SimpleFileNames) %>%
    separate(ShortName, 
             into = c("Test", "Loc", "Year", "Yield", "MV"), 
             sep = "_") %>%
    mutate(ConflictedCopy = str_detect(Yield, "conflicted"))
  
  # Read in all the workbooks
  AllWorkbooks <- lapply(NoteLookup$LongName, read_excel)
  
  # A function to perform simple cleaning of workbooks
  WorkbookClean_Initial <- function(Workbook){
    Workbook[Workbook == "."] <- NA
    Workbook %>%
      # Remove empty columns
      dplyr::select(-dplyr::starts_with("..")) %>%
      # Format columns
      mutate(Genotype = as.character(Genotype),
             Loc      = as.character(Loc),
             Test     = as.character(Test),
             Year     = as.numeric(Year),
             Rep      = as.numeric(Rep),
             Code     = as.numeric(Code),
             Plot     = as.numeric(Plot),
             FC       = as.character(FC),
             MD       = as.character(MD),
             PC       = as.character(PC),
             LOD      = as.numeric (LOD),
             HT       = as.numeric(HT),
             Yield    = as.numeric(Yield),
             SDWT     = as.numeric(SDWT))
  }
  
  AllWorkbooks_clean <- lapply(AllWorkbooks, WorkbookClean_Initial)
  
  # Add these workbooks as a dataframe column in the NoteLookup
  NoteLookup$DataCol <- AllWorkbooks_clean
  NoteLookup$nCols   <- sapply(NoteLookup$DataCol, ncol)
  
  # Group data by test and location. Ultimately, there needs to be one
  # set of data for for each test and location, currently a few sites
  # have more than one for each because some sets have the field data
  # and some have the weight. These will have to be identified before 
  # they are processed
  CopyCount <- NoteLookup %>% 
    group_by(Test, Loc) %>%
    count()
  
  # The files with multiple copies. 
  # There are currently two reasons why a file might have more than one copy. 
  #
  # 1. One file has the field notes and another has the weight data
  # 2. There was a file conflict in nextcloud so another copy was made. These 
  #    are easily identified by their value in the "Yield" or "ConflictedCopy" columns
  MultipleCopies <- CopyCount %>%
    dplyr::filter(n > 1) %>%
    left_join(., NoteLookup, by = c("Test", "Loc"))
  
  # A function to merge (technically coalesce) the workbooks
  MergeYield <- function(DupData){
    
    # Check if both dataframes have the same number of columns. 
    # If they dont, coalesce them starting with the dataframe that 
    # has the most columns. This as far as I can tell is always due to
    # columns being added in one dataframe to record seed quality or agronomic score
    if(all(DupData$nCols == DupData$nCols[[1]])){
      MoreCols  <- 1
      FewerCols <- 2
    }else{
      MoreCols  <- which(DupData$nCols == max(DupData$nCols))
      FewerCols <- which(DupData$nCols != max(DupData$nCols))
    }
    
    # Return the coalesced data
    coalesce(DupData$DataCol[[MoreCols]], DupData$DataCol[[FewerCols]])
  }
  
  # Take the data with multiple copies, group it by test and location
  # nest on this grouping, and then coalesce the groups using the 
  # above function.
  #
  # This returns a dataframe for one row for each Test/Loc combination
  # that previously had multiple files. The coalesced dataframe is stored
  # in the "Coalesced_data" column
  Coalesced_copies <- MultipleCopies %>% 
    group_by(Test, Loc) %>%
    nest() %>%
    mutate(FieldData = map(data, MergeYield)) %>%
    select(Test, 
           Loc,
           FieldData) %>%
    unnest()
  
  # The data with only one copy
  AllFieldData <- CopyCount %>%
    dplyr::filter(n == 1) %>%
    left_join(., NoteLookup, by = c("Test", "Loc")) %>%
    dplyr::select(Test, 
                  Loc, 
                  DataCol) %>%
    rename(FieldData = DataCol) %>%
    unnest()
  
  
  AllFieldData
}
