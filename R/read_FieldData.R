##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Mian_FieldFiles
read_FieldData <- function(Mian_FieldFiles) {

  # Get a unique set of files (File names are repeated if they are currently open)
  ReducedFiles <- Mian_FieldFiles %>% 
    str_replace("~\\$", "") %>% 
    unique() 
  
  # Get the base file name without an extension so that their names can be split to get
  # identifying information
  SimpleFileNames <- ReducedFiles %>%
    basename() %>%
    file_path_sans_ext()
  
  # Make a dataframe to lookup tests based on the file names
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
  
  # Define column types for easier processing later on
  Numeric_Cols   <- c("Year", "Rep", "Code", "Plot", "LOD", "HT", "Yield", "SDWT", "AgScore", "agscore", "SQ")
  Character_Cols <- c("ID", "Genotype", "Loc", "Test", "FC", "MD", "PC", "Note1", "Note 1", "Note 2", "det/indet", "herbicide")
  
  # A function to standardize note dataframes using these column types
  Standardize_Columns <- function(FieldData, NumCols = Numeric_Cols, CharCols = Character_Cols){
    
    # What columns are expected in the data
    cols <- c(Year      = "Year",
              Rep       = "rep",
              Code      = "Code",
              Plot      = "Plot",
              LOD       = "LOD",
              HT        = "HT",
              Yield     = "Yield",
              SDWT      = "SDWT",
              AgScore   = "AgScore",
              ID        = "ID",
              Genotype  = "Genotype",
              Loc       = "Loc",
              Test      = "Test",
              FC        = "FC", 
              MD        = "MD",
              PC        = "PC", 
              Note1     = "Note1", 
              Note2     = "Note 2", 
              det_indet = "det/indet", 
              herbicide = "herbicide",
              SQ        = "SQ") 
    
    # Convert numeric columns to numerics, and character columns to characters. 
    # Rename the note and agscore columns if they are in one of the "alternate"
    # forms
    FieldData %>% 
      rename_at(vars(one_of("Note 1")), ~ 'Note1') %>%
      rename_at(vars(one_of("Note 2")), ~ 'Note2') %>%
      rename_at(vars(one_of("agscore")), ~ 'AgScore') %>% 
      rename_at(vars(one_of("det/indet")), ~ 'det_indet') %>%
      add_column(!!!cols[!names(cols) %in% names(.)]) %>%
      mutate_at(vars(one_of(!!NumCols)), as.numeric) %>%
      mutate_at(vars(one_of(!!CharCols)), as.character)
    
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
    ungroup() %>%
    mutate(FieldData = map(data, MergeYield)) %>%
    mutate(FieldData = map(FieldData, Standardize_Columns)) %>%
    select(FieldData) %>%
    unnest(FieldData) %>% 
    arrange(Test, Loc, Code, Rep) %>%
    mutate(det_indet = str_replace(det_indet, "det/indet", ""),
           Note1     = str_replace(Note1, "Note 1", ""),
           Note2     = str_replace(Note2, "Note 2", ""),
           herbicide = str_replace(herbicide, "herbicide", ""))
  
  # The data with only one copy
  AllFieldData <- CopyCount %>%
    dplyr::filter(n == 1) %>%
    left_join(., NoteLookup, by = c("Test", "Loc")) %>%
    dplyr::select(Test, 
                  Loc, 
                  DataCol) %>%
    ungroup() %>%
    rename(FieldData = DataCol) %>%
    mutate(FieldData = map(FieldData, Standardize_Columns)) %>%
    select(FieldData) %>%
    unnest(FieldData) %>% 
    arrange(Test, Loc, Code, Rep) %>%
    mutate(det_indet = str_replace(det_indet, "det/indet", ""),
           Note1     = str_replace(Note1, "Note 1", ""),
           Note2     = str_replace(Note2, "Note 2", ""),
           herbicide = str_replace(herbicide, "herbicide", ""))
  
  
  return(bind_rows(Coalesced_copies, AllFieldData))
}
