##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Mian_Leadsheets
read_Leadsheets <- function(Mian_Leadsheets) {
  
  # Read in each leadsheet
  AllSheets <- lapply(Mian_Leadsheets, function(x) read_excel(x, sheet = 1))

  # A function to handle a single lead sheet file
  Process_leadsheet <- function(Leadsheet){
    
    # Get the test name
    TestName <- as.character(Leadsheet[1, 1])
    
    # Get the locations where the test was grown
    LocIndex <- which(Leadsheet[, 1] == "LOCS:")
    
    # Which column do the location identifiers end in
    LocEnd <- min(which(is.na(as.character(Leadsheet[LocIndex, ])))) - 1
    
    # Get the test locations
    TestLocs <- as.character(Leadsheet[LocIndex, c(2:LocEnd)])
    
    # Extract the first design table
    RepIndex       <- LocIndex + 1
    Table1End      <- RepIndex + 3
    OtherDesignRow <- Table1End + 2
    
    # Pull out the tables that store information about the experiment
    Table1 <- Leadsheet[c(RepIndex:Table1End, OtherDesignRow), 1:2]
    Table2 <- Leadsheet[RepIndex:OtherDesignRow, 3:4]
    
    # Get other data that is relevant to the design (Maturity group)
    DispositonRow <- which(Leadsheet[, 1] == "Disposition:")
    
    Disposition_Special <- Leadsheet[c(DispositonRow, DispositonRow + 3), c(1, 3)]
    
    MGData <- as.character(Leadsheet[1, 9])%>% 
      str_split(., ":")
    
    MGData <- MGData[[1]] %>%
      t() %>% 
      as.data.frame() %>%
      mutate(V2 = str_trim(V2))

    # Make column names of each table the same and then bind them together
    colnames(Table2) <- colnames(Table1) <- colnames(Disposition_Special) <- colnames(MGData) <- c("Component", "Value")
    ExperimentTable  <- bind_rows(Table1, Table2, Disposition_Special, MGData) %>% 
      mutate(Test      = TestName,
             Component = str_replace(Component, "#", "") %>% str_replace(., ":", "")) %>%
      pivot_wider(names_from = Component, values_from = Value) %>%
      clean_names()
    
    # Get the table holding the lines included in the test
    # Starting row of the table
    EntryTableStart <- which(Leadsheet[, 1] == "Code")
    EntryTableEnd   <- which(Leadsheet[, 1] == "Data to be Collected:") - 1
    
    EntryTableEndCol <- Leadsheet[EntryTableStart, ] %>% 
      as.character() %>% 
      gsub("NA", NA, .) %>% 
      is.na() %>% 
      which() %>% 
      min()
    EntryTableEndCol <- EntryTableEndCol - 1
    
    EntryTable <- Leadsheet[EntryTableStart:EntryTableEnd, 1:EntryTableEndCol] %>% 
      row_to_names(1) %>% 
      mutate(Test = TestName)
    
    # Get the table that holds data about what traits to measure, and on which reps
    TraitTable1Start <- grep("TRAIT", as.character(matrix(t(Leadsheet[, 2]))), ignore.case = TRUE) + 1
    TraitTable1End   <- TraitTable1Start + 7
    
    TraitTable1 <- Leadsheet[TraitTable1Start:TraitTable1End, 2:3]
    TraitTable2 <- Leadsheet[TraitTable1Start:TraitTable1End, 4:5]
    
    colnames(TraitTable1) <- colnames(TraitTable2) <- c("Trait", "Value")
    TraitTable <- bind_rows(TraitTable1, TraitTable2) %>%
      mutate(Test = TestName) %>%
      pivot_wider(names_from = Trait, values_from = Value) %>%
      clean_names()
    
    
    return(list(ExperimentDetails = ExperimentTable, 
                EntryDetails      = EntryTable,
                TraitDetails      = TraitTable))
  }
  
  # Apply the processing function to each leadsheet
  Processed_sheets <- lapply(AllSheets, Process_leadsheet)
  
  # Merge the tables from each leadsheet
  AllExperiments <- lapply(Processed_sheets, function(x) x[[1]]) %>% do.call(bind_rows, .)
  AllEntries     <- lapply(Processed_sheets, function(x) x[[2]]) %>% do.call(bind_rows, .)
  AllTraits      <- lapply(Processed_sheets, function(x) x[[3]]) %>% do.call(bind_rows, .)
  
  
  # Add a total plot column to the experiment data
  AllExperiments %>% 
    mutate(total_plots = as.numeric(reps) * as.numeric(total_entries)) -> AllExperiments
  
  # Add reps, total entries, and total plots to the trait data
  AllExperiments %>%
    select(test, reps, total_entries, total_plots) %>%
    left_join(AllTraits, ., by = "test") %>%
    mutate(total_plots = as.character(total_plots)) %>%
    mutate_if(is.character, function(x) str_replace(x, " Reps", "")) -> AllTraits

  # A function that replaces all matches of a string with a value stored
  # at the position of the string in another vector and returns a tibble
  replaceDF <- function(inputData = AllTraits, replacephrase = "ALL"){
    
    replace_All <- function(x, inputData = inputData, replacephrase = replacephrase){
      replaceCol <- inputData$reps
      x[grep(replacephrase, x)] <- replaceCol[grep(replacephrase, x)]
      x
    }
    
    inputData <- apply(inputData, 2, replace_All, inputData = inputData, replacephrase = replacephrase)
    
    inputData %>%
      as.data.frame() %>%
      tibble::tibble()
  }
    
  
  # Replace all "ALL" and "YES" with the number of reps in the test
  AllTraits %<>% 
    replaceDF() %>%
    replaceDF(replacephrase = "YES") %>%
    replaceDF(replacephrase = "Yes")
  
  # Replace all "No" with 0
  AllTraits[AllTraits == "No"] <- "0"
  AllTraits[AllTraits == "NO"] <- "0"
  
  # Convert back
  AllTraits %<>% 
    mutate_at(vars(-test), as.numeric) %>%
    rename(FC    = flower_color, 
           HT    = height,
           SQ    = seed_quality,
           SDWT  = x100_seed_wt,
           Yield = yield, 
           MD    = maturity, 
           LOD   = lodging)
  
  # Convert the number of entries expected for each trait to number of plots
  AllTraits_byPlot <- AllTraits %>%
    mutate_at(vars(-test, -reps, -total_entries, -total_plots), ~.x * total_entries)
  
  return(list(ExperimentData   = AllExperiments,
              EntryData        = AllEntries,
              TraitData        = AllTraits,
              TraitData_byPlot = AllTraits_byPlot))
}


