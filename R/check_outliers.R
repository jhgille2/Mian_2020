##' Find potential outliers for measured traits: protein, oil, and moisture. 
##' This data isn't used anymore for formatting the output but could still
##' be useful for diagnosing potential bugs when applying formatting
##' in the workbook step. Right now, the outliers are identified within
##' the workbook creation step itself so refer to make_workbook to see how it is
##' implemented
##'
##' .. content for \details{} ..
##'
##' @title
##' @param NIR_Cleaned
check_outliers <- function(NIR_Cleaned) {
  
  # A function to define an outlier
  # If mode = "median" count observations if they are greater than 
  # 2 standard deviations from the median. If mode = "IQR" count observations
  # as an outlier if they are more than 
  Outlierfn <- function(values, mode = "IQR"){
    if(mode == "median"){
      ifelse(abs(values - median(values)) > 2*sd(values), 1, 0)
    }else if (mode == "IQR"){
      
      Q1 <- quantile(values, 0.25, na.rm = TRUE)
      Q3 <- quantile(values, 0.75, na.rm = TRUE)
      
      IQR <- (Q3 - Q1)
      
      Left  <- Q1 - (1.5*IQR)
      Right <- Q3 + (1.5*IQR)
      
      ifelse(values < Left | values > Right, 1, 0)
    }
  }
  
  # Split the data following test and location so that outliers can be identified within each set. 
  NIR_Cleaned_split <- split(NIR_Cleaned, list(NIR_Cleaned$Test, NIR_Cleaned$Loc))
  
  # Look into nesting/purr for this step. May not need to go through the split/loop 
  # structure that I'm using here and elsewhere. What is important is that outliers
  # are calculated within test/loc combinations but just grouping by these
  # variables gave incorrect results. I'll have to come back and try something 
  # in the future, but this code accomplishes what it needs to do for now. 
  for(i in 1:length(NIR_Cleaned_split)){
    
    NIR_Cleaned_split[[i]] <- NIR_Cleaned_split[[i]] %>% 
      group_by(Test, Loc) %>%
      # Find potential outliers within each test
      mutate(oil_outlier      = Outlierfn(oil_dry_basis),
             protein_outlier  = Outlierfn(protein_dry_basis),
             moisture_outlier = Outlierfn(moisture)) %>%
      ungroup() %>%
      group_by(Test, Loc, Genotype) %>%
      # Calculate variance of genotype measurements within each test
      mutate(oil_var      = round(sd(oil_dry_basis, na.rm = TRUE), 3),
             protein_var  = round(sd(protein_dry_basis, na.rm = TRUE), 3),
             moisture_var = round(sd(moisture, na.rm = TRUE), 3)) %>%
      ungroup()
    
  }

  AllData <- do.call(bind_rows, NIR_Cleaned_split)
  
  # SPlit this data following the Loc/Test
  SplitData <- split(AllData, list(AllData$Test, AllData$Loc))
  
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
  
  # Return the final list
  ResultList
}
