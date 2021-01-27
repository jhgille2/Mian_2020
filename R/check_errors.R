##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Outliers_Checked
check_errors <- function(Outliers_Checked) {

  # A function that will act on each test. 
  # The purpose is to find both measurement repeats, and 
  # missing measurements
  errorCheck <- function(TestData){
    
    # Count NIR numbers. Each NIR number should only be in the data once
    NirNo_count <- TestData %>% count(NIR_Number) %>% dplyr::filter(n > 1)
    
    DupNIRs <- TestData %>% dplyr::filter(NIR_Number %in% NirNo_count$NIR_Number)
    
    # Count how many entries have how many replicates.
    # Each entry should have the same number of replications
    RepCounts  <- TestData %>% group_by(Genotype, Test, Loc) %>% count() %>% ungroup()
    RepClasses <- TestData %>% group_by(Genotype, Test, Loc) %>% count() %>% ungroup() %>% count(n)
    
      TopClass   <- RepClasses %>% top_n(1, nn)
      OtherClass <- RepClasses %>% dplyr::filter(n != TopClass$n[[1]])
      
    
    UnusualReps <- RepCounts %>% dplyr::filter(n %in% OtherClass$n)
    

    # Return a list of dataframes (DupNIRs and UnusualReps)
    return(list(Duplicated_NIRs = DupNIRs, Unusual_Replications = UnusualReps))
  }
  
  # Initialize a vector to hold checks for all the tests
  ResultList        <- vector('list', length = length(Outliers_Checked))
  names(ResultList) <- names(Outliers_Checked)
  
  for(i in 1:length(Outliers_Checked)){
    
    # Make a list that is the same length as the number of dataframes in the current set of tests
    TestList        <- vector("list", length = length(Outliers_Checked[[i]]))
    names(TestList) <- names(Outliers_Checked[[i]])
    
    # Check for errors in each dataframe in the set of tests
    for(j in 1:length(TestList)){
      TestList[[j]] <- errorCheck(Outliers_Checked[[i]][[j]])
    }
    
    # Put the current list of lists into the result vector
    ResultList[[i]] <- TestList
  }
  
  # Return the result list
  ResultList
}
