##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Errors_checked
clean_errors <- function(Errors_checked) {

  # A function to pull out just the duplicated NIR 
  # and unusual replication dataframes from a set of tests
  get_errorDFs <- function(TestSet){
    DupNIRs     <- do.call(bind_rows, lapply(TestSet, function(x) x[[1]]))
    UnusualReps <- do.call(bind_rows, lapply(TestSet, function(x) x[[2]]))
    
    return(list(DupNIRs, UnusualReps))
  }
  
  # Get all error DFs
  AllErrorDFs <- lapply(Errors_checked, get_errorDFs)
  
  # All duplicated NIRs and unusual number of replications
  AllDups    <- do.call(bind_rows, lapply(AllErrorDFs, function(x) x[[1]]))
  AllUnusual <- do.call(bind_rows, lapply(AllErrorDFs, function(x) x[[2]]))
  
  
  # Make an excel workbook to store the data
  wb <- createWorkbook()
  
  addWorksheet(wb, "Duplicated NIRs")
  addWorksheet(wb, "Unusual number of replications")
  
  writeDataTable(wb, "Duplicated NIRs", AllDups)
  writeDataTable(wb, "Unusual number of replications", AllUnusual)
  
  # Return a list that holds the dataframes and the excel workbook
  return(list(AllDuplicated = AllDups, AllUnusual = AllUnusual, SummaryWorkbook = wb))
}
