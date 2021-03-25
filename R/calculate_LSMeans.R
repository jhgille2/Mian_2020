##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param FullData
##' @param MeasureVars
calculate_LSMeans <- function(Data = FullData, MeasureVars = Phenos) {

  # Get just the reduced data from the full data list
  Reduced <- Data$ReducedData
  
  # Split by test
  SplitData <- split(Reduced, Reduced$Test)
  
  # Calculate LSMeans for each test
  map(SplitData, Means_ByTest, Pheno = MeasureVars)
  
#   Results <- vector("list", length = length(SplitData))
#   names(Results) <- names(SplitData)
#   
# for(i in seq_along(SplitData)){
#   Results[[i]] <- Means_ByTest(SplitData[[i]], MeasureVars)
# }

}
