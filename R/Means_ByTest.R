##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param TestData
##' @param Pheno
Means_ByTest <- function(TestData = df, Pheno = MeasureVars){
  
  # The variables in the data that need to be factors
  FactVars <- c("Genotype", 
                "Loc", 
                "Rep")
  
  # Covert appropriate columns to factors and numerics
  TestData %<>% 
    mutate_at(FactVars, as.factor) %>%
    mutate_at(Pheno, as.numeric)
  
  # How many locations are there
  nLocs <- nlevels(TestData$Loc)
  
  # If there is only one location, only get the by-location LSMEans, 
  # otherwise calculate both by-location and overall means and 
  # join the two tables by genotype
  if(nLocs == 1){
    Means_ByLoc(data = TestData, pheno = Pheno)
  }else{
    MeanData_ByLoc   <- Means_ByLoc(data = TestData, pheno = Pheno)
    MeanData_Overall <- Means_Overall(data = TestData, pheno = Pheno)
    
    left_join(MeanData_ByLoc, MeanData_Overall, by = "Genotype")
  }
  
}