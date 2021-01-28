##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param FieldData
validate_FieldData <- function(FieldData, ExpectedTraitPlotCount) {

  # Count how many measurements for each trait are in the data
   FieldData %>% 
    pivot_longer(cols = c(LOD, HT, Yield, SDWT, AgScore, SQ)) %>%
    group_by(Test, Loc, name) %>%
    summarise(Count_nonNa = sum(!is.na(value))) %>%
    ungroup() %>%
    pivot_wider(id_cols     = c(Test, Loc),
                names_from  = name,
                values_from = Count_nonNa) -> MeasureCounts
    

}
