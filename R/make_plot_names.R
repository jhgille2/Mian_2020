##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Outliers_Checked
make_plot_names <- function(Outliers_Checked) {

  # Split the data based on loc and test
  Data_split <- split(Outliers_Checked, list(Outliers_Checked$Test, Outliers_Checked$Loc))

  Data_names <- names(which(lapply(Data_split, nrow) > 0)) %>%
    str_replace("\\.", "_")
  
  paste0(here(), "/Exports/Plots/", Data_names, ".png")
}
