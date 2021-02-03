
# A function to generate a summary report of a final "SAS Ready" test report

# An example data set
LU_5E <- read_excel("C:\\Users\\Jay\\Desktop\\Documents\\R\\Mian_2020\\Exports\\Mian_2020_LU.xlsx", sheet = "LU 5 Early")


Test_Report <- function(TestData){
  # The columns to convert to numerics
  NumericCols <- c('Code', 
                   'Rep', 
                   'Plot', 
                   'MD', 
                   'HT', 
                   'Yieldg', 
                   'SDWT', 
                   'SQ', 
                   'Pro 13%M', 
                   'Oil 13%M', 
                   'PO13', 
                   'Pro', 
                   'Oil', 
                   'PO')
  
  MeasureVars <- NumericCols[4:11]
  
  TestData %<>% 
    mutate(across(everything(), ~na_if(.x, "."))) %>%
    mutate_at(vars(one_of(NumericCols)), as.numeric)
  
  TestData %>%
    dplyr::select(Loc, Genotype, Rep, !!MeasureVars) %>%
    pivot_longer(cols = MeasureVars) -> PlotData
  
  PlotData
}

ggplot(testLong, aes(y = value, fill = Loc)) + geom_boxplot() + facet_wrap(.~name, scales = "free_y")
