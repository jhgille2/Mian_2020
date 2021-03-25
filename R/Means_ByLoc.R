##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
##' @param pheno
Means_ByLoc <- function(data = df, pheno = MeasureVars){
  
  # A function to fit a linear model within each location once the data has been grouped by 
  # the location and the phenotype
  Model_ByLoc <- function(LocationData){
    
    tryCatch(
      {
        Model <- with(LocationData, lm(value ~ Genotype + Rep))
        
        Model %>%
          emmeans("Genotype") %>%
          as.data.frame() %>%
          dplyr::select(Genotype, emmean) %>%
          mutate(emmean = round(emmean, 1)) %>%
          rename(LSMean = emmean)
      },
      error = function(cnd) {
        LocationData %>% 
          dplyr::select(Genotype) %>%
          group_by(Genotype) %>%
          sample_n(1) %>%
          ungroup() %>%
          mutate(LSMean = NA) -> EmptyData
        
        return(EmptyData)
      }
    )
    
  }
  
  # Apply some formatting to the data,
  # fit the model within each location by nesting on variable and location,
  # extract the lsmeans, 
  # and format the resulting LSmeans table
  data %>% 
    dplyr::select(Genotype, 
                  Loc, 
                  Rep, 
                  one_of(pheno)) %>% 
    mutate_at(pheno, as.numeric) %>%
    reshape2::melt(measure.vars = pheno) %>%
    group_by(variable, Loc) %>%
    nest() %>%
    mutate(LocMeans = map(data, Model_ByLoc)) %>%
    unnest(., LocMeans) %>%
    dplyr::select(-one_of("data")) %>%
    pivot_wider(names_from = c(Loc, variable), values_from = LSMean, names_sep = "-") -> LSMeanData
  
  # Make a tibble to order the colum names by location first, then by phenotype
  tibble(FullName = colnames(LSMeanData)[2:length(colnames(LSMeanData))]) %>%
    separate(FullName, into = c("Loc", "Pheno"), sep = "-", remove = FALSE) %>%
    arrange(Loc, Pheno) -> ColOrder
  
  LSMeanData %>%
    dplyr::select(one_of(c("Genotype", ColOrder$FullName)))
}