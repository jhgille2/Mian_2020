##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
##' @param pheno
Means_Overall <- function(data = df, pheno = MeasureVars){
  
  # A function to fit the overall model once data has been grouped by the phenotypes
  Model_Overall <- function(OverallData){
    
    tryCatch(
      {
        Model <- with(OverallData, lmer(value ~ Genotype + (1|Loc/Rep) + (1|Loc:Genotype)))
        
        Model %>%
          emmeans("Genotype") %>%
          as.data.frame() %>%
          dplyr::select(Genotype, emmean) %>%
          mutate(emmean = round(emmean, 1)) %>%
          rename(LSMean = emmean)
      },
      error = function(cnd) {
        OverallData %>% 
          dplyr::select(Genotype) %>%
          group_by(Genotype) %>%
          sample_n(1) %>%
          ungroup() %>%
          mutate(LSMean = NA) -> EmptyData
        
        return(EmptyData)
      }
    )
    
  }
  
  # Apply some formatting, 
  # nest by variable, 
  # fit mixed models, 
  # and extract lsmeans from the models
  data %>% 
    dplyr::select(Genotype, 
                  Loc, 
                  Rep, 
                  one_of(pheno)) %>%
    mutate_at(pheno, as.numeric) %>%
    reshape2::melt(measure.vars = pheno) %>%
    group_by(variable) %>%
    nest() %>% 
    mutate(OverallMeans = map(data, Model_Overall)) %>% 
    unnest(., OverallMeans) %>%
    dplyr::select(-one_of("data")) %>%
    pivot_wider(names_from = c(variable), values_from = LSMean)
  
}
