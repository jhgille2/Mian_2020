##################################################
## Project: Seed processing
## Script purpose: Sandbox for developing functions to make LSMeans tables from 
## field/NIR dataframes
## Date: 2021-03-24
## Author: Jay Gillenwater
##################################################

MeasureVars <- c("Yield",
                 "SDWT",
                 "SQ",
                 "Pro13",
                 "Oil13",
                 "PO13",
                 "protein_dry_basis",
                 "oil_dry_basis",
                 "protein_plus_oil")

# A function that returns a dataframe of the LSMeans by location
Means_ByLoc <- function(data = df, pheno = MeasureVars){
  
  # A function to fit a linear model within each location once the data has been grouped by 
  # the location and the phenotype
  Model_ByLoc <- function(LocationData){
    Model <- with(LocationData, lm(value ~ Genotype + Rep))
    
    Model %>%
      emmeans("Genotype") %>%
      as.data.frame() %>%
      dplyr::select(Genotype, emmean) %>%
      mutate(emmean = round(emmean, 1)) %>%
      rename(LSMean = emmean)
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
    mutate_at(MeasureVars, as.numeric) %>%
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

test_byloc <- Means_ByLoc(LP_6L)

tibble(FullName = colnames(test_byloc)[2:length(colnames(test_byloc))]) %>%
  separate(FullName, into = c("Loc", "Pheno"), sep = "-", remove = FALSE) %>%
  arrange(Loc, Pheno)


# A function to extract overall LSMeans
Means_Overall <- function(data = df, pheno = MeasureVars){
  
  # A function to fit the overall model once data has been grouped by the phenotypes
  Model_Overall <- function(OverallData){
    
    Model <- with(OverallData, lmer(value ~ Genotype + (1|Loc/Rep) + (1|Loc:Genotype)))
    
    Model %>%
      emmeans("Genotype") %>%
      as.data.frame() %>%
      dplyr::select(Genotype, emmean) %>%
      mutate(emmean = round(emmean, 1)) %>%
      rename(LSMean = emmean)
    
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
    mutate_at(MeasureVars, as.numeric) %>%
    reshape2::melt(measure.vars = pheno) %>%
    group_by(variable) %>%
    nest() %>% 
    mutate(OverallMeans = map(data, Model_Overall)) %>% 
    unnest(., OverallMeans) %>%
    dplyr::select(-one_of("data")) %>%
    pivot_wider(names_from = c(variable), values_from = LSMean)
  
}


# A function that combines the two functions above and returns a single table of LSMeans
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
    Means_ByLoc(data = Testdata, pheno = Pheno)
  }else{
    MeanData_ByLoc   <- Means_ByLoc(data = TestData, pheno = Pheno)
    MeanData_Overall <- Means_Overall(data = TestData, pheno = Pheno)
    
    left_join(MeanData_ByLoc, MeanData_Overall, by = "Genotype")
  }
  
}


