AllDat <- readd(FullData)


LP_6L <- AllDat$ReducedList$LP$`LP 6L`

MeasureVars <- c("Yield",
                 "SDWT",
                 "SQ",
                 "Pro13",
                 "Oil13",
                 "PO13",
                 "protein_dry_basis",
                 "oil_dry_basis",
                 "protein_plus_oil")

FactVars <- c("Genotype", 
              "Loc", 
              "Rep")

LP_6L[LP_6L == "."] <- NA

Model_ByLoc <- function(LocationData){
  Model <- with(LocationData, lm(value ~ Genotype + Rep))
  
  Model %>%
    emmeans("Genotype") %>%
    as.data.frame() %>%
    dplyr::select(Genotype, emmean) %>%
    mutate(emmean = round(emmean, 1)) %>%
    rename(LSMean = emmean)
}

Model_Overall <- function(OverallData){
  
  Model <- with(OverallData, lmer(value ~ Genotype + (1|Loc/Rep) + (1|Loc:Genotype)))
  
  Model %>%
    emmeans("Genotype") %>%
    as.data.frame() %>%
    dplyr::select(Genotype, emmean) %>%
    mutate(emmean = round(emmean, 1)) %>%
    rename(LSMean = emmean)
  
}

LP_6L %>%
  dplyr::select(Genotype, 
                Loc, 
                Test, 
                Year, 
                Rep, 
                Code, 
                Plot, 
                FC, 
                Yield,
                SDWT,
                SQ,
                Pro13,
                Oil13,
                PO13,
                protein_dry_basis,
                oil_dry_basis,
                protein_plus_oil) %>%
  mutate_at(MeasureVars, as.numeric) %>%
  mutate_at(FactVars, as.factor) %>%
  reshape2::melt(measure.vars = MeasureVars) %>%
  dplyr::filter(variable %in% c("Yield", "Pro13", "Oil13", "PO13")) %>%
  group_by(variable) %>%
  nest() %>%
  mutate(OverallMeans = map(data, Model_Overall)) %>%
  {. ->> OverallMeanData} %>%
  unnest(., data) %>%
  group_by(variable, Loc) %>%
  nest() %>%
  mutate(LocMeans = map(data, Model_ByLoc)) %>%
  unnest(., LocMeans) %>%
  dplyr::select(-one_of("data")) %>%
  pivot_wider(names_from = c(Loc, variable), values_from = LSMean) -> LocationMeanData


OverallMeanData %<>% unnest(., OverallMeans) %>%
  dplyr::select(-one_of("data")) %>%
  pivot_wider(names_from = c(variable), values_from = LSMean)

AllData <- left_join(LocationMeanData, OverallMeanData)


wb <- createWorkbook()
addWorksheet(wb, sheetName = "MeanData")

writeData(wb, "MeanData", AllData)


saveWorkbook(wb, paste0(paste0(here(), "/Exports/LP6L.xlsx")), overwrite = TRUE)
