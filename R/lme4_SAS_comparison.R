##################################################
## Project: Mian 2020 test processing
## Script purpose: Demonstrate the equivalence of models 
## fit with lme4 to SAS PROC MIXED
## Date: 2021-02-21
## Author: Jay Gillenwater
##################################################

# Load the emmeans and lme4 packages for extracting marginal means and fitting
# mixed models, respectively.
library(emmeans)
library(lme4)

# Load the full data (this was exported to the final spreadsheets)
loadd(FullData)

# Get the data for the LU5E test
LU5E <- FullData$ReducedList$LU$`LU 5 Early`

# Replace the .s with NAs
LU5E[LU5E == "."] <- NA
LU5E$Pro13 <- as.numeric(LU5E$Pro13)

# Split into datasets based on location
SplitLoc <- split(LU5E, LU5E$Loc)

# Fit a linear model for each location
ClaModel <- with(SplitLoc$CLA, lm(Pro13 ~ Genotype + Rep))
PlyModel <- with(SplitLoc$PLY, lm(Pro13 ~ Genotype + Rep))

LocModels <- list(ClaModel, PlyModel)

# Extract LSMeans from the models
GetLSMeans <- function(Model){
  Model %>%
    emmeans("Genotype") %>%
    as.data.frame() %>%
    dplyr::select(Genotype, emmean) %>%
    mutate(emmean = round(emmean, 1))
}

# Combine the by location LSMeans into one dataframe
All_LSMEans <- lapply(LocModels, GetLSMeans)
colnames(All_LSMEans[[1]]) <- c("Genotype", "emmean_CLA")
colnames(All_LSMEans[[2]]) <- c("Genotype", "emmean_PLY")

AllMeans <- left_join(All_LSMEans[[1]], All_LSMEans[[2]], by = "Genotype")

# Fit a mixed model with the overall data, random effects for location, Rep within location, and Loc:Genotype interaction
# and fixed effects for genotype 
Overall_mixed <- with(LU5E, lmer(Pro13 ~ Genotype + (1|Loc/Rep) + (1|Loc:Genotype)))

# Read in the data that I've copied from the SAS output
SasData <- read.csv(paste0(here(), "/Data/tests/Lu5EPro13.csv"))

# Merge all the data sets for comparison
emmeans(Overall_mixed, "Genotype") %>%
  as.data.frame() %>%
  dplyr::select(Genotype, emmean) %>%
  dplyr::rename(emmean_Overall = emmean) %>%
  left_join(AllMeans, by = "Genotype") %>%
  left_join(SasData, by = "Genotype") %>%
  mutate(emmean_Overall = round(emmean_Overall, 1)) %>%
  dplyr::select(Genotype, 
                emmean_Overall, Pro13_Overall, 
                emmean_CLA, Pro13_Cla, 
                emmean_PLY, Pro13_Ply)
