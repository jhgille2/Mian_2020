# Mian 2020 test processing
A [drake](https://github.com/ropensci/drake) workflow for processing the 2020 Mian soybean trials. Potentially could also be used for similar data in the future.

## Main directory files 
*(Just the important ones)*
- \_drake.R: Setup for drake workflow. Run this first. 
- packages.R: The packages used in the workflow

## Data
**This folder holds the workflow input data sets.**
- 2020_Mian_Test Data: Pre-processed agronomic notes which are used in the current workflow.
- Field Data: Old set of agronomic notes that have been replaced in the workflow by the files in 2020_Mian_Test Data.
- Leadsheets: Lead sheets for the 2020 soybean trials. 
- NIR_Exports: Exports from the NIR machine which have the protein and oil measurements. 

## Exports
**This folder holds the workflow output data sets.**
- Mian_2020_[Test category].xlsx: An excel workbook with the merged data for a set of tests belonging to some test category. The workflow has a sheet for each test in the category. 
- ErrorSumamry.xlsx: A record of NIR measurements which do not match the expected number of measurements for a given genotype, eighter too few or too many indicating missing data or repeated measurements. 

## R
**This folder holds the R scripts used in the processing workflow**  
*Check function descriptions for specific details about their purpose*
- plan.R: The main drake plan. 
- Others: Components of the plan


