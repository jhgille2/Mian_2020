
FullData <- readd(FullData)
Reduced <- FullData$ReducedData


Reduced %<>%
  dplyr::select(Test, 
                Year, 
                Loc,
                Genotype, 
                Code, 
                Rep,
                Plot, 
                MD, 
                HT, 
                Yield, 
                SDWT, 
                SQ, 
                Pro13,
                Oil13,
                PO13, 
                protein_dry_basis, 
                oil_dry_basis, 
                protein_plus_oil,
                FC,
                PC,
                Note1) %>%
  rename(Pro        = protein_dry_basis, 
         Oil        = oil_dry_basis, 
         PO         = protein_plus_oil,
         Yieldg     = Yield, 
         `Pro 13%M` = Pro13,
         `Oil 13%M` = Oil13) %>%
  arrange(as.numeric(Code), Loc, Rep)

SplitData <- split(Reduced, Reduced$Test)

for(i in seq_along(SplitData)){
  
  wb <- createWorkbook()
  
  TestName <- names(SplitData)[[i]]
  
  addWorksheet(wb, TestName)
  writeData(wb, TestName, SplitData[[i]])
  
  saveWorkbook(wb, paste0(here(), "/Exports/IndividualTests/", TestName, ".xlsx"), overwrite = TRUE)
}
