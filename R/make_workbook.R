##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Outliers_Checked
make_workbook <- function(Outliers_Checked) {

  # Split the data based on test and location
  SplitData <- split(Outliers_Checked, list(Outliers_Checked$Test, Outliers_Checked$Loc))
  
  # A function to make a boxplot of oil and protein content with outliers labeled
  
  
  # Make the excel workbook
  wb <- createWorkbook()
  
  ############################################
  # Define styles for different conditions that can be met
  
  # Style for identifying outliers
  outlierStyle <- createStyle(bgFill = "#B22223")
  
  # General style for the page
  GeneralStyle <- createStyle(halign = "center", valign = "center")
  
  # Format for the date column
  DateStyle <- createStyle(numFmt = "DATE")
  
  # Apply conditional formatting to a column
  # Moisture = 11, Protein = 12, Oil = 13
  OutlierFormat <- function(Dataset, SheetName, workbook, Style = outlierStyle, columnIndex = 11){
    
    # A function that returns criteria expression to define outliers
    Outlierfn <- function(values){
      Q1 <- quantile(values, 0.25, na.rm = TRUE)
      Q3 <- quantile(values, 0.75, na.rm = TRUE)
      
      IQR <- (Q3 - Q1)
      
      Left  <- Q1 - (1.5*IQR)
      Right <- Q3 + (1.5*IQR)
      
      ExpressionLeft  <- paste0("<", Left)
      ExpressionRight <- paste0(">", Right) 
      
      return(list(Left = ExpressionLeft, Right = ExpressionRight))
    }
    
    OutlierList <- Outlierfn(Dataset[, columnIndex])
    
    conditionalFormatting(wb    = workbook, 
                          sheet = SheetName, 
                          rows  = 2:(nrow(Dataset) + 1), 
                          cols  = columnIndex, 
                          rule  = OutlierList[[1]], 
                          style = Style)
    
    conditionalFormatting(wb    = workbook, 
                          sheet = SheetName, 
                          rows  = 2:(nrow(Dataset) + 1), 
                          cols  = columnIndex, 
                          rule  = OutlierList[[2]], 
                          style = Style)
    
  }
  
  # Make a workbook page for each test/location
  for(i in 1:length(SplitData)){
    
    if(nrow(SplitData[[i]]) == 0){ next }
    
    # Format the test name and location to use it as the tab name in the workbook
    CurrentNames <- str_split(names(SplitData)[[i]], "\\.")[[1]]
    TabName      <- paste(CurrentNames, collapse = " - ")
    
    # Add a worksheet to the workbook using this name
    addWorksheet(wb, sheetName = TabName)
    
    
    
    # Format the moisture column
    OutlierFormat(Dataset     = SplitData[[i]], 
                  SheetName   = TabName,
                  workbook    = wb,
                  columnIndex = 11)
    
    # The protein column
    OutlierFormat(Dataset     = SplitData[[i]], 
                  SheetName   = TabName,
                  workbook    = wb,
                  columnIndex = 12)
    
    # ANd the oil column
    OutlierFormat(Dataset     = SplitData[[i]], 
                  SheetName   = TabName,
                  workbook    = wb,
                  columnIndex = 13)
    
    # Rename columns for readability
    SplitData[[i]] <- SplitData[[i]] %>%
      rename(Moisture                = moisture,
             Protein                 = protein_dry_basis, 
             Oil                     = oil_dry_basis, 
             `Protein + Oil`         = protein_plus_oil,
             # `Oil Outlier`           = oil_outlier, 
             # `Protein Outlier`       = protein_outlier,
             # `Moisture Outlier`      = moisture_outlier,
             `Standard deviation of Oil`      = oil_var,
             `Standard deviation of Protein`  = protein_var,
             `Standard deviation of Moisture` = moisture_var) %>%
      dplyr::select(-one_of(c("oil_outlier", "protein_outlier", "moisture_outlier")))
    
    # Write the table to the worksheet
    writeDataTable(wb, TabName, SplitData[[i]])
    
    # Add the general style to the data table
    addStyle(wb,
             sheet      = TabName,
             style      = GeneralStyle,
             gridExpand = TRUE,
             cols       = 1:ncol(SplitData[[i]]),
             rows       = 1:(nrow(SplitData[[i]]) + 1),
             stack      = TRUE)


  }
  
  # Return the workbook
  wb
}
