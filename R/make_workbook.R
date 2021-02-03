##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Outliers_Checked
make_workbook <- function(FullData_reduced) {
  
  WorkbookList        <- vector('list', length = length(FullData_reduced))
  names(WorkbookList) <- names(FullData_reduced)
  
  for(j in 1:length(WorkbookList)){

    # Get the current split data
    SplitData <- FullData_reduced[[j]]
    
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
        
        # Calculate the IQR
        IQR <- (Q3 - Q1)
        
        Left  <- Q1 - (1.5*IQR)
        Right <- Q3 + (1.5*IQR)
        
        # Expressions to be passed to excel at what qualifies an outlier (Less than 1.5*IQR or greater than 1.5*IQR)
        ExpressionLeft  <- paste0("<", Left)
        ExpressionRight <- paste0(">", Right) 
        
        # Store these expressions in a list
        return(list(Left = ExpressionLeft, Right = ExpressionRight))
      }
      
      # Get the outlier expressions for a given variable
      OutlierList <- Outlierfn(Dataset[, columnIndex])
      
      # Apply "less than" outlier formatting to the column
      conditionalFormatting(wb    = workbook, 
                            sheet = SheetName, 
                            rows  = 2:(nrow(Dataset) + 1), 
                            cols  = columnIndex, 
                            rule  = OutlierList[[1]], 
                            style = Style)
      
      # Apply "greater than" outlier formatting to the column
      conditionalFormatting(wb    = workbook, 
                            sheet = SheetName, 
                            rows  = 2:(nrow(Dataset) + 1), 
                            cols  = columnIndex, 
                            rule  = OutlierList[[2]], 
                            style = Style)
      
    }
    
    # Make a workbook page for each test/location
    for(i in 1:length(SplitData)){
      
      # Not all test/location combinations exist, move to the next list element if no data for a combination exists
      if(nrow(SplitData[[i]]) == 0){ next }
      
      # Format the test name and location to use it as the tab name in the workbook
      # CurrentNames <- str_split(names(SplitData)[[i]], "\\.")[[1]]
      # TabName      <- paste(CurrentNames, collapse = " - ")
      # 

      TabName <- names(SplitData)[[i]]
      
      # Add a worksheet to the workbook using this name
      addWorksheet(wb, sheetName = TabName)
      
      # # Format the moisture column
      # OutlierFormat(Dataset     = SplitData[[i]], 
      #               SheetName   = TabName,
      #               workbook    = wb,
      #               columnIndex = 11)
      # 
      # # The protein column
      # OutlierFormat(Dataset     = SplitData[[i]], 
      #               SheetName   = TabName,
      #               workbook    = wb,
      #               columnIndex = 12)
      # 
      # # ANd the oil column
      # OutlierFormat(Dataset     = SplitData[[i]], 
      #               SheetName   = TabName,
      #               workbook    = wb,
      #               columnIndex = 13)

      # Rename columns for readability and remove the outlier indicator columns
      # In previous versions of the script, I used these columns to apply formatting
      # to the trait columns but elected to calculate the statistics for the outliers
      # and apply formatting directly using the three functions above
      SplitData[[i]] <- SplitData[[i]] %>%
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
      
      # Write the table to the worksheet
      writeData(wb, TabName, SplitData[[i]])
      
      # Add the general style to the whole worksheet
      addStyle(wb,
               sheet      = TabName,
               style      = GeneralStyle,
               gridExpand = TRUE,
               cols       = 1:ncol(SplitData[[i]]),
               rows       = 1:(nrow(SplitData[[i]]) + 1),
               stack      = TRUE)
      
    }
    
    # Add the workbook to the workbook list
    WorkbookList[[j]] <- wb
  }

  # Return the workbook list
  WorkbookList
}
