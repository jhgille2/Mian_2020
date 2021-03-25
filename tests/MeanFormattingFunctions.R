
# All LSMeans
Means <- readd(LSMeanData)

# LSMeans for tests with multiple, and one location(s)
MultLocs <- Means[[1]]
OneLoc   <- Means[[3]]

str_replace(colnames(MultLocs),"[^-]+", "")

str_match(colnames(MultLocs), "[^-]+")[str_detect(colnames(MultLocs), "-")]
str_detect(colnames(MultLocs), "-")

# A function to find location-level mean columns
TraitLookup <- read_csv(paste0(here(),"/Data/HelperFiles/TraitLookup.csv"))
FormatMeans <- function(MeanData, TraitConversion = TraitLookup){

    # Text that comes before a "-" character in the column names
    LocPrefixes <- str_match(colnames(MeanData), "[^-]+")
    
    UniqueLocs <- LocPrefixes[str_detect(colnames(MeanData), "-")] %>%
      unique()
    
    # A function that takes the a character and finds its minimum and maximum position in a vector
    ValRange <- function(Loc = "CAS", FullVec = LocPrefixes){
      range(which(FullVec == Loc))
    }
    
    LocRanges <- map(UniqueLocs, ValRange, LocPrefixes)
    names(LocRanges) <- UniqueLocs
    
    FullRange <- range(unlist(LocRanges))
    FullRange <- c((FullRange[[1]]):(FullRange[[2]]))
    
    ByLocCols <- colnames(MeanData)[FullRange]
    
    ByLocCols %>%
      str_remove(., "[^-]+") %>%
      str_remove(., "-") -> ByLoc_Cleaned
    
    NewColNames <- colnames(MeanData)
    NewColNames[FullRange] <- ByLoc_Cleaned
    
    NewMeanData <- as.data.frame(MeanData)
    colnames(NewMeanData) <- NewColNames
    colnames(NewMeanData) <- TraitConversion$NewName[match(names(NewMeanData), TraitConversion$OldName)]
    
    return(list("LocationRanges" = LocRanges, "NewMeanData" = NewMeanData))
}

TestData_Mult <- FormatMeans(MultLocs)
TestData_One  <- FormatMeans(OneLoc)

FormattedMeans <- map(Means, FormatMeans)

MakeMeanWorkbooks <- function(MeanData){
  
  FormattedMeans <- map(MeanData, FormatMeans)
}


testwb <- createWorkbook()

MainTableStyle <- createStyle(halign      = "center",
                              borderStyle = "thin",
                              fontName    = "calibri",
                              fontSize    = 11)

HeaderStyle <- createStyle(halign         = "center", 
                           fontSize       = 11,
                           textDecoration = "bold",
                           borderStyle    = "thick",
                           border = "TopBottomLeftRight")

addWorksheet(testwb, "testsheet")

writeData(testwb, "testsheet", "LSMEANS by LOCATION", startCol = 2, startRow = 1)
mergeCells(testwb, "testsheet", cols = 2:19, rows = 1)

writeData(testwb, "testsheet", "Overall LSMEANS", startCol = 20, startRow = 2)
mergeCells(testwb, "testsheet", cols = 20:ncol(TestData_Mult$NewMeanData), rows = 2)

writeData(testwb, "testsheet", "CAS", startCol = 2, startRow = 2)
mergeCells(testwb, "testsheet", cols = 2:10, rows = 2)

writeData(testwb, "testsheet", "CLA", startCol = 11, startRow = 2)
mergeCells(testwb, "testsheet", cols = 11:19, rows = 2)

addStyle(testwb, 
         "testsheet", 
         HeaderStyle,
         cols = 2:ncol(TestData_Mult$NewMeanData), 
         rows = 1:2, 
         gridExpand = TRUE, 
         stack = TRUE)


writeData(testwb, "testsheet", TestData_Mult$NewMeanData, startRow = 3, borders = "all", headerStyle = HeaderStyle)
addStyle(testwb, "testsheet", MainTableStyle, cols = 1:ncol(TestData_Mult$NewMeanData), rows = 3:(nrow(TestData_Mult$NewMeanData) + 3), gridExpand = TRUE, stack = TRUE)

width_vec <- map(names(TestData_Mult$NewMeanData), function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE)) 

setColWidths(testwb, 
             "testsheet", 
             cols = 1:ncol(TestData_Mult$NewMeanData), 
             widths = width_vec,
             ignoreMergedCells = TRUE)

saveWorkbook(testwb, file = paste0(here(), "/test.xlsx"), overwrite = TRUE)