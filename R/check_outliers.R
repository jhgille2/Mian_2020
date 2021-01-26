##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param NIR_Cleaned
check_outliers <- function(NIR_Cleaned) {
  
  # A function to define an outlier
  # If mode = "median" count observations if they are greater than 
  # 2 standard deviations from the median. If mode = "IQR" count observations
  # as an outlier if they are more than 
  Outlierfn <- function(values, mode = "IQR"){
    if(mode == "median"){
      ifelse(abs(values - median(values)) > 2*sd(values), 1, 0)
    }else if (mode == "IQR"){
      
      Q1 <- quantile(values, 0.25, na.rm = TRUE)
      Q3 <- quantile(values, 0.75, na.rm = TRUE)
      
      IQR <- (Q3 - Q1)
      
      Left  <- Q1 - (1.5*IQR)
      Right <- Q3 + (1.5*IQR)
      
      ifelse(values < Left | values > Right, 1, 0)
    }
  }
  
  NIR_Cleaned_split <- split(NIR_Cleaned, list(NIR_Cleaned$Test, NIR_Cleaned$Loc))
  
  for(i in 1:length(NIR_Cleaned_split)){
    NIR_Cleaned_split[[i]] <- NIR_Cleaned_split[[i]] %>% 
      group_by(Test, Loc) %>%
      # Find potential outliers within each test
      mutate(oil_outlier      = Outlierfn(oil_dry_basis),
             protein_outlier  = Outlierfn(protein_dry_basis),
             moisture_outlier = Outlierfn(moisture)) %>%
      ungroup() %>%
      group_by(Test, Loc, Genotype) %>%
      # Calculate variance of genotype measurements within each test
      mutate(oil_var      = round(sd(oil_dry_basis, na.rm = TRUE), 3),
             protein_var  = round(sd(protein_dry_basis, na.rm = TRUE), 3),
             moisture_var = round(sd(moisture, na.rm = TRUE), 3)) %>%
      ungroup()
  }

  do.call(bind_rows, NIR_Cleaned_split)
}
