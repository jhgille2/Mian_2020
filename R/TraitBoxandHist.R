


TraitBoxplot <- function(SplitDataset){
  
  PlotData <- SplitDataset %>%
    select(Test, Genotype, oil_dry_basis, protein_dry_basis, oil_outlier, protein_outlier) %>%
    rename(Oil     = oil_dry_basis, 
           Protein = protein_dry_basis) %>%
    mutate(oil_outlier     = ifelse(oil_outlier == 1, Genotype, NA),
           protein_outlier = ifelse(protein_outlier == 1, Genotype, NA))
  
  Plot_protein_box <- ggplot(PlotData, aes(y = Protein, x =)) + 
    theme_bw() + 
    geom_boxplot(fill = "red") + 
    ylab("Protein content (percent dry basis)") + 
    xlab("Percent composition (dry basis)") + 
    ggtitle("Protein") + 
    theme(axis.text.x = element_blank())+ 
    ggrepel::geom_label_repel(data=. %>% dplyr::filter(!is.na(protein_outlier)), aes(label = protein_outlier), position = position_dodge(0.8))
  
  Plot_oil_box <- ggplot(PlotData, aes(y = Oil, x = factor("Oil"))) + 
    theme_bw() + 
    geom_boxplot(fill = "dodgerblue") + 
    ylab("Oil content (percent dry basis)") + 
    xlab("Percent composition (dry basis)") + 
    ggtitle("Oil") + 
    theme(axis.text.x = element_blank()) + 
    ggrepel::geom_label_repel(data=. %>% dplyr::filter(!is.na(oil_outlier)), aes(label = oil_outlier), position = position_dodge(0.8))
  
  Plot_protein_hist <- ggplot(PlotData, aes(x = Protein)) + 
    theme_bw() + 
    geom_histogram(colour = 'light gray', fill = "red", bins = 10) + 
    ylab("Count") + 
    xlab("Percent composition (dry basis)") + 
    ggtitle("Protein")
  
  Plot_oil_hist <- ggplot(PlotData, aes(x = Oil)) + 
    theme_bw() + 
    geom_histogram(colour = 'light gray', fill = "dodgerblue", bins = 10) + 
    ylab("Count") + 
    xlab("Percent composition (dry basis)") + 
    ggtitle("Oil")
  
  plot_grid(ncol = 2, 
            nrow = 2, 
            plotlist = list(Plot_oil_hist, Plot_oil_box, Plot_protein_hist, Plot_protein_box),
            scale = 0.9)
  
}
