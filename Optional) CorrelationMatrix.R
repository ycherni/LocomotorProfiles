library(pheatmap)
setwd('C:/Users/silve/Desktop/DOCTORAT/UNIV MONTREAL/TRAVAUX-THESE/Surfaces_Irregulieres/Datas/Script/gaitAnalysisGUI/result/Statistical_Analysis_LMM/Prepared_Data/R_Clusters_Output/Tests_Multicolinearite')

# Lire le CSV
corr <- read.csv2("Multicolinearity_Highnorm_abs.csv", row.names = 1, check.names = FALSE)

# Convertir en matrice numérique
corr_mat <- as.matrix(corr)
corr_mat <- apply(corr_mat, 2, as.numeric)
rownames(corr_mat) <- rownames(corr)

# Arrondir pour affichage
corr_mat_round <- round(corr_mat, 2)

# Palette adaptée aux valeurs absolues : blanc = faible, rouge = fort
my_colors <- colorRampPalette(c("white", "red"))(100)

# Export TIFF
tiff("correlation_matrix_absolute.tiff",
     width = 10000, height = 5000, units = "px",
     res = 400, compression = "lzw")

pheatmap(corr_mat,
         color = my_colors,
         breaks = seq(0, 1, length.out = 101),
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         display_numbers = corr_mat_round,
         number_color = "black",
         fontsize_number = 15,
         fontsize_row = 12,
         fontsize_col = 12,
         angle_col = 45,
         border_color = "grey90",
         main = "Absolute correlation matrix")

dev.off()