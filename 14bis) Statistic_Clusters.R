setwd("C:/Users/silve/Desktop/DOCTORAT/UNIV MONTREAL/TRAVAUX-THESE/Surfaces_Irregulieres/Datas/Script/gaitAnalysisGUI/result/Fig/Clustering")

# ---------------------------
# I - Stats entre les clusters (chi², t-test, visualisation Age - Sexe - Surface)
#----------------------------

# 1. Chargement
library(tidyverse)
library(rstatix)
library(ggalluvial)
library(gridExtra)
library(grid)
library(gtable)
library(fmsb)



# 2. Importation
df_clust <- read.csv("DATA_FOR_R_GLOBAL_20260123_1409.csv", sep = ";", check.names = FALSE)
df_meta <- read.csv(file.choose(), sep = ";", check.names = FALSE)
# Aller chercher le fichier "participant.metadonnees" dans le dossier stats LMM - stats descriptives

View(df_meta)
View(df_clust)



# 3. Jointure (Lien entre l'ID et le Sexe/Age)
# On ne garde que Participant, AgeMonths et Sex du fichier meta pour éviter les doublons
df_meta_clean <- df_meta %>%
  select(Participant, AgeMonths, Sex) 

df <- left_join(df_clust, df_meta_clean, by = "Participant")

# -----------------------------------------------------------
# 3.bis AJOUT DE LA VITESSE DE MARCHE BRUTE DEPUIS ACP_Clustering_DATA
# + TEST MANN-WHITNEY ENTRE C1 ET C2
# -----------------------------------------------------------

# 1. Charger le fichier ACP_Clustering_DATA
df_acp <- read.csv(
  "C:/Users/silve/Desktop/DOCTORAT/UNIV MONTREAL/TRAVAUX-THESE/Surfaces_Irregulieres/Datas/Script/gaitAnalysisGUI/result/Statistical_Analysis_LMM/Prepared_Data/ACP_Clustering_DATA.csv",
  sep = ";",
  check.names = FALSE
)

View(df_acp)

# 2. Harmoniser le nom de la colonne de surface pour la jointure
# Ici, on suppose que df contient "Condition" et df_acp contient "Surface"
# et que les valeurs sont déjà identiques (ex. Plat / Medium / High)

df <- df %>%
  mutate(Surface_join = Condition)

df_acp <- df_acp %>%
  mutate(Surface_join = Surface)

# 3. Extraire la vitesse brute
# Vérifie bien que le nom de colonne est exactement celui-ci dans ton fichier
df_speed <- df_acp %>%
  select(Participant, Surface_join, `Mean_Gait speed (m.s^{-1})`) %>%
  rename(GaitSpeed_abs = `Mean_Gait speed (m.s^{-1})`)

# 4. Jointure avec le dataframe principal
df <- df %>%
  left_join(df_speed, by = c("Participant", "Surface_join"))

# Vérification
cat("\nNombre de lignes avec vitesse brute retrouvée :", sum(!is.na(df$GaitSpeed_abs)), "/", nrow(df), "\n")

# 5. Test Mann-Whitney entre clusters sur la vitesse brute
df_speed_test <- df %>%
  select(ClusterID, GaitSpeed_abs) %>%
  filter(!is.na(GaitSpeed_abs))

speed_wilcox <- df_speed_test %>%
  wilcox_test(GaitSpeed_abs ~ ClusterID) %>%
  add_significance("p")

speed_effsize <- df_speed_test %>%
  wilcox_effsize(GaitSpeed_abs ~ ClusterID)

# 6. Résumé console
cat("\n=== MANN-WHITNEY : VITESSE DE MARCHE BRUTE ENTRE CLUSTERS ===\n")
cat("W =", speed_wilcox$statistic, "\n")
cat("p =", speed_wilcox$p, " (", speed_wilcox$p.signif, ")\n")
cat("rrb =", round(speed_effsize$effsize, 3), "(", speed_effsize$magnitude, ")\n")

# 7. Médiane [IQR] par cluster pour la vitesse brute
speed_summary <- df_speed_test %>%
  group_by(ClusterID) %>%
  summarise(
    med = median(GaitSpeed_abs, na.rm = TRUE),
    q25 = quantile(GaitSpeed_abs, 0.25, na.rm = TRUE),
    q75 = quantile(GaitSpeed_abs, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Summary = paste0(round(med, 2), " [", round(q25, 2), " - ", round(q75, 2), "]")
  )

print(speed_summary)


# 3.1 Préparation des variables
# On récupère les colonnes numériques de df_clust pour les boxplots
vars_marche <- colnames(df_clust)[sapply(df_clust, is.numeric)]
vars_marche <- vars_marche[!vars_marche %in% c("ClusterID", "AgeMonths")]

# Df format long pour fig descriptives
df_long <- df %>%
  pivot_longer(cols = all_of(vars_marche), 
               names_to = "Variable", 
               values_to = "Valeur")

# 3.2 Création des Boxplots Individuels avec nettoyage des noms
if(!dir.exists("Boxplots_Individuels")) dir.create("Boxplots_Individuels")

# Vos couleurs manuelles
mes_couleurs <- c("1" = "#3498db", "2" = "#e74c3c")

for (v in vars_marche) {
  
  # --- LOGIQUE DE NETTOYAGE DU NOM ---
  clean_label <- v
  
  # 1. On retire "Mean_"
  clean_label <- gsub("^Mean_", "", clean_label)
  
  # 2. Cas spécifique : Norm Gait Speed -> unité (ua)
  if (grepl("Norm Gait Speed", clean_label, ignore.case = TRUE)) {
    clean_label <- "Norm Gait Speed (ua)"
  }
  
  # 3. On remplace "CV_" par "C.V. " et on force l'unité (%)
  # (Sauf si c'est déjà traité par le cas Norm Gait Speed plus haut)
  if (startsWith(v, "CV_")) {
    clean_label <- gsub("^CV_", "C.V. ", clean_label)
    clean_label <- paste0(gsub(" \\(.*\\)", "", clean_label), " (%)")
  }
  
  # 4. On remplace "SI_" par "S.I. " et on retire TOUTE unité
  if (startsWith(v, "SI_")) {
    clean_label <- gsub("^SI_", "S.I. ", clean_label)
    clean_label <- gsub(" \\(.*\\)", "", clean_label)
  }
  
  # --- CONSTRUCTION DU GRAPHIQUE ---
  p <- ggplot(df, aes(x = factor(ClusterID), y = .data[[v]], fill = factor(ClusterID))) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.1, alpha = 0.4, size = 1) +
    scale_fill_manual(values = mes_couleurs) + 
    labs(title = clean_label, 
         x = "Cluster ID", 
         y = clean_label, 
         fill = "Cluster") + # Note 'caption' retirée ici
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      axis.title.y = element_text(size = 10)
    )
  
  # Sauvegarde
  file_name <- gsub("[^[:alnum:]]", "_", v)
  ggsave(paste0("Boxplots_Individuels/Boxplot_", file_name, ".png"), 
         plot = p, width = 12, height = 10, units = "cm")
}

# --- PANEL GLOBAL : 5 lignes x 3 colonnes ---
library(patchwork)

plots_list <- list()

for (v in vars_marche) {
  
  clean_label <- v
  clean_label <- gsub("^Mean_", "", clean_label)
  
  # Ordre important : cas spécifiques AVANT les règles génériques
  if (grepl("Norm Gait Speed", clean_label, ignore.case = TRUE)) {
    clean_label <- "Norm Gait Speed (au)"
  } else if (grepl("MoS ML HS", clean_label, ignore.case = TRUE)) {
    clean_label <- "MoS ML (%L0)"
  } else if (grepl("MoS AP HS", clean_label, ignore.case = TRUE)) {
    clean_label <- "MoS AP (%L0)"
  } else if (grepl("COM SPARC Magnitude", clean_label, ignore.case = TRUE)) {
    clean_label <- "SPARC (au)"
  } else if (startsWith(v, "CV_")) {
    clean_label <- gsub("^CV_", "C.V. ", clean_label)
    clean_label <- paste0(gsub(" \\(.*\\)", "", clean_label), " (%)")
  } else if (startsWith(v, "SI_")) {
    clean_label <- gsub("^SI_", "S.I. ", clean_label)
    clean_label <- gsub(" \\(.*\\)", "", clean_label)
    clean_label <- paste0(clean_label, " (%)")           
  }
  
  # Remplacement global de "ua" par "au" pour les cas restants
  clean_label <- gsub("\\(ua\\)", "(au)", clean_label)
  
  p <- ggplot(df, aes(x = factor(ClusterID), y = .data[[v]], fill = factor(ClusterID))) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.1, alpha = 0.4, size = 0.8) +
    scale_fill_manual(values = mes_couleurs) +
    labs(title = clean_label, x = NULL, y = NULL, fill = "Cluster") +
    theme_classic(base_size = 8) +
    theme(
      plot.title      = element_text(hjust = 0.5, face = "bold", size = 15),
      axis.text.x     = element_text(size = 15),
      axis.text.y     = element_text(size = 13),
      legend.position = "none"
    )
  
  plots_list[[v]] <- p
}

# --- AJOUT DES DOMAINES À GAUCHE DU PANEL ---

domain_labels <- c(
  "Pace",
  "Rhythm",
  "Dynamic\nstability",
  "Asymmetry",
  "Variability"
)

# Créer les panneaux de domaine
domain_plots <- lapply(domain_labels, function(lab) {
  ggplot() +
    annotate(
      "text",
      x = 0.5, y = 0.5,
      label = lab,
      angle = 90,
      fontface = "bold",
      size = 6,
      hjust = 0.5,
      vjust = 0.5
    ) +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_void() +
    theme(
      plot.margin = margin(2, 2, 2, 2)
    )
})

# S'assurer que les figures sont dans le bon ordre
plots_ordered <- plots_list[vars_marche]

# Construire une liste complète : 1 colonne domaine + 3 variables par ligne
panel_list <- list(
  domain_plots[[1]], plots_ordered[[1]],  plots_ordered[[2]],  plots_ordered[[3]],
  domain_plots[[2]], plots_ordered[[4]],  plots_ordered[[5]],  plots_ordered[[6]],
  domain_plots[[3]], plots_ordered[[7]],  plots_ordered[[8]],  plots_ordered[[9]],
  domain_plots[[4]], plots_ordered[[10]], plots_ordered[[11]], plots_ordered[[12]],
  domain_plots[[5]], plots_ordered[[13]], plots_ordered[[14]], plots_ordered[[15]]
)

# Assemblage final : 4 colonnes, dont la première très étroite
panel_all <- wrap_plots(panel_list, ncol = 4, byrow = TRUE) +
  plot_layout(
    widths = c(0.35, 1, 1, 1),
    heights = rep(1, 5)
  ) &
  theme(legend.position = "none")


ggsave("Panel_All_Variables_Clusters.png",
       plot   = panel_all,
       width  = 27, height = 30, units = "cm",
       dpi    = 300, bg = "white")

ggsave("Panel_All_Variables_Clusters.tif",
       plot   = panel_all,
       width  = 27, height = 30, units = "cm",
       dpi    = 600,
       compression = "lzw")

print("Panel global exporté.")


# 4 - TABLEAU RÉCAPITULATIF (Médiane [IQR]) & TESTS STATISTIQUES
# -----------------------------------------------------------

# 4.1. Sélection automatique des variables
vars_exclude <- c("Participant", "ClusterID", "Condition", "Sex", "AgeMonths", 
                  "AgeGroup", "AgeGroup.x", "AgeGroup.y", "AgeMonths.x", 
                  "Sex_EN", "AgeGroup_EN", "Condition_EN", "Surface_join")

vars_gait <- colnames(df)[!colnames(df) %in% vars_exclude]

# 4.2. Calcul des Médianes et IQR (25ème et 75ème percentiles)
summary_table <- df %>%
  select(ClusterID, any_of("AgeMonths.y"), all_of(vars_gait)) %>%
  group_by(ClusterID) %>%
  summarise(across(everything(), 
                   list(med = ~median(.x, na.rm = TRUE), 
                        q25 = ~quantile(.x, 0.25, na.rm = TRUE),
                        q75 = ~quantile(.x, 0.75, na.rm = TRUE)))) %>%
  pivot_longer(cols = -ClusterID, 
               names_to = c("Variable", ".value"), 
               names_sep = "_(?=[^_]+$)")

# 4.3. Nettoyage et Formatage Médiane [IQR]
summary_formatted <- summary_table %>%
  mutate(
    CleanVar = Variable,
    CleanVar = gsub("^Mean_", "", CleanVar),
    CleanVar = if_else(grepl("Norm Gait Speed", CleanVar, ignore.case = TRUE), "Norm Gait Speed (ua)", CleanVar),
    CleanVar = if_else(grepl("^CV_", CleanVar), paste0(gsub("^CV_", "C.V. ", gsub(" \\(.*\\)", "", CleanVar)), " (%)"), CleanVar),
    CleanVar = if_else(grepl("^SI_", CleanVar), gsub("^SI_", "S.I. ", gsub(" \\(.*\\)", "", CleanVar)), CleanVar),
    # Formatage de la cellule
    Med_IQR = paste0(round(med, 2), " [", round(q25, 2), " - ", round(q75, 2), "]")
  ) %>%
  select(ClusterID, CleanVar, Med_IQR) %>%
  pivot_wider(names_from = ClusterID, values_from = Med_IQR, names_prefix = "Cluster_") %>%
  rename(Variable = CleanVar)


# 4.3.bis TEST DE NORMALITÉ (Shapiro-Wilk)
shapiro_results <- df %>%
  select(ClusterID, all_of(vars_gait)) %>%
  pivot_longer(cols = -ClusterID, names_to = "Variable", values_to = "Value") %>%
  group_by(Variable, ClusterID) %>%
  # On ne teste que si on a assez d'observations (n > 3)
  filter(n() >= 3) %>%
  shapiro_test(Value) %>%
  # Un p < 0.05 signifie que la distribution N'EST PAS normale
  mutate(is_normal = p > 0.05) 

# Visualisation rapide des variables problématiques
non_normal_vars <- shapiro_results %>% filter(is_normal == FALSE)
print(non_normal_vars)

# 4.4 & 4.5. Tests de Wilcoxon 
stats_tests_clean <- df %>%
  select(ClusterID, any_of("AgeMonths.y"), all_of(vars_gait)) %>%
  pivot_longer(cols = -ClusterID, names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  wilcox_test(Value ~ ClusterID)

# Effect size (r de rang biserial) pour chaque variable
effect_sizes <- df %>%
  select(ClusterID, any_of("AgeMonths.y"), all_of(vars_gait)) %>%
  pivot_longer(cols = -ClusterID, names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  wilcox_effsize(Value ~ ClusterID)   # donne r (rang biserial) + magnitude

# SÉCURITÉ : AJOUT DES ÉTOILES SI ELLES MANQUENT ---
# Si le test n'a pas généré p.signif, on le crée manuellement
if (!"p.signif" %in% colnames(stats_tests_clean)) {
  stats_tests_clean <- stats_tests_clean %>% add_significance("p")
}

# NETTOYAGE DES NOMS (IDENTIQUE AU TABLEAU SUMMARY) ---
stats_tests_clean <- stats_tests_clean %>%
  mutate(
    Variable = gsub("^Mean_", "", Variable),
    Variable = if_else(grepl("Norm Gait Speed", Variable, ignore.case = TRUE), "Norm Gait Speed (ua)", Variable),
    Variable = if_else(grepl("^CV_", Variable), paste0(gsub("^CV_", "C.V. ", gsub(" \\(.*\\)", "", Variable)), " (%)"), Variable),
    Variable = if_else(grepl("^SI_", Variable), gsub("^SI_", "S.I. ", gsub(" \\(.*\\)", "", Variable)), Variable),
    Variable = dplyr::recode(Variable, "AgeMonths.y" = "Age (months)")
  )

# NETTOYAGE DES NOMS sur effect_sizes (identique aux autres)
effect_sizes <- effect_sizes %>%
  mutate(
    Variable = gsub("^Mean_", "", Variable),
    Variable = if_else(grepl("Norm Gait Speed", Variable, ignore.case = TRUE), "Norm Gait Speed (ua)", Variable),
    Variable = if_else(grepl("^CV_", Variable), paste0(gsub("^CV_", "C.V. ", gsub(" \\(.*\\)", "", Variable)), " (%)"), Variable),
    Variable = if_else(grepl("^SI_", Variable), gsub("^SI_", "S.I. ", gsub(" \\(.*\\)", "", Variable)), Variable),
    Variable = dplyr::recode(Variable, "AgeMonths.y" = "Age (months)")
  )

# FUSION SÉCURISÉE ---
# On vérifie quelles colonnes existent réellement pour ne pas faire planter select()
# Jointure effect size sur les tests
stats_tests_clean <- left_join(stats_tests_clean, 
                               effect_sizes %>% select(Variable, effsize, magnitude),
                               by = "Variable")

# FUSION SÉCURISÉE ---
cols_disponibles <- intersect(c("Variable", "p", "p.signif", "effsize", "magnitude"), 
                              colnames(stats_tests_clean))

final_stats_table <- left_join(summary_formatted, 
                               stats_tests_clean %>% select(all_of(cols_disponibles)), 
                               by = "Variable")

# 4.6. EXPORT ET MISE EN FORME GRAPHIQUE

# On retire les variables anthropométriques et on formate les p-values
table_to_export <- final_stats_table %>%
  filter(!Variable %in% c("Height_cm", "Weight_kg", "L0_m", "IMC")) %>%
  mutate(p = format.pval(p, digits = 2, eps = 0.001))

# Thème du tableau
tt <- ttheme_default(
  core = list(fg_params=list(cex = 0.7), 
              bg_params=list(fill=c("grey95", "white"), col=NA)),
  colhead = list(fg_params=list(cex = 0.8, fontface="bold"), 
                 bg_params=list(fill="grey80"))
)

# Création du tableau graphique
g_table <- tableGrob(table_to_export, rows = NULL, theme = tt)

# 5) Ajout des annotations en bas du tableau
notes <- textGrob("Values are Median [Interquartile Range]. P-values calculated using Wilcoxon rank-sum test.\nr = rank-biserial correlation (effect size): small ≥ 0.1, medium ≥ 0.3, large ≥ 0.5.\nC.V. = Coefficient of Variation (%); S.I. = Symmetry Index; (ua) = arbitrary units.", 
                  x = 0, hjust = 0, vjust = 1, 
                  gp = gpar(fontsize = 8, fontitalic = TRUE))

# Assemblage : On ajoute une ligne de 2 cm pour la note
final_plot <- gtable_add_rows(g_table, heights = unit(2, "cm"))
final_plot <- gtable_add_grob(final_plot, notes, t = nrow(final_plot), l = 1, r = ncol(final_plot))

# --- CALCUL AUTOMATIQUE DE LA HAUTEUR ---
# On compte le nombre de lignes + entête + la note (environ 0.8 cm par ligne de donnée)
hauteur_calculee <- (nrow(table_to_export) * 0.8) + 4 

# EXPORTATION AVEC GGSAVE (Remplace png() et pdf())
ggsave("Table_Stats_Descriptives.png", 
       plot = final_plot, 
       width = 22, 
       height = hauteur_calculee, 
       units = "cm", 
       dpi = 300, 
       bg = "white")

ggsave("Table_Stats_Descriptives.pdf", 
       plot = final_plot, 
       width = 22, 
       height = hauteur_calculee, 
       units = "cm", 
       bg = "white")

print(paste("Le tableau a été exporté. Hauteur utilisée :", round(hauteur_calculee, 1), "cm"))


# 4.7 TEST MANN-WHITNEY SUR L'ÂGE EN MOIS

# Identifier le bon nom de la colonne âge dans df
age_col <- intersect(c("AgeMonths", "AgeMonths.y", "AgeMonths.x"), colnames(df))[1]
cat("Colonne âge utilisée :", age_col, "\n")

# Créer un df temporaire avec le bon nom
df_age <- df %>%
  select(ClusterID, all_of(age_col)) %>%
  setNames(c("ClusterID", "AgeMonths"))

# Test de Wilcoxon
age_wilcox <- df_age %>%
  wilcox_test(AgeMonths ~ ClusterID) %>%
  add_significance("p")

# Effect size (r rang biserial)
age_effsize <- df_age %>%
  wilcox_effsize(AgeMonths ~ ClusterID)

# Résumé
cat("\n=== MANN-WHITNEY : ÂGE EN MOIS ENTRE CLUSTERS ===\n")
cat("W =", age_wilcox$statistic, "\n")
cat("p =", age_wilcox$p, " (", age_wilcox$p.signif, ")\n")
cat("r =", round(age_effsize$effsize, 3), "(", age_effsize$magnitude, ")\n")


# 5.TEST CHI² ---
tab_sexe <- table(df$Sex, df$ClusterID)
chi2_sexe <- chisq_test(tab_sexe)

tab_age <- table(df$AgeGroup, df$ClusterID)
fisher_age <- fisher_test(tab_age)  # Fisher car effectifs théoriques < 5 (Ado et Adults dans C2)

tab_surface <- table(df$Condition, df$ClusterID)
chi2_surface <- chisq_test(tab_surface)

# 5.1. Vérifier les chiffres réels pour l'Âge
table(df$AgeGroup, df$ClusterID)

# 5.2. Vérifier les chiffres réels pour le Sexe
table(df$Sex, df$ClusterID)

# 5.3. Vérifier les chiffres réels pour la surface de marche
table(df$Condition, df$ClusterID)

print("--- RÉSULTATS CHI² ---")
print(fisher_age)
print(chi2_sexe)
print(chi2_surface)

# 5.4 Exporter les données
# Regrouper les résultats des tests Chi² dans un seul dataframe
# On extrait le nom de la variable, la statistique, le ddl (n) et la p-value
chi2_results <- bind_rows(
  # Fisher pour Age Group (pas de statistic ni df)
  fisher_age %>% mutate(Variable = "Age Group", statistic = NA, df = NA, n = nrow(df)),
  # Chi² pour Sex et Surface
  chi2_sexe    %>% mutate(Variable = "Sex"),
  chi2_surface %>% mutate(Variable = "Surface")
) %>%
  select(Variable, n, statistic, df, p) %>%
  add_significance() %>%
  mutate(p = format.pval(p, digits = 2, eps = 0.001))

# Création de l'objet graphique (Tableau)
# Utilisation du même thème que pour les stats descriptives pour la cohérence
tt_chi2 <- ttheme_default(
  core = list(fg_params=list(cex = 0.9)),
  colhead = list(fg_params=list(cex = 1, fontface="bold"), 
                 bg_params=list(fill="grey80"))
)

g_chi2 <- tableGrob(chi2_results, rows = NULL, theme = tt_chi2)

# EXPORTATION
# Enregistrement en PNG
png("Table_Chi2_Results.png", width = 15, height = 8, units = "cm", res = 300)
grid.draw(g_chi2)
dev.off()

# Enregistrement en PDF
pdf("Table_Chi2_Results.pdf", width = 7, height = 4)
grid.draw(g_chi2)
dev.off()

print("Le tableau des résultats Chi² a été exporté.")


# 6. Observer les migrations des participants entre les 2 clusters
# 6.1. On prépare un tableau large pour comparer Plat vs High
migration_df <- df %>%
  filter(Condition %in% c("Plat", "High")) %>%
  select(Participant, AgeGroup, Condition, ClusterID) %>%
  pivot_wider(names_from = Condition, values_from = ClusterID) %>%
  # On crée une colonne qui indique s'il y a eu migration
  mutate(Migration = if_else(Plat == High, "Stable", "Migrant")) %>%
  filter(!is.na(Migration)) # On enlève ceux qui n'ont pas fait les deux tests

# 6.2. Qui sont les migrants ?
les_migrants <- migration_df %>% filter(Migration == "Migrant")

print(paste("Nombre de migrants :", nrow(les_migrants)))
View(les_migrants)

# 6.3. Tableau récapitulatif par groupe d'âge
recap_migration <- migration_df %>%
  group_by(AgeGroup) %>%
  summarise(
    Total = n(),
    Nb_Migrants = sum(Migration == "Migrant"),
    Perc_Migrants = round(100 * Nb_Migrants / Total, 1)
  ) %>%
  arrange(desc(Perc_Migrants))

print(recap_migration)




# ___________________________________________________________________________
# II - VISUALISATION
# ___________________________________________________________________________


# --- 1. PRÉPARATION DES LABELS EN ANGLAIS ---
df <- df %>%
  mutate(
    # On renomme et on ordonne en une seule étape
    AgeGroup_EN = factor(AgeGroup, 
                         levels = c("JeunesEnfants", "Enfants", "Adolescents", "Adultes"),
                         labels = c("Young Children", "Children", "Adolescents", "Adults")),
    
    Sex_EN = factor(Sex, 
                    levels = c("F", "M"), 
                    labels = c("Female", "Male")),
    
    Condition_EN = factor(Condition, 
                          levels = c("Plat", "Medium", "High"),
                          labels = c("Even", "Medium", "High"))
  )


# --- 2. BOXPLOT DES ÂGES ---
ggplot(df, aes(x = as.factor(ClusterID), y = AgeMonths.y, fill = as.factor(ClusterID))) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) + 
  scale_fill_manual(values = c("#3498db", "#e74c3c")) +
  labs(title = "Age distribution by cluster",
       x = "Cluster ID", y = "Age (months)", fill = "Cluster") +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 13)
  )


# --- 3. RÉPARTITIONS (PROPORTIONS) ---

my_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 13)
  )

# A. Age group
p1 <- ggplot(df, aes(x = AgeGroup_EN, fill = as.factor(ClusterID))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#3498db", "#e74c3c")) +
  labs(title = "Age group", x = "", y = "% of sample", fill = "Cluster") +
  my_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14))

# B. Sex
p2 <- ggplot(df, aes(x = Sex_EN, fill = as.factor(ClusterID))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#3498db", "#e74c3c")) +
  labs(title = "Sex", x = "", y = "", fill = "Cluster") +
  my_theme

# C. Surface (Condition)
p3 <- ggplot(df, aes(x = Condition_EN, fill = as.factor(ClusterID))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#3498db", "#e74c3c")) +
  labs(title = "Surface", x = "", y = "", fill = "Cluster") +
  my_theme


# --- 4. AFFICHAGE COMBINÉ ---
library(patchwork)
(p1 + p2 + p3) + 
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Distribution of clusters across Age groups, Sex and Surfaces",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
  )

# ============================================================
# VALEURS NUMÉRIQUES DES GRAPHIQUES DE PROPORTIONS
# même logique que geom_bar(position = "fill")
# ============================================================

# 1. Proportion de clusters dans chaque groupe d'âge
prop_agegroup_cluster <- df %>%
  count(AgeGroup_EN, ClusterID) %>%
  group_by(AgeGroup_EN) %>%
  mutate(
    total = sum(n),
    percent = round(100 * n / total, 1)
  ) %>%
  ungroup()

print(prop_agegroup_cluster)

# 2. Proportion de clusters dans chaque sexe
prop_sex_cluster <- df %>%
  count(Sex_EN, ClusterID) %>%
  group_by(Sex_EN) %>%
  mutate(
    total = sum(n),
    percent = round(100 * n / total, 1)
  ) %>%
  ungroup()

print(prop_sex_cluster)

# 3. Proportion de clusters dans chaque surface
prop_surface_cluster <- df %>%
  count(Condition_EN, ClusterID) %>%
  group_by(Condition_EN) %>%
  mutate(
    total = sum(n),
    percent = round(100 * n / total, 1)
  ) %>%
  ungroup()

print(prop_surface_cluster)

# --- 5. MIGRATIONS DE PARTICIPANTS ENTRE CLUSTERS ---
# 1. Préparation des labels de légende avec les % calculés en Section II
# On utilise votre tableau 'recap_migration' pour extraire les valeurs exactes
labels_avec_perc <- c(
  "Young Children" = paste0("Young Children (", recap_migration$Perc_Migrants[recap_migration$AgeGroup == "JeunesEnfants"], "%)"),
  "Children"       = paste0("Children (", recap_migration$Perc_Migrants[recap_migration$AgeGroup == "Enfants"], "%)"),
  "Adolescents"    = paste0("Adolescents (", recap_migration$Perc_Migrants[recap_migration$AgeGroup == "Adolescents"], "%)"),
  "Adults"         = paste0("Adults (", recap_migration$Perc_Migrants[recap_migration$AgeGroup == "Adultes"], "%)")
)

# 2. Préparation des données pour le flux
df_flux <- migration_df %>%
  mutate(
    AgeGroup_EN = factor(AgeGroup, 
                         levels = c("JeunesEnfants", "Enfants", "Adolescents", "Adultes"),
                         labels = c("Young Children", "Children", "Adolescents", "Adults")),
    # On force l'ordre des clusters pour la clarté visuelle (1 en bas, 2 en haut)
    Plat = factor(Plat, levels = c(2, 1)),
    High = factor(High, levels = c(2, 1))
  ) %>%
  group_by(AgeGroup_EN, Plat, High) %>%
  summarise(n = n(), .groups = 'drop')

# 3. Création du graphique Alluvial
plot_migration <- ggplot(df_flux, aes(y = n, axis1 = Plat, axis2 = High)) +
  # Rubans de flux : 'discern = TRUE' aide à supprimer les messages d'avis
  geom_alluvium(aes(fill = AgeGroup_EN), width = 1/12, alpha = 0.5, discern = TRUE) +
  
  # Blocs de clusters (Strates)
  geom_stratum(width = 1/6, fill = "white", color = "black", discern = TRUE) +
  
  # Étiquettes des clusters à l'intérieur des blocs
  geom_text(stat = "stratum", aes(label = paste0("Cluster ", after_stat(stratum))), 
            size = 3.5, fontface = "bold") +
  
  # Configuration des couleurs et de la légende
  scale_fill_manual(
    values = c(
      "Young Children" = "blue", # Bleu
      "Children"       = "chocolate3", # Orange
      "Adolescents"    = "darkred", # Vert
      "Adults"         = "purple"  # Violet
    ),
    labels = labels_avec_perc
  ) +
  
  # Configuration des axes et titres
  scale_x_discrete(limits = c("Even", "High"), expand = c(.1, .1)) +
  labs(
    title = "Individual Migration from Even to High Surface",
    subtitle = "Flows represent participants shifting gait profiles (% = migration rate)",
    fill = "Age Group (Migration rate %)",
    y = "Number of participants"
  ) +
  
  # Thème et mise en page
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom" 
  )

# Affichage et sauvegarde
print(plot_migration)


# --- 6. EXPORT DES FIGURES ---
# A. Enregistrement du Boxplot
# On le redessine rapidement pour être sûr qu'il soit le "dernier" affiché
plot_age <- ggplot(df, aes(x = as.factor(ClusterID), y = AgeMonths.y, fill = as.factor(ClusterID))) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) + 
  scale_fill_manual(values = c("#3498db", "#e74c3c")) +
  labs(title = "Age distribution by cluster",
       x = "Cluster ID", y = "Age (months)", fill = "Cluster") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Boxplot_Age_Clusters.png", plot = plot_age, width = 15, height = 12, units = "cm", dpi = 300)
ggsave("Boxplot_Age_Clusters.pdf", 
       plot = plot_age, 
       width = 15, 
       height = 12, 
       units = "cm")


# B. Enregistrement du combiné (Proportions)
plot_combined <- (p1 + p2 + p3) + 
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Distribution of clusters across Age groups, Sex and Surfaces",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
  )

ggsave("Combined_Repartition_Clusters.png", plot = plot_combined, width = 25, height = 12, units = "cm", dpi = 300)
ggsave("Combined_Repartition_Clusters.pdf", 
       plot = plot_combined, 
       width = 25, 
       height = 12, 
       units = "cm")

# B. Enregistrement du graph migration
ggsave("Migration_Alluvial_Plot.png", plot = plot_migration, width = 20, height = 15, units = "cm", dpi = 300)
ggsave("Migration_Alluvial_Final.pdf", 
       plot = plot_migration, 
       width = 22, 
       height = 16, 
       units = "cm") # Pas besoin de DPI pour le PDF car c'est du vectoriel

print("Les images ont été enregistrées dans votre dossier de travail.")




# ___________________________________________________________________________
# III - RADAR PLOT DES CLUSTERS (TOUTES SURFACES CONFONDUES)
# ___________________________________________________________________________

# --- 1. PRÉPARATION DES VARIABLES ET DOMAINES ---

# Variables présentes dans votre CSV (après standardisation des noms)
# Note : on utilise les noms EXACTS tels qu'ils apparaissent dans votre CSV
vars_radar <- c(
  "Mean_Norm Gait Speed (m.s^{-1})",
  "Mean_Norm Step length (ua)",
  "Mean_Norm WR (ua)",
  "Mean_Double support time (%)",
  "Mean_Norm Cadence (ua)",
  "Mean_COM SPARC Magnitude (ua)",
  "Mean_Norm StepWidth (ua)",
  "Mean_MoS ML HS (%L0)",
  "Mean_MoS AP HS (%L0)",
  "SI_Stride length (m)",
  "SI_Double support time (%)",
  "SI_StepWidth (cm)",
  "CV_Norm StepWidth (ua)",
  "Mean_GVI (ua)",
  "CV_Gait speed (m.s^{-1})"
)

# Vérification que toutes les variables sont présentes
vars_radar_present <- intersect(vars_radar, names(df))
if(length(vars_radar_present) < length(vars_radar)) {
  message("Variables manquantes : ", paste(setdiff(vars_radar, vars_radar_present), collapse = ", "))
}

# Labels lisibles pour l'affichage
radar_labels <- c(
  "Norm Gait Speed (ua)",
  "Norm Step length (ua)",
  "Norm WR (ua)",
  "Double support time (%)",
  "Norm Cadence (ua)",
  "COM SPARC (ua)",
  "Norm StepWidth (ua)",
  "MoS ML HS (%L0)",
  "MoS AP HS (%L0)",
  "S.I. Stride length",
  "S.I. Double support",
  "S.I. StepWidth",
  "C.V. StepWidth (%)",
  "GVI (ua)",
  "C.V. Gait speed (%)"
)

# --- 2. DÉFINITION DES DOMAINES ---
domains_vars <- list(
  PACE = c(
    "Mean_Norm Gait Speed (m.s^{-1})",
    "Mean_Norm Step length (ua)",
    "Mean_Norm WR (ua)"
  ),
  RHYTHM = c(
    "Mean_Double support time (%)",
    "Mean_Norm Cadence (ua)",
    "Mean_COM SPARC Magnitude (ua)"
  ),
  `POSTURAL CONTROL` = c(
    "Mean_Norm StepWidth (ua)",
    "Mean_MoS ML HS (%L0)",
    "Mean_MoS AP HS (%L0)"
  ),
  ASYMMETRY = c(
    "SI_Stride length (m)",
    "SI_Double support time (%)",
    "SI_StepWidth (cm)"
  ),
  VARIABILITY = c(
    "CV_Norm StepWidth (ua)",
    "Mean_GVI (ua)",
    "CV_Gait speed (m.s^{-1})"
  )
)

# Couleurs des domaines
domain_colors <- c(
  PACE = "lightblue",
  RHYTHM = "lightcoral",
  `POSTURAL CONTROL` = "palegreen",
  ASYMMETRY = "plum",
  VARIABILITY = "lightyellow"
)

# Couleurs des clusters
cluster_colors <- c("1" = "blue", "2" = "red")  # Bleu et Rouge

# --- 3. FONCTION UTILITAIRE : ASSOCIER CHAQUE VARIABLE À SON DOMAINE ---
get_domain_for_vars <- function(vars_in_radar, domains_list) {
  dom_vec <- rep(NA_character_, length(vars_in_radar))
  names(dom_vec) <- vars_in_radar
  for (d in names(domains_list)) {
    dom_vec[vars_in_radar %in% domains_list[[d]]] <- d
  }
  dom_vec
}

# --- 4. FONCTION : DESSINER LES FONDS COLORÉS PAR DOMAINE ---
draw_domain_background <- function(domains_by_var, domain_cols, alpha = 0.18, r = 1) {
  n <- length(domains_by_var)
  if (n < 3) return(invisible(NULL))
  
  # Angles des axes (radar classique: premier en haut)
  angles <- seq(0, 2*pi, length.out = n + 1)[1:n] + (pi/2)
  
  # Limites entre axes = milieux angulaires
  bounds <- angles - (pi / n)
  bounds <- c(bounds, bounds[1] + 2*pi)
  
  # Regrouper les variables contiguës d'un même domaine
  runs <- rle(domains_by_var)
  idx_end <- cumsum(runs$lengths)
  idx_start <- c(1, head(idx_end, -1) + 1)
  
  for (k in seq_along(runs$values)) {
    dom <- runs$values[k]
    if (is.na(dom)) next
    col <- domain_cols[[dom]]
    if (is.null(col) || is.na(col)) next
    
    i1 <- idx_start[k]
    i2 <- idx_end[k]
    
    # Bornes angulaires du bloc contigu
    a_start <- bounds[i1]
    a_end   <- bounds[i2 + 1]
    
    # Points du secteur
    aa <- seq(a_start, a_end, length.out = 80)
    x <- c(0, r * cos(aa), 0)
    y <- c(0, r * sin(aa), 0)
    
    polygon(
      x, y,
      col = grDevices::adjustcolor(col, alpha.f = alpha),
      border = NA
    )
  }
  
  invisible(NULL)
}

# --- 5. CALCUL DES BORNES MIN/MAX GLOBALES (NORMALISATION) ---
radar_min_max <- df %>%
  dplyr::select(dplyr::all_of(vars_radar_present)) %>%
  dplyr::summarise(dplyr::across(
    dplyr::everything(),
    list(
      min = ~min(.x, na.rm = TRUE),
      max = ~max(.x, na.rm = TRUE)
    )
  ))

mins_raw <- radar_min_max %>% dplyr::select(dplyr::ends_with("_min")) %>% unlist() %>% as.numeric()
maxs_raw <- radar_min_max %>% dplyr::select(dplyr::ends_with("_max")) %>% unlist() %>% as.numeric()

# --- 6. FONCTION PRINCIPALE : CRÉER LE RADAR PLOT ---
create_cluster_radar <- function(df_full, vars, labels) {
  
  # A) Calcul des MÉDIANES par cluster (toutes surfaces confondues)
  data_median <- df_full %>%
    dplyr::group_by(ClusterID) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(vars), ~median(.x, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::arrange(ClusterID)
  
  # B) Extraction des données individuelles (toutes surfaces confondues)
  data_indiv <- df_full %>%
    dplyr::select(ClusterID, dplyr::all_of(vars))
  
  # C) Récupération des min/max globaux (pour normalisation)
  mins <- as.vector(radar_min_max[grep("_min$", names(radar_min_max))])
  maxs <- as.vector(radar_min_max[grep("_max$", names(radar_min_max))])
  
  # D) Normalisation des médianes (0–1)
  radar_df_median <- as.data.frame(data_median[, -1, drop = FALSE])
  
  normalized_median <- as.data.frame(lapply(seq_len(ncol(radar_df_median)), function(i) {
    denom <- (maxs[[i]] - mins[[i]])
    if (is.na(denom) || denom == 0) return(rep(0, nrow(radar_df_median)))
    (radar_df_median[, i] - mins[[i]]) / denom
  }))
  
  colnames(normalized_median) <- labels
  
  # Format attendu par fmsb::radarchart :
  # - 1ère ligne : max (=1)
  # - 2ème ligne : min (=0)
  # - lignes suivantes : données (ici = médianes par cluster)
  final_radar_median <- rbind(rep(1, length(vars)), rep(0, length(vars)), normalized_median)
  
  # E) Normalisation des individus (0–1)
  radar_df_indiv <- data_indiv[, -1, drop = FALSE]
  
  normalized_indiv <- as.data.frame(lapply(seq_len(ncol(radar_df_indiv)), function(i) {
    denom <- (maxs[[i]] - mins[[i]])
    if (is.na(denom) || denom == 0) return(rep(0, nrow(radar_df_indiv)))
    (radar_df_indiv[, i] - mins[[i]]) / denom
  }))
  
  colnames(normalized_indiv) <- labels
  
  # F) Couleurs : clusters
  colors_border_median <- cluster_colors[as.character(data_median$ClusterID)]
  colors_in_median <- grDevices::adjustcolor(colors_border_median, alpha.f = 0.20)
  
  # G) Tracé en 3 couches (du fond vers l'avant)
  # 1) Cadre (axes, grille, titre) avec polygones invisibles
  # 2) Fonds colorés par domaine
  # 3) Individus (gris, derrière)
  # 4) Médianes de cluster (couleur, devant)
  
  # =========================
  # G1) Cadre du radar
  # =========================
  
  ng <- nrow(data_median)
  transparent <- grDevices::adjustcolor("white", alpha.f = 0)
  
  fmsb::radarchart(
    final_radar_median,
    axistype = 0,
    seg = 4,
    pcol  = rep(transparent, ng),
    pfcol = rep(transparent, ng),
    plwd  = rep(0.01, ng),
    plty  = rep(1, ng),
    cglcol = "grey70", 
    cglty = 1, 
    cglwd = 0.8,
    vlcex = 0.7,
    title = "Gait Profile Comparison Between Clusters (All Surfaces)"
  )
  
  # === Préparation des variables pour les étiquettes ===
  nvar <- length(vars)
  angles <- seq(0, 2*pi, length.out = nvar + 1)[1:nvar] + (pi/2)
  
  pct <- c(0.25, 0.50, 0.75, 1.00)
  r_levels <- pct
  
  ticks_real <- sapply(seq_len(nvar), function(i) {
    mins[[i]] + pct * (maxs[[i]] - mins[[i]])
  })
  
  # =========================
  # G2) Fond coloré par domaine
  # =========================
  domains_by_var <- get_domain_for_vars(vars, domains_vars)
  
  par(new = TRUE)  # superpose sur le même repère
  draw_domain_background(
    domains_by_var = domains_by_var,
    domain_cols    = domain_colors,
    alpha          = 0.35,
    r              = 1
  )
  
  # === Affichage des étiquettes de valeurs réelles ===
  for (i in seq_len(nvar)) {
    angle <- angles[i]
    
    for (j in seq_along(pct)) {
      r <- r_levels[j]
      
      # Position du texte (légèrement décalé vers l'extérieur)
      x_pos <- r * cos(angle) * 1.05
      y_pos <- r * sin(angle) * 1.05
      
      # Valeur réelle
      val <- round(ticks_real[j, i], 2)
      
      # Afficher le texte
      text(
        x = x_pos,
        y = y_pos,
        labels = val,
        cex = 1.2,
        col = "grey20",
        font = 1
      )
    }
  }
  
  # =========================
  # G3) Individus : tracés en gris derrière
  # =========================
  indiv_col <- grDevices::adjustcolor("grey30", alpha.f = 0.10)
  
  for (i in seq_len(nrow(normalized_indiv))) {
    par(new = TRUE)
    fmsb::radarchart(
      rbind(rep(1, length(vars)), rep(0, length(vars)), normalized_indiv[i, , drop = FALSE]),
      axistype = 0,
      vlabels = rep("", length(vars)),
      pcol = indiv_col,
      pfcol = NA,
      plwd = 0.7,
      plty = 1,
      cglcol = NA,
      axislabcol = NA,
      vlcex = 0,
      seg = length(vars)
    )
  }
  
  # =========================
  # G4) Médianes : tracés colorés au premier plan
  # =========================
  par(new = TRUE)
  fmsb::radarchart(
    final_radar_median,
    axistype = 0,
    vlabels = rep("", length(vars)),
    pcol = colors_border_median,
    pfcol = colors_in_median,
    plwd = 3.5,
    plty = 1,
    cglcol = NA,
    axislabcol = NA,
    vlcex = 0,
    seg = length(vars)
  )
  
  # --- Légende
  legend(
    x = "bottom",
    legend = c("Cluster 1", "Cluster 2"),
    inset = -0.15,
    horiz = TRUE,
    bty = "n",
    pch = 20,
    col = cluster_colors,
    text.col = "black",
    cex = 0.8,
    pt.cex = 1.5,
    xpd = TRUE
  )
}

# --- 7. GÉNÉRATION DES RADAR PLOTS ---

# 7.1) Export PDF
pdf("Radar_Plot_Clusters.pdf", width = 12, height = 12)
par(mfrow = c(1, 1))

create_cluster_radar(df, vars_radar_present, radar_labels)

dev.off()

# 7.2) Export PNG haute qualité
png(filename = "Radar_Plot_Clusters.png", width = 5200, height = 5200, res = 600, type = "cairo")

tiff(
  filename = "Radar_Plot_Clusters.tiff",
  width = 12,
  height = 12,
  units = "in",
  res = 600,
  compression = "lzw"
)

op <- par(no.readonly = TRUE)

par(mfrow = c(1, 1))
par(mar = c(8, 5, 5, 5))     # bottom, left, top, right
par(oma = c(0, 0, 0, 0))
par(xaxs = "i", yaxs = "i")
par(xpd = NA)
par(cex = 0.85)

create_cluster_radar(df, vars_radar_present, radar_labels)

par(op)
dev.off()

message("Radar plot des clusters généré avec succès !")
message("Fichiers créés : Radar_Plot_Clusters.pdf et Radar_Plot_Clusters.png")



# ___________________________________________________________________________
# IV - MIGRATIONS ENFANTS DE C1 à C2
# ___________________________________________________________________________

# ANALYSE DES PROFILS DE MIGRATION DES ENFANTS
# Comparaison entre enfants stables Cluster 1 et migrants vers Cluster 2
# Entre surfaces Even et High

setwd("C:/Users/silve/Desktop/DOCTORAT/UNIV MONTREAL/TRAVAUX-THESE/Surfaces_Irregulieres/Datas/Script/gaitAnalysisGUI/result/Fig/Clustering")

# =========================================================
# 0) LIBRAIRIES
# =========================================================
library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(gtsummary)
library(gt)
library(rstatix)
library(gridExtra)
library(grid)
library(patchwork)
library(rcompanion)

# =========================================================
# 1) IMPORT DES DONNÉES
# =========================================================

# 1.1 Données de clustering (avec ClusterID par condition)
df_clust <- read.csv("DATA_FOR_R_GLOBAL_20260123_1409.csv", sep = ";", check.names = FALSE)

# 1.2 Métadonnées (âge, sexe, anthropométrie)
df_meta <- read.csv(file.choose(), sep = ";", check.names = FALSE)
# Note : Aller chercher "participant.metadonnees" dans le dossier stats LMM

# 1.3 Activité physique (z-scores)
df_pa <- read.csv(file.choose(), sep = ";", check.names = FALSE)
# Note : Aller chercher "PhysicalActivity_Zscored.csv" généré par le script 12

# Vérification rapide
View(df_clust)
View(df_meta)
View(df_pa)

# =========================================================
# 2) PRÉPARATION DES DONNÉES
# =========================================================

# 2.1 Nettoyage des métadonnées (garder seulement les variables nécessaires)
df_meta_clean <- df_meta %>%
  select(Participant, AgeMonths, Sex, AgeGroup, Height_cm, Weight_kg, IMC) %>%
  distinct()

# 2.2 Nettoyage des données d'activité physique
df_pa_clean <- df_pa %>%
  select(PARTICIPANT, AgeGroup, Zscore) %>%
  rename(PA_Zscore = Zscore) %>%
  distinct()

# 2.3 Jointure cluster + métadonnées
df <- left_join(df_clust, df_meta_clean, by = "Participant")

# 2.4 Vérifier les noms de colonnes pour l'âge (gérer les doublons possibles)
if("AgeMonths.x" %in% colnames(df)) {
  df <- df %>%
    mutate(AgeMonths = coalesce(AgeMonths.x, AgeMonths.y)) %>%
    select(-AgeMonths.x, -AgeMonths.y)
}

if("AgeGroup.x" %in% colnames(df)) {
  df <- df %>%
    mutate(AgeGroup = coalesce(AgeGroup.x, AgeGroup.y)) %>%
    select(-AgeGroup.x, -AgeGroup.y)
}

# =========================================================
# 3) IDENTIFICATION DES PROFILS DE MIGRATION (CHILDREN SEULEMENT)
# =========================================================

# 3.1 Filtrer uniquement les enfants (Children / Enfants)
# Note : Adapter selon le nom exact dans tes données
df_children <- df %>%
  filter(AgeGroup %in% c("Children", "Enfants"))

# 3.2 Créer un tableau de migration Even -> High
migration_children <- df_children %>%
  filter(Condition %in% c("Plat", "High")) %>%
  select(Participant, Condition, ClusterID) %>%
  pivot_wider(names_from = Condition, values_from = ClusterID) %>%
  # Renommer pour clarté
  rename(Cluster_Even = Plat, Cluster_High = High) %>%
  # Filtrer seulement ceux qui ont les deux conditions
  filter(!is.na(Cluster_Even) & !is.na(Cluster_High))

# 3.3 Créer la variable "Profil" de migration
migration_children <- migration_children %>%
  mutate(
    Profil = case_when(
      Cluster_Even == 1 & Cluster_High == 1 ~ "Stable_C1",
      Cluster_Even == 1 & Cluster_High == 2 ~ "Migrant_C2",
      TRUE ~ "Autre"  # Pour capturer les autres cas (C2->C1, C2->C2, etc.)
    )
  )

# 3.4 Filtrer uniquement les deux profils d'intérêt
migration_children_filtered <- migration_children %>%
  filter(Profil %in% c("Stable_C1", "Migrant_C2"))

# Vérification
table(migration_children_filtered$Profil)
View(migration_children_filtered)

# =========================================================
# 4) JOINTURE AVEC LES VARIABLES D'INTÉRÊT
# =========================================================

# 4.1 Joindre les métadonnées anthropométriques
df_analysis <- migration_children_filtered %>%
  left_join(df_meta_clean, by = "Participant")

# 4.2 Joindre l'activité physique z-scorée
df_analysis <- df_analysis %>%
  left_join(df_pa_clean, by = c("Participant" = "PARTICIPANT"))

# 4.3 Convertir Profil en facteur ordonné
df_analysis$Profil <- factor(df_analysis$Profil, 
                             levels = c("Stable_C1", "Migrant_C2"),
                             ordered = TRUE)

# Vérification finale
View(df_analysis)
summary(df_analysis)
write_csv(df_analysis, "Clusters_C_Migration.csv")
# =========================================================
# 5) STATISTIQUES DESCRIPTIVES
# =========================================================

# 5.1 Tableau récapitulatif (Mean ± SD pour variables continues, n (%) pour sexe)
tab_migration <- df_analysis %>%
  select(Profil, AgeMonths, Height_cm, Weight_kg, Sex, PA_Zscore) %>%
  tbl_summary(
    by = Profil,
    label = list(
      AgeMonths ~ "Age (months)",
      Height_cm ~ "Height (cm)",
      Weight_kg ~ "Weight (kg)",
      Sex ~ "Sex",
      PA_Zscore ~ "Physical Activity (Z-score)"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ 2),
    missing = "no"
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "t.test",  # ou "wilcox.test" si préféré
      all_categorical() ~ "fisher.test"  # ou "chisq.test" si n suffisant
    )
  ) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Migration Profile**") %>%
  modify_footnote(all_stat_cols() ~ "Mean ± SD for continuous; n (%) for categorical")

# Affichage
print(tab_migration)

# =========================================================
# 6) EXPORT DU TABLEAU (PDF + PNG)
# =========================================================

gt_table_migration <- tab_migration %>% as_gt()

# Export PDF
gt::gtsave(gt_table_migration, "Table_Migration_Children.pdf")

# Export PNG
gt::gtsave(gt_table_migration, "Table_Migration_Children.png", expand = 10)

message("Tableau exporté dans : ", getwd())

# =========================================================
# 7) VISUALISATIONS
# =========================================================

# Palette couleur pour les profils
cols_profil <- c(
  "Stable_C1" = "#3498db",    # Bleu (comme Cluster 1)
  "Migrant_C2" = "#e74c3c"    # Rouge (comme Cluster 2)
)

# 7.1 BOXPLOT : ÂGE
p_age <- ggplot(df_analysis, aes(x = Profil, y = AgeMonths, fill = Profil)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5, color = "black") +
  scale_fill_manual(values = cols_profil) +
  labs(
    title = "Age distribution by migration profile",
    x = "Migration Profile",
    y = "Age (months)",
    fill = "Profile"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

# 7.2 BOXPLOT : POIDS
p_weight <- ggplot(df_analysis, aes(x = Profil, y = Weight_kg, fill = Profil)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5, color = "black") +
  scale_fill_manual(values = cols_profil) +
  labs(
    title = "Weight distribution by migration profile",
    x = "Migration Profile",
    y = "Weight (kg)",
    fill = "Profile"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

# 7.3 BOXPLOT : TAILLE
p_height <- ggplot(df_analysis, aes(x = Profil, y = Height_cm, fill = Profil)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5, color = "black") +
  scale_fill_manual(values = cols_profil) +
  labs(
    title = "Height distribution by migration profile",
    x = "Migration Profile",
    y = "Height (cm)",
    fill = "Profile"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

# 7.4 BOXPLOT : ACTIVITÉ PHYSIQUE
p_pa <- ggplot(df_analysis, aes(x = Profil, y = PA_Zscore, fill = Profil)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = cols_profil) +
  labs(
    title = "Physical activity by migration profile",
    x = "Migration Profile",
    y = "Physical Activity (Z-score)",
    fill = "Profile"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

# 7.5 BARPLOT : SEXE
# Calcul des proportions
sex_summary <- df_analysis %>%
  group_by(Profil, Sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Profil) %>%
  mutate(prop = n / sum(n) * 100)

p_sex <- ggplot(sex_summary, aes(x = Profil, y = prop, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8, color = "black") +
  geom_text(aes(label = paste0(n, " (", round(prop, 1), "%)")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("F" = "#FF6B9D", "M" = "#4A90E2")) +
  labs(
    title = "Sex distribution by migration profile",
    x = "Migration Profile",
    y = "Percentage (%)",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    panel.grid.major.x = element_blank()
  )

# =========================================================
# 8) ASSEMBLAGE ET EXPORT DES FIGURES
# =========================================================

# 8.1 Figure combinée (2x3 grid)
p_combined <- (p_age | p_weight | p_height) / 
  (p_pa | p_sex | plot_spacer()) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Comparison of Children Migration Profiles (Cluster 1 Even → Cluster 1 or 2 High)",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
  )

# Affichage
p_combined

# Export PNG haute résolution
ggsave(
  filename = "Migration_Children_Comparison.png",
  plot = p_combined,
  width = 28, height = 18, units = "cm",
  dpi = 600,
  bg = "white"
)

# Export PDF
ggsave(
  filename = "Migration_Children_Comparison.pdf",
  plot = p_combined,
  width = 28, height = 18, units = "cm"
)

# 8.2 Exports individuels (optionnel)
ggsave("Migration_Age.png", plot = p_age, width = 12, height = 10, units = "cm", dpi = 600, bg = "white")
ggsave("Migration_Weight.png", plot = p_weight, width = 12, height = 10, units = "cm", dpi = 600, bg = "white")
ggsave("Migration_Height.png", plot = p_height, width = 12, height = 10, units = "cm", dpi = 600, bg = "white")
ggsave("Migration_PA.png", plot = p_pa, width = 12, height = 10, units = "cm", dpi = 600, bg = "white")
ggsave("Migration_Sex.png", plot = p_sex, width = 12, height = 10, units = "cm", dpi = 600, bg = "white")

# =========================================================
# 9) TESTS STATISTIQUES DÉTAILLÉS + TAILLES D'EFFET + EXPORT CSV
# =========================================================

# 9.1 Test de normalité (Shapiro-Wilk)
print("=== TESTS DE NORMALITÉ ===")
normality_tests <- df_analysis %>%
  group_by(Profil) %>%
  shapiro_test(AgeMonths, Height_cm, Weight_kg, PA_Zscore) %>%
  ungroup()

print(normality_tests)

# 9.2 Tests t de Student avec Cohen's d
print("=== T-TEST : ÂGE ===")
t_test_age <- df_analysis %>%
  t_test(AgeMonths ~ Profil) %>%
  add_significance() %>%
  mutate(Variable = "Age (months)", .before = 1)
print(t_test_age)

# Calcul du Cohen's d pour l'âge
cohens_d_age <- df_analysis %>%
  cohens_d(AgeMonths ~ Profil, var.equal = FALSE) %>%
  mutate(Variable = "Age (months)", .before = 1)

print("=== T-TEST : POIDS ===")
t_test_weight <- df_analysis %>%
  t_test(Weight_kg ~ Profil) %>%
  add_significance() %>%
  mutate(Variable = "Weight (kg)", .before = 1)
print(t_test_weight)

# Calcul du Cohen's d pour le poids
cohens_d_weight <- df_analysis %>%
  cohens_d(Weight_kg ~ Profil, var.equal = FALSE) %>%
  mutate(Variable = "Weight (kg)", .before = 1)

print("=== T-TEST : TAILLE ===")
t_test_height <- df_analysis %>%
  t_test(Height_cm ~ Profil) %>%
  add_significance() %>%
  mutate(Variable = "Height (cm)", .before = 1)
print(t_test_height)

# Calcul du Cohen's d pour la taille
cohens_d_height <- df_analysis %>%
  cohens_d(Height_cm ~ Profil, var.equal = FALSE) %>%
  mutate(Variable = "Height (cm)", .before = 1)

print("=== T-TEST : ACTIVITÉ PHYSIQUE ===")
t_test_pa <- df_analysis %>%
  t_test(PA_Zscore ~ Profil) %>%
  add_significance() %>%
  mutate(Variable = "Physical Activity (Z-score)", .before = 1)
print(t_test_pa)

# Calcul du Cohen's d pour l'activité physique
cohens_d_pa <- df_analysis %>%
  cohens_d(PA_Zscore ~ Profil, var.equal = FALSE) %>%
  mutate(Variable = "Physical Activity (Z-score)", .before = 1)

# 9.3 Test du Chi² (ou Fisher exact) pour le Sexe avec Cramér's V
print("=== TEST EXACT DE FISHER : SEXE ===")
tab_sex <- table(df_analysis$Profil, df_analysis$Sex)
fisher_sex <- fisher_test(tab_sex) %>%
  mutate(Variable = "Sex", .before = 1)
print(fisher_sex)

# Calcul du Cramér's V (taille d'effet pour test catégoriel)
library(rcompanion)  # Pour cramerV
cramers_v_sex <- cramerV(tab_sex)
print(paste("Cramér's V pour Sexe :", round(cramers_v_sex, 3)))

# =========================================================
# 9.4 CONSOLIDATION ET EXPORT DES RÉSULTATS STATISTIQUES
# =========================================================

# A) Regrouper tous les Cohen's d
all_cohens_d <- bind_rows(
  cohens_d_age,
  cohens_d_weight,
  cohens_d_height,
  cohens_d_pa
)

# B) Regrouper tous les tests continus (t-tests) avec tailles d'effet
all_ttests <- bind_rows(
  t_test_age,
  t_test_weight,
  t_test_height,
  t_test_pa
) %>%
  # Joindre les tailles d'effet (Cohen's d)
  left_join(
    all_cohens_d %>% select(Variable, effsize, magnitude),
    by = "Variable"
  ) %>%
  # Sélectionner et renommer les colonnes importantes
  select(
    Variable,
    Test = .y.,
    group1,
    group2,
    n1,
    n2,
    statistic,
    df,
    p,
    p.signif,
    Cohen_d = effsize,
    Effect_Size = magnitude
  ) %>%
  # Formater la p-value et ajouter les infos de test
  mutate(
    p_formatted = format.pval(p, digits = 3, eps = 0.001),
    Test_Type = "t-test (Welch)",
    method = "Welch Two Sample t-test"  # Ajouté manuellement
  )

# C) Formater le test de Fisher avec Cramér's V
fisher_formatted <- fisher_sex %>%
  mutate(
    p_formatted = format.pval(p, digits = 3, eps = 0.001),
    Test_Type = "Fisher's Exact Test",
    method = "Fisher's Exact Test for Count Data",  # Ajouté manuellement
    # Ajouter des colonnes vides pour cohérence avec t-tests
    group1 = "Stable_C1",
    group2 = "Migrant_C2",
    statistic = NA,
    df = NA,
    n1 = NA,
    n2 = NA,
    Test = "Sex",
    Cohen_d = NA,
    Cramers_V = cramers_v_sex,
    Effect_Size = case_when(
      cramers_v_sex < 0.1 ~ "negligible",
      cramers_v_sex < 0.3 ~ "small",
      cramers_v_sex < 0.5 ~ "moderate",
      TRUE ~ "large"
    )
  ) %>%
  select(
    Variable,
    Test,
    group1,
    group2,
    n1,
    n2,
    statistic,
    df,
    p,
    p_formatted,
    p.signif,
    Cohen_d,
    Cramers_V,
    Effect_Size,
    Test_Type,
    method
  )

# D) Combiner tous les résultats statistiques
all_stats_results <- bind_rows(
  all_ttests %>% mutate(Cramers_V = NA),  # Ajouter colonne vide pour cohérence
  fisher_formatted
) %>%
  # Réorganiser les colonnes pour meilleure lisibilité
  select(
    Variable,
    Test_Type,
    method,
    group1,
    group2,
    n1,
    n2,
    statistic,
    df,
    p,
    p_formatted,
    p.signif,
    Cohen_d,
    Cramers_V,
    Effect_Size
  )

# E) Export en CSV
write_csv(all_stats_results, "Statistical_Tests_Results_Migration.csv")

# F) Optionnel : Export des tests de normalité
write_csv(normality_tests, "Normality_Tests_Results_Migration.csv")

# G) Export du tableau des tailles d'effet uniquement
effect_sizes_summary <- all_stats_results %>%
  select(Variable, Test_Type, Cohen_d, Cramers_V, Effect_Size, p.signif)

write_csv(effect_sizes_summary, "Effect_Sizes_Summary_Migration.csv")

message("Résultats statistiques exportés :")
message("  - Statistical_Tests_Results_Migration.csv (complet)")
message("  - Effect_Sizes_Summary_Migration.csv (tailles d'effet)")
message("  - Normality_Tests_Results_Migration.csv (normalité)")

# H) Affichage du tableau récapitulatif dans la console
print("=== TABLEAU RÉCAPITULATIF DES TESTS STATISTIQUES ===")
print(all_stats_results)

print("=== RÉSUMÉ DES TAILLES D'EFFET ===")
print(effect_sizes_summary)


# =========================================================
# IV - BIS : TRAJECTOIRES DE MIGRATION DES ENFANTS
#            Even --> Medium --> High
# =========================================================
# Cette section identifie à quel moment de la progression
# de surface les enfants "migrants" ont basculé de cluster.
#
# Profils de trajectoire possibles (parmi les enfants en C1 sur Even) :
#   - "Stable"         : C1 → C1 → C1
#   - "Early migrant"  : C1 → C2 → C2  (bascule dès Medium)
#   - "Late migrant"   : C1 → C1 → C2  (bascule uniquement sur High)
#   - "Transient"      : C1 → C2 → C1  (migration instable, retour sur High)
# =========================================================


# ---------------------------------------------------------
# 1) CONSTRUCTION DU TABLEAU DE TRAJECTOIRES
# ---------------------------------------------------------

# On part de df_children (déjà filtré sur AgeGroup "Enfants"/"Children")
# et on extrait les ClusterID pour les 3 surfaces

trajectories_children <- df_children %>%
  filter(Condition %in% c("Plat", "Medium", "High")) %>%
  select(Participant, Condition, ClusterID) %>%
  pivot_wider(names_from = Condition, values_from = ClusterID) %>%
  rename(
    Cluster_Even   = Plat,
    Cluster_Medium = Medium,
    Cluster_High   = High
  ) %>%
  # Garder uniquement les participants avec les 3 conditions
  filter(!is.na(Cluster_Even) & !is.na(Cluster_Medium) & !is.na(Cluster_High))

# Vérification
cat("Nombre d'enfants avec les 3 surfaces :", nrow(trajectories_children), "\n")
View(trajectories_children)


# ---------------------------------------------------------
# 2) CLASSIFICATION DES TRAJECTOIRES
# ---------------------------------------------------------

trajectories_children <- trajectories_children %>%
  mutate(
    Trajectory = case_when(
      # Enfants de C1 sur Even (les "migrants potentiels")
      Cluster_Even == 1 & Cluster_Medium == 1 & Cluster_High == 1 ~ "Stable C1",
      Cluster_Even == 1 & Cluster_Medium == 2 & Cluster_High == 2 ~ "Early Migrant",
      Cluster_Even == 1 & Cluster_Medium == 1 & Cluster_High == 2 ~ "Late Migrant",
      Cluster_Even == 1 & Cluster_Medium == 2 & Cluster_High == 1 ~ "Transient",
      # Cas hors focus (déjà en C2 sur Even)
      Cluster_Even == 2 & Cluster_Medium == 2 & Cluster_High == 2 ~ "Stable C2",
      Cluster_Even == 2 & Cluster_Medium == 1 & Cluster_High == 1 ~ "Reverse Migrant Early",
      Cluster_Even == 2 & Cluster_Medium == 2 & Cluster_High == 1 ~ "Reverse Migrant Late",
      Cluster_Even == 2 & Cluster_Medium == 1 & Cluster_High == 2 ~ "Reverse Transient",
      TRUE ~ "Other"
    ),
    # Version courte pour le diagramme
    Trajectory_Label = paste0("C", Cluster_Even, " → C", Cluster_Medium, " → C", Cluster_High)
  )

# Tableau de résumé des trajectoires
cat("\n=== TABLEAU DES TRAJECTOIRES DE MIGRATION ===\n")
recap_trajectories <- trajectories_children %>%
  count(Trajectory, Trajectory_Label) %>%
  arrange(desc(n)) %>%
  mutate(Pct = round(100 * n / sum(n), 1))

print(recap_trajectories)

# Focus sur les profils C1 → ...
cat("\n=== FOCUS : ENFANTS DÉBUTANT EN C1 (Even) ===\n")
recap_c1_starters <- trajectories_children %>%
  filter(Cluster_Even == 1) %>%
  count(Trajectory, Trajectory_Label) %>%
  arrange(desc(n)) %>%
  mutate(Pct = round(100 * n / sum(n), 1))

print(recap_c1_starters)

# Export CSV
write_csv(trajectories_children, "Clusters_Children_Trajectories_3surfaces.csv")


# ---------------------------------------------------------
# 3) JOINTURE AVEC LES MÉTADONNÉES (pour caractériser les profils)
# ---------------------------------------------------------

df_traj_analysis <- trajectories_children %>%
  left_join(df_meta_clean, by = "Participant") %>%
  left_join(df_pa_clean, by = c("Participant" = "PARTICIPANT")) %>%
  mutate(
    Trajectory = factor(Trajectory, levels = c(
      "Stable C1", "Early Migrant", "Late Migrant", "Transient",
      "Stable C2", "Reverse Migrant Early", "Reverse Migrant Late",
      "Reverse Transient", "Other"
    ))
  )


# ---------------------------------------------------------
# 4) DIAGRAMME DE SANKEY / ALLUVIAL (Even → Medium → High)
# ---------------------------------------------------------

# 4.1 Préparation des données pour le diagramme alluvial

# Palette de couleurs pour les trajectoires principales
traj_colors <- c(
  "Stable C1"           = "#3498db",   # Bleu (stable performant)
  "Early Migrant"       = "#e74c3c",   # Rouge (bascule tôt)
  "Late Migrant"        = "#e67e22",   # Orange (bascule tard)
  "Transient"           = "#9b59b6",   # Violet (instable)
  "Stable C2"           = "#c0392b",   # Rouge foncé (stable moins performant)
  "Reverse Migrant Early" = "#27ae60", # Vert (amélioration précoce)
  "Reverse Migrant Late"  = "#2ecc71", # Vert clair (amélioration tardive)
  "Reverse Transient"   = "#f39c12",   # Jaune (instable autre sens)
  "Other"               = "grey60"
)

# On force l'ordre des clusters pour la lisibilité (C1 = en bas, C2 = en haut)
df_alluvial <- trajectories_children %>%
  mutate(
    Even   = factor(paste0("C", Cluster_Even),   levels = c("C2", "C1")),
    Medium = factor(paste0("C", Cluster_Medium), levels = c("C2", "C1")),
    High   = factor(paste0("C", Cluster_High),   levels = c("C2", "C1"))
  ) %>%
  group_by(Trajectory, Even, Medium, High) %>%
  summarise(n = n(), .groups = "drop")


# 4.2 Création du diagramme Sankey / Alluvial (ggalluvial)
plot_sankey <- ggplot(
  df_alluvial,
  aes(y = n, axis1 = Even, axis2 = Medium, axis3 = High)
) +
  # Rubans de flux colorés par trajectoire
  geom_alluvium(
    aes(fill = Trajectory),
    width = 1/8,
    alpha = 0.70,
    discern = FALSE
  ) +
  
  # Blocs de clusters (strates)
  geom_stratum(
    width = 1/6,
    fill = "white",
    color = "black",
    linewidth = 0.6
  ) +
  
  # Étiquettes dans les blocs : Cluster + effectif
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  
  # Couleurs des trajectoires
  scale_fill_manual(values = traj_colors) +
  
  # Axes
  scale_x_discrete(
    limits = c("Even", "Medium", "High"),
    expand = c(0.05, 0.05)
  ) +
  scale_y_continuous(
    name = "Number of children",
    breaks = scales::breaks_pretty()
  ) +
  
  # Titres et labels
  labs(
    title    = "Children gait cluster trajectories across surfaces",
    subtitle = "Flows represent individual children shifting cluster membership",
    fill     = "Trajectory",
    x        = "Walking Surface"
  ) +
  
  # Thème
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey40"),
    axis.text.x   = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y   = element_text(size = 9),
    legend.position  = "right",
    legend.title  = element_text(face = "bold"),
    panel.grid    = element_blank()
  )

print(plot_sankey)


# ---------------------------------------------------------
# 5) VERSION ALTERNATIVE : FOCUS SUR LES ENFANTS EN C1 SUR EVEN
#    (= les "migrants potentiels", la population d'intérêt principale)
# ---------------------------------------------------------

df_alluvial_c1 <- trajectories_children %>%
  filter(Cluster_Even == 1) %>%
  mutate(
    Even   = factor(paste0("C", Cluster_Even),   levels = c("C2", "C1")),
    Medium = factor(paste0("C", Cluster_Medium), levels = c("C2", "C1")),
    High   = factor(paste0("C", Cluster_High),   levels = c("C2", "C1"))
  ) %>%
  group_by(Trajectory, Even, Medium, High) %>%
  summarise(n = n(), .groups = "drop")

# Couleurs simplifiées pour ce focus
traj_colors_c1 <- c(
  "Stable C1"     = "#3498db",
  "Early Migrant" = "#e74c3c",
  "Late Migrant"  = "#e67e22",
  "Transient"     = "#9b59b6"
)

plot_sankey_c1 <- ggplot(
  df_alluvial_c1,
  aes(y = n, axis1 = Even, axis2 = Medium, axis3 = High)
) +
  geom_alluvium(
    aes(fill = Trajectory),
    width = 1/8,
    alpha = 0.75,
    discern = FALSE
  ) +
  geom_stratum(
    width = 1/6,
    fill = "white",
    color = "black",
    linewidth = 0.7
  ) +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 4,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_manual(
    values = traj_colors_c1,
    labels = c(
      "Stable C1"     = "Stable (C1 → C1 → C1)",
      "Early Migrant" = "Early Migrant (C1 → C2 → C2)",
      "Late Migrant"  = "Late Migrant (C1 → C1 → C2)",
      "Transient"     = "Transient (C1 → C2 → C1)"
    )
  ) +
  scale_x_discrete(
    limits = c("Even", "Medium", "High"),
    expand = c(0.05, 0.05)
  ) +
  scale_y_continuous(
    name = "Number of children",
    breaks = scales::breaks_pretty()
  ) +
  labs(
    title    = "Migration trajectories of children starting in Cluster 1 (Even surface)",
    subtitle = "When do children shift their gait profile toward Cluster 2?",
    fill     = "Migration Profile",
    x        = "Walking Surface"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 13),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey40"),
    axis.text.x   = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y   = element_text(size = 9),
    legend.position  = "right",
    legend.title  = element_text(face = "bold", size = 10),
    legend.text   = element_text(size = 9),
    panel.grid    = element_blank()
  )

print(plot_sankey_c1)


# ---------------------------------------------------------
# 6) STATISTIQUES : COMPARAISON EARLY vs LATE MIGRANT
#    (sur les variables anthropométriques et d'activité physique)
# ---------------------------------------------------------

df_migrant_comparison <- df_traj_analysis %>%
  filter(Trajectory %in% c("Early Migrant", "Late Migrant"))

# Vérification des effectifs
cat("\n=== EFFECTIFS : EARLY vs LATE MIGRANT ===\n")
print(table(df_migrant_comparison$Trajectory))

if (nrow(df_migrant_comparison) >= 4) {
  
  # Tests de comparaison (Wilcoxon car petits effectifs probables)
  vars_comparaison <- c("AgeMonths", "Height_cm", "Weight_kg", "PA_Zscore")
  
  stats_migrant_type <- df_migrant_comparison %>%
    pivot_longer(cols = all_of(vars_comparaison),
                 names_to = "Variable", values_to = "Value") %>%
    group_by(Variable) %>%
    wilcox_test(Value ~ Trajectory) %>%
    add_significance() %>%
    mutate(
      Variable = dplyr::recode(Variable,
                               "AgeMonths"  = "Age (months)",
                               "Height_cm"  = "Height (cm)",
                               "Weight_kg"  = "Weight (kg)",
                               "PA_Zscore"  = "Physical Activity (Z-score)"
      )
    )
  
  cat("\n=== COMPARAISON EARLY vs LATE MIGRANT (Wilcoxon) ===\n")
  print(stats_migrant_type)
  
  write_csv(stats_migrant_type, "Stats_EarlyVsLate_Migrant_Children.csv")
  
} else {
  cat("Effectifs insuffisants pour la comparaison Early vs Late Migrant.\n")
}


# ---------------------------------------------------------
# 7) EXPORT DES FIGURES
# ---------------------------------------------------------

# Figure 1 : Toutes trajectoires (tous enfants)
ggsave(
  filename = "Sankey_All_Trajectories_Children.png",
  plot = plot_sankey,
  width = 28, height = 18, units = "cm",
  dpi = 600, bg = "white"
)
ggsave(
  filename = "Sankey_All_Trajectories_Children.pdf",
  plot = plot_sankey,
  width = 28, height = 18, units = "cm"
)

# Figure 2 : Focus C1 starters (population principale)
ggsave(
  filename = "Sankey_C1Starters_Trajectories_Children.png",
  plot = plot_sankey_c1,
  width = 28, height = 18, units = "cm",
  dpi = 600, bg = "white"
)
ggsave(
  filename = "Sankey_C1Starters_Trajectories_Children.pdf",
  plot = plot_sankey_c1,
  width = 28, height = 18, units = "cm"
)

message("=== IV-bis terminé ===")
message("Fichiers créés :")
message("  - Clusters_Children_Trajectories_3surfaces.csv")
message("  - Sankey_All_Trajectories_Children.png / .pdf")
message("  - Sankey_C1Starters_Trajectories_Children.png / .pdf")
message("  - Stats_EarlyVsLate_Migrant_Children.csv (si effectifs suffisants)")



# ___________________________________________________________________________
# V - COMPARAISON DES 3 PROFILS D'ENFANTS : STABLE C1, MIGRANT, STABLE C2
# ___________________________________________________________________________
# Cette section compare les caractéristiques anthropométriques et d'activité
# physique entre 3 groupes d'enfants basés sur leur comportement Even → High :
#   - Stable C1     : C1 sur Even ET C1 sur High
#   - Migrant C1→C2 : C1 sur Even ET C2 sur High
#   - Stable C2     : C2 sur Even ET C2 sur High
# ___________________________________________________________________________


# =========================================================
# 1) IDENTIFICATION DES 3 PROFILS
# =========================================================

# 1.1 Préparation des données de migration Even → High (enfants uniquement)
profiles_3groups <- df_children %>%
  filter(Condition %in% c("Plat", "High")) %>%
  select(Participant, Condition, ClusterID) %>%
  pivot_wider(names_from = Condition, values_from = ClusterID) %>%
  rename(Cluster_Even = Plat, Cluster_High = High) %>%
  # Garder uniquement ceux qui ont les deux conditions
  filter(!is.na(Cluster_Even) & !is.na(Cluster_High))

# 1.2 Classification en 3 profils
profiles_3groups <- profiles_3groups %>%
  mutate(
    Profile = case_when(
      Cluster_Even == 1 & Cluster_High == 1 ~ "Stable C1",
      Cluster_Even == 1 & Cluster_High == 2 ~ "Migrant C1→C2",
      Cluster_Even == 2 & Cluster_High == 2 ~ "Stable C2",
      TRUE ~ "Other"  # C2→C1 (migration inverse) ou autres cas rares
    )
  )

# 1.3 Vérification des effectifs
cat("\n=== RÉPARTITION DES 3 PROFILS ===\n")
table_profiles <- table(profiles_3groups$Profile)
print(table_profiles)

# Ne garder que les 3 profils d'intérêt
profiles_3groups <- profiles_3groups %>%
  filter(Profile %in% c("Stable C1", "Migrant C1→C2", "Stable C2"))

cat("\nEffectifs finaux après filtrage :\n")
print(table(profiles_3groups$Profile))


# =========================================================
# 2) JOINTURE AVEC LES VARIABLES D'INTÉRÊT
# =========================================================

# 2.1 Métadonnées (âge, sexe, anthropométrie)
df_profiles <- profiles_3groups %>%
  left_join(df_meta_clean, by = "Participant")

# 2.2 Activité physique (Z-score)
df_profiles <- df_profiles %>%
  left_join(df_pa_clean, by = c("Participant" = "PARTICIPANT"))

# 2.3 Factorisation du profil avec ordre logique
df_profiles$Profile <- factor(
  df_profiles$Profile, 
  levels = c("Stable C1", "Migrant C1→C2", "Stable C2"),
  ordered = TRUE
)

# Vérification
View(df_profiles)
summary(df_profiles)

# Export CSV
write_csv(df_profiles, "Profiles_3Groups_Children.csv")


# =========================================================
# 3) STATISTIQUES DESCRIPTIVES : TABLEAU RÉCAPITULATIF
# =========================================================

library(gtsummary)
library(gt)

# 3.1 Tableau avec Médiane [IQR] et test de Kruskal-Wallis
tab_profiles <- df_profiles %>%
  select(Profile, AgeMonths, Sex, PA_Zscore) %>%
  tbl_summary(
    by = Profile,
    label = list(
      AgeMonths ~ "Age (months)",
      Sex ~ "Sex",
      PA_Zscore ~ "Physical Activity (Z-score)"
    ),
    statistic = list(
      all_continuous() ~ "{median} [{p25}, {p75}]",  # MÉDIANE + IQR
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ 2),
    missing = "no"
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "kruskal.test",  # KRUSKAL-WALLIS
      all_categorical() ~ "fisher.test"
    )
  ) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Profile (Even → High)**") %>%
  modify_footnote(
    all_stat_cols() ~ "Median [IQR] for continuous; n (%) for categorical. P-values from Kruskal-Wallis or Fisher's exact test."
  )

# Affichage
print(tab_profiles)


# =========================================================
# 4) EXPORT DU TABLEAU (PDF + PNG)
# =========================================================

gt_table_profiles <- tab_profiles %>% as_gt()

# Export PDF
gt::gtsave(gt_table_profiles, "Table_Profiles_3Groups_Children.pdf")

# Export PNG
gt::gtsave(gt_table_profiles, "Table_Profiles_3Groups_Children.png", expand = 10)

message("Tableau exporté : Table_Profiles_3Groups_Children.pdf / .png")


# =========================================================
# 5) TESTS POST-HOC (COMPARAISONS 2 À 2)
# =========================================================
# Si l'ANOVA est significative, on fait des comparaisons par paires

library(rstatix)

# 5.1 Test de Tukey (post-hoc après ANOVA)
vars_continuous <- c("AgeMonths", "PA_Zscore")

posthoc_results <- lapply(vars_continuous, function(v) {
  df_profiles %>%
    tukey_hsd(as.formula(paste(v, "~ Profile"))) %>%
    mutate(Variable = v, .before = 1)
}) %>%
  bind_rows() %>%
  mutate(
    Variable = dplyr::recode(Variable,
                             "AgeMonths"  = "Age (months)",
                             "PA_Zscore"  = "Physical Activity (Z-score)"
    )
  ) %>%
  add_significance("p.adj")

cat("\n=== TESTS POST-HOC (TUKEY HSD) ===\n")
print(posthoc_results)

# Export CSV
write_csv(posthoc_results, "PostHoc_Profiles_3Groups_Children.csv")


# =========================================================
# 6) VISUALISATIONS : BOXPLOTS COMPARATIFS
# =========================================================

# Palette de couleurs pour les 3 profils
cols_profiles <- c(
  "Stable C1"     = "#3498db",  # Bleu (stable performant)
  "Migrant C1→C2" = "#e67e22",  # Orange (transition)
  "Stable C2"     = "#e74c3c"   # Rouge (stable moins performant)
)

# 6.1 BOXPLOT : ÂGE
p_age_3groups <- ggplot(df_profiles, aes(x = Profile, y = AgeMonths, fill = Profile)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5, color = "black") +
  scale_fill_manual(values = cols_profiles) +
  labs(
    title = "Age distribution by profile",
    x = "Profile (Even → High)",
    y = "Age (months)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )


# 6.4 BOXPLOT : ACTIVITÉ PHYSIQUE
p_pa_3groups <- ggplot(df_profiles, aes(x = Profile, y = PA_Zscore, fill = Profile)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = cols_profiles) +
  labs(
    title = "Physical activity by profile",
    x = "Profile (Even → High)",
    y = "Physical Activity (Z-score)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

# 6.5 BARPLOT : SEXE
sex_summary_3groups <- df_profiles %>%
  group_by(Profile, Sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Profile) %>%
  mutate(prop = n / sum(n) * 100)

p_sex_3groups <- ggplot(sex_summary_3groups, aes(x = Profile, y = prop, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8, color = "black") +
  geom_text(
    aes(label = paste0(n, " (", round(prop, 1), "%)")), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5, size = 3
  ) +
  scale_fill_manual(values = c("F" = "#FF6B9D", "M" = "#4A90E2")) +
  labs(
    title = "Sex distribution by profile",
    x = "Profile (Even → High)",
    y = "Percentage (%)",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )


# =========================================================
# 7) ASSEMBLAGE ET EXPORT DES FIGURES
# =========================================================

library(patchwork)

# 7.1 Figure combinée (2x3 grid)
p_combined_3groups <- (p_age_3groups | p_sex_3groups | p_pa_3groups) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Comparison of 3 Children Profiles: Stable C1, Migrant C1→C2, Stable C2",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
  )

# Affichage
print(p_combined_3groups)

# Export PNG haute résolution
ggsave(
  filename = "Profiles_3Groups_Children_Comparison.png",
  plot = p_combined_3groups,
  width = 32, height = 20, units = "cm",
  dpi = 600,
  bg = "white"
)

# Export PDF
ggsave(
  filename = "Profiles_3Groups_Children_Comparison.pdf",
  plot = p_combined_3groups,
  width = 32, height = 20, units = "cm"
)

# 7.2 Exports individuels (optionnel)
ggsave("Profiles_Age_3Groups.png", plot = p_age_3groups, 
       width = 14, height = 12, units = "cm", dpi = 600, bg = "white")
ggsave("Profiles_PA_3Groups.png", plot = p_pa_3groups, 
       width = 14, height = 12, units = "cm", dpi = 600, bg = "white")
ggsave("Profiles_Sex_3Groups.png", plot = p_sex_3groups, 
       width = 14, height = 12, units = "cm", dpi = 600, bg = "white")


# =========================================================
# 8) TESTS STATISTIQUES DÉTAILLÉS + TAILLES D'EFFET
# =========================================================

# 8.1 Test de Kruskal-Wallis (non-paramétrique, plus robuste)
library(rstatix)
library(effectsize)

kruskal_results <- df_profiles %>%
  pivot_longer(cols = all_of(vars_continuous),
               names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  kruskal_test(Value ~ Profile) %>%
  add_significance() %>%
  mutate(
    Variable = dplyr::recode(Variable,
                             "AgeMonths"  = "Age (months)",
                             "Height_cm"  = "Height (cm)",
                             "Weight_kg"  = "Weight (kg)",
                             "PA_Zscore"  = "Physical Activity (Z-score)"
    ),
    p_formatted = format.pval(p, digits = 3, eps = 0.001)
  )

cat("\n=== RÉSULTATS KRUSKAL-WALLIS (3 GROUPES) ===\n")
print(kruskal_results)

# 8.2 Taille d'effet : Epsilon² (équivalent de Eta² pour Kruskal-Wallis)
# Formule : ε² = H / (n² - 1) / (n + 1)
kruskal_results <- kruskal_results %>%
  mutate(
    n_total = n,
    Epsilon_squared = statistic / ((n^2 - 1) / (n + 1)),
    Effect_magnitude = case_when(
      Epsilon_squared < 0.01 ~ "negligible",
      Epsilon_squared < 0.06 ~ "small",
      Epsilon_squared < 0.14 ~ "medium",
      TRUE ~ "large"
    )
  )

cat("\n=== TAILLES D'EFFET (EPSILON²) ===\n")
print(kruskal_results %>% select(Variable, statistic, p, Epsilon_squared, Effect_magnitude))

# =========================================================
# 8.3) Test Post-hoc
# =========================================================
# POST-HOC : Dunn avec correction de Holm
posthoc_dunn <- df_profiles %>%
  pivot_longer(cols = all_of(vars_continuous),
               names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  dunn_test(Value ~ Profile, p.adjust.method = "holm") %>%
  add_significance("p.adj") %>%
  mutate(
    Variable = dplyr::recode(Variable,
                             "AgeMonths"  = "Age (months)",
                             "PA_Zscore"  = "Physical Activity (Z-score)"
    )
  )

cat("\n=== TESTS POST-HOC (DUNN avec Holm) ===\n")
print(posthoc_dunn)

# Export CSV
write_csv(posthoc_dunn, "PostHoc_Dunn_Profiles_3Groups_Children.csv")


# =========================================================
# 8.4) Test de Fisher pour le Sexe
# =========================================================

tab_sex_3groups <- table(df_profiles$Profile, df_profiles$Sex)
fisher_sex_3groups <- fisher_test(tab_sex_3groups) %>%
  mutate(Variable = "Sex", Test = "Fisher's Exact Test", .before = 1)

cat("\n=== TEST EXACT DE FISHER : SEXE (3 GROUPES) ===\n")
print(fisher_sex_3groups)

# Cramér's V
library(rcompanion)
cramers_v_3groups <- cramerV(tab_sex_3groups)
cat(paste("\nCramér's V pour Sexe (3 groupes) :", round(cramers_v_3groups, 3), "\n"))


# =========================================================
# 9) EXPORT CONSOLIDÉ DES RÉSULTATS
# =========================================================

# Formatage Fisher
fisher_formatted_3groups <- fisher_sex_3groups %>%
  mutate(
    p_formatted = format.pval(p, digits = 3, eps = 0.001),
    Cramers_V = cramers_v_3groups,
    Effect_magnitude = case_when(
      cramers_v_3groups < 0.1 ~ "negligible",
      cramers_v_3groups < 0.3 ~ "small",
      cramers_v_3groups < 0.5 ~ "moderate",
      TRUE ~ "large"
    ),
    statistic_kw = NA,
    df_kw = NA,
    Epsilon_squared = NA,
    n_total = NA
  ) %>%
  rename(p_value = p) %>%
  select(Variable, Test, statistic_kw, df_kw, n_total, p_value, p_formatted, 
         p.signif, Epsilon_squared, Cramers_V, Effect_magnitude)

# Combiner Kruskal + Fisher
all_stats_3groups <- bind_rows(
  kruskal_results %>% 
    mutate(
      Test = "Kruskal-Wallis",
      Cramers_V = NA,
      statistic_kw = statistic,
      df_kw = df
    ) %>%
    select(Variable, Test, statistic_kw, df_kw, n_total, p_value = p, 
           p_formatted, p.signif, Epsilon_squared, Cramers_V, Effect_magnitude),
  fisher_formatted_3groups
)

# Export CSV
write_csv(all_stats_3groups, "Statistical_Tests_Profiles_3Groups.csv")

cat("\n=== TABLEAU RÉCAPITULATIF DES TESTS (3 GROUPES) ===\n")
print(all_stats_3groups)

message("\n=== Statistiques exportées avec Kruskal-Wallis (non-paramétrique) ===")

message("\n=== SECTION V TERMINÉE ===")
message("Fichiers créés :")
message("  - Profiles_3Groups_Children.csv")
message("  - Table_Profiles_3Groups_Children.pdf / .png")
message("  - PostHoc_Profiles_3Groups_Children.csv")
message("  - Profiles_3Groups_Children_Comparison.png / .pdf")
message("  - Statistical_Tests_Profiles_3Groups.csv")