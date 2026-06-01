# ==============================================================================
# ANALYSE DES CLUSTERS : TRI AUTOMATIQUE PAR ÂGE (Cluster 1 = Les plus vieux)
# ==============================================================================

library(tidyverse)

# Dossier de travail
setwd("C:/Users/silve/Desktop/DOCTORAT/UNIV MONTREAL/TRAVAUX-THESE/Surfaces_Irregulieres/Datas/Script/gaitAnalysisGUI/result/Fig/Clustering")


files <- list.files(pattern = "DATA_FOR_STATS_R_.*\\.csv")
all_results <- list()

for (f in files) {
  
  # 1. Extraction du nom de la surface
  cond_name <- str_match(f, "DATA_FOR_STATS_R_(.*)_\\d")[,2]
  df <- read.csv(f, check.names = FALSE)
  
  # 2. LOGIQUE DE TRI AUTOMATIQUE PAR ÂGE
  # On calcule l'âge moyen par ClusterID actuel
  age_rank <- df %>%
    group_by(ClusterID) %>%
    summarise(mean_age = mean(AgeMonths, na.rm = TRUE)) %>%
    arrange(desc(mean_age)) %>%  # On trie du plus vieux au plus jeune
    mutate(NewClusterID = row_number()) # Le plus vieux devient 1, le suivant 2...
  
  # On applique ce nouveau classement au dataset
  df <- df %>%
    left_join(age_rank %>% select(ClusterID, NewClusterID), by = "ClusterID") %>%
    mutate(ClusterID = NewClusterID) %>%
    select(-NewClusterID)
  
  message(paste("✅ Clusters réorganisés par âge pour :", cond_name))
  
  # 2bis. Jointure avec les métadonnées
  df <- df %>%
    left_join(df_meta_clean, by = "Participant")
  
  # 3. Calcul des statistiques (Médiane [IQR])
  summary_table <- df %>%
    group_by(ClusterID) %>%
    summarise(
      Surface = cond_name,
      N = n(),
      # Formatage de toutes les colonnes numériques (incluant l'âge)
      across(where(is.numeric) & !contains("ClusterID"), 
             ~ paste0(format(round(median(.x, na.rm = TRUE), 3), nsmall = 3), 
                      " [", 
                      format(round(quantile(.x, 0.25, na.rm = TRUE), 3), nsmall = 3), 
                      "-", 
                      format(round(quantile(.x, 0.75, na.rm = TRUE), 3), nsmall = 3), 
                      "]"))
    ) %>%
    select(Surface, ClusterID, N, everything())
  
  all_results[[f]] <- summary_table
}

# 4. Fusion et Export
final_recensement <- bind_rows(all_results)
write.csv(final_recensement, "TABLEAU_RECAP_CLUSTERS_TRI_AGE.csv", row.names = FALSE)

cat("\n🚀 Terminé ! Le cluster 1 est désormais toujours le groupe le plus âgé.")


# ==============================================================================
# ANALYSE DES CLUSTERS : STATS + NORMALITÉ (SHAPIRO-WILK)
# ==============================================================================

library(tidyverse)
library(rstatix) # Pour un test de Shapiro facile sur des groupes

# Dossier de travail
setwd("C:/Users/silve/Desktop/DOCTORAT/UNIV MONTREAL/TRAVAUX-THESE/Surfaces_Irregulieres/Datas/Script/gaitAnalysisGUI/result/Fig/Clustering")

# Chargement des métadonnées (une seule fois)
df_meta <- read.csv(file.choose(), sep = ";", check.names = FALSE)
df_meta_clean <- df_meta %>%
  select(Participant, Sex)

files <- list.files(pattern = "DATA_FOR_STATS_R_.*\\.csv")
all_results <- list()
fisher_results <- list()

for (f in files) {
  
  # 1. Chargement et extraction du nom
  cond_name <- str_match(f, "DATA_FOR_STATS_R_(.*)_\\d")[,2]
  df <- read.csv(f, check.names = FALSE)
  
  # 2. Tri automatique par âge (Cluster 1 = Plus vieux)
  age_rank <- df %>%
    group_by(ClusterID) %>%
    summarise(mean_age = mean(AgeMonths, na.rm = TRUE)) %>%
    arrange(desc(mean_age)) %>%
    mutate(NewClusterID = row_number())
  
  df <- df %>%
    left_join(age_rank %>% select(ClusterID, NewClusterID), by = "ClusterID") %>%
    mutate(ClusterID = NewClusterID) %>%
    select(-NewClusterID)
  
  df <- df %>%
    left_join(df_meta_clean, by = "Participant")
  
  # 3. TEST DE NORMALITÉ (Shapiro-Wilk)
  # On pivote les données pour tester chaque variable dans chaque cluster
  normality_tests <- df %>%
    select(ClusterID, where(is.numeric)) %>%
    pivot_longer(cols = -ClusterID, names_to = "Variable", values_to = "Value") %>%
    group_by(Variable, ClusterID) %>%
    filter(n() >= 3) %>% # Shapiro nécessite au moins 3 observations
    shapiro_test(Value) %>%
    mutate(Distribution = ifelse(p > 0.05, "Normal", "Non-Normal")) %>%
    select(Variable, ClusterID, p_norm = p, Distribution)
  
  # 4. CALCUL DES STATS DESCRIPTIVES (Médiane [IQR])
  summary_stats <- df %>%
    group_by(ClusterID) %>%
    summarise(
      Surface = cond_name,
      N = n(),
      across(where(is.numeric) & !contains("ClusterID"), 
             ~ paste0(format(round(median(.x, na.rm = TRUE), 3), nsmall = 3), 
                      " [", 
                      format(round(quantile(.x, 0.25, na.rm = TRUE), 3), nsmall = 3), 
                      "-", 
                      format(round(quantile(.x, 0.75, na.rm = TRUE), 3), nsmall = 3), 
                      "]"))
    ) %>%
    pivot_longer(cols = -c(ClusterID, Surface, N), names_to = "Variable", values_to = "Stats_Median_IQR")
  
  # 5. JOINTURE STATS + NORMALITÉ
  combined_table <- left_join(summary_stats, normality_tests, by = c("Variable", "ClusterID"))
  
  all_results[[f]] <- combined_table
  
  # 6. Test de Fisher : Sexe ~ Cluster
  if ("Sex" %in% colnames(df)) {
    tab_sex <- table(df$ClusterID, df$Sex)
    fisher_sex <- fisher_test(tab_sex) %>%
      mutate(Variable = "Sex", Surface = cond_name)
    fisher_results[[f]] <- fisher_sex   
  }
}

# 7. Fusion finale et mise en forme pour la lecture
final_table <- bind_rows(all_results) %>%
  select(Surface, ClusterID, N, Variable, Stats_Median_IQR, p_norm, Distribution)
final_fisher <- bind_rows(fisher_results)
write.csv(final_fisher, "RESULTATS_FISHER_SEX_ClusterBYsurface.csv", row.names = FALSE)

# Export
write.csv(final_table, "RECENSEMENT_COMPLET_NORMALITE.csv", row.names = FALSE)

cat("\n✅ Fichier généré : RECENSEMENT_COMPLET_NORMALITE.csv")



# ==============================================================================
# ANALYSE DES CLUSTERS : STATS DESCRIPTIVES + TEST DE MANN-WHITNEY (SYSTÉMATIQUE)
# ==============================================================================

library(tidyverse)
library(rstatix)

# Dossier de travail
setwd("C:/Users/silve/Desktop/DOCTORAT/UNIV MONTREAL/TRAVAUX-THESE/Surfaces_Irregulieres/Datas/Script/gaitAnalysisGUI/result/Fig/Clustering")

files <- list.files(pattern = "DATA_FOR_STATS_R_.*\\.csv")
all_results <- list()

for (f in files) {
  
  # 1. Chargement et nom de la condition
  cond_name <- str_match(f, "DATA_FOR_STATS_R_(.*)_\\d")[,2]
  df <- read.csv(f, check.names = FALSE)
  
  # 2. Tri automatique par âge (Cluster 1 = Plus vieux)
  age_rank <- df %>%
    group_by(ClusterID) %>%
    summarise(mean_age = mean(AgeMonths, na.rm = TRUE)) %>%
    arrange(desc(mean_age)) %>%
    mutate(NewClusterID = row_number())
  
  df <- df %>%
    left_join(age_rank %>% select(ClusterID, NewClusterID), by = "ClusterID") %>%
    mutate(ClusterID = NewClusterID) %>%
    select(-NewClusterID)
  
  # 3. Calcul systématique du test de Mann-Whitney (Wilcoxon)
  comparison_stats <- df %>%
    select(ClusterID, where(is.numeric)) %>%
    pivot_longer(cols = -ClusterID, names_to = "Variable", values_to = "Value") %>%
    group_by(Variable) %>%
    wilcox_test(Value ~ ClusterID) %>%
    adjust_pvalue(method = "none") %>%
    add_significance("p") %>%
    select(Variable, p_wilcox = p, p_signif = p.signif)
  
  # AJOUT : Effect size (r rang biserial)
  effect_sizes <- df %>%
    select(ClusterID, where(is.numeric)) %>%
    pivot_longer(cols = -ClusterID, names_to = "Variable", values_to = "Value") %>%
    group_by(Variable) %>%
    wilcox_effsize(Value ~ ClusterID) %>%
    select(Variable, effsize, magnitude)
  
  # Jointure effect size sur les tests
  comparison_stats <- left_join(comparison_stats, effect_sizes, by = "Variable")
  
  # 4. Calcul des Médianes [IQR]
  summary_stats <- df %>%
    group_by(ClusterID) %>%
    summarise(
      Surface = cond_name,
      N = n(),
      across(where(is.numeric) & !contains("ClusterID"), 
             ~ paste0(format(round(median(.x, na.rm = TRUE), 3), nsmall = 3), 
                      " [", 
                      format(round(quantile(.x, 0.25, na.rm = TRUE), 3), nsmall = 3), 
                      "-", 
                      format(round(quantile(.x, 0.75, na.rm = TRUE), 3), nsmall = 3), 
                      "]"))
    ) %>%
    pivot_longer(cols = -c(ClusterID, Surface, N), names_to = "Variable", values_to = "Stats_Median_IQR")
  
  # 5. Fusion des résultats
  combined <- summary_stats %>%
    left_join(comparison_stats, by = "Variable")
  
  all_results[[f]] <- combined
}

# 6. Exportation Finale
final_recensement_mann_whitney <- bind_rows(all_results)
write.csv(final_recensement_mann_whitney, "RESULTATS_CLUSTERS_MANN_WHITNEY.csv", row.names = FALSE)

cat("\n✅ Analyse terminée avec tests de Mann-Whitney uniquement.")
cat("\nFichier généré : RESULTATS_CLUSTERS_MANN_WHITNEY.csv\n")


# ==============================================================================
# RADAR PLOTS PAR SURFACE (Plat / Medium / High)
# - Compare les clusters par surface
# - Montre les individus (gris)
# - Surbrillance des domaines (couleurs)
# - Normalisation GLOBALE (toutes surfaces) pour comparer les échelles
# - Affiche les valeurs RÉELLES à 25/50/75/100% sur CHAQUE axe (comme script 14)
# ==============================================================================

library(tidyverse)
library(fmsb)

# ------------------------------------------------------------
# 0) VARIABLES RADAR
# ------------------------------------------------------------
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
  "SI_Norm StepWidth (ua)",
  "CV_Norm StepWidth (ua)",
  "Mean_GVI (ua)",
  "CV_Gait speed (m.s^{-1})"
)

# ------------------------------------------------------------
# 1) Domaines + couleurs
# ------------------------------------------------------------
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
    "SI_Norm StepWidth (ua)"
  ),
  VARIABILITY = c(
    "CV_Norm StepWidth (ua)",
    "Mean_GVI (ua)",
    "CV_Gait speed (m.s^{-1})"
  )
)

domain_colors <- c(
  PACE = "lightblue",
  RHYTHM = "lightcoral",
  `POSTURAL CONTROL` = "palegreen",
  ASYMMETRY = "plum",
  VARIABILITY = "lightyellow"
)

# ------------------------------------------------------------
# 2) Charger + fusionner les DATA_FOR_STATS_R_*.csv (données individuelles)
# ------------------------------------------------------------
setwd("C:/Users/silve/Desktop/DOCTORAT/UNIV MONTREAL/TRAVAUX-THESE/Surfaces_Irregulieres/Datas/Script/gaitAnalysisGUI/result/Fig/Clustering")
files <- list.files(pattern = "DATA_FOR_STATS_R_.*\\.csv")

df_all <- map_dfr(files, function(f) {
  cond_name <- stringr::str_match(f, "DATA_FOR_STATS_R_(.*)_\\d")[,2]
  read.csv(f, check.names = FALSE) %>%
    mutate(Surface = cond_name)
})

# Harmonise Surface si "Plat_20260303" etc.
df_all <- df_all %>%
  mutate(Surface = gsub("_.*$", "", Surface))

stopifnot(all(c("Surface","ClusterID","AgeMonths") %in% names(df_all)))

# Variables présentes
vars_radar_present <- intersect(vars_radar, names(df_all))
if (length(vars_radar_present) < length(vars_radar)) {
  message("⚠️ Variables manquantes (ignorées) : ",
          paste(setdiff(vars_radar, vars_radar_present), collapse = ", "))
}
if (length(vars_radar_present) < 3) stop("Pas assez de variables radar présentes (<3).")

# Labels (tu peux remettre ceux du script 14 si tu veux)
# Labels affichés sur la figure radar (noms du .tif, dans le même ordre que vars_radar)
labels_map <- c(
  "Mean_Norm Gait Speed (m.s^{-1})"   = "Normalized gait speed (au)",
  "Mean_Norm Step length (ua)"         = "Normalized step length (au)",
  "Mean_Norm WR (ua)"                  = "Normalized walk ratio (au)",
  "Mean_Double support time (%)"       = "Double support time (%)",
  "Mean_Norm Cadence (ua)"             = "Normalized cadence (au)",
  "Mean_COM SPARC Magnitude (ua)"      = "SPARC (au)",
  "Mean_Norm StepWidth (ua)"           = "Normalized step width (au)",
  "Mean_MoS ML HS (%L0)"              = "MoS ML (%L0)",
  "Mean_MoS AP HS (%L0)"              = "MoS AP (%L0)",
  "SI_Stride length (m)"               = "SI stride length (%)",
  "SI_Double support time (%)"         = "SI double support time (%)",
  "SI_Norm StepWidth (ua)"             = "SI step width (%)",
  "CV_Norm StepWidth (ua)"             = "CV step width (%)",
  "Mean_GVI (ua)"                      = "GVI (au)",
  "CV_Gait speed (m.s^{-1})"          = "CV gait speed (%)"
)
radar_labels <- ifelse(vars_radar_present %in% names(labels_map),
                       labels_map[vars_radar_present],
                       vars_radar_present)

# ------------------------------------------------------------
# 3) Fonctions utilitaires domaines
# ------------------------------------------------------------
get_domain_for_vars <- function(vars_in_radar, domains_list) {
  dom_vec <- rep(NA_character_, length(vars_in_radar))
  names(dom_vec) <- vars_in_radar
  for (d in names(domains_list)) {
    dom_vec[vars_in_radar %in% domains_list[[d]]] <- d
  }
  dom_vec
}

draw_domain_background <- function(domains_by_var, domain_cols, alpha = 0.18, r = 1) {
  n <- length(domains_by_var)
  if (n < 3) return(invisible(NULL))
  
  angles <- seq(0, 2*pi, length.out = n + 1)[1:n] + (pi/2)
  bounds <- angles - (pi / n)
  bounds <- c(bounds, bounds[1] + 2*pi)
  
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
    a_start <- bounds[i1]
    a_end   <- bounds[i2 + 1]
    
    aa <- seq(a_start, a_end, length.out = 80)
    x <- c(0, r * cos(aa), 0)
    y <- c(0, r * sin(aa), 0)
    
    polygon(x, y, col = grDevices::adjustcolor(col, alpha.f = alpha), border = NA)
  }
  invisible(NULL)
}

# ------------------------------------------------------------
# 4) Min/Max GLOBAUX alignés (évite le mauvais ordre de unlist())
# ------------------------------------------------------------
range_global <- df_all %>%
  summarise(across(all_of(vars_radar_present),
                   list(min = ~min(.x, na.rm = TRUE),
                        max = ~max(.x, na.rm = TRUE))))

mins_raw <- setNames(as.numeric(range_global[1, paste0(vars_radar_present, "_min")]),
                     vars_radar_present)
maxs_raw <- setNames(as.numeric(range_global[1, paste0(vars_radar_present, "_max")]),
                     vars_radar_present)

# Retire les variables avec range global nul/non fini (sinon denom=0)
valid_vars <- vars_radar_present[
  is.finite(mins_raw[vars_radar_present]) &
    is.finite(maxs_raw[vars_radar_present]) &
    (maxs_raw[vars_radar_present] - mins_raw[vars_radar_present] > 0)
]

if (length(valid_vars) < length(vars_radar_present)) {
  message("⚠️ Variables exclues (range global nul/non fini) : ",
          paste(setdiff(vars_radar_present, valid_vars), collapse = ", "))
}

vars_radar_present <- valid_vars
radar_labels <- radar_labels[match(vars_radar_present, intersect(vars_radar, vars_radar_present))]  # robuste

if (length(vars_radar_present) < 3) stop("Après filtrage, <3 variables : radar impossible.")

# ------------------------------------------------------------
# 5) Fonction radar PAR SURFACE + TICKS RÉELS (25/50/75/100%)
# ------------------------------------------------------------
create_cluster_radar_surface <- function(df_surface, vars, labels, title_txt) {
  
  if (nrow(df_surface) == 0) {
    plot.new(); title(title_txt)
    text(0.5, 0.5, "Aucune donnée pour cette surface.")
    return(invisible(NULL))
  }
  
  # A) Médianes par cluster
  data_median <- df_surface %>%
    group_by(ClusterID) %>%
    summarise(across(all_of(vars), ~median(.x, na.rm = TRUE)), .groups = "drop") %>%
    arrange(ClusterID)
  
  if (nrow(data_median) < 1) {
    plot.new(); title(title_txt)
    text(0.5, 0.5, "Aucun cluster exploitable.")
    return(invisible(NULL))
  }
  
  # B) Individus
  data_indiv <- df_surface %>% select(ClusterID, all_of(vars))
  
  # C) Min/Max globaux alignés par NOM
  mins <- mins_raw[vars]
  maxs <- maxs_raw[vars]
  
  # D) Normalisation médianes (0–1) avec sécurité denom==0 -> 0.5
  radar_df_median <- as.data.frame(data_median[, setdiff(names(data_median), "ClusterID"), drop = FALSE])
  normalized_median <- as.data.frame(lapply(seq_len(ncol(radar_df_median)), function(i) {
    denom <- (maxs[[i]] - mins[[i]])
    if (!is.finite(denom) || denom == 0) return(rep(0.5, nrow(radar_df_median)))
    (radar_df_median[, i] - mins[[i]]) / denom
  }))
  colnames(normalized_median) <- labels
  
  final_radar_median <- rbind(rep(1, length(vars)), rep(0, length(vars)), normalized_median)
  
  # E) Normalisation individus (0–1)
  radar_df_indiv <- data_indiv[, setdiff(names(data_indiv), "ClusterID"), drop = FALSE]
  normalized_indiv <- as.data.frame(lapply(seq_len(ncol(radar_df_indiv)), function(i) {
    denom <- (maxs[[i]] - mins[[i]])
    if (!is.finite(denom) || denom == 0) return(rep(0.5, nrow(radar_df_indiv)))
    (radar_df_indiv[, i] - mins[[i]]) / denom
  }))
  colnames(normalized_indiv) <- labels
  
  # Nettoyage (au cas où)
  normalized_median[!is.finite(as.matrix(normalized_median))] <- 0.5
  normalized_indiv[!is.finite(as.matrix(normalized_indiv))] <- 0.5
  
  # F) Couleurs clusters (K clusters)
  K <- nrow(data_median)
  cluster_cols <- grDevices::hcl.colors(K, palette = "Dark 3")
  colors_border_median <- cluster_cols
  colors_in_median <- grDevices::adjustcolor(colors_border_median, alpha.f = 0.20)
  
  # =========================
  # G1) Cadre du radar (axes/grille)
  # =========================
  transparent <- grDevices::adjustcolor("white", alpha.f = 0)
  
  fmsb::radarchart(
    final_radar_median,
    axistype = 0,
    seg = 4,
    pcol  = rep(transparent, K),
    pfcol = rep(transparent, K),
    plwd  = rep(0.01, K),
    plty  = rep(1, K),
    cglcol = "grey70",
    cglty = 1,
    cglwd = 0.8,
    vlcex = 0.7,
    title = title_txt
  )
  
  # =========================
  # Préparation angles + ticks réels (comme script 14)
  # =========================
  nvar <- length(vars)
  angles <- seq(0, 2*pi, length.out = nvar + 1)[1:nvar] + (pi/2)
  
  pct <- c(0.25, 0.50, 0.75, 1.00)
  r_levels <- pct
  
  # ticks_real[j, i] = valeur réelle pour pct[j] sur variable i
  ticks_real <- sapply(seq_len(nvar), function(i) {
    mins[[i]] + pct * (maxs[[i]] - mins[[i]])
  })
  # ticks_real est une matrice (4 x nvar)
  ticks_real <- t(ticks_real)  # devient (nvar x 4) -> plus simple à lire
  
  # =========================
  # G2) Fond domaines
  # =========================
  domains_by_var <- get_domain_for_vars(vars, domains_vars)
  
  par(new = TRUE)
  draw_domain_background(
    domains_by_var = domains_by_var,
    domain_cols    = domain_colors,
    alpha          = 0.35,
    r              = 1
  )
  
  # =========================
  # G2bis) Affichage des ticks RÉELS sur chaque axe (25/50/75/100%)
  # =========================
  for (i in seq_len(nvar)) {
    angle <- angles[i]
    
    for (j in seq_along(pct)) {
      r <- r_levels[j]
      
      # position texte (légèrement plus loin que le cercle)
      x_pos <- r * cos(angle) * 1.05
      y_pos <- r * sin(angle) * 1.05
      
      val <- round(ticks_real[i, j], 2)
      
      text(
        x = x_pos, y = y_pos,
        labels = val,
        cex = 0.48,
        col = "grey20",
        font = 1
      )
    }
  }
  
  # =========================
  # G3) Individus (gris)
  # =========================
  indiv_col <- grDevices::adjustcolor("grey30", alpha.f = 0.10)
  
  if (nrow(normalized_indiv) > 0) {
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
  }
  
  # =========================
  # G4) Médianes clusters (devant)
  # =========================
  par(new = TRUE)
  fmsb::radarchart(
    final_radar_median,
    axistype = 0,
    vlabels = rep("", length(vars)),
    pcol = colors_border_median,
    pfcol = colors_in_median,
    plwd = rep(3.5, K),
    plty = rep(1, K),
    cglcol = NA,
    axislabcol = NA,
    vlcex = 0,
    seg = length(vars)
  )
  
  legend(
    x = "bottom",
    legend = paste0("Cluster ", data_median$ClusterID),
    inset = -0.15,
    horiz = TRUE,
    bty = "n",
    pch = 20,
    col = colors_border_median,
    text.col = "black",
    cex = 0.8,
    pt.cex = 1.5,
    xpd = TRUE
  )
}

# ------------------------------------------------------------
# 6) Loop surfaces + tri ClusterID par âge + panel 1x3 + exports
# ------------------------------------------------------------
surfaces <- c("Plat", "Medium", "High")
surfaces <- surfaces[surfaces %in% unique(df_all$Surface)]
if (length(surfaces) == 0) stop("Aucune surface Plat/Medium/High trouvée.")

plot_panel <- function(device_open_fun, device_close_fun) {
  
  device_open_fun()
  par(mfrow = c(1, length(surfaces)), mar = c(2.5, 2.5, 4, 2.5), oma = c(0, 0, 2, 0))
  
  for (s in surfaces) {
    
    df_s <- df_all %>% filter(Surface == s)
    
    # Tri ClusterID par âge (Cluster 1 = plus vieux) POUR CETTE SURFACE
    age_rank <- df_s %>%
      group_by(ClusterID) %>%
      summarise(mean_age = mean(AgeMonths, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(mean_age)) %>%
      mutate(NewClusterID = row_number())
    
    df_s <- df_s %>%
      left_join(age_rank %>% select(ClusterID, NewClusterID), by = "ClusterID") %>%
      mutate(ClusterID = NewClusterID) %>%
      select(-NewClusterID)
    
    create_cluster_radar_surface(
      df_surface = df_s,
      vars = vars_radar_present,
      labels = radar_labels[seq_along(vars_radar_present)],
      title_txt = paste0("Surface: ", s)
    )
  }
  
  mtext("Radar plots — Clusters compared within each surface (individuals + domains)",
        outer = TRUE, cex = 1.1, font = 2)
  
  device_close_fun()
}

# PDF
plot_panel(
  device_open_fun = function() pdf("Radar_Clusters_BySurface.pdf", width = 18, height = 6),
  device_close_fun = function() dev.off()
)

# PNG HD
plot_panel(
  device_open_fun = function() png("Radar_Clusters_BySurface.png", width = 6000, height = 2000, res = 300, type = "cairo"),
  device_close_fun = function() dev.off()
)

# ==============================================================================
# EXPORTS PNG INDIVIDUELS (1 fichier par surface)
# ==============================================================================

for (s in surfaces) {
  
  # Filtre surface
  df_s <- df_all %>% filter(Surface == s)
  
  # Tri ClusterID par âge (Cluster 1 = plus vieux) POUR CETTE SURFACE
  age_rank <- df_s %>%
    group_by(ClusterID) %>%
    summarise(mean_age = mean(AgeMonths, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(mean_age)) %>%
    mutate(NewClusterID = row_number())
  
  df_s <- df_s %>%
    left_join(age_rank %>% select(ClusterID, NewClusterID), by = "ClusterID") %>%
    mutate(ClusterID = NewClusterID) %>%
    select(-NewClusterID)
  
  # Nom de fichier propre
  out_name <- paste0("Radar_Clusters_Surface_", s, ".png")
  
  # PNG haute qualité (1 radar)
  png(filename = out_name, width = 2400, height = 2400, res = 300, type = "cairo")
  
  op <- par(no.readonly = TRUE)
  par(mfrow = c(1, 1), mar = c(2.5, 2.5, 4, 2.5), oma = c(0, 0, 0, 0))
  par(xaxs = "i", yaxs = "i", xpd = NA, cex = 0.95)
  
  create_cluster_radar_surface(
    df_surface = df_s,
    vars = vars_radar_present,
    labels = radar_labels[seq_along(vars_radar_present)],
    title_txt = paste0("Surface: ", s)
  )
  
  par(op)
  dev.off()
}

message("✅ PNG individuels créés : Radar_Clusters_Surface_Plat.png / Medium / High")


message("✅ Fichiers créés : Radar_Clusters_BySurface.pdf et Radar_Clusters_BySurface.png")

message("✅ Fichiers créés : Radar_Clusters_BySurface.pdf et Radar_Clusters_BySurface.png")