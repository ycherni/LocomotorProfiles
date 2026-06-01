## Script en 3 parties 
# I. Stat.descriptives pop.
# II. Comparaison STP variables & tableaux/fig. descriptifs
# III. Calcul du Gait Adaptation Score
# IV. LMM & Post-hocs sur variables d'intérêt

## ============================================================
## I. STATISTIQUES DESCRIPTIVES POPULATION
## ============================================================
## =========================================================
## =========================================================
## Table 1 (descriptifs) à partir de participants_metadata.csv
## - Quantitatives : moyenne ± ET
## - Qualitatives  : n (%)
## - Colonnes : Global (Sexe uniquement) + par AgeGroup
## =========================================================

setwd("XX") # vers les résultats

library(readr)
library(dplyr)
library(stringr)
library(gtsummary)
library(gt)
library(janitor)
library(performance)
library(tidyverse)

chemin <- "XX" #Vers la sortie des LMM

# 0) Import + nettoyage des noms (snake_case)
df <- read.csv(
  file.path(chemin, "participants_metadata.csv"),
  sep = ";",
  check.names = FALSE,
  stringsAsFactors = FALSE
) %>%
  janitor::clean_names()

# 1) Typage + facteurs
df <- df %>%
  mutate(
    participant = as.character(participant),
    age_group = factor(
      age_group,
      levels = c("Jeunes Enfants","Enfants","Adolescents","Adultes"),
      labels = c("Young children", "Children", "Adolescents", "Adults")
    ),
    sex         = factor(sex, levels = c("F","M")),
    age_months  = as.numeric(age_months),
    height_cm   = as.numeric(height_cm),
    weight_kg   = as.numeric(weight_kg),
    l0_m        = as.numeric(l0_m),
    imc         = as.numeric(imc)
  )

# 2) Construction de la Table 1 en deux parties

# A. Partie Sexe (Qualitative) - SANS add_p()
tab_sex <- df %>%
  select(age_group, sex) %>%
  tbl_summary(
    by = age_group,
    label = list(sex ~ "Sex"),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_categorical() ~ c(0, 1)),
    missing = "no"
  ) %>%
  add_overall(last = FALSE, col_label = "**Global**")

# B. Partie Anthropométrique (Quantitative) + p-value
tab_others <- df %>%
  select(age_group, age_months, height_cm, weight_kg, l0_m, imc) %>%
  tbl_summary(
    by = age_group,
    label = list(
      age_months ~ "Age (months)",
      height_cm  ~ "Height (cm)",
      weight_kg  ~ "Weight (kg)",
      l0_m       ~ "L0 (m)",
      imc        ~ "BMI (kg/m²)"
    ),
    statistic = list(all_continuous() ~ "{mean} ± {sd}"),
    digits = list(all_continuous() ~ 2),
    missing = "no"
  ) %>%
  add_p(test = all_continuous() ~ "kruskal.test")

# C. Fusion et Nettoyage des indices
tab1_final <- tbl_stack(list(tab_sex, tab_others)) %>%
  bold_labels() %>%
  modify_header(
    label ~ "**Variable**",
    p.value ~ "**p-value**"
  ) %>%
  modify_spanning_header(all_stat_cols() ~ "**Age groups**") %>%
  modify_footnote(all_stat_cols() ~ "n (%), Mean ± SD")

# Affichage du résultat
tab1_final

# 3) Calculs de vérification
age_counts <- df %>% count(age_group)
sex_by_age <- df %>% count(age_group, sex)

print(age_counts)
print(sex_by_age)

# 4) Export HTML
as_gt(tab1_final) %>% gt::gtsave("Table1_participants.html")

# 5) Export pdf
# Convertir le tableau gtsummary en objet gt
gt_table <- as_gt(tab1_final)
# Sauvegarde en PDF (Qualité vectorielle parfaite)
gt::gtsave(gt_table, "Table1_participants.pdf")

print("Parti I effectuée avec succés ! On a la Table 1")

## 6) FIGURE PANEL ANTHROPOMÉTRIQUE GLOBAL (SANS SEXE)
library(ggplot2)
library(patchwork)

# Palette de couleurs pour les groupes d'âge
age_colors <- c("Young children" = "#FF9999", 
                "Children"       = "#66B2FF", 
                "Adolescents"    = "#99FF99", 
                "Adults"         = "#FFCC99")

# Fonction pour créer un subplot global
plot_anthro_global <- function(var_name, label_y) {
  ggplot(df %>% filter(!is.na(.data[[var_name]])), 
         aes(x = age_group, y = .data[[var_name]], fill = age_group)) +
    # On utilise un violin plot + boxplot pour bien voir la distribution
    geom_violin(alpha = 0.3, color = NA) +
    geom_boxplot(width = 0.2, alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.1, size = 1, alpha = 0.4, color = "grey30") +
    labs(title = label_y, x = "", y = "") +
    scale_fill_manual(values = age_colors) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 1, size = 10),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

# Création des 4 panneaux
p1_g <- plot_anthro_global("height_cm", "Height (cm)")
p2_g <- plot_anthro_global("weight_kg", "Weight (kg)")
p3_g <- plot_anthro_global("l0_m",      "L0 (m)")
p4_g <- plot_anthro_global("imc",       "BMI (kg/m²)")

# Assemblage du panel (2x2)
global_anthro_panel <- (p1_g | p2_g) / (p3_g | p4_g)

global_anthro_panel <- global_anthro_panel + 
  plot_annotation(
    title = "Anthropometric Characteristics across Age Groups",
    subtitle = "Global distribution (N = total sample)",
    caption = "Boxplots represent medians and quartiles; Violin plots show the density distribution."
  )

# Sauvegarde
ggsave("Figure_Table1_Anthro_Global.png", global_anthro_panel, width = 10, height = 9, dpi = 300)

## ============================================================
## I.1 bis) TABLEAU DESCRIPTIF PAR SEXE
## ============================================================

tab1_bis_sex <- df %>%
  select(age_group, sex, age_months, height_cm, weight_kg, l0_m, imc) %>%
  tbl_summary(
    by = sex,  # Stratification par sexe
    include = -age_group, # On enlève age_group du corps mais on l'utilise pour le filtrage
    label = list(
      age_months ~ "Age (months)",
      height_cm  ~ "Height (cm)",
      weight_kg  ~ "Weight (kg)",
      l0_m       ~ "L0 (m)",
      imc        ~ "BMI (kg/m²)"
    ),
    statistic = list(all_continuous() ~ "{mean} ± {sd}"),
    digits = list(all_continuous() ~ 2),
    missing = "no"
  ) %>%
  add_p(test = all_continuous() ~ "t.test") %>% # Comparaison F vs M
  bold_labels() %>%
  modify_header(label ~ "**Variable**", p.value ~ "**p-value (F vs M)**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Sex**")

# Pour avoir ce tableau par groupe d'âge (plus précis) :
tab1_bis_stratified <- df %>%
  select(age_group, sex, age_months, height_cm, weight_kg, l0_m, imc) %>%
  tbl_strata(
    strata = age_group,
    .tbl_fun = ~ .x %>%
      tbl_summary(
        by = sex,
        statistic = list(all_continuous() ~ "{mean} ± {sd}"),
        missing = "no"
      ) %>%
      add_p()
  )

# Export
as_gt(tab1_bis_stratified) %>% gt::gtsave("Table1_bis_Anthro_Sexe.pdf")


## ============================================================
## I.2 bis) FIGURE PANEL ANTHROPOMÉTRIQUE
## ============================================================
library(ggplot2)
library(patchwork)

# Préparation des données long format pour faciliter le plotting
df_long <- df %>%
  select(age_group, sex, height_cm, weight_kg, l0_m, imc) %>%
  pivot_longer(cols = c(height_cm, weight_kg, l0_m, imc), 
               names_to = "measure", values_to = "value") %>%
  mutate(measure = case_when(
    measure == "height_cm" ~ "Height (cm)",
    measure == "weight_kg" ~ "Weight (kg)",
    measure == "l0_m"      ~ "L0 (m)",
    measure == "imc"       ~ "BMI (kg/m²)"
  ))

# Fonction pour créer un subplot par mesure
plot_anthro <- function(var_name, color_hex) {
  ggplot(df %>% filter(!is.na(.data[[var_name]])), 
         aes(x = age_group, y = .data[[var_name]], fill = sex)) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA) +
    geom_point(aes(color = sex), position = position_jitterdodge(jitter.width = 0.1), size = 1) +
    labs(title = var_name, x = "", y = "") +
    scale_fill_manual(values = c("F" = "#F8766D", "M" = "#00BFC4")) +
    scale_color_manual(values = c("F" = "#F8766D", "M" = "#00BFC4")) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 30, hjust = 1))
}

# Création des graphiques individuels
p_height <- plot_anthro("height_cm", "#F8766D") + labs(title = "Height (cm)")
p_weight <- plot_anthro("weight_kg", "#00BFC4") + labs(title = "Weight (kg)")
p_l0     <- plot_anthro("l0_m", "#7CAE00")      + labs(title = "L0 (m)")
p_imc    <- plot_anthro("imc", "#C77CFF")       + labs(title = "BMI (kg/m²)")

# Assemblage du panel
anthro_panel <- (p_height | p_weight) / (p_l0 | p_imc) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

anthro_panel <- anthro_panel + 
  plot_annotation(title = "Evolution of Anthropometric Data by Age Group and Sex",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

# Sauvegarde
ggsave("Figure_Anthro_Panel.png", anthro_panel, width = 10, height = 8, dpi = 300)


## ============================================================
## II. MOYENNE +- SD DES VARIABLES SPT
## ============================================================

# ---------------------------------------------------------
# 0) Préparation de l'environnement
# ---------------------------------------------------------
setwd('XX') # Chemin des résultats

library(tidyverse)
library(readr)
library(gt)

# ---------------------------------------------------------
# 1) Chargement du fichier de données
# ---------------------------------------------------------
file_path <- "XX"   # Sur le CSV de la grande matrice

# Lecture robuste (gère séparateur ; ou ,)
first_line <- readLines(file_path, n = 1, warn = FALSE)
delim <- ifelse(grepl(";", first_line), ";", ",")
df <- read_delim(file_path, delim = delim, show_col_types = FALSE)

# ---------------------------------------------------------
# 2) Standardisation des noms de colonnes (pour matcher variables_interet)
#    Objectif : uniformiser les unités et remplacer espaces/ponctuation par "_"
# ---------------------------------------------------------
standardize_names <- function(x) {
  x %>%
    # 1) trim + remplacer espaces / ponctuation par _
    str_trim() %>%
    str_replace_all("[[:space:]]+", "_") %>%
    str_replace_all("[\\-]+", "_") %>%
    str_replace_all("[,;:]+", "_") %>%
    
    # 2) unités / parenthèses -> suffixes normalisés
    str_replace_all("\\(mm\\)", "mm") %>%
    str_replace_all("\\(%L0\\)", "pL0") %>%
    str_replace_all("\\(ua\\)", "ua") %>%
    str_replace_all("\\(%\\)", "p") %>%
    str_replace_all("\\(m\\.s\\^\\{-1\\}\\)", "ms1") %>%
    str_replace_all("\\(step\\.min\\^\\{-1\\}\\)", "stepmin1") %>%
    
    # 3) enlever parenthèses restantes
    str_replace_all("[\\(\\)]", "") %>%
    
    # 4) cleanup underscores
    str_replace_all("__+", "_") %>%
    str_replace_all("_$", "")
}

names(df) <- standardize_names(names(df))

# Optionnel : vérifier les noms standardisés
# print(names(df))

# ---------------------------------------------------------
# 3) Définition des variables d’intérêt (dans l’ordre souhaité)
# ---------------------------------------------------------
variables_interet <- c(
  "NCycles_Left", "NCycles_Right",
  
  "Mean_Gait_speed_m.s^{_1}", "Mean_Norm_Gait_Speed_m.s^{_1}", "Mean_Step_length_m", "Mean_Stride_length_m",  "Mean_Norm_Step_length_ua", "Mean_WalkRatio", "Mean_Norm_WR_ua", "Mean_Stride_time_s", "Mean_StepTime_s",
  
  "Mean_Double_support_time_p", "Mean_Cadence_step.min^{_1}", "Mean_Norm_Cadence_ua", "Mean_COM_SPARC_Magnitude_ua", "Mean_StepTime_s", "Mean_StanceTime_s", "Mean_SwingTime_s",
  
  "Mean_StepWidth_cm", "Mean_Norm_StepWidth_ua", "Mean_MoS_AP_HS_mm", "Mean_MoS_ML_HS_mm", "Mean_MoS_AP_Stance_mm", "Mean_MoS_ML_Stance_mm", "Mean_MoS_AP_HS_pL0", "Mean_MoS_ML_HS_pL0", "Mean_MoS_AP_Stance_pL0", "Mean_MoS_ML_Stance_pL0", "Mean_StanceTime_s", "Mean_Single_support_time_p", "Mean_SwingTime_s",
  
  "Mean_GVI_ua", "CV_Norm_StepWidth_ua", "CV_Gait_speed_m.s^{_1}",
  
  "SI_Stride_length_m", "SI_Double_support_time_p", "SI_Norm_StepWidth_ua"
)

# ---------------------------------------------------------
# 4) Identification automatique des colonnes AgeGroup et Surface
# ---------------------------------------------------------
age_candidates <- c("AgeGroup", "Groupe", "Age_Group", "AgeGroup.x", "AgeGrp")
surf_candidates <- c("Surface", "Condition", "Surf")

age_col  <- intersect(age_candidates, names(df))[1]
surf_col <- intersect(surf_candidates, names(df))[1]

if (is.na(age_col) || is.na(surf_col)) {
  stop(
    "Impossible de trouver les colonnes AgeGroup / Surface.\n",
    "Colonnes dispo: ", paste(names(df), collapse = ", "), "\n",
    "Renomme tes colonnes ou modifie age_candidates / surf_candidates."
  )
}

# ---------------------------------------------------------
# 5) Vérification des variables disponibles (présentes vs absentes)
# ---------------------------------------------------------
vars_present <- intersect(variables_interet, names(df))
vars_missing <- setdiff(variables_interet, names(df))

if (length(vars_missing) > 0) {
  message("Variables absentes dans le fichier (ignorées) :\n- ", paste(vars_missing, collapse = "\n- "))
}
if (length(vars_present) == 0) stop("Aucune variable_interet trouvée dans le fichier.")

# ---------------------------------------------------------
# 6) Passage au format long + calcul des descriptifs (moyenne, SD, n, IC95%)
# ---------------------------------------------------------
df_long <- df %>%
  mutate(
    AgeGroup = as.factor(.data[[age_col]]),
    Surface  = as.factor(.data[[surf_col]])
  ) %>%
  filter(Surface %in% c("Plat", "Medium", "High")) %>%
  select(AgeGroup, Surface, all_of(vars_present)) %>%
  pivot_longer(cols = all_of(vars_present), names_to = "Variable", values_to = "Value") %>%
  mutate(Value = suppressWarnings(as.numeric(Value)))

desc <- df_long %>%
  group_by(AgeGroup, Surface, Variable) %>%
  summarise(
    n     = sum(!is.na(Value)),
    mean  = mean(Value, na.rm = TRUE),
    sd    = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Calcul de l'IC95%
    sem = sd / sqrt(n),
    ci_lower = mean - (1.96 * sem),
    ci_upper = mean + (1.96 * sem),
    
    # Mise en forme de la cellule : Moyenne ± SD [IC95_bas, IC95_haut]
    cell = ifelse(
      n == 0, "",
      sprintf("%.2f \u00B1 %.2f [%.2f, %.2f]", mean, sd, ci_lower, ci_upper)
    )
  ) %>%
  select(AgeGroup, Surface, Variable, cell)

# ---------------------------------------------------------
# 7) Mise en forme "large" : colonnes = (AgeGroup x Surface)
# ---------------------------------------------------------
tab_wide <- desc %>%
  mutate(col = paste0(as.character(AgeGroup), "___", as.character(Surface))) %>%
  select(Variable, col, cell) %>%
  pivot_wider(names_from = col, values_from = cell) %>%
  arrange(match(Variable, variables_interet))

# ---------------------------------------------------------
# 8) Fonction d'étiquetage
#    - retire "Mean_"
#    - applique unités (mm, cm, m, s, %, %L0, ua, m/s, step/min)
#    - CV -> "C.V ... (%)"
#    - SI -> "S.I ..." (sans unité)
#    - remplace "_" par espaces
# ---------------------------------------------------------
make_pretty_label <- function(varname) {
  
  v <- varname
  
  # 1) Retirer le préfixe Mean_ dans l'affichage
  v <- str_replace(v, "^Mean_", "")
  
  # SI / CV : format publication
  is_cv <- str_detect(v, "^CV_")
  is_si <- str_detect(v, "^SI_")
  
  # Retirer "Norm_" pour CV et SI uniquement
  if (is_cv || is_si) {
    v <- str_replace(v, "Norm_", "")
    v <- str_replace(v, "_ua$", "")
  }
  
  if (is_cv) {
    v <- str_replace(v, "^(CV_[^_]+_[^_]+).*$", "\\1")
    v <- str_replace(v, "^CV_", "")
    v <- paste0("C.V. ", v, " (%)")
  }
  
  if (is_si) {
    v <- str_replace(v, "^(SI_[^_]+_[^_]+).*$", "\\1")
    v <- str_replace(v, "^SI_", "")
    v <- paste0("S.I. ", v, " (%)")
  }
  
  # 2) Unités (suffixes) -> format publication
  v <- str_replace(v, "_mm$", " (mm)")
  v <- str_replace(v, "_cm$", " (cm)")
  v <- str_replace(v, "_m$",  " (m)")
  v <- str_replace(v, "_s$",  " (s)")
  v <- str_replace(v, "_pL0$", " (%L0)")
  v <- str_replace(v, "_p$",   " (%)")
  v <- str_replace(v, "_ua$",  " (au)")  # <-- MODIF (i): ua -> au
  
  # Vitesse / cadence : gérer variantes "ms1" / "m.s^{-1}" / "m.s^{_1}"
  v <- str_replace(v, "_ms1$", " (m/s)")
  v <- str_replace(v, "_m\\.s\\^\\{-1\\}$", " (m/s)")
  v <- str_replace(v, "_m\\.s\\^\\{_1\\}$", " (m/s)")
  v <- str_replace(v, "_m\\.s\\^\\{\\-1\\}$", " (m/s)")
  v <- str_replace(v, "_m\\.s\\^\\{\\-?1\\}$", " (m/s)")
  
  v <- str_replace(v, "_stepmin1$", " (step/min)")
  v <- str_replace(v, "_step\\.min\\^\\{-1\\}$", " (step/min)")
  v <- str_replace(v, "_step\\.min\\^\\{_1\\}$", " (step/min)")
  v <- str_replace(v, "_step\\.min\\^\\{\\-1\\}$", " (step/min)")
  v <- str_replace(v, "_step\\.min\\^\\{\\-?1\\}$", " (step/min)")
  
  # 3) Exception demandée : Norm Gait Speed doit être (au)
  v <- str_replace(v, "^Norm Gait Speed \\(m/s\\)$", "Norm Gait Speed (au)")
  v <- str_replace(v, "^Norm_Gait_Speed \\(m/s\\)$", "Norm Gait Speed (au)")
  v <- str_replace(v, "^Norm_Gait_Speed$", "Norm Gait Speed (au)")
  v <- str_replace(v, "^Norm Gait Speed$", "Norm Gait Speed (au)")
  
  # <-- MODIF (iii): Walk Ratio en cm.min.pas^-1
  v <- str_replace(v, "^WalkRatio$", "Walk Ratio (cm.min.step⁻¹)")
  v <- str_replace(v, "^Walk_?Ratio$", "Walk Ratio (cm.min.step⁻¹)")
  
  # <-- MODIF (iv): COM SPARC Magnitude -> SPARC
  v <- str_replace(v, "^COM SPARC Magnitude \\(au\\)$", "SPARC (au)")
  v <- str_replace(v, "^COM_SPARC_Magnitude \\(au\\)$", "SPARC (au)")
  v <- str_replace(v, "^COM_SPARC_Magnitude$", "SPARC (au)")
  
  # 4) Rendre lisible : underscores -> espaces
  v <- str_replace_all(v, "_", " ")
  v <- str_replace(v, "^StanceTime \\(s\\)$", "Stance Time (s)")
  v <- str_replace(v, "^SwingTime \\(s\\)$",  "Swing Time (s)")
  v <- str_replace(v, "^StepTime \\(s\\)$",   "Step Time (s)")
  v <- str_replace(v, "^StepWidth \\(cm\\)$",   "Step Width (cm)")
  
  # 5) Petites mises en forme (optionnel mais utile pour article)
  v <- str_replace_all(v, "\\bAP\\b", "AP")
  v <- str_replace_all(v, "\\bML\\b", "ML")
  
  # <-- MODIF (v): Retirer "HS" des MoS
  v <- str_replace_all(v, "\\bHS\\b", "")
  v <- str_replace_all(v, "  +", " ")  # nettoyer les doubles espaces
  
  # Double support time : harmoniser
  v <- str_replace_all(v, "Double support time", "Double support time")
  v <- str_replace_all(v, "Double support", "Double support")
  
  # SI / CV : garder le préfixe explicite
  # v <- str_replace(v, "^SI ", "S.I. ")
  # v <- str_replace(v, "^C\\.V ", "C.V. ")
  
  # NCycles : rendre plus propre
  v <- str_replace(v, "^NCycles Left$", "N cycles Left")
  v <- str_replace(v, "^NCycles Right$", "N cycles Right")
  
  # Trim final
  v <- str_trim(v)
  
  return(v)
}

# ---------------------------------------------------------
# 9) Forcer l'ordre des colonnes : Groupes d'âge puis Surfaces
#    (important : gt ne réordonne pas les colonnes, il faut le faire avant)
# ---------------------------------------------------------
age_order <- c("JeunesEnfants", "Enfants", "Adolescents", "Adultes")
surface_levels <- c("Plat", "Medium", "High")

wanted_cols <- as.vector(outer(age_order, surface_levels, paste, sep = "___"))
wanted_cols <- wanted_cols[wanted_cols %in% names(tab_wide)]  # garde seulement celles présentes

tab_wide <- tab_wide %>%
  select(Variable, all_of(wanted_cols))

# ---------------------------------------------------------
# 10) Construction du mapping : nom technique -> label publication
# ---------------------------------------------------------
variable_labels <- setNames(
  vapply(tab_wide$Variable, make_pretty_label, character(1)),
  tab_wide$Variable
)

# ---------------------------------------------------------
# 11) Création de la table GT + application des labels "publication"
# ---------------------------------------------------------
gt_tbl <- gt(tab_wide) %>%
  cols_label(Variable = "Variables") %>%
  tab_options(table.font.size = px(12)) %>%
  text_transform(
    locations = cells_body(columns = Variable),
    fn = function(x) unname(variable_labels[x])
  )

# ---------------------------------------------------------
# 12) Ajout des spanners : Groupes d’âge -> sous-colonnes Surface
# ---------------------------------------------------------
col_names <- setdiff(names(tab_wide), "Variable")

age_levels <- age_order[age_order %in% sub("___.*$", "", col_names)]
surface_levels <- c("Plat", "Medium", "High")

# Traduction des groupes d'âge (affichage uniquement)
age_labels_en <- c(
  JeunesEnfants = "Young Children",
  Enfants       = "Children",
  Adolescents   = "Adolescents",
  Adultes       = "Adults"
)

surface_labels_en <- c(
  Plat   = "Even",
  Medium = "Medium",
  High   = "High"
)

# (A) Renommer les sous-colonnes
for (cn in col_names) {
  surf <- sub("^.*___", "", cn)
  gt_tbl <- gt_tbl %>% cols_label(!!cn := surface_labels_en[surf])
}

# (B) Ajouter les spanners par groupe d’âge, dans l’ordre défini
for (ag in age_levels) {
  cols_ag <- paste0(ag, "___", surface_levels)
  cols_ag <- cols_ag[cols_ag %in% col_names]
  
  gt_tbl <- gt_tbl %>%
    tab_spanner(label = age_labels_en[ag], columns = all_of(cols_ag))
}

# ---------------------------------------------------------
# 13) Affichage & Export
# ---------------------------------------------------------
gt_tbl <- gt_tbl %>%
  tab_header(
    title = "Descriptive values of gait parameters according to age group and walking surface",
    subtitle = "Values are reported as Mean ± SD (n)"
  )

gt_tbl
View(gt_tbl)

gtsave(gt_tbl, "Table_Descriptive_Gait_AgeGroup_Surface.pdf")

# ---------------------------------------------------------
# 14) Construction figures de l'ensemble des variables d'intérêt
#     (Surface en abscisse)
# ---------------------------------------------------------

output_dir <- "Boxplots_Gait_Results"
if (!dir.exists(output_dir)) dir.create(output_dir)

generate_gait_boxplot <- function(var_name, data) {
  
  # Label "propre" via ta fonction existante
  pretty_title <- make_pretty_label(var_name)
  
  # Préparation des données
  df_plot <- data %>%
    select(AgeGroup, Surface, all_of(var_name)) %>%
    rename(Value = !!sym(var_name)) %>%
    mutate(
      Value = as.numeric(as.character(Value)),
      AgeGroup = factor(
        AgeGroup,
        levels = c("JeunesEnfants", "Enfants", "Adolescents", "Adultes"),
        labels = c("Young Children", "Children", "Adolescents", "Adults")
      ),
      Surface = factor(
        Surface,
        levels = c("Plat", "Medium", "High"),
        labels = c("Even", "Medium", "High")
      )
    ) %>%
    filter(!is.na(Value))
  
  # Création du graphique : Surface en X, fill = AgeGroup
  p <- ggplot(df_plot, aes(x = Surface, y = Value, fill = AgeGroup)) +
    geom_boxplot(
      position = position_dodge(0.85), width = 0.7,
      alpha = 0.7, outlier.shape = NA, color = "black"
    ) +
    geom_jitter(
      aes(group = AgeGroup),
      position = position_dodge(0.85),
      size = 1.2, alpha = 0.3, color = "black"
    ) +
    # Couleurs des âges
    scale_fill_manual(values = c(
      "Young Children" = "blue",
      "Children"       = "chocolate3",
      "Adolescents"    = "darkred",
      "Adults"         = "purple"
    )) +
    labs(
      title = paste(pretty_title, "across surfaces and age groups"),
      y = pretty_title,
      x = "Surface",
      fill = "Age Group"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "bottom",
      panel.grid.major.x = element_blank()
    )
  
  # Sauvegarde automatique
  file_name_png <- file.path(output_dir, paste0(var_name, "_SurfaceX.png"))
  ggsave(file_name_png, plot = p, width = 8, height = 6, dpi = 300)
  
  file_name_pdf <- file.path(output_dir, paste0(var_name, "_SurfaceX.pdf"))
  ggsave(file_name_pdf, plot = p, width = 10, height = 8)
  
  return(p)
}

message("Génération des boxplots (Surface en X) en cours...")
walk(vars_present, ~generate_gait_boxplot(.x, df))
message("Terminé ! Les graphiques sont dans le dossier : ", output_dir)

## ============================================================
## 14bis) PANELS PAR DOMAINE — BOXPLOTS + MÉDIANES RELIÉES
##        (médiane de chaque groupe reliée entre les 3 surfaces)
## ============================================================

library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)

output_dir_panels <- "Boxplots_Gait_Results/Panels_Domaines"
if (!dir.exists(output_dir_panels)) dir.create(output_dir_panels, recursive = TRUE)

# ------------------------------------------------------------------
# A) Définition des domaines et de leurs variables
# ------------------------------------------------------------------
domains_boxplot <- list(
  
  PACE = c(
    "Mean_Gait_speed_m.s^{_1}",
    "Mean_Norm_Gait_Speed_m.s^{_1}",
    "Mean_Step_length_m",
    "Mean_Stride_length_m",
    "Mean_Norm_Step_length_ua",
    "Mean_WalkRatio",
    "Mean_Norm_WR_ua"
  ),
  
  RHYTHM = c(
    "Mean_Double_support_time_p",
    "Mean_Single_support_time_p",
    "Mean_StanceTime_s",
    "Mean_SwingTime_s",
    "Mean_StepTime_s",
    "Mean_Stride_time_s",
    "Mean_Cadence_step.min^{_1}",
    "Mean_Norm_Cadence_ua",
    "Mean_COM_SPARC_Magnitude_ua"
  ),
  
  `DYNAMIC STABILITY` = c(
    "Mean_StepWidth_cm",
    "Mean_Norm_StepWidth_ua",
    "Mean_MoS_AP_HS_mm",
    "Mean_MoS_ML_HS_mm",
    "Mean_MoS_AP_HS_pL0",
    "Mean_MoS_ML_HS_pL0"
  ),
  
  VARIABILITY = c(
    "Mean_GVI_ua",
    "CV_Norm_StepWidth_ua",
    "CV_Gait_speed_m.s^{_1}"
  ),
  
  ASYMMETRY = c(
    "SI_Stride_length_m",
    "SI_Double_support_time_p",
    "SI_Norm_StepWidth_ua"
  )
)

# ------------------------------------------------------------------
# B) Palette AgeGroup + ordre des facteurs
# ------------------------------------------------------------------
age_palette <- c(
  "Young Children" = "blue",
  "Children"       = "chocolate3",
  "Adolescents"    = "darkred",
  "Adults"         = "purple"
)

age_order_en    <- c("Young Children", "Children", "Adolescents", "Adults")
surface_levels_en <- c("Even", "Medium", "High")

# ------------------------------------------------------------------
# C) Préparation du dataframe de base
# ------------------------------------------------------------------
df_panels <- df %>%
  filter(Surface %in% c("Plat", "Medium", "High")) %>%
  mutate(
    AgeGroup = factor(
      AgeGroup,
      levels = c("JeunesEnfants", "Enfants", "Adolescents", "Adultes"),
      labels = age_order_en
    ),
    Surface = factor(
      Surface,
      levels = c("Plat", "Medium", "High"),
      labels = surface_levels_en
    )
  )

# ------------------------------------------------------------------
# D) Fonction : générer UN sous-graphique (1 variable)
#    Boxplots par AgeGroup × Surface
#    + ligne reliant les MÉDIANES de chaque groupe entre les surfaces
#    + points individuels en fond (jitter léger)
# ------------------------------------------------------------------
make_median_connected_boxplot <- function(var_name, data) {
  
  pretty_label <- make_pretty_label(var_name)
  
  df_plot <- data %>%
    select(Participant, AgeGroup, Surface, all_of(var_name)) %>%
    rename(Value = !!sym(var_name)) %>%
    mutate(Value = suppressWarnings(as.numeric(Value))) %>%
    filter(!is.na(Value))
  
  df_medians <- df_plot %>%
    group_by(AgeGroup, Surface) %>%
    summarise(Median = median(Value, na.rm = TRUE), .groups = "drop")
  
  dw <- 0.7
  
  ggplot(df_plot, aes(x = Surface, y = Value, color = AgeGroup, fill = AgeGroup)) +
    
    geom_point(
      size     = 0.9,
      alpha    = 0.25,
      position = position_jitterdodge(jitter.width = 0.15, dodge.width = dw),
      show.legend = FALSE
    ) +
    
    geom_boxplot(
      width         = 0.55,
      alpha         = 0.55,
      outlier.shape = NA,
      linewidth     = 0.5,
      position      = position_dodge(dw),
      show.legend   = TRUE
    ) +
    
    geom_line(
      data      = df_medians,
      aes(x = Surface, y = Median, group = AgeGroup, color = AgeGroup),
      linewidth = 1.1,
      linetype  = "dashed",
      position  = position_dodge(dw),
      show.legend = FALSE
    ) +
    
    geom_point(
      data     = df_medians,
      aes(x = Surface, y = Median, color = AgeGroup),
      size     = 3.5,
      shape    = 18,
      position = position_dodge(dw),
      show.legend = FALSE
    ) +
    
    scale_color_manual(values = age_palette) +
    scale_fill_manual(values  = age_palette) +
    
    labs(
      title = pretty_label,
      x     = NULL,
      y     = NULL,
      color = "Age group",
      fill  = "Age group"
    ) +
    
    theme_minimal(base_size = 25) +         
    theme(
      plot.title         = element_text(face = "bold", hjust = 0.5, size = 25),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x        = element_text(size = 20, face = "bold"),
      axis.text.y        = element_text(size = 20),
      axis.ticks         = element_line(linewidth = 0.4),
      plot.margin        = margin(10, 14, 10, 14)
    )
}

# ------------------------------------------------------------------
# E) Fonction : assembler et exporter le PANEL d'un domaine
# ------------------------------------------------------------------
generate_domain_panel <- function(domain_name, var_list, data) {
  
  vars_ok <- intersect(var_list, names(data))
  
  if (length(vars_ok) == 0) {
    message("Domaine ", domain_name, " : aucune variable disponible, panel ignoré.")
    return(invisible(NULL))
  }
  
  if (length(vars_ok) < length(var_list)) {
    message("Domaine ", domain_name, " — variables absentes :\n  ",
            paste(setdiff(var_list, names(data)), collapse = "\n  "))
  }
  
  message("Génération du panel : ", domain_name,
          " (", length(vars_ok), " variables)")
  
  # Génération de tous les sous-graphiques
  plots_list <- lapply(vars_ok, make_median_connected_boxplot, data = data)
  
  # Nombre de colonnes : 3 max
  n_cols <- min(3, length(plots_list))
  n_rows <- ceiling(length(vars_ok) / n_cols)
  
  # Légende collective
  panel <- wrap_plots(plots_list, ncol = n_cols, guides = "collect") &
    theme(legend.position = "bottom",
          legend.text      = element_text(size = 28),
          legend.title     = element_text(size = 30, face = "bold"),
          legend.key.size  = unit(2.5, "lines"))
  
  panel <- panel +
    plot_annotation(
      theme    = theme(
        plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption  = element_blank()
      )
    )
  
  # Dimensions agrandies pour absorber la police plus grande
  fig_w <- n_cols * 8.0     
  fig_h <- n_rows * 7.0 + 2.5  
  
  safe_name <- gsub("[^A-Za-z0-9_]", "_", domain_name)
  
  ggsave(
    filename = file.path(output_dir_panels, paste0("Panel_", safe_name, ".png")),
    plot     = panel,
    width    = fig_w,
    height   = fig_h,
    dpi      = 300
  )
  
  ggsave(
    filename = file.path(output_dir_panels, paste0("Panel_", safe_name, ".pdf")),
    plot     = panel,
    width    = fig_w,
    height   = fig_h
  )
  
  ggsave(
    filename = file.path(output_dir_panels, paste0("Panel_", safe_name, ".tif")),
    plot     = panel,
    width    = fig_w,
    height   = fig_h,
    dpi = 600,
  )
  
  message("  → Exporté : Panel_", safe_name, ".png / .pdf")
  return(panel)
}

# ------------------------------------------------------------------
# F) Boucle principale
# ------------------------------------------------------------------
message("\n=== Génération des 5 panels par domaine ===\n")

panels_generated <- mapply(
  FUN         = generate_domain_panel,
  domain_name = names(domains_boxplot),
  var_list    = domains_boxplot,
  MoreArgs    = list(data = df_panels),
  SIMPLIFY    = FALSE
)

message("\n✓ Terminé. Fichiers dans : ", output_dir_panels)

# ---------------------------------------------------------
# 15) Génération des RADAR PLOTS par surface
# ---------------------------------------------------------

# 15.1) Définition des variables incluses dans le radar
vars_radar <- c(
  "Mean_Gait_speed_m.s^{_1}", "Mean_Norm_Gait_Speed_m.s^{_1}", "Mean_Step_length_m",
  "Mean_Norm_Step_length_ua", "Mean_Stride_length_m", "Mean_WalkRatio", "Mean_Norm_WR_ua",
  "Mean_Double_support_time_p", "Mean_Single_support_time_p", "Mean_StanceTime_s", "Mean_SwingTime_s", "Mean_StepTime_s", "Mean_Stride_time_s","Mean_Cadence_step.min^{_1}", "Mean_Norm_Cadence_ua",
  "Mean_COM_SPARC_Magnitude_ua", "Mean_StepWidth_cm", "Mean_Norm_StepWidth_ua",
  "Mean_MoS_AP_HS_pL0", "Mean_MoS_ML_HS_pL0", "Mean_MoS_AP_HS_mm", "Mean_MoS_ML_HS_mm", "Mean_GVI_ua",
  "CV_Norm_StepWidth_ua", "CV_Gait_speed_m.s^{_1}", "SI_Stride_length_m",
  "SI_Double_support_time_p", "SI_StepWidth_cm"
)

# 15.2) Vérification des variables présentes et préparation des labels
# On ne garde que les variables réellement disponibles dans df (après standardisation/cleaning)
vars_radar_present <- intersect(vars_radar, names(df))
# Labels lisibles (mise en forme) pour l’affichage autour du radar
radar_labels <- vapply(vars_radar_present, make_pretty_label, character(1))

# 15.3) Dossier d’export
if (!dir.exists("Radar_Plots")) dir.create("Radar_Plots")

# 15.4) Palette de couleurs par groupe d’âge (pour les moyennes) et par domaine
# Remarque : tu convertis ensuite "Adultes" -> "Adults" dans la fonction (sécurisation)
age_colors <- c(
  "Young Children" = "blue",
  "Children"       = "chocolate3",
  "Adolescents"    = "darkred",
  "Adultes"        = "purple"
)

# --- (A) Définir les domaines et leurs variables (ordre non critique ici)
domains_vars <- list(
  PACE = c(
    "Mean_Gait_speed_m.s^{_1}", "Mean_Norm_Gait_Speed_m.s^{_1}",
    "Mean_Step_length_m","Mean_Norm_Step_length_ua", "Mean_Stride_length_m", 
    "Mean_WalkRatio", "Mean_Norm_WR_ua"
  ),
  RHYTHM = c(
    "Mean_Double_support_time_p", "Mean_Single_support_time_p", 
    "Mean_StanceTime_s", "Mean_SwingTime_s",
    "Mean_StepTime_s", "Mean_Stride_time_s",
    "Mean_Cadence_step.min^{_1}", "Mean_Norm_Cadence_ua", 
    "Mean_COM_SPARC_Magnitude_ua"
  ),
  `POSTURAL CONTROL` = c(
    "Mean_StepWidth_cm", "Mean_Norm_StepWidth_ua",
    "Mean_MoS_AP_HS_pL0", "Mean_MoS_ML_HS_pL0",
    "Mean_MoS_AP_HS_mm", "Mean_MoS_ML_HS_mm"
  ),
  ASYMMETRY = c(
    "SI_Stride_length_m", "SI_Double_support_time_p", "SI_StepWidth_cm"
  ),
  VARIABILITY = c(
    "Mean_GVI_ua", "CV_Norm_StepWidth_ua", "CV_Gait_speed_m.s^{_1}"
  )
)

# --- (B) Couleurs des domaines (tu peux remplacer par tes hex exacts)
domain_colors <- c(
  PACE = "lightblue",
  RHYTHM = "lightcoral",
  `POSTURAL CONTROL` = "palegreen",
  ASYMMETRY = "plum",
  VARIABILITY = "lightyellow"
)

# --- (C) Fonction: associer chaque variable (dans l’ordre du radar) à son domaine
get_domain_for_vars <- function(vars_in_radar, domains_list) {
  dom_vec <- rep(NA_character_, length(vars_in_radar))
  names(dom_vec) <- vars_in_radar
  for (d in names(domains_list)) {
    dom_vec[vars_in_radar %in% domains_list[[d]]] <- d
  }
  dom_vec
}

# Dessine des secteurs (wedge) par domaine selon l'ordre des variables
draw_domain_background <- function(domains_by_var, domain_cols, alpha = 0.18, r = 1) {
  # domains_by_var: vecteur nommé ou non, longueur = nb variables, contenant le nom du domaine pour chaque variable
  n <- length(domains_by_var)
  if (n < 3) return(invisible(NULL))
  
  # Angles des axes (radar classique: premier en haut)
  angles <- seq(0, 2*pi, length.out = n + 1)[1:n] + (pi/2)
  
  # Limites entre axes = milieux angulaires
  bounds <- angles - (pi / n)
  bounds <- c(bounds, bounds[1] + 2*pi)
  
  # Pour regrouper les variables d'un même domaine
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
    
    # bornes angulaires du bloc contigu
    a_start <- bounds[i1]
    a_end   <- bounds[i2 + 1]
    
    # points du secteur
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

# 15.5) Calcul des bornes min/max globales (scaling)
# Principe :
# - on fixe les bornes min/max de chaque variable sur l’ensemble des participants
# - cela garantit une normalisation cohérente entre surfaces et groupes
radar_min_max <- df %>%
  dplyr::select(dplyr::all_of(vars_radar_present)) %>%
  dplyr::summarise(dplyr::across(
    dplyr::everything(),
    list(
      min = ~min(.x, na.rm = TRUE),
      max = ~max(.x, na.rm = TRUE)
    )
  ))
# Extraction explicite (utile pour debug/contrôle si besoin)
mins_raw <- radar_min_max %>% dplyr::select(dplyr::ends_with("_min")) %>% unlist() %>% as.numeric()
maxs_raw <- radar_min_max %>% dplyr::select(dplyr::ends_with("_max")) %>% unlist() %>% as.numeric()

# 15.6) Nettoyage graphique (pour repartir sur une base propre)
graphics.off()
par(mfrow = c(1, 1))

# 15.7) Fonction : création d’un radar plot pour UNE surface
create_surface_radar <- function(surf_name, df_full, vars, labels) {
  
  # A) Calcul des moyennes par groupe d'âge (pour la surface)
  data_avg <- df_full %>%
    dplyr::filter(Surface == surf_name) %>%
    dplyr::group_by(AgeGroup) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::mutate(
      AgeGroup = factor(
        AgeGroup,
        levels = c("JeunesEnfants", "Enfants", "Adolescents", "Adultes"),
        labels = c("Young Children", "Children", "Adolescents", "Adults")
      )
    ) %>%
    dplyr::arrange(AgeGroup)
  
  # B) Extraction des données individuelles (pour la surface)
  data_indiv <- df_full %>%
    dplyr::filter(Surface == surf_name) %>%
    dplyr::mutate(
      AgeGroup = factor(
        AgeGroup,
        levels = c("JeunesEnfants", "Enfants", "Adolescents", "Adultes"),
        labels = c("Young Children", "Children", "Adolescents", "Adults")
      )
    ) %>%
    dplyr::select(AgeGroup, dplyr::all_of(vars))
  
  # C) Récupération des min/max globaux (pour normalisation)
  mins <- as.vector(radar_min_max[grep("_min$", names(radar_min_max))])
  maxs <- as.vector(radar_min_max[grep("_max$", names(radar_min_max))])
  
  # D) Normalisation des moyennes (0–1)
  radar_df_avg <- as.data.frame(data_avg[, -1, drop = FALSE])
  
  normalized_avg <- as.data.frame(lapply(seq_len(ncol(radar_df_avg)), function(i) {
    denom <- (maxs[[i]] - mins[[i]])
    if (is.na(denom) || denom == 0) return(rep(0, nrow(radar_df_avg)))
    (radar_df_avg[, i] - mins[[i]]) / denom
  }))
  
  colnames(normalized_avg) <- labels
  
  final_radar_avg <- rbind(rep(1, length(vars)), rep(0, length(vars)), normalized_avg)
  
  # E) Normalisation des individus (0–1)
  radar_df_indiv <- data_indiv[, -1, drop = FALSE]
  
  normalized_indiv <- as.data.frame(lapply(seq_len(ncol(radar_df_indiv)), function(i) {
    denom <- (maxs[[i]] - mins[[i]])
    if (is.na(denom) || denom == 0) return(rep(0, nrow(radar_df_indiv)))
    (radar_df_indiv[, i] - mins[[i]]) / denom
  }))
  
  colnames(normalized_indiv) <- labels
  
  # F) Couleurs
  age_colors_fixed <- age_colors
  if ("Adultes" %in% names(age_colors_fixed) && !("Adults" %in% names(age_colors_fixed))) {
    age_colors_fixed["Adults"] <- age_colors_fixed["Adultes"]
    age_colors_fixed <- age_colors_fixed[names(age_colors_fixed) != "Adultes"]
  }
  
  colors_border_avg <- age_colors_fixed
  colors_in_avg <- grDevices::adjustcolor(colors_border_avg, alpha.f = 0.20)
  
  # =========================
  # G1) Cadre du radar INITIAL
  # =========================
  
  ng <- nrow(data_avg)
  transparent <- grDevices::adjustcolor("white", alpha.f = 0)
  
  fmsb::radarchart(
    final_radar_avg,
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
    title = paste("Gait Profile on", surf_name, "Surface across age groups")
  )
  
  # === Préparation des variables pour les étiquettes ===
  nvar <- length(vars)
  angles <- seq(0, 2*pi, length.out = nvar + 1)[1:nvar] + (pi/2)
  
  pct <- c(0.25, 0.50, 0.75, 1.00)
  r_levels <- pct
  
  ticks_real <- sapply(seq_len(nvar), function(i) {
    mins[[i]] + pct * (maxs[[i]] - mins[[i]])
  })
  
  # --- Fond coloré par domaine
  domains_by_var <- get_domain_for_vars(vars, domains_vars)
  
  par(new = TRUE)
  draw_domain_background(
    domains_by_var = domains_by_var,
    domain_cols    = domain_colors,
    alpha          = 0.35,
    r              = 1
  )
  
  # === AFFICHAGE DES ÉTIQUETTES DE VALEURS RÉELLES ===
  
  for (i in seq_len(nvar)) {
    angle <- angles[i]
    
    for (j in seq_along(pct)) {
      r <- r_levels[j]
      
      # Position exacte sur le cercle (sans décalage)
      x_pos <- r * cos(angle)
      y_pos <- r * sin(angle)
      
      # Valeur réelle
      val <- round(ticks_real[j, i], 2)
      
      # Calculer un petit décalage pour éviter que le texte chevauche l'axe
      # Le décalage dépend de l'angle pour positionner le texte vers l'extérieur
      offset <- 0.08  # distance du décalage
      x_offset <- offset * cos(angle)
      y_offset <- offset * sin(angle)
      
      # Afficher avec fond blanc semi-transparent pour lisibilité
      text(
        x = x_pos + x_offset,
        y = y_pos + y_offset,
        labels = val,
        cex = 0.45,
        col = "grey20",
        font = 1
      )
    }
  }
  
  # --- G2) Individus : tracés en gris derrière
  indiv_col <- grDevices::adjustcolor("grey30", alpha.f = 0.18)
  
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
  
  # --- G3) Moyennes : tracés colorés au premier plan
  par(new = TRUE)
  fmsb::radarchart(
    final_radar_avg,
    axistype = 0,
    vlabels = rep("", length(vars)),
    pcol = colors_border_avg,
    pfcol = colors_in_avg,
    plwd = 2.2,
    plty = 1,
    cglcol = NA,
    axislabcol = NA,
    vlcex = 0,
    seg = length(vars)
  )
  
  # --- Légende
  legend(
    x = "bottom",
    legend = names(colors_border_avg),
    inset = -0.15,
    horiz = TRUE,
    bty = "n",
    pch = 20,
    col = colors_border_avg,
    text.col = "black",
    cex = 0.8,
    pt.cex = 1.5,
    xpd = TRUE
  )
}

# 15.8) Export PDF et PNGs: un radar par surface
pdf("Radar_Plots/Gait_Radar_Profiles.pdf", width = 12, height = 12)
par(mfrow = c(1, 1))

for (s in c("Plat", "Medium", "High")) {
  
  create_surface_radar(s, df, vars_radar_present, radar_labels)
}

dev.off()   


# 15.9) Export PNG haute qualité (plus lisible) — sans modifier le PDF
# ---------------------------------------------------------
for (s in c("Plat", "Medium", "High")) {
  
  fname <- paste0("Radar_Plots/Gait_Radar_Profile_", s, ".png")
  
  png(filename = fname, width = 5200, height = 5200, res = 600, type = "cairo")
  
  op <- par(no.readonly = TRUE)
  
  par(mfrow = c(1, 1))
  
  # Marges plus petites => le radar prend plus de place
  par(mar = c(8, 5, 5, 5))     # bottom, left, top, right
  
  # Pas de marge externe
  par(oma = c(0, 0, 0, 0))
  
  # Evite que R rajoute une "expansion" d'axes qui réduit visuellement le cercle
  par(xaxs = "i", yaxs = "i")
  
  # Autorise texte/legend hors zone (évite coupures)
  par(xpd = NA)
  
  # Texte légèrement plus petit si besoin (optionnel)
  par(cex = 0.85)
  
  create_surface_radar(s, df, vars_radar_present, radar_labels)
  
  par(op)
  dev.off()
}

## ============================================================
## 16) PANEL DE 4 RADARS PAR GROUPE D'ÂGE (Effet de la Surface)
## ============================================================
## - 1 radar par groupe d'âge (JeunesEnfants, Enfants, Adolescents, Adultes)
## - À l'intérieur de chaque radar : 3 lignes = Even / Medium / High
## - Données individuelles conservées (gris transparent)
## - Même normalisation globale min/max que la partie 15
## - Mêmes domaines (PACE, RHYTHM, POSTURAL CONTROL, ASYMMETRY, VARIABILITY)
## - Export : PDF (panel 2x2) + PNG haute qualité par âge
## ============================================================

# Dossier de sortie (identique à la partie 15)
if (!dir.exists("Radar_Plots")) dir.create("Radar_Plots")

# ------------------------------------------------------------------
# 16.1) Couleurs des surfaces
# ------------------------------------------------------------------
surface_colors <- c(
  "Even"   = "blue",   # bleu
  "Medium" = "green",   # orange
  "High"   = "red"    # rouge
)

# ------------------------------------------------------------------
# 16.2) Labels traduits des groupes d'âge (pour titres)
# ------------------------------------------------------------------
age_labels_panel <- c(
  JeunesEnfants = "Young Children",
  Enfants       = "Children",
  Adolescents   = "Adolescents",
  Adultes       = "Adults"
)

# ------------------------------------------------------------------
# 16.3) Fonction : radar pour UN groupe d'âge (3 surfaces en overlay)
# ------------------------------------------------------------------
create_agegroup_radar <- function(age_name, df_full, vars, labels,
                                  mins_raw, maxs_raw) {
  
  age_label <- age_labels_panel[age_name]
  
  # A) Calcul des moyennes par Surface (pour ce groupe d'âge)
  data_avg <- df_full %>%
    dplyr::filter(AgeGroup == age_name,
                  Surface %in% c("Plat", "Medium", "High")) %>%
    dplyr::group_by(Surface) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(vars), ~mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Surface = factor(
        Surface,
        levels = c("Plat", "Medium", "High"),
        labels = c("Even", "Medium", "High")
      )
    ) %>%
    dplyr::arrange(Surface)
  
  # B) Données individuelles (pour ce groupe d'âge)
  data_indiv <- df_full %>%
    dplyr::filter(AgeGroup == age_name,
                  Surface %in% c("Plat", "Medium", "High")) %>%
    dplyr::mutate(
      Surface = factor(
        Surface,
        levels = c("Plat", "Medium", "High"),
        labels = c("Even", "Medium", "High")
      )
    ) %>%
    dplyr::select(Surface, dplyr::all_of(vars))
  
  # C) Vérification : au moins une surface disponible
  if (nrow(data_avg) == 0) {
    message("Pas de données pour le groupe : ", age_name)
    return(invisible(NULL))
  }
  
  # D) Normalisation des moyennes (0–1) sur les bornes globales
  radar_df_avg <- as.data.frame(data_avg[, -1, drop = FALSE])
  
  normalized_avg <- as.data.frame(lapply(seq_len(ncol(radar_df_avg)), function(i) {
    denom <- maxs_raw[i] - mins_raw[i]
    if (is.na(denom) || denom == 0) return(rep(0, nrow(radar_df_avg)))
    (radar_df_avg[, i] - mins_raw[i]) / denom
  }))
  colnames(normalized_avg) <- labels
  
  # Format fmsb : max row, min row, puis données
  final_radar_avg <- rbind(
    rep(1, length(vars)),   # max
    rep(0, length(vars)),   # min
    normalized_avg
  )
  
  # E) Normalisation des individus (0–1)
  radar_df_indiv <- as.data.frame(data_indiv[, -1, drop = FALSE])
  
  normalized_indiv <- as.data.frame(lapply(seq_len(ncol(radar_df_indiv)), function(i) {
    denom <- maxs_raw[i] - mins_raw[i]
    if (is.na(denom) || denom == 0) return(rep(0, nrow(radar_df_indiv)))
    (radar_df_indiv[, i] - mins_raw[i]) / denom
  }))
  colnames(normalized_indiv) <- labels
  
  # F) Couleurs des surfaces (uniquement celles présentes)
  surfaces_present <- levels(data_avg$Surface)
  colors_border    <- surface_colors[surfaces_present]
  colors_fill      <- grDevices::adjustcolor(colors_border, alpha.f = 0.18)
  
  ng <- length(surfaces_present)
  transparent <- grDevices::adjustcolor("white", alpha.f = 0)
  
  # ------------------------------------------------------------------
  # G1) Cadre vide du radar (axes + grille + étiquettes variables)
  # ------------------------------------------------------------------
  fmsb::radarchart(
    final_radar_avg,
    axistype = 0,
    seg      = 4,
    pcol     = rep(transparent, ng),
    pfcol    = rep(transparent, ng),
    plwd     = rep(0.01, ng),
    plty     = rep(1, ng),
    cglcol   = "grey70",
    cglty    = 1,
    cglwd    = 0.8,
    vlcex    = 0.65,
    title    = age_label
  )
  
  # ------------------------------------------------------------------
  # G2) Fond coloré par domaine (même fonction que partie 15)
  # ------------------------------------------------------------------
  domains_by_var <- get_domain_for_vars(vars, domains_vars)
  
  par(new = TRUE)
  draw_domain_background(
    domains_by_var = domains_by_var,
    domain_cols    = domain_colors,
    alpha          = 0.25,
    r              = 1
  )
  
  # ------------------------------------------------------------------
  # G3) Étiquettes de valeurs réelles sur les axes
  # ------------------------------------------------------------------
  nvar <- length(vars)
  angles  <- seq(0, 2 * pi, length.out = nvar + 1)[1:nvar] + (pi / 2)
  pct     <- c(0.25, 0.50, 0.75, 1.00)
  r_levels <- pct
  
  ticks_real <- sapply(seq_len(nvar), function(i) {
    mins_raw[i] + pct * (maxs_raw[i] - mins_raw[i])
  })
  
  for (i in seq_len(nvar)) {
    angle <- angles[i]
    for (j in seq_along(pct)) {
      r   <- r_levels[j]
      val <- round(ticks_real[j, i], 2)
      offset <- 0.08
      text(
        x      = r * cos(angle) + offset * cos(angle),
        y      = r * sin(angle) + offset * sin(angle),
        labels = val,
        cex    = 0.40,
        col    = "grey25",
        font   = 1
      )
    }
  }
  
  # ------------------------------------------------------------------
  # G4) Données individuelles (gris transparent, par surface)
  # ------------------------------------------------------------------
  for (surf_i in surfaces_present) {
    indiv_rows <- which(data_indiv$Surface == surf_i)
    if (length(indiv_rows) == 0) next
    
    col_surf <- grDevices::adjustcolor(surface_colors[surf_i], alpha.f = 0.12)
    
    for (i in indiv_rows) {
      par(new = TRUE)
      fmsb::radarchart(
        rbind(rep(1, length(vars)), rep(0, length(vars)),
              normalized_indiv[i, , drop = FALSE]),
        axistype   = 0,
        vlabels    = rep("", length(vars)),
        pcol       = col_surf,
        pfcol      = NA,
        plwd       = 0.6,
        plty       = 1,
        cglcol     = NA,
        axislabcol = NA,
        vlcex      = 0,
        seg        = length(vars)
      )
    }
  }
  
  # ------------------------------------------------------------------
  # G5) Moyennes par surface (tracés colorés au premier plan)
  # ------------------------------------------------------------------
  par(new = TRUE)
  fmsb::radarchart(
    final_radar_avg,
    axistype   = 0,
    vlabels    = rep("", length(vars)),
    pcol       = colors_border,
    pfcol      = colors_fill, # Si je veux remplir le polygone je met "colors_fill" plutôt que le NA
    plwd       = 2.5,
    plty       = 1,
    cglcol     = NA,
    axislabcol = NA,
    vlcex      = 0,
    seg        = length(vars)
  )
  
  # ------------------------------------------------------------------
  # G6) Légende (surfaces)
  # ------------------------------------------------------------------
  legend(
    x        = "bottom",
    legend   = surfaces_present,
    inset    = -0.15,
    horiz    = TRUE,
    bty      = "n",
    pch      = 20,
    col      = colors_border,
    text.col = "black",
    cex      = 0.8,
    pt.cex   = 1.5,
    xpd      = TRUE
  )
}

# ------------------------------------------------------------------
# 16.4) Export PDF — panel 2x2 (un radar par groupe d'âge)
# ------------------------------------------------------------------
pdf("Radar_Plots/Gait_Radar_Panel_AgeGroup_Surface.pdf", width = 20, height = 20)

par(mfrow = c(2, 2))
par(mar   = c(7, 4, 5, 4))    # marges : bas, gauche, haut, droite
par(oma   = c(2, 2, 4, 2))    # marge externe pour titre global

for (ag in c("JeunesEnfants", "Enfants", "Adolescents", "Adultes")) {
  create_agegroup_radar(
    age_name  = ag,
    df_full   = df,
    vars      = vars_radar_present,
    labels    = radar_labels,
    mins_raw  = mins_raw,
    maxs_raw  = maxs_raw
  )
}

# Titre global du panel
mtext(
  "Gait Radar Profile by Age Group — Effect of Walking Surface",
  outer = TRUE, cex = 1.6, font = 2, line = 1
)
mtext(
  "Each radar = 1 age group  |  Lines = Even (blue) / Medium (orange) / High (red)",
  outer = TRUE, cex = 1.0, line = -0.5
)

dev.off()

message("PDF panel exporté : Radar_Plots/Gait_Radar_Panel_AgeGroup_Surface.pdf")

# ------------------------------------------------------------------
# 16.5) Export PNG haute qualité — panel 2x2
# ------------------------------------------------------------------
png(
  filename = "Radar_Plots/Gait_Radar_Panel_AgeGroup_Surface.png",
  width    = 10400,
  height   = 10400,
  res      = 600,
  type     = "cairo"
)

op <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
par(mar   = c(7, 4, 5, 4))
par(oma   = c(2, 2, 4, 2))
par(xaxs  = "i", yaxs = "i")
par(xpd   = NA)
par(cex   = 0.85)

for (ag in c("JeunesEnfants", "Enfants", "Adolescents", "Adultes")) {
  create_agegroup_radar(
    age_name  = ag,
    df_full   = df,
    vars      = vars_radar_present,
    labels    = radar_labels,
    mins_raw  = mins_raw,
    maxs_raw  = maxs_raw
  )
}

mtext(
  "Gait Radar Profile by Age Group — Effect of Walking Surface",
  outer = TRUE, cex = 1.6, font = 2, line = 1
)
mtext(
  "Each radar = 1 age group  |  Lines = Even (blue) / Medium (orange) / High (red)",
  outer = TRUE, cex = 1.0, line = -0.5
)

par(op)
dev.off()

message("PNG panel exporté : Radar_Plots/Gait_Radar_Panel_AgeGroup_Surface.png")

# ------------------------------------------------------------------
# 16.6) Export PNG individuel par groupe d'âge (haute qualité)
# ------------------------------------------------------------------
for (ag in c("JeunesEnfants", "Enfants", "Adolescents", "Adultes")) {
  
  fname_ag <- paste0("Radar_Plots/Radar_AgeGroup_", ag, "_SurfaceEffect.png")
  
  png(
    filename = fname_ag,
    width    = 5200,
    height   = 5200,
    res      = 600,
    type     = "cairo"
  )
  
  op <- par(no.readonly = TRUE)
  par(mfrow = c(1, 1))
  par(mar   = c(8, 5, 5, 5))
  par(oma   = c(0, 0, 0, 0))
  par(xaxs  = "i", yaxs = "i")
  par(xpd   = NA)
  par(cex   = 0.85)
  
  create_agegroup_radar(
    age_name  = ag,
    df_full   = df,
    vars      = vars_radar_present,
    labels    = radar_labels,
    mins_raw  = mins_raw,
    maxs_raw  = maxs_raw
  )
  
  par(op)
  dev.off()
  
  message("PNG individuel exporté : ", fname_ag)
}

message("\n✓ Section 16 terminée — Panel 2x2 + PNGs individuels disponibles dans Radar_Plots/")

saveRDS(df, file.path(chemin, "df_clean.rds"))

## ============================================================
## 17) HEATMAP DES MOYENNES PAR ÂGE ET SURFACE
##     NORMALISATION PAR VARIABLE (échelle propre à chaque variable)
##     + dégradé spécifique au domaine de chaque variable
## ============================================================

library(tidyverse)
library(ggplot2)

heatmap_dir <- "Heatmaps_Gait"
if (!dir.exists(heatmap_dir)) dir.create(heatmap_dir)

# ---------------------------------------------------------
# 17.0) Définition des domaines et de leurs couleurs
# ---------------------------------------------------------
domains_vars <- list(
  PACE = c(
    "Mean_Gait_speed_m.s^{_1}",
    "Mean_Norm_Gait_Speed_m.s^{_1}",
    "Mean_Step_length_m",
    "Mean_Stride_length_m",
    "Mean_Norm_Step_length_ua",
    "Mean_WalkRatio",
    "Mean_Norm_WR_ua"
  ),
  
  RHYTHM = c(
    "Mean_Double_support_time_p",
    "Mean_Single_support_time_p",
    "Mean_StanceTime_s",
    "Mean_SwingTime_s",
    "Mean_StepTime_s",
    "Mean_Stride_time_s",
    "Mean_Cadence_step.min^{_1}",
    "Mean_Norm_Cadence_ua",
    "Mean_COM_SPARC_Magnitude_ua"
  ),
  
  `DYNAMIC STABILITY` = c(
    "Mean_StepWidth_cm",
    "Mean_Norm_StepWidth_ua",
    "Mean_MoS_AP_HS_mm",
    "Mean_MoS_ML_HS_mm",
    "Mean_MoS_AP_HS_pL0",
    "Mean_MoS_ML_HS_pL0"
  ),
  
  VARIABILITY = c(
    "Mean_GVI_ua",
    "CV_Gait_speed_m.s^{_1}",
    "CV_Norm_StepWidth_ua"
  ),
  
  ASYMMETRY = c(
    "SI_Stride_length_m",
    "SI_Double_support_time_p",
    "SI_Norm_StepWidth_ua"
  )
)

domain_colors <- c(
  PACE = "#2C7FB8",                # bleu
  RHYTHM = "#D7191C",              # rouge
  `DYNAMIC STABILITY` = "#1A9641", # vert
  VARIABILITY = "#FFD700",         # jaune
  ASYMMETRY = "#7B3294"            # violet
)

get_domain_for_var <- function(var, domain_list) {
  for (d in names(domain_list)) {
    if (var %in% domain_list[[d]]) return(d)
  }
  return(NA_character_)
}

blend_with_white <- function(base_color, value) {
  value <- pmin(pmax(value, 0), 1)
  rgb_mat <- grDevices::col2rgb(c("white", base_color))
  white_rgb <- rgb_mat[, 1]
  base_rgb  <- rgb_mat[, 2]
  
  mixed_rgb <- white_rgb + (base_rgb - white_rgb) * value
  
  grDevices::rgb(
    red   = mixed_rgb[1],
    green = mixed_rgb[2],
    blue  = mixed_rgb[3],
    maxColorValue = 255
  )
}

# ---------------------------------------------------------
# Variables à garder dans la heatmap (ordre imposé)
# ---------------------------------------------------------
vars_heatmap <- c(
  # PACE
  "Mean_Gait_speed_m.s^{_1}",
  "Mean_Norm_Gait_Speed_m.s^{_1}",
  "Mean_Step_length_m",
  "Mean_Norm_Step_length_ua",
  "Mean_Stride_length_m",
  "Mean_WalkRatio",
  "Mean_Norm_WR_ua",
  
  # RHYTHM
  "Mean_Double_support_time_p",
  "Mean_Single_support_time_p",
  "Mean_StanceTime_s",
  "Mean_SwingTime_s",
  "Mean_StepTime_s",
  "Mean_Stride_time_s",
  "Mean_Cadence_step.min^{_1}",
  "Mean_Norm_Cadence_ua",
  
  # DYNAMIC STABILITY
  "Mean_COM_SPARC_Magnitude_ua",
  "Mean_StepWidth_cm",
  "Mean_Norm_StepWidth_ua",
  "Mean_MoS_AP_HS_mm",
  "Mean_MoS_ML_HS_mm",
  "Mean_MoS_AP_HS_pL0",
  "Mean_MoS_ML_HS_pL0",
  
  # VARIABILITY
  "Mean_GVI_ua",
  "CV_Gait_speed_m.s^{_1}",
  "CV_Norm_StepWidth_ua",
  
  # ASYMMETRY
  "SI_Stride_length_m",
  "SI_Double_support_time_p",
  "SI_Norm_StepWidth_ua"
)

vars_heatmap_present <- intersect(vars_heatmap, names(df))
vars_heatmap_missing <- setdiff(vars_heatmap, names(df))

if (length(vars_heatmap_missing) > 0) {
  message(
    "Variables demandées mais absentes du fichier :\n- ",
    paste(vars_heatmap_missing, collapse = "\n- ")
  )
}

if (length(vars_heatmap_present) == 0) {
  stop("Aucune des variables demandées pour la heatmap n'est présente dans le dataset.")
}

# ---------------------------------------------------------
# 17.1) Préparation des moyennes par AgeGroup x Surface
# ---------------------------------------------------------
heat_df <- df %>%
  mutate(
    AgeGroup = factor(
      AgeGroup,
      levels = c("JeunesEnfants", "Enfants", "Adolescents", "Adultes"),
      labels = c("Young Children", "Children", "Adolescents", "Adults")
    ),
    Surface = factor(
      Surface,
      levels = c("Plat", "Medium", "High"),
      labels = c("Even", "Medium", "High")
    )
  ) %>%
  filter(Surface %in% c("Even", "Medium", "High")) %>%
  select(AgeGroup, Surface, all_of(vars_heatmap_present)) %>%
  pivot_longer(
    cols = all_of(vars_heatmap_present),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Value = suppressWarnings(as.numeric(Value)),
    Domain = vapply(Variable, get_domain_for_var, character(1), domain_list = domains_vars),
    Variable_pretty = vapply(Variable, make_pretty_label, character(1))
  ) %>%
  group_by(AgeGroup, Surface, Variable, Domain, Variable_pretty) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    sd_value   = sd(Value, na.rm = TRUE),
    n          = sum(!is.na(Value)),
    .groups = "drop"
  )

# Vérification : variables sans domaine
vars_without_domain <- heat_df %>%
  filter(is.na(Domain)) %>%
  distinct(Variable)

if (nrow(vars_without_domain) > 0) {
  warning(
    "Certaines variables n'ont pas de domaine associé :\n- ",
    paste(vars_without_domain$Variable, collapse = "\n- ")
  )
}

# Ordre d'affichage des variables
var_order <- rev(vapply(vars_heatmap_present, make_pretty_label, character(1)))

heat_df <- heat_df %>%
  mutate(
    Variable_pretty = factor(Variable_pretty, levels = var_order)
  )

# ---------------------------------------------------------
# 17.2) NORMALISATION MIN-MAX PAR VARIABLE
#     Chaque variable est ramenée entre 0 et 1 sur ses 12 cellules
# ---------------------------------------------------------
heat_df_minmax <- heat_df %>%
  group_by(Variable) %>%
  mutate(
    var_min = min(mean_value, na.rm = TRUE),
    var_max = max(mean_value, na.rm = TRUE),
    mean_scaled = case_when(
      is.na(var_min) | is.na(var_max) ~ NA_real_,
      var_max == var_min ~ 0.5,
      TRUE ~ (mean_value - var_min) / (var_max - var_min)
    )
  ) %>%
  ungroup()

# Couleur de remplissage par domaine + intensité relative
heat_df_minmax <- heat_df_minmax %>%
  rowwise() %>%
  mutate(
    fill_color = blend_with_white(domain_colors[Domain], mean_scaled),
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 17.3) Heatmap finale : couleur des cases selon domaine
# ---------------------------------------------------------
p_heat_domain <- ggplot(
  heat_df_minmax,
  aes(x = Surface, y = Variable_pretty)
) +
  geom_tile(aes(fill = fill_color), color = "white", linewidth = 0.4) +
  geom_text(
    aes(label = round(mean_value, 2)),
    color = "black",
    size = 4
  ) +
  scale_fill_identity() +
  facet_wrap(~ AgeGroup, nrow = 1) +
  labs(
    x = "Surface",
    y = "Variables"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(size = 13, face = "bold"),   # Even / Medium / High
    axis.text.y = element_text(size = 12),                  # variables
    
    axis.title.x = element_text(size = 15, face = "bold"),  # Surface
    axis.title.y = element_text(size = 15, face = "bold"),  # Variables
    
    strip.text = element_text(face = "bold", size = 13),    # AgeGroup
    
    panel.grid = element_blank(),
    legend.position = "none"
  )

ggsave(
  filename = file.path(heatmap_dir, "Heatmap_Mean_MinMax_DomainColors_byVariable_Age_Surface.png"),
  plot = p_heat_domain,
  width = 15,
  height = 12,
  dpi = 600
)

ggsave(
  filename = file.path(heatmap_dir, "Heatmap_Mean_MinMax_DomainColors_byVariable_Age_Surface.pdf"),
  plot = p_heat_domain,
  width = 15,
  height = 12
)

ggsave(
  filename = file.path(heatmap_dir, "Heatmap_Mean_MinMax_DomainColors_byVariable_Age_Surface.tif"),
  plot = p_heat_domain,
  width = 15,
  height = 12,
  dpi = 600,
  units = "in",
  compression = "lzw"
)

message("✓ Heatmap domaine/min-max exportée dans : ", heatmap_dir)

saveRDS(df, file.path(chemin, "df_clean.rds"))



## ============================================================
## III. CALCUL GAIT ADAPTATION SCORE
## ============================================================

## Objectif : Quantifier l'ampleur de la modification de marche
## de chaque participant face aux surfaces irrégulières.
##
## Pipeline :
##   (i)   Calcul du Δ individuel par variable (High−Plat, Medium−Plat)
##   (ii)  Conversion en z-score global (|z|) → comparable entre domaines
##   (iii) Score global GRS + 1 score par domaine (5 domaines)
##
## Domaines & variables :
##   PACE              → Mean_Gait_speed_m.s^{_1}
##   RHYTHM            → Mean_Cadence_step.min^{_1}
##   DYNAMIC STABILITY → Mean_MoS_ML_HS_mm
##   VARIABILITY       → Mean_GVI_ua
##   ASYMMETRY         → SI_Stride_length_m
## ============================================================

## -- 0) Packages -----------------------------------------------
library(tidyverse)
library(ggplot2)
library(patchwork)
library(openxlsx)

## -- 1) Chargement des données ---------------------------------
## Réutilise le fichier déjà chargé dans la Section II/III.
## Si tu lances ce script de manière autonome, décommente les lignes ci-dessous.

# file_path <- "C:/Users/silve/Desktop/.../ACP_Clustering_DATA.csv"
# first_line <- readLines(file_path, n = 1, warn = FALSE)
# delim <- ifelse(grepl(";", first_line), ";", ",")
# df_raw <- read_delim(file_path, delim = delim, show_col_types = FALSE)
# names(df_raw) <- standardize_names(names(df_raw))   # fonction définie en Section II

#df_raw <- readRDS(file.path(chemin, "df_clean.rds"))

## -- 2) Paramètres : domaines & variables ----------------------

# Renommage immédiatement après, dans le même bloc
df_raw <- df
names(df_raw)[names(df_raw) == "Mean_Gait_speed_m.s^{_1}"]  <- "Mean_Gait_speed"
names(df_raw)[names(df_raw) == "Mean_Cadence_step.min^{_1}"] <- "Mean_Cadence"
names(df_raw)[names(df_raw) == "Mean_MoS_ML_HS_mm"]          <- "Mean_MoS_ML_HS"
names(df_raw)[names(df_raw) == "Mean_GVI_ua"]                 <- "Mean_GVI"
names(df_raw)[names(df_raw) == "SI_Stride_length_m"]          <- "SI_Stride_length"

domains <- list(
  PACE               = "Mean_Gait_speed",
  RHYTHM             = "Mean_Cadence",
  DYNAMIC_STABILITY  = "Mean_MoS_ML_HS",
  VARIABILITY        = "Mean_GVI",
  ASYMMETRY          = "SI_Stride_length"
)

surf_even   <- "Plat"
surf_medium <- "Medium"
surf_high   <- "High"

age_order <- c("JeunesEnfants", "Enfants", "Adolescents", "Adultes")
age_labels <- c(
  JeunesEnfants = "Young Children",
  Enfants       = "Children",
  Adolescents   = "Adolescents",
  Adultes       = "Adults"
)

## -- 3) Vérification des colonnes requises ---------------------

required_cols <- c("Participant", "AgeGroup", "Surface", unname(unlist(domains)))

# Identifie les colonnes présentes/absentes
cols_present <- intersect(required_cols, names(df_raw))
cols_missing <- setdiff(required_cols, names(df_raw))

if (length(cols_missing) > 0) {
  warning(
    "Colonnes manquantes dans df_raw :\n  ",
    paste(cols_missing, collapse = "\n  "),
    "\n→ Ces domaines seront ignorés dans le calcul du GRS."
  )
  # Retire les domaines dont la variable est absente
  domains <- domains[sapply(domains, function(v) v %in% names(df_raw))]
}

if (length(domains) == 0) stop("Aucun domaine disponible. Vérifie tes noms de colonnes.")

message("Domaines retenus pour le GRS : ", paste(names(domains), collapse = ", "))

## -- 4) Préparation du tableau "large par surface" -------------
## Format : 1 ligne par participant, colonnes = variable_Surface

df_grs_prep <- df_raw %>%
  # Garde uniquement les colonnes utiles
  select(Participant, AgeGroup, Surface, all_of(unname(unlist(domains)))) %>%
  # Convertit les variables en numérique (sécurité)
  mutate(across(all_of(unname(unlist(domains))), ~ suppressWarnings(as.numeric(.)))) %>%
  # Filtre sur les 3 surfaces
  filter(Surface %in% c(surf_even, surf_medium, surf_high)) %>%
  # Facteur AgeGroup dans le bon ordre
  mutate(
    AgeGroup = factor(AgeGroup, levels = age_order),
    Surface  = factor(Surface,  levels = c(surf_even, surf_medium, surf_high))
  )

## -- 5) Calcul des Δ (variation vs surface Even/Plat) ----------
## Pour chaque participant : Δ_High = valeur_High − valeur_Plat
##                           Δ_Medium = valeur_Medium − valeur_Plat

# Pivote en large : 1 ligne par Participant×AgeGroup, colonnes = var_Surface
df_wide <- df_grs_prep %>%
  pivot_longer(
    cols      = all_of(unname(unlist(domains))),
    names_to  = "Variable",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from  = Surface,
    values_from = Value,
    # Si plusieurs entrées par Participant×Variable×Surface, prend la moyenne
    values_fn   = mean
  )

# Calcul des deltas (la colonne "Plat" sert de référence)
df_delta <- df_wide %>%
  mutate(
    Delta_High   = .data[[surf_high]]   - .data[[surf_even]],
    Delta_Medium = .data[[surf_medium]] - .data[[surf_even]]
  ) %>%
  select(Participant, AgeGroup, Variable, Delta_High, Delta_Medium)

## -- 6) Conversion en z-score global (|z|) --------------------
## Normalisation sur TOUS les participants (poolés)
## puis valeur absolue → l'ampleur de la modification

df_zscore <- df_delta %>%
  group_by(Variable) %>%
  mutate(
    # z-score centré-réduit global (μ et σ calculés sur tous les sujets)
    z_High   = (Delta_High   - mean(Delta_High,   na.rm = TRUE)) /
      sd(Delta_High,   na.rm = TRUE),
    z_Medium = (Delta_Medium - mean(Delta_Medium, na.rm = TRUE)) /
      sd(Delta_Medium, na.rm = TRUE),
    
    # Valeur absolue → ampleur brute de la modification
    abs_z_High   = abs(z_High),
    abs_z_Medium = abs(z_Medium)
  ) %>%
  ungroup()

## -- 7) Attribution du domaine à chaque variable ---------------
domain_map <- tibble(
  Variable = unname(unlist(domains)),
  Domain   = names(domains)
)

df_zscore <- df_zscore %>%
  left_join(domain_map, by = "Variable")

## -- 8) Score par domaine + score global -----------------------
## Score par domaine = |z| de la variable du domaine (1 variable/domaine)
## GRS global = moyenne des scores domaines (pour chaque contrast de surface)

## (a) Score par domaine, par participant
df_domain_scores <- df_zscore %>%
  select(Participant, AgeGroup, Domain, abs_z_High, abs_z_Medium) %>%
  rename(
    Score_High   = abs_z_High,
    Score_Medium = abs_z_Medium
  )

## (b) Score global GRS = moyenne des domaines (par participant × contrast)
df_grs_global <- df_domain_scores %>%
  group_by(Participant, AgeGroup) %>%
  summarise(
    GRS_High   = mean(Score_High,   na.rm = TRUE),
    GRS_Medium = mean(Score_Medium, na.rm = TRUE),
    n_domains  = sum(!is.na(Score_High)),
    .groups = "drop"
  )

## -- 9) Tableau récapitulatif complet (1 ligne/participant) -----
## Colonnes : GRS_global + score par domaine (High & Medium)

df_scores_wide <- df_domain_scores %>%
  pivot_wider(
    names_from  = Domain,
    values_from = c(Score_High, Score_Medium)
  ) %>%
  left_join(df_grs_global, by = c("Participant", "AgeGroup"))

message("✓ Scores calculés pour ", nrow(df_grs_global), " participants.")
print(head(df_grs_global, 10))

## -- 11) VISUALISATION -----------------------------------------

## Palette groupes d'âge
age_colors <- c(
  "JeunesEnfants" = "blue",
  "Enfants"       = "chocolate3",
  "Adolescents"   = "darkred",
  "Adultes"       = "purple"
)

# plot violon-boxplot pour un score donné
plot_grs <- function(data, score_col, title_str, subtitle_str = "") {
  
  df_plot <- data %>%
    filter(!is.na(.data[[score_col]]), !is.na(AgeGroup)) %>%
    mutate(AgeGroup = factor(AgeGroup, levels = age_order))
  
  ggplot(df_plot, aes(x = AgeGroup, y = .data[[score_col]], fill = AgeGroup)) +
    geom_violin(alpha = 0.3, color = NA, trim = FALSE) +
    geom_boxplot(width = 0.25, alpha = 0.8, outlier.shape = 21,
                 outlier.size = 1.5, outlier.alpha = 0.5) +
    geom_jitter(width = 0.08, size = 1.2, alpha = 0.4, color = "grey30") +
    scale_fill_manual(values = age_colors) +
    scale_x_discrete(labels = age_labels) +
    labs(
      title    = title_str,
      subtitle = subtitle_str,
      x        = "Age Group",
      y        = "|z-score| (GAS)"
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position  = "none",
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(angle = 30, hjust = 1),
      plot.title       = element_text(face = "bold", hjust = 0.5),
      plot.subtitle    = element_text(hjust = 0.5, color = "grey50")
    )
}

## Figure 1 : GRS Global (High et Medium côte-à-côte)
p_grs_high   <- plot_grs(df_scores_wide, "GAS_High",
                         "Global GAS", "High vs Even")
p_grs_medium <- plot_grs(df_scores_wide, "GAS_Medium",
                         "Global GAS", "Medium vs Even")

fig_global <- (p_grs_high | p_grs_medium) +
  plot_annotation(
    title   = "Gait Adaptation Score Global"
  )

ggsave("GRS_Global_High_vs_Medium.png", fig_global,
       width = 10, height = 5, dpi = 300)

## Figure 2 : Scores par domaine (High vs Even) — 1 panneau par domaine
domain_plots_high <- lapply(names(domains), function(dom) {
  col_name <- paste0("Score_High_", dom)
  if (!col_name %in% names(df_scores_wide)) return(NULL)
  plot_grs(df_scores_wide, col_name,
           gsub("_", " ", dom),
           "High vs Even")
})
domain_plots_high <- Filter(Negate(is.null), domain_plots_high)

fig_domains_high <- wrap_plots(domain_plots_high, ncol = 3) +
  plot_annotation(
    title   = "Gait Adaptation Score by Domain (High vs Even)"
  )

ggsave("GRS_ByDomain_High.png", fig_domains_high,
       width = 14, height = 8, dpi = 300)

## Figure 3 : Scores par domaine (Medium vs Even)
domain_plots_medium <- lapply(names(domains), function(dom) {
  col_name <- paste0("Score_Medium_", dom)
  if (!col_name %in% names(df_scores_wide)) return(NULL)
  plot_grs(df_scores_wide, col_name,
           gsub("_", " ", dom),
           "Medium vs Even")
})
domain_plots_medium <- Filter(Negate(is.null), domain_plots_medium)

fig_domains_medium <- wrap_plots(domain_plots_medium, ncol = 3) +
  plot_annotation(
    title   = "Gait Adaptation Score by Domain (Medium vs Even)"
  )

ggsave("GRS_ByDomain_Medium.png", fig_domains_medium,
       width = 14, height = 8, dpi = 300)

## Figure 4 : Heatmap avec gradient spécifique à chaque domaine
library(tidyverse)
library(scales)
library(ggtext)

## ---------------------------
## 1) Couleurs domaines
## ---------------------------

# Couleurs foncées (valeurs élevées)
domain_dark_colors <- c(
  "PACE"              = "#2166AC",  # bleu soutenu
  "RHYTHM"            = "#D7191C",  # rouge soutenu
  "DYNAMIC_STABILITY" = "#1A9641",  # vert soutenu
  "VARIABILITY"       = "#B8860B",  # ocre/moutarde foncé
  "ASYMMETRY"         = "#7B3294"   # violet soutenu
)

# Couleurs très claires (valeurs faibles)
domain_light_colors <- c(
  "PACE"              = "#F4F8FD",
  "RHYTHM"            = "#FDEEEE",
  "DYNAMIC_STABILITY" = "#EFF8EF",
  "VARIABILITY"       = "#FFF8E8",
  "ASYMMETRY"         = "#F7F0FA"
)

## ---------------------------
## 2) Données heatmap
## ---------------------------

df_heatmap <- df_domain_scores %>%
  pivot_longer(
    c(Score_High, Score_Medium),
    names_to = "Contrast",
    values_to = "Score"
  ) %>%
  mutate(
    Contrast = ifelse(Contrast == "Score_High", "High vs Even", "Medium vs Even")
  ) %>%
  group_by(AgeGroup, Domain, Contrast) %>%
  summarise(Mean_GRS = mean(Score, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    AgeGroup = factor(
      AgeGroup,
      levels = age_order,
      labels = unname(age_labels[age_order])
    ),
    Domain = factor(Domain, levels = names(domains))
  )

## ---------------------------
## 3) Mise à l'échelle globale
## ---------------------------
## Garde la comparabilité entre domaines

score_min <- min(df_heatmap$Mean_GRS, na.rm = TRUE)
score_max <- max(df_heatmap$Mean_GRS, na.rm = TRUE)

## ---------------------------
## 4) Fonctions utilitaires
## ---------------------------

# Couleur du texte selon la luminosité de la case
get_text_color <- function(hex_color) {
  rgb <- col2rgb(hex_color)
  # luminance perceptuelle
  luminance <- 0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]
  ifelse(luminance > 160, "black", "white")
}

## ---------------------------
## 5) Couleur de chaque case
## ---------------------------

df_heatmap <- df_heatmap %>%
  rowwise() %>%
  mutate(
    score_scaled = rescale(
      Mean_GRS,
      to = c(0, 1),
      from = c(score_min, score_max)
    ),
    fill_color = colour_ramp(c(
      domain_light_colors[as.character(Domain)],
      domain_dark_colors[as.character(Domain)]
    ))(score_scaled),
    text_color = get_text_color(fill_color)
  ) %>%
  ungroup()

## ---------------------------
## 6) Labels domaines colorés
## ---------------------------

domain_labels_colored <- c(
  "PACE"              = "<span style='color:#2166AC;'><b>PACE</b></span>",
  "RHYTHM"            = "<span style='color:#D7191C;'><b>RHYTHM</b></span>",
  "DYNAMIC_STABILITY" = "<span style='color:#1A9641;'><b>DYNAMIC<br>STABILITY</b></span>",
  "VARIABILITY"       = "<span style='color:#B8860B;'><b>VARIABILITY</b></span>",
  "ASYMMETRY"         = "<span style='color:#7B3294;'><b>ASYMMETRY</b></span>"
)

## ---------------------------
## 7) Heatmap finale
## ---------------------------

fig_heatmap <- ggplot(df_heatmap, aes(x = Domain, y = AgeGroup)) +
  geom_tile(aes(fill = fill_color), color = "white", linewidth = 1.1) +
  geom_text(
    aes(label = sprintf("%.2f", Mean_GRS), color = text_color),
    size = 4.0,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_discrete(labels = domain_labels_colored) +
  facet_wrap(~ Contrast, nrow = 1) +
  labs(
    title = "Gait Adaptation Score",
    x = "Gait Domain",
    y = "Age Group"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = ggtext::element_markdown(
      angle = 30, hjust = 1, size = 11, face = "plain"
    ),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "grey30", fill = NA, linewidth = 0.8),
    strip.background = element_rect(fill = "grey90", color = "grey30", linewidth = 0.8)
  )

ggsave(
  "GRS_Heatmap_AgeGroup_Domain_DomainGradient_Improved.png",
  fig_heatmap,
  width = 12.5,
  height = 5.4,
  dpi = 300
)


## -- 12) STATISTIQUES -----------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)

## Contrastes recommandés pour ANOVA Type III
options(contrasts = c("contr.sum", "contr.poly"))

## ============================================================
## 12.1) GLOBAL GRS
## ============================================================

# Mise en format long
df_global_long <- df_grs_global %>%
  pivot_longer(
    cols = c(GRS_High, GRS_Medium),
    names_to = "Contrast",
    values_to = "GRS"
  ) %>%
  mutate(
    Contrast = recode(Contrast,
                      "GRS_High"   = "High",
                      "GRS_Medium" = "Medium"),
    Contrast = factor(Contrast, levels = c("Medium", "High")),
    AgeGroup = factor(AgeGroup, levels = age_order)
  ) %>%
  filter(!is.na(GRS), !is.na(AgeGroup), !is.na(Participant))

# Vérifications structure
nrow(df_global_long)
n_distinct(df_global_long$Participant)

df_global_long %>%
  distinct(Participant, AgeGroup) %>%
  count(AgeGroup)

df_global_long %>%
  count(Participant)

# Modèle linéaire mixte
model_global <- lmer(
  GRS ~ AgeGroup * Contrast + (1 | Participant),
  data = df_global_long,
  REML = TRUE
)

# Résultats du modèle
summary(model_global)

# ANOVA Type III
anova_global <- anova(model_global, type = 3)
anova_global

# Post-hoc
emm_global_age <- emmeans(model_global, ~ AgeGroup | Contrast)
posthoc_global_age <- pairs(emm_global_age, adjust = "holm")

emm_global_contrast <- emmeans(model_global, ~ Contrast | AgeGroup)
posthoc_global_contrast <- pairs(emm_global_contrast, adjust = "holm")

# Affichage
emm_global_age
posthoc_global_age

emm_global_contrast
posthoc_global_contrast


## ============================================================
## 12.2) GRS PAR DOMAINE
## ============================================================

# Mise en format long
df_domain_long <- df_domain_scores %>%
  pivot_longer(
    cols = c(Score_High, Score_Medium),
    names_to = "Contrast",
    values_to = "Score"
  ) %>%
  mutate(
    Contrast = recode(Contrast,
                      "Score_High"   = "High",
                      "Score_Medium" = "Medium"),
    Contrast = factor(Contrast, levels = c("Medium", "High")),
    AgeGroup = factor(AgeGroup, levels = age_order),
    Domain   = factor(Domain, levels = names(domains))
  ) %>%
  filter(!is.na(Score), !is.na(AgeGroup), !is.na(Domain), !is.na(Participant))

# Vérifications structure
nrow(df_domain_long)
n_distinct(df_domain_long$Participant)

df_domain_long %>%
  distinct(Participant, AgeGroup) %>%
  count(AgeGroup)

df_domain_long %>%
  count(Participant)

# Modèle linéaire mixte
model_domain <- lmer(
  Score ~ AgeGroup * Contrast * Domain + (1 | Participant),
  data = df_domain_long,
  REML = TRUE
)

# Résultats du modèle
summary(model_domain)

# ANOVA Type III
anova_domain <- anova(model_domain, type = 3)
anova_domain

# Post-hoc : effet Domain global
emm_domain_main <- emmeans(model_domain, pairwise ~ Domain)
posthoc_domain_main <- pairs(emm_domain_main, adjust = "holm")

# Post-hoc : effet Age global
emm_domain_age <- emmeans(model_domain, pairwise ~ AgeGroup)
posthoc_domain_age <- pairs(emm_domain_age, adjust = "holm")

# Post-hoc : comparaison des groupes d'âge pour chaque domaine et contraste
emm_domain_age <- emmeans(model_domain, ~ AgeGroup | Domain * Contrast)
posthoc_domain_age <- pairs(emm_domain_age, adjust = "holm")

# Post-hoc : comparaison High vs Medium dans chaque groupe et domaine
emm_domain_contrast <- emmeans(model_domain, ~ Contrast | AgeGroup * Domain)
posthoc_domain_contrast <- pairs(emm_domain_contrast, adjust = "holm")

# Affichage
emm_domain_age
posthoc_domain_age

emm_domain_contrast
posthoc_domain_contrast

## -- 13) EXPORT EXCEL DES RÉSULTATS ----------------------------

library(openxlsx)

# --------------------------------------------------------------
# 13.1) Conversion des objets en data.frame
# --------------------------------------------------------------

# ANOVA
df_anova_global <- as.data.frame(anova_global)
df_anova_global <- tibble::rownames_to_column(df_anova_global, var = "Effect")

df_anova_domain <- as.data.frame(anova_domain)
df_anova_domain <- tibble::rownames_to_column(df_anova_domain, var = "Effect")

# Post-hoc globaux
df_posthoc_global_age <- as.data.frame(posthoc_global_age)
df_posthoc_global_contrast <- as.data.frame(posthoc_global_contrast)

# Post-hoc par domaine
df_posthoc_domain_age <- as.data.frame(posthoc_domain_age)
df_posthoc_domain_contrast <- as.data.frame(posthoc_domain_contrast)

df_emm_domain_by_age <- as.data.frame(emm_domain_by_age)
df_posthoc_domain_by_age <- as.data.frame(posthoc_domain_by_age)
# Moyennes estimées marginales (optionnel mais utile)
df_emm_global_age <- as.data.frame(emm_global_age)
df_emm_global_contrast <- as.data.frame(emm_global_contrast)

df_emm_domain_age <- as.data.frame(emm_domain_age)
df_emm_domain_contrast <- as.data.frame(emm_domain_contrast)

# --------------------------------------------------------------
# 13.2) Descriptifs
# --------------------------------------------------------------

desc_global <- df_global_long %>%
  group_by(AgeGroup, Contrast) %>%
  summarise(
    n    = n(),
    mean = mean(GRS, na.rm = TRUE),
    sd   = sd(GRS, na.rm = TRUE),
    med  = median(GRS, na.rm = TRUE),
    Q1   = quantile(GRS, 0.25, na.rm = TRUE),
    Q3   = quantile(GRS, 0.75, na.rm = TRUE),
    min  = min(GRS, na.rm = TRUE),
    max  = max(GRS, na.rm = TRUE),
    .groups = "drop"
  )

desc_domain <- df_domain_long %>%
  group_by(AgeGroup, Domain, Contrast) %>%
  summarise(
    n    = n(),
    mean = mean(Score, na.rm = TRUE),
    sd   = sd(Score, na.rm = TRUE),
    med  = median(Score, na.rm = TRUE),
    Q1   = quantile(Score, 0.25, na.rm = TRUE),
    Q3   = quantile(Score, 0.75, na.rm = TRUE),
    min  = min(Score, na.rm = TRUE),
    max  = max(Score, na.rm = TRUE),
    .groups = "drop"
  )

# --------------------------------------------------------------
# 13.3) Arrondir les colonnes numériques
# --------------------------------------------------------------

round_df <- function(df, digits = 4) {
  df %>%
    mutate(across(where(is.numeric), ~ round(.x, digits)))
}

df_anova_global            <- round_df(df_anova_global)
df_anova_domain            <- round_df(df_anova_domain)
df_posthoc_global_age      <- round_df(df_posthoc_global_age)
df_posthoc_global_contrast <- round_df(df_posthoc_global_contrast)
df_posthoc_domain_age      <- round_df(df_posthoc_domain_age)
df_posthoc_domain_contrast <- round_df(df_posthoc_domain_contrast)
df_emm_global_age          <- round_df(df_emm_global_age)
df_emm_global_contrast     <- round_df(df_emm_global_contrast)
df_emm_domain_age          <- round_df(df_emm_domain_age)
df_emm_domain_contrast     <- round_df(df_emm_domain_contrast)
desc_global                <- round_df(desc_global)
desc_domain                <- round_df(desc_domain)

# --------------------------------------------------------------
# 13.4) Création du classeur Excel
# --------------------------------------------------------------

wb <- createWorkbook()

# Styles
style_header <- createStyle(
  textDecoration = "bold",
  halign = "center",
  border = "Bottom"
)

# --------------------------------------------------------------
# 13.5) Ajout des feuilles
# --------------------------------------------------------------

# ANOVA Global
addWorksheet(wb, "ANOVA_Global")
writeData(wb, "ANOVA_Global", df_anova_global)
addStyle(wb, "ANOVA_Global", style_header, rows = 1, cols = 1:ncol(df_anova_global), gridExpand = TRUE)
setColWidths(wb, "ANOVA_Global", cols = 1:ncol(df_anova_global), widths = "auto")

# EMM Global Age
addWorksheet(wb, "EMM_Global_Age")
writeData(wb, "EMM_Global_Age", df_emm_global_age)
addStyle(wb, "EMM_Global_Age", style_header, rows = 1, cols = 1:ncol(df_emm_global_age), gridExpand = TRUE)
setColWidths(wb, "EMM_Global_Age", cols = 1:ncol(df_emm_global_age), widths = "auto")

# Posthoc Global Age
addWorksheet(wb, "Posthoc_Global_Age")
writeData(wb, "Posthoc_Global_Age", df_posthoc_global_age)
addStyle(wb, "Posthoc_Global_Age", style_header, rows = 1, cols = 1:ncol(df_posthoc_global_age), gridExpand = TRUE)
setColWidths(wb, "Posthoc_Global_Age", cols = 1:ncol(df_posthoc_global_age), widths = "auto")

# EMM Global Contrast
addWorksheet(wb, "EMM_Global_Contrast")
writeData(wb, "EMM_Global_Contrast", df_emm_global_contrast)
addStyle(wb, "EMM_Global_Contrast", style_header, rows = 1, cols = 1:ncol(df_emm_global_contrast), gridExpand = TRUE)
setColWidths(wb, "EMM_Global_Contrast", cols = 1:ncol(df_emm_global_contrast), widths = "auto")

# Posthoc Global Contrast
addWorksheet(wb, "Posthoc_Global_Contrast")
writeData(wb, "Posthoc_Global_Contrast", df_posthoc_global_contrast)
addStyle(wb, "Posthoc_Global_Contrast", style_header, rows = 1, cols = 1:ncol(df_posthoc_global_contrast), gridExpand = TRUE)
setColWidths(wb, "Posthoc_Global_Contrast", cols = 1:ncol(df_posthoc_global_contrast), widths = "auto")

# ANOVA Domain
addWorksheet(wb, "ANOVA_Domain")
writeData(wb, "ANOVA_Domain", df_anova_domain)
addStyle(wb, "ANOVA_Domain", style_header, rows = 1, cols = 1:ncol(df_anova_domain), gridExpand = TRUE)
setColWidths(wb, "ANOVA_Domain", cols = 1:ncol(df_anova_domain), widths = "auto")

# EMM Domain Age
addWorksheet(wb, "EMM_Domain_Age")
writeData(wb, "EMM_Domain_Age", df_emm_domain_age)
addStyle(wb, "EMM_Domain_Age", style_header, rows = 1, cols = 1:ncol(df_emm_domain_age), gridExpand = TRUE)
setColWidths(wb, "EMM_Domain_Age", cols = 1:ncol(df_emm_domain_age), widths = "auto")

# Posthoc Domain Age
addWorksheet(wb, "Posthoc_Domain_Age")
writeData(wb, "Posthoc_Domain_Age", df_posthoc_domain_age)
addStyle(wb, "Posthoc_Domain_Age", style_header, rows = 1, cols = 1:ncol(df_posthoc_domain_age), gridExpand = TRUE)
setColWidths(wb, "Posthoc_Domain_Age", cols = 1:ncol(df_posthoc_domain_age), widths = "auto")

# EMM Domain Contrast
addWorksheet(wb, "EMM_Domain_Contrast")
writeData(wb, "EMM_Domain_Contrast", df_emm_domain_contrast)
addStyle(wb, "EMM_Domain_Contrast", style_header, rows = 1, cols = 1:ncol(df_emm_domain_contrast), gridExpand = TRUE)
setColWidths(wb, "EMM_Domain_Contrast", cols = 1:ncol(df_emm_domain_contrast), widths = "auto")

# Posthoc Domain Contrast
addWorksheet(wb, "Posthoc_Domain_Contrast")
writeData(wb, "Posthoc_Domain_Contrast", df_posthoc_domain_contrast)
addStyle(wb, "Posthoc_Domain_Contrast", style_header, rows = 1, cols = 1:ncol(df_posthoc_domain_contrast), gridExpand = TRUE)
setColWidths(wb, "Posthoc_Domain_Contrast", cols = 1:ncol(df_posthoc_domain_contrast), widths = "auto")

# Descriptifs globaux
addWorksheet(wb, "Descriptifs_Global")
writeData(wb, "Descriptifs_Global", desc_global)
addStyle(wb, "Descriptifs_Global", style_header, rows = 1, cols = 1:ncol(desc_global), gridExpand = TRUE)
setColWidths(wb, "Descriptifs_Global", cols = 1:ncol(desc_global), widths = "auto")

# Descriptifs par domaine
addWorksheet(wb, "Descriptifs_Domain")
writeData(wb, "Descriptifs_Domain", desc_domain)
addStyle(wb, "Descriptifs_Domain", style_header, rows = 1, cols = 1:ncol(desc_domain), gridExpand = TRUE)
setColWidths(wb, "Descriptifs_Domain", cols = 1:ncol(desc_domain), widths = "auto")

# --------------------------------------------------------------
# 13.6) Sauvegarde
# --------------------------------------------------------------

addWorksheet(wb, "Scores_Individuels")

writeData(wb, "Scores_Individuels", df_scores_wide)

addStyle(
  wb, "Scores_Individuels", style_header,
  rows = 1, cols = 1:ncol(df_scores_wide), gridExpand = TRUE
)

setColWidths(
  wb, "Scores_Individuels",
  cols = 1:ncol(df_scores_wide), widths = "auto"
)

addWorksheet(wb, "Scores_Par_Domaine")

writeData(wb, "Scores_Par_Domaine", df_domain_scores)

addStyle(
  wb, "Scores_Par_Domaine", style_header,
  rows = 1, cols = 1:ncol(df_domain_scores), gridExpand = TRUE
)

setColWidths(
  wb, "Scores_Par_Domaine",
  cols = 1:ncol(df_domain_scores), widths = "auto"
)

addWorksheet(wb, "Zscore_Details")

writeData(wb, "Zscore_Details", df_zscore)

addStyle(
  wb, "Zscore_Details", style_header,
  rows = 1, cols = 1:ncol(df_zscore), gridExpand = TRUE
)

setColWidths(
  wb, "Zscore_Details",
  cols = 1:ncol(df_zscore), widths = "auto"
)

saveWorkbook(
  wb,
  file = "LMM_GRS_Results.xlsx",
  overwrite = TRUE
)

message("✓ Fichier Excel enregistré : LMM_GRS_Results.xlsx")




## ============================================================
## IV. LMM & POST-HOCS SUR VARIABLES D'INTÉRÊT
## ============================================================
## 1) ANOVA Type III
## 2) Post-hocs : Effet GLOBAL de la Surface (toutes surfaces confondues)
## 3) Post-hocs : Effet GLOBAL de l'Âge (tous âges confondus)
## 4) Post-hocs : Effet de la Surface par Groupe d'Âge
## 5) Post-hocs : Effet de l'Âge par Surface
## ============================================================

## 1) Chargement des packages nécessaires
library(lme4)
library(lmerTest)
library(emmeans)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(openxlsx)
library(stringr)
library(effectsize)

## 2) Chargement et préparation des données
csv_path <- "XX" #Vers le csv de la matrice de données extraite

first_line <- readLines(csv_path, n = 1, warn = FALSE)
delim <- ifelse(grepl(";", first_line), ";", ",")
df <- read_delim(csv_path, delim = delim, show_col_types = FALSE)

# Fonction de standardisation des noms (identique à la partie II)
standardize_names <- function(x) {
  x %>%
    str_trim() %>%
    str_replace_all("[[:space:]]+", "_") %>%
    str_replace_all("[\\-]+", "_") %>%
    str_replace_all("[,;:]+", "_") %>%
    str_replace_all("\\(mm\\)", "mm") %>%
    str_replace_all("\\(%L0\\)", "pL0") %>%
    str_replace_all("\\(ua\\)", "ua") %>%
    str_replace_all("\\(%\\)", "p") %>%
    str_replace_all("\\(m\\.s\\^\\{-1\\}\\)", "ms1") %>%
    str_replace_all("\\(step\\.min\\^\\{-1\\}\\)", "stepmin1") %>%
    str_replace_all("[\\(\\)]", "") %>%
    str_replace_all("__+", "_") %>%
    str_replace_all("_$", "")
}
names(df) <- standardize_names(names(df))

## 3) Définir l'ordre des facteurs
surfaces <- c("Plat","Medium","High")
groups   <- c("JeunesEnfants","Enfants","Adolescents","Adultes")

df <- df %>%
  mutate(
    Participant = factor(Participant),
    Surface     = factor(Surface, levels = surfaces),
    AgeGroup    = factor(AgeGroup, levels = groups, ordered = TRUE)
  )

## 4) Liste des variables à tester
variables_to_test <- c(
  "Mean_Gait_speed_m.s^{_1}", "Mean_Norm_Gait_Speed_m.s^{_1}", "Mean_Step_length_m", "Mean_Stride_length_m",  "Mean_Norm_Step_length_ua", "Mean_WalkRatio", "Mean_Norm_WR_ua", "Mean_Stride_time_s", "Mean_StepTime_s",
  
  "Mean_Double_support_time_p", "Mean_Cadence_step.min^{_1}", "Mean_Norm_Cadence_ua", "Mean_COM_SPARC_Magnitude_ua", "Mean_StanceTime_s", "Mean_SwingTime_s",
  
  "Mean_StepWidth_cm", "Mean_Norm_StepWidth_ua", "Mean_MoS_AP_HS_mm", "Mean_MoS_ML_HS_mm", "Mean_MoS_AP_Stance_mm", "Mean_MoS_ML_Stance_mm", "Mean_MoS_AP_HS_pL0", "Mean_MoS_ML_HS_pL0", "Mean_MoS_AP_Stance_pL0", "Mean_MoS_ML_Stance_pL0", "Mean_StanceTime_s", "Mean_Single_support_time_p", "Mean_SwingTime_s",
  
  "Mean_GVI_ua", "CV_Norm_StepWidth_ua", "CV_Gait_speed_m.s^{_1}",
  
  "SI_Stride_length_m", "SI_Double_support_time_p", "SI_Norm_StepWidth_ua"
)

## 5) Fonction pour fitter un LMM + ANOVA type III + 4 TYPES DE POST-HOCS
fit_one_variable <- function(var_name, data, p_adjust = "holm") {
  
  if(!var_name %in% names(data)) {
    return(list(
      anova = tibble::tibble(Variable = var_name, Effect = NA, df1 = NA, df2 = NA, F = NA, p = NA,
                             Note = "Variable absente du CSV"),
      ph_surface_global = NULL,
      ph_age_global = NULL,
      ph_surface_within_age = NULL,
      ph_age_within_surface = NULL
    ))
  }
  
  d <- data %>%
    dplyr::select(Participant, Surface, AgeGroup, dplyr::all_of(var_name)) %>%
    dplyr::filter(!is.na(.data[[var_name]]))
  
  fml <- stats::as.formula(paste0("`", var_name, "` ~ Surface * AgeGroup + (1|Participant)"))
  
  m <- lmerTest::lmer(fml, data = d, REML = TRUE)
  
  ## ============================================================
  ## A) ANOVA Type III & Effect Sizes (Eta2 Partiels)
  ## ============================================================
  a_raw <- stats::anova(m, type = 3, ddf = "Satterthwaite")
  
  # Calcul des Eta2 partiels
  es <- effectsize::eta_squared(m, partial = TRUE, generalized = FALSE, ci = 0.95)
  
  # Conversion en data.frame pour manipulation
  a_df_temp <- as.data.frame(a_raw)
  
  # Extraction sécurisée des colonnes 
  get_col <- function(df, possible_names) {
    name <- intersect(possible_names, names(df))[1]
    if (is.na(name)) return(NA_real_)
    return(df[[name]])
  }
  
  a_df <- a_df_temp %>%
    tibble::rownames_to_column("Effect") %>%
    dplyr::left_join(as.data.frame(es) %>% dplyr::rename(Effect = Parameter), by = "Effect") %>%
    dplyr::mutate(
      Variable = var_name,
      df1 = get_col(a_df_temp, c("NumDF", "Df")),
      df2 = get_col(a_df_temp, c("DenDF", "df.res")),
      F   = get_col(a_df_temp, c("F value", "F.value", "F")),
      p   = get_col(a_df_temp, c("Pr(>F)", "p.value", "P"))
    ) %>%
    dplyr::select(Variable, Effect, df1, df2, F, p, Eta2_partial, CI_low, CI_high)
  
  ## ============================================================
  ## B) Post-hocs : Effet GLOBAL de la Surface
  ## ============================================================
  em_surf_glob <- emmeans::emmeans(m, ~ Surface)
  # infer = c(TRUE, TRUE) ajoute lower.CL et upper.CL (IC95%)
  ph_surf_global <- as.data.frame(pairs(em_surf_glob, adjust = p_adjust, infer = c(TRUE, TRUE)))
  # Ajout du d de Cohen
  d_surf_glob <- as.data.frame(eff_size(em_surf_glob, sigma = sigma(m), edf = df.residual(m)))
  
  ph_surf_global <- ph_surf_global %>%
    dplyr::mutate(Variable = var_name, Analysis = "Global Surface", 
                  cohen_d = d_surf_glob$effect.size) %>%
    dplyr::relocate(Variable, Analysis)
  
  ## ============================================================
  ## C) Post-hocs : Effet GLOBAL de l'Âge
  ## ============================================================
  em_age_glob <- emmeans::emmeans(m, ~ AgeGroup)
  ph_age_global <- as.data.frame(pairs(em_age_glob, adjust = p_adjust, infer = c(TRUE, TRUE)))
  d_age_glob <- as.data.frame(eff_size(em_age_glob, sigma = sigma(m), edf = df.residual(m)))
  
  ph_age_global <- ph_age_global %>%
    dplyr::mutate(Variable = var_name, Analysis = "Global AgeGroup",
                  cohen_d = d_age_glob$effect.size) %>%
    dplyr::relocate(Variable, Analysis)
  
  ## ============================================================
  ## D) Post-hocs : Effet de la Surface par Groupe d'Âge
  ## ============================================================
  em_surf_by_age <- emmeans::emmeans(m, ~ Surface | AgeGroup)
  ph1 <- as.data.frame(pairs(em_surf_by_age, adjust = p_adjust, infer = c(TRUE, TRUE)))
  d_surf_by_age <- as.data.frame(eff_size(em_surf_by_age, sigma = sigma(m), edf = df.residual(m)))
  
  ph1 <- ph1 %>%
    dplyr::mutate(Variable = var_name, Analysis = "Surface within AgeGroup",
                  cohen_d = d_surf_by_age$effect.size) %>%
    dplyr::relocate(Variable, Analysis)
  
  ## ============================================================
  ## E) Post-hocs : Effet de l'Âge par Surface
  ## ============================================================
  em_age_by_surf <- emmeans::emmeans(m, ~ AgeGroup | Surface)
  ph2 <- as.data.frame(pairs(em_age_by_surf, adjust = p_adjust, infer = c(TRUE, TRUE)))
  d_age_by_surf <- as.data.frame(eff_size(em_age_by_surf, sigma = sigma(m), edf = df.residual(m)))
  
  ph2 <- ph2 %>%
    dplyr::mutate(Variable = var_name, Analysis = "AgeGroup within Surface",
                  cohen_d = d_age_by_surf$effect.size) %>%
    dplyr::relocate(Variable, Analysis)
  
  ## ============================================================
  ## F) Calcul des R² (Nakagawa)
  ## ============================================================
  r2_vals <- performance::r2_nakagawa(m)
  
  list(
    model = m,
    anova = a_df,
    ph_surface_global = ph_surf_global,
    ph_age_global = ph_age_global,
    ph_surface_within_age = ph1,
    ph_age_within_surface = ph2,
    r2 = tibble::tibble(
      Variable = var_name,
      R2_Marginal = r2_vals$R2_marginal,
      R2_Conditional = r2_vals$R2_conditional
    )
  )
}

## 6) Lancer sur toutes les variables
message("Analyse en cours pour ", length(variables_to_test), " variables...")
results <- map(variables_to_test, fit_one_variable, data = df, p_adjust = "holm")

## 7) Extraire les résultats
anova_all <- bind_rows(map(results, "anova"))

posthoc_surface_global <- bind_rows(compact(map(results, "ph_surface_global")))
posthoc_age_global <- bind_rows(compact(map(results, "ph_age_global")))
posthoc_surface_within_age <- bind_rows(compact(map(results, "ph_surface_within_age")))
posthoc_age_within_surface <- bind_rows(compact(map(results, "ph_age_within_surface")))

r2_all <- bind_rows(compact(map(results, "r2")))

## 8) Appliquer FDR par famille d'effets
apply_fdr <- function(tbl) {
  tbl %>%
    filter(Effect %in% c("Surface","AgeGroup","Surface:AgeGroup")) %>%
    mutate(EffectFamily = case_when(
      Effect == "Surface" ~ "Surface",
      Effect == "AgeGroup" ~ "AgeGroup",
      Effect == "Surface:AgeGroup" ~ "Interaction"
    )) %>%
    group_by(EffectFamily) %>%
    mutate(p_fdr = p.adjust(p, method = "BH")) %>%
    ungroup()
}

anova_all_fdr <- apply_fdr(anova_all)

## ============================================================
## 9) RÉCAPITULATIF POUR LA DISCUSSION
## ============================================================

# A) Variables influencées par chaque effet (p_fdr < 0.05) avec Eta2 Moyen
listes_effets <- anova_all_fdr %>%
  filter(p_fdr < 0.05) %>%
  group_by(EffectFamily) %>%
  summarise(
    Nombre_Variables = n(),
    Eta2_moyen = mean(Eta2_partial, na.rm = TRUE),
    Variables = paste(unique(Variable), collapse = " | ")
  ) %>%
  rename(Type_Effet = EffectFamily)

# B) Tableau de maturation par surface
maturation_final <- posthoc_age_within_surface %>%
  filter(grepl("Adultes", contrast)) %>%
  mutate(Groupe = str_remove(contrast, " - Adultes") %>% str_trim()) %>%
  filter(p.value > 0.05) %>%
  group_by(Variable, Surface) %>%
  arrange(factor(Groupe, levels = c("JeunesEnfants", "Enfants", "Adolescents"))) %>%
  slice(1) %>%
  select(Variable, Surface, Groupe) %>%
  pivot_wider(names_from = Surface, values_from = Groupe)

## ============================================================
## 10) EXPORT DES RÉSULTATS
## ============================================================
out_dir <- file.path(dirname(csv_path), "R_LMM_Output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# A) Fichier Excel multi-onglets PRINCIPAL
xlsx_path <- file.path(out_dir, "LMM_ANOVA_and_Posthocs_COMPLET.xlsx")
wb <- createWorkbook()

# Onglet 1 : ANOVA Type III avec FDR
addWorksheet(wb, "ANOVA_TypeIII_FDR")
writeData(wb, "ANOVA_TypeIII_FDR", anova_all_fdr)

# Onglet 2 : R² (Marginal et Conditionnel)
addWorksheet(wb, "R2_Effect_Sizes")
writeData(wb, "R2_Effect_Sizes", r2_all)

# Onglet 3 : Post-hocs GLOBAL Surface
addWorksheet(wb, "Posthoc_Surface_Global")
writeData(wb, "Posthoc_Surface_Global", posthoc_surface_global)

# Onglet 4 : Post-hocs GLOBAL Âge
addWorksheet(wb, "Posthoc_Age_Global")
writeData(wb, "Posthoc_Age_Global", posthoc_age_global)

# Onglet 5 : Post-hocs Surface par Âge
addWorksheet(wb, "Posthoc_Surface_by_Age")
writeData(wb, "Posthoc_Surface_by_Age", posthoc_surface_within_age)

# Onglet 6 : Post-hocs Âge par Surface
addWorksheet(wb, "Posthoc_Age_by_Surface")
writeData(wb, "Posthoc_Age_by_Surface", posthoc_age_within_surface)

saveWorkbook(wb, xlsx_path, overwrite = TRUE)

# B) Fichier Excel SYNTHÈSE pour la discussion
xlsx_synthese <- file.path(out_dir, "Synthese_Resultats_LMM_Discussion.xlsx")
wb_synth <- createWorkbook()

addWorksheet(wb_synth, "Liste_Effets_Significatifs")
writeData(wb_synth, "Liste_Effets_Significatifs", listes_effets)

addWorksheet(wb_synth, "Maturation_Par_Surface")
writeData(wb_synth, "Maturation_Par_Surface", maturation_final)

saveWorkbook(wb_synth, xlsx_synthese, overwrite = TRUE)

# C) Export CSV (optionnel)
write_csv(anova_all_fdr, file.path(out_dir, "ANOVA_TypeIII_with_FDR.csv"))
write_csv(r2_all, file.path(out_dir, "R2_Effect_Sizes.csv"))
write_csv(posthoc_surface_global, file.path(out_dir, "Posthoc_Surface_GLOBAL_Holm.csv"))
write_csv(posthoc_age_global, file.path(out_dir, "Posthoc_Age_GLOBAL_Holm.csv"))
write_csv(posthoc_surface_within_age, file.path(out_dir, "Posthoc_Surface_within_AgeGroup_Holm.csv"))
write_csv(posthoc_age_within_surface, file.path(out_dir, "Posthoc_AgeGroup_within_Surface_Holm.csv"))

## ============================================================
## 12) VISUALISATION : PANEL 3 FIGURES (PAR SURFACE)
## ============================================================
library(ggplot2)
library(patchwork)

# Dossier de sortie spécifique
plots_tri_dir <- file.path(out_dir, "Figures_Evolution_Age_Sexe")
dir.create(plots_tri_dir, showWarnings = FALSE)

# Fonction de génération du triplet
plot_triple_panel <- function(var_name, data) {
  
  # Filtrage des données valides
  df_plot <- data %>%
    select(AgeGroup, Surface, Sex, all_of(var_name)) %>%
    filter(!is.na(.data[[var_name]]))
  
  # Création d'une fonction interne pour générer un sous-graphique par surface
  make_sub_plot <- function(surf_level, title_letter, show_y_label = TRUE) {
    df_sub <- df_plot %>% filter(Surface == surf_level)
    
    p <- ggplot(df_sub, aes(x = AgeGroup, y = .data[[var_name]], fill = Sex)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.6) +
      geom_point(aes(color = Sex), 
                 position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6), 
                 size = 1, alpha = 0.4) +
      labs(title = paste0(title_letter, ") Surface : ", surf_level),
           x = "Groupe d'Âge",
           y = if(show_y_label) var_name else "") +
      scale_fill_manual(values = c("F" = "#F8766D", "M" = "#00BFC4")) +
      scale_color_manual(values = c("F" = "#F8766D", "M" = "#00BFC4")) +
      theme_bw() +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    return(p)
  }
  
  # Génération des 3 panneaux
  p1 <- make_sub_plot("Plat", "a", show_y_label = TRUE)
  p2 <- make_sub_plot("Medium", "b", show_y_label = FALSE)
  p3 <- make_sub_plot("High", "c", show_y_label = FALSE)
  
  # Assemblage avec patchwork
  # On récupère la légende d'un des graphiques pour l'afficher en bas
  combined_plot <- (p1 | p2 | p3) + 
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom")
  
  combined_plot <- combined_plot + 
    plot_annotation(
      title = paste("Évolution de", var_name, "par Âge, Sexe et Surface"),
      caption = "Les boîtes représentent les quartiles ; les points individuels sont jitterisés."
    )
  
  # Sauvegarde
  file_var <- standardize_names(var_name)
  ggsave(file.path(plots_tri_dir, paste0("Triple_", file_var, ".png")), 
         combined_plot, width = 15, height = 6, dpi = 300)
}

# Lancer la boucle sur toutes les variables
message("Génération des panels triples en cours...")
walk(variables_to_test, ~ {
  if(.x %in% names(df)) plot_triple_panel(.x, df)
})

message("✓ Terminée ! Les figures sont dans : ", plots_tri_dir)

message("\n=== ANALYSE TERMINÉE ===")
