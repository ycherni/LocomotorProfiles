## ============================================================
## III. LMM & POST-HOCS AVEC COVARIABLE ACTIVITÉ PHYSIQUE
## Groupes inclus : Enfants, Adolescents, Adultes
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
library(performance)

## 2) Chargement et préparation des données
csv_path <- "C:/Users/silve/Desktop/DOCTORAT/UNIV MONTREAL/TRAVAUX-THESE/Surfaces_Irregulieres/Datas/Script/gaitAnalysisGUI/result/Statistical_Analysis_LMM/Prepared_Data/ACP_Clustering_DATA.csv"

# Lecture du fichier principal
first_line <- readLines(csv_path, n = 1, warn = FALSE)
delim <- ifelse(grepl(";", first_line), ";", ",")
df <- read_delim(csv_path, delim = delim, show_col_types = FALSE)

# --- AJOUT : Chargement activité physique avec détection de délimiteur ---
path_phys <- "C:/Users/silve/Desktop/DOCTORAT/UNIV MONTREAL/TRAVAUX-THESE/Surfaces_Irregulieres/Datas/PhysicalActi_et_VAS/PhysicalActivity_Zscored.csv"
line_phys <- readLines(path_phys, n = 1, warn = FALSE)
delim_phys <- ifelse(grepl(";", line_phys), ";", ",")

df_phys <- read_delim(path_phys, delim = delim_phys, show_col_types = FALSE)

# Normalisation du nom "Participant" si BOM Excel présent (ï..Participant)
names(df_phys)[1] <- "Participant"

df_phys <- df_phys %>% select(Participant, zscore)

# Fusion
df <- left_join(df, df_phys, by = "Participant")

# Fonction de standardisation des noms
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

## 3) Définir l'ordre des facteurs (On retire JeunesEnfants pour cette analyse)
surfaces <- c("Plat","Medium","High")
groups   <- c("Enfants","Adolescents","Adultes") 

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

## 5) Fonction pour fitter un LMM + ANOVA + Post-hocs
fit_one_variable <- function(var_name, data, p_adjust = "holm") {
  
  if(!var_name %in% names(data)) return(NULL)
  
  # Filtrage : On garde uniquement ceux qui ont la variable ET le zscore 
  # (Cela exclut automatiquement les JeunesEnfants)
  d <- data %>%
    dplyr::select(Participant, Surface, AgeGroup, zscore, dplyr::all_of(var_name)) %>%
    dplyr::filter(!is.na(.data[[var_name]]), !is.na(zscore)) 
  
  if(nrow(d) == 0) return(NULL)
  
  # Formule avec covariable
  fml <- stats::as.formula(paste0("`", var_name, "` ~ Surface * AgeGroup + zscore + (1|Participant)"))
  
  m <- lmerTest::lmer(fml, data = d, REML = TRUE)
  
  # A) ANOVA Type III & Effect Sizes
  a_raw <- stats::anova(m, type = 3, ddf = "Satterthwaite")
  es <- effectsize::eta_squared(m, partial = TRUE, ci = 0.95)
  
  a_df <- as.data.frame(a_raw) %>%
    tibble::rownames_to_column("Effect") %>%
    dplyr::left_join(as.data.frame(es) %>% dplyr::rename(Effect = Parameter), by = "Effect") %>%
    dplyr::mutate(Variable = var_name)
  
  # B) Post-hocs
  em_surf_global <- emmeans::emmeans(m, ~ Surface)
  ph_surf_global <- as.data.frame(pairs(em_surf_global, adjust = p_adjust)) %>%
    dplyr::mutate(Variable = var_name, Analysis = "Global Surface")
  
  em_age_global <- emmeans::emmeans(m, ~ AgeGroup)
  ph_age_global <- as.data.frame(pairs(em_age_global, adjust = p_adjust)) %>%
    dplyr::mutate(Variable = var_name, Analysis = "Global AgeGroup")
  
  em_surf_by_age <- emmeans::emmeans(m, ~ Surface | AgeGroup)
  ph_surf_age <- as.data.frame(pairs(em_surf_by_age, adjust = p_adjust)) %>%
    dplyr::mutate(Variable = var_name, Analysis = "Surface within AgeGroup")
  
  em_age_by_surf <- emmeans::emmeans(m, ~ AgeGroup | Surface)
  ph_age_surf <- as.data.frame(pairs(em_age_by_surf, adjust = p_adjust)) %>%
    dplyr::mutate(Variable = var_name, Analysis = "AgeGroup within Surface")
  
  # C) R2
  r2_vals <- performance::r2_nakagawa(m)
  
  list(
    anova = a_df,
    ph_surface_global = ph_surf_global,
    ph_age_global = ph_age_global,
    ph_surface_within_age = ph_surf_age,
    ph_age_within_surface = ph_age_surf,
    r2 = tibble::tibble(Variable = var_name, R2_Marginal = r2_vals$R2_marginal, R2_Conditional = r2_vals$R2_conditional)
  )
}

## 6) Exécution
results <- map(variables_to_test, fit_one_variable, data = df) %>% compact()

## 7) Extraction et FDR
anova_all <- bind_rows(map(results, "anova"))

apply_fdr <- function(tbl) {
  tbl %>%
    filter(Effect %in% c("Surface", "AgeGroup", "Surface:AgeGroup", "zscore")) %>%
    group_by(Effect) %>%
    mutate(p_fdr = p.adjust(`Pr(>F)`, method = "BH")) %>%
    ungroup()
}
anova_all_fdr <- apply_fdr(anova_all)

## 10) EXPORT
out_dir <- file.path(dirname(csv_path), "R_LMM_Output_With_PhysActivity")
dir.create(out_dir, showWarnings = FALSE)

xlsx_path <- file.path(out_dir, "LMM_Results_PhysActivity_3Groups.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "ANOVA_FDR")
writeData(wb, "ANOVA_FDR", anova_all_fdr)
addWorksheet(wb, "Posthoc_Surf_by_Age")
writeData(wb, "Posthoc_Surf_by_Age", bind_rows(map(results, "ph_surface_within_age")))
addWorksheet(wb, "Posthoc_Age_by_Surf")
writeData(wb, "Posthoc_Age_by_Surf", bind_rows(map(results, "ph_age_within_surface")))
saveWorkbook(wb, xlsx_path, overwrite = TRUE)

message("✓ Analyse terminée. Résultats exportés dans : ", out_dir)