# ============================================================
# MIGRATIONS CLUSTERS — TOUS LES PARTICIPANTS (n = 68)
# Even → Medium → High
# ============================================================
# Structure :
#   A. Alluvial global (68 participants)
#   B. Classification 3 groupes : Stable C1 / Migrants / Stable C2 / Autre
#   C. Caractérisation âge + sexe par groupe (Kruskal-Wallis + Fisher)
#   D. Sous-analyse migrants : Early vs Late (Medium vs High)
#   E. Comparaison d'âge Early vs Late (Wilcoxon)
#   F. Visualisation Early vs Late
#   G. Construire un panel des 15 variables
#   H. Tableau descriptif des varaibles pour tous les groupes
# ============================================================

library(dplyr)
library(tidyverse)
library(ggalluvial)
library(ggplot2)
library(gtsummary)
library(gt)
library(rstatix)
library(rcompanion)
library(patchwork)
library(ggpubr)

setwd("C:/Users/silve/Desktop/DOCTORAT/UNIV MONTREAL/TRAVAUX-THESE/Surfaces_Irregulieres/Datas/Script/gaitAnalysisGUI/result/Fig/Clustering")


# ============================================================
# 0. IMPORT DES DONNÉES
# ============================================================

df_clust <- read.csv("DATA_FOR_R_GLOBAL_20260123_1409.csv",
                     sep = ";", check.names = FALSE)

df_meta  <- read.csv(file.choose(), sep = ";", check.names = FALSE)
# → "participant.metadonnees"

df_pa    <- read.csv(file.choose(), sep = ";", check.names = FALSE)
# → "PhysicalActivity_Zscored.csv"


# ============================================================
# 1. NETTOYAGE MÉTADONNÉES
# ============================================================

df_meta_clean <- df_meta %>%
  select(Participant, AgeMonths, Sex, AgeGroup, Height_cm, Weight_kg, IMC) %>%
  distinct()

df_pa_clean <- df_pa %>%
  select(Participant, AgeGroup, zscore) %>%
  rename(PA_Zscore = zscore) %>%
  distinct()


# ============================================================
# 2. CONSTRUCTION DU TABLEAU DE TRAJECTOIRES (TOUS PARTICIPANTS)
# ============================================================

# 2.1 Pivot : une ligne par participant avec les 3 ClusterID
trajectories_all <- df_clust %>%
  filter(Condition %in% c("Plat", "Medium", "High")) %>%
  select(Participant, Condition, ClusterID) %>%
  pivot_wider(names_from = Condition, values_from = ClusterID) %>%
  rename(
    Cluster_Even   = Plat,
    Cluster_Medium = Medium,
    Cluster_High   = High
  ) %>%
  filter(!is.na(Cluster_Even) & !is.na(Cluster_Medium) & !is.na(Cluster_High))

cat("Participants avec les 3 surfaces :", nrow(trajectories_all), "\n")

# 2.2 Classification des trajectoires
trajectories_all <- trajectories_all %>%
  mutate(
    Trajectory = case_when(
      Cluster_Even == 1 & Cluster_Medium == 1 & Cluster_High == 1 ~ "Stable C1",
      Cluster_Even == 2 & Cluster_Medium == 2 & Cluster_High == 2 ~ "Stable C2",
      # Migrants C1→C2 : basculent vers C2 sur High (via Medium ou non)
      Cluster_Even == 1 & Cluster_Medium == 2 & Cluster_High == 2 ~ "Early Migrant",
      Cluster_Even == 1 & Cluster_Medium == 1 & Cluster_High == 2 ~ "Late Migrant",
      Cluster_Even == 1 & Cluster_Medium == 2 & Cluster_High == 1 ~ "Transient",  # Transient
      # Tous les autres (C2→C1, C2→..., etc.)
      TRUE ~ "Transient"
    ),
    # Groupe de haut niveau (3 catégories pour les comparaisons)
    Group = case_when(
      Trajectory == "Stable C1"                        ~ "Stable C1",
      Trajectory %in% c("Early Migrant","Late Migrant") ~ "Migrants C1->C2",
      Trajectory == "Stable C2"                        ~ "Stable C2",
      TRUE                                             ~ "Transient"
    ),
    Trajectory_Label = paste0("C", Cluster_Even, "→C", Cluster_Medium, "→C", Cluster_High)
  )

# Résumé
cat("\n=== RÉPARTITION DES TRAJECTOIRES ===\n")
recap <- trajectories_all %>%
  count(Trajectory, Trajectory_Label) %>%
  arrange(desc(n)) %>%
  mutate(Pct = round(100 * n / sum(n), 1))
print(recap)

write_csv(trajectories_all, "Trajectories_AllParticipants.csv")


# ============================================================
# 3. JOINTURE AVEC MÉTADONNÉES
# ============================================================

df_all <- trajectories_all %>%
  left_join(df_meta_clean, by = "Participant") %>%
  left_join(df_pa_clean, by = c("Participant" = "Participant"))

# Gérer les doublons de colonnes si nécessaire
if ("AgeMonths.x" %in% colnames(df_all)) {
  df_all <- df_all %>%
    mutate(AgeMonths = coalesce(AgeMonths.x, AgeMonths.y)) %>%
    select(-AgeMonths.x, -AgeMonths.y)
}
if ("AgeGroup.x" %in% colnames(df_all)) {
  df_all <- df_all %>%
    mutate(AgeGroup = coalesce(AgeGroup.x, AgeGroup.y)) %>%
    select(-AgeGroup.x, -AgeGroup.y)
}

# Factorisation
df_all$Group <- factor(
  df_all$Group,
  levels = c("Stable C1", "Migrants C1->C2", "Stable C2", "Transient")
)
df_all$Trajectory <- factor(
  df_all$Trajectory,
  levels = c("Stable C1", "Early Migrant", "Late Migrant", "Stable C2", "Transient")
)

write_csv(df_all, "AllParticipants_Groups.csv")


# ============================================================
# A1. DIAGRAMME ALLUVIAL GLOBAL (tous participants)
# ============================================================

# Couleurs par trajectoire
traj_colors_all <- c(
  "Stable C1"    = "#2C7FB8",
  "Early Migrant"= "#FDB863",
  "Late Migrant" = "darkred",
  "Stable C2"    = "#31A354",
  "Transient"    = "#9E9E9E"
)

# Préparation données alluviales
df_allu_all <- df_all %>%
  mutate(
    Even   = factor(paste0("C", Cluster_Even),   levels = c("C2", "C1")),
    Medium = factor(paste0("C", Cluster_Medium), levels = c("C2", "C1")),
    High   = factor(paste0("C", Cluster_High),   levels = c("C2", "C1"))
  ) %>%
  group_by(Trajectory, Even, Medium, High) %>%
  summarise(n = n(), .groups = "drop")

plot_alluvial_all <- ggplot(
  df_allu_all,
  aes(y = n, axis1 = Even, axis2 = Medium, axis3 = High)
) +
  geom_alluvium(aes(fill = Trajectory), width = 1/8, alpha = 0.70) +
  geom_stratum(width = 1/6, fill = "white", color = "black", linewidth = 0.6) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size = 3.5, fontface = "bold", color = "black") +
  scale_fill_manual(values = traj_colors_all) +
  scale_x_discrete(limits = c("Even", "Medium", "High"), expand = c(0.05, 0.05)) +
  scale_y_continuous(name = "Number of participants", breaks = scales::breaks_pretty()) +
  labs(
    title    = "Gait cluster trajectories across surfaces — All participants (n = 68)",
    subtitle = "Flows represent individual shifts in cluster membership",
    fill     = "Trajectory",
    x        = "Walking surface"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey40"),
    axis.text.x   = element_text(size = 12, face = "bold", color = "black"),
    legend.position  = "right",
    legend.title  = element_text(face = "bold"),
    panel.grid    = element_blank()
  )

print(plot_alluvial_all)
ggsave(
  "Plot_Alluvial_ALL.tiff",
  plot = plot_alluvial_all,
  device = "tiff",
  width = 8,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

# ============================================================
# A2. TRAJECTOIRES INDIVIDUELLES ORDONNÉES PAR ÂGE
#     (alternative au alluvial pour visualiser l'âge)
# ============================================================

df_long_age <- df_all %>%
  select(Participant, AgeMonths, Trajectory,
         Cluster_Even, Cluster_Medium, Cluster_High) %>%
  pivot_longer(
    cols = c(Cluster_Even, Cluster_Medium, Cluster_High),
    names_to = "Surface",
    values_to = "Cluster"
  ) %>%
  mutate(
    Surface = factor(
      Surface,
      levels = c("Cluster_Even", "Cluster_Medium", "Cluster_High"),
      labels = c("Even", "Medium", "High")
    ),
    Cluster = factor(paste0("C", Cluster), levels = c("C2", "C1"))
  )

plot_age_trajectories <- ggplot(
  df_long_age,
  aes(x = Surface, y = AgeMonths, group = Participant)
) +
  geom_line(aes(color = Trajectory), alpha = 0.35, linewidth = 3) +
  geom_point(aes(shape = Cluster, fill = Cluster), size = 2.8, color = "black") +
  scale_color_manual(values = traj_colors_all) +
  scale_fill_manual(values = c("C2" = "white", "C1" = "black")) +
  labs(
    title = "Individual cluster trajectories ordered by chronological age",
    subtitle = "Y-axis = Age in months; each line = one participant",
    x = "Walking surface",
    y = "Age (months)",
    color = "Trajectory",
    shape = "Cluster",
    fill  = "Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle   = element_text(hjust = 0.5, size = 10, color = "grey40"),
    axis.text.x     = element_text(size = 12, face = "bold", color = "black"),
    legend.position = "right",
    legend.title    = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(plot_age_trajectories)

ggsave(
  "Plot_Age_Trajectories.tiff",
  plot = plot_age_trajectories,
  device = "tiff",
  width = 8,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw"
)


# ============================================================
# B. RÉPARTITION DES 3 GROUPES + AUTRE
# ============================================================

cat("\n=== RÉPARTITION DES GROUPES (3 + Transient) ===\n")
recap_groups <- df_all %>%
  count(Group) %>%
  mutate(Pct = round(100 * n / sum(n), 1))
print(recap_groups)

# Sous-répartition migrants : Early vs Late
cat("\n=== SOUS-RÉPARTITION DES MIGRANTS ===\n")
df_migrants <- df_all %>% filter(Group == "Migrants C1->C2")
recap_migrant_type <- df_migrants %>%
  count(Trajectory) %>%
  mutate(Pct = round(100 * n / sum(n), 1))
print(recap_migrant_type)

cat(sprintf("\nProportion Early Migrants (bascule dès Medium) : %s sur %s migrants (%.1f%%)\n",
            sum(df_migrants$Trajectory == "Early Migrant"),
            nrow(df_migrants),
            100 * mean(df_migrants$Trajectory == "Early Migrant")))


# ============================================================
# C. CARACTÉRISATION PAR GROUPE (Stable C1 / Migrants / Stable C2 / Transient)
# ============================================================

# Tableau descriptif avec tests
tab_groups <- df_all %>%
  select(Group, AgeMonths, Sex) %>%
  tbl_summary(
    by = Group,
    label = list(
      AgeMonths ~ "Age (months)",
      Sex       ~ "Sex"
    ),
    statistic = list(
      all_continuous()  ~ "{median} [{p25}, {p75}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits  = list(all_continuous() ~ 1),
    missing = "no"
  ) %>%
  add_p(
    test = list(
      all_continuous()  ~ "kruskal.test",
      all_categorical() ~ "fisher.test"
    )
  ) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Group (Even → High)**") %>%
  modify_footnote(all_stat_cols() ~ "Median [IQR] ; n (%) ; Kruskal-Wallis or Fisher's exact test")

print(tab_groups)

# Export tableau
gt_groups <- tab_groups %>% as_gt()
gt::gtsave(gt_groups, "Table_Groups_AllParticipants.pdf")
gt::gtsave(gt_groups, "Table_Groups_AllParticipants.png", expand = 10)

cat("\n=== EFFET DE L'ÂGE SELON LE GROUPE ===\n")

# Sous-échantillon pour les comparaisons statistiques
df_groups_comp <- df_all %>%
  filter(Group %in% c("Stable C1", "Migrants C1->C2", "Transient", "Stable C2")) %>%
  droplevels()

# Si besoin, fixer l'ordre d'affichage
df_groups_comp$Group <- factor(
  df_groups_comp$Group,
  levels = c("Stable C1", "Migrants C1->C2", "Transient", "Stable C2")
)

# Tableau descriptif avec tests
tab_groups <- df_groups_comp %>%
  select(Group, AgeMonths, Sex) %>%
  tbl_summary(
    by = Group,
    label = list(
      AgeMonths ~ "Age (months)",
      Sex       ~ "Sex"
    ),
    statistic = list(
      all_continuous()  ~ "{median} [{p25}, {p75}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits  = list(all_continuous() ~ 1),
    missing = "no"
  ) %>%
  add_p(
    test = list(
      all_continuous()  ~ "kruskal.test",
      all_categorical() ~ "fisher.test"
    )
  ) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Group (Even → High)**") %>%
  modify_footnote(all_stat_cols() ~ "Median [IQR] ; n (%) ; Kruskal-Wallis or Fisher's exact test")

print(tab_groups)

# Export tableau
gt_groups <- tab_groups %>% as_gt()
gt::gtsave(gt_groups, "Table_Groups_AllParticipants.pdf")
gt::gtsave(gt_groups, "Table_Groups_AllParticipants.png", expand = 10)

cat("\n=== EFFET DE L'ÂGE SELON LE GROUPE ===\n")

# Kruskal-Wallis global
kw_age_groups <- kruskal_test(df_groups_comp, AgeMonths ~ Group)
print(kw_age_groups)

# Taille d'effet globale (epsilon squared)
kw_age_groups_effsize <- kruskal_effsize(df_groups_comp, AgeMonths ~ Group)
print(kw_age_groups_effsize)

# Post-hoc Dunn
dunn_age_groups <- df_groups_comp %>%
  dunn_test(AgeMonths ~ Group, p.adjust.method = "holm")

print(dunn_age_groups)

# ============================================================
# ANNOTATIONS STATISTIQUES POUR LE BOXPLOT DES 4 GROUPES
# ============================================================

stat_groups <- dunn_age_groups %>%
  filter(p.adj < 0.05) %>%
  mutate(
    group1 = factor(group1, levels = levels(df_groups_comp$Group)),
    group2 = factor(group2, levels = levels(df_groups_comp$Group)),
    label = case_when(
      p.adj < 0.001 ~ "***",
      p.adj < 0.01  ~ "**",
      p.adj < 0.05  ~ "*",
      TRUE          ~ "ns"
    )
  )

y_max_groups <- max(df_groups_comp$AgeMonths, na.rm = TRUE)

stat_groups <- stat_groups %>%
  mutate(
    y.position = y_max_groups + seq(10, by = 15, length.out = n())
  )

print(stat_groups)

# Export CSV
write_csv(as.data.frame(kw_age_groups), "Kruskal_Age_Groups.csv")
write_csv(as.data.frame(kw_age_groups_effsize), "KruskalEffsize_Age_Groups.csv")
write_csv(as.data.frame(dunn_age_groups), "PostHoc_Dunn_Age_Groups.csv")

# Fisher exact : Sexe
tab_sex_groups <- table(df_groups_comp$Group, df_groups_comp$Sex)
fisher_sex_groups <- fisher_test(tab_sex_groups)
print(fisher_sex_groups)

cramers_v_groups <- cramerV(tab_sex_groups)
print(cramers_v_groups)

# ============================================================
# D. VISUALISATIONS COMPARATIVES PAR GROUPE
# ============================================================

cols_groups <- c(
  "Stable C1"       = "#2C7FB8",
  "Migrants C1->C2" = "#e67e22",
  "Stable C2"       = "#31A354",
  "Transient"       = "#9E9E9E"
)

df_all$Group <- factor(
  df_all$Group,
  levels = c("Stable C1", "Migrants C1->C2", "Transient", "Stable C2")
)

# Boxplot âge
p_age_groups <- ggplot(df_groups_comp, aes(x = Group, y = AgeMonths, fill = Group)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5, color = "black") +
  scale_fill_manual(values = cols_groups) +
  labs(title = "Age by group", x = NULL, y = "Age (months)") +
  stat_pvalue_manual(
    stat_groups,
    label = "label",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    bracket.size = 0.6,
    size = 5,
    hide.ns = TRUE
  ) +
  expand_limits(y = max(stat_groups$y.position, na.rm = TRUE) + 10) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "none",
    axis.text.x = element_text(angle = 15, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(10, 10, 20, 10)
  )

# Barplot sexe
sex_summary_groups <- df_all %>%
  group_by(Group, Sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Group) %>%
  mutate(prop = n / sum(n) * 100)

p_sex_groups <- ggplot(sex_summary_groups, aes(x = Group, y = prop, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8, color = "black") +
  geom_text(aes(label = paste0(n, " (", round(prop, 1), "%)")),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("F" = "#FF6B9D", "M" = "#4A90E2")) +
  labs(title = "Sex distribution by group", x = NULL, y = "Percentage (%)", fill = "Sex") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text.x = element_text(angle = 15, hjust = 1),
        panel.grid.major.x = element_blank())

p_combined_groups <- (p_age_groups | p_sex_groups) +
  plot_annotation(
    title = "Characteristics by migration group — All participants",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
  )

print(p_combined_groups)


ggsave(
  "Groups_AllParticipants_Comparison.tiff",
  plot = p_combined_groups,
  device = "tiff",
  width = 8,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

ggsave(
  "Plot_Age_Groups.tiff",
  plot = p_age_groups,
  device = "tiff",
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

# ============================================================
# E. SOUS-ANALYSE MIGRANTS : EARLY vs LATE
# ============================================================

cat("\n=== SOUS-ANALYSE MIGRANTS : EARLY vs LATE ===\n")
cat(sprintf("  Total migrants : %d\n", nrow(df_migrants)))
cat(sprintf("  Early Migrants (bascule Medium) : %d (%.1f%%)\n",
            sum(df_migrants$Trajectory == "Early Migrant"),
            100 * mean(df_migrants$Trajectory == "Early Migrant")))
cat(sprintf("  Late Migrants  (bascule High)   : %d (%.1f%%)\n",
            sum(df_migrants$Trajectory == "Late Migrant"),
            100 * mean(df_migrants$Trajectory == "Late Migrant")))

# Âge par type de migration
cat("\n--- Âge des migrants (tous) ---\n")
df_migrants %>%
  summarise(
    n       = n(),
    Median  = median(AgeMonths, na.rm = TRUE),
    IQR_low = quantile(AgeMonths, 0.25, na.rm = TRUE),
    IQR_up  = quantile(AgeMonths, 0.75, na.rm = TRUE),
    Mean    = mean(AgeMonths, na.rm = TRUE),
    SD      = sd(AgeMonths, na.rm = TRUE)
  ) %>% print()

cat("\n--- Âge des migrants selon le timing ---\n")
df_migrants %>%
  group_by(Trajectory) %>%
  summarise(
    n       = n(),
    Median  = median(AgeMonths, na.rm = TRUE),
    IQR_low = quantile(AgeMonths, 0.25, na.rm = TRUE),
    IQR_up  = quantile(AgeMonths, 0.75, na.rm = TRUE),
    Mean    = mean(AgeMonths, na.rm = TRUE),
    SD      = sd(AgeMonths, na.rm = TRUE)
  ) %>% print()


# ============================================================
# E2. COMPARAISON D'ÂGE : EARLY vs LATE MIGRANTS
#     Wilcoxon rank-sum + taille d'effet
# ============================================================

cat("\n=== COMPARAISON D'ÂGE : EARLY vs LATE MIGRANTS ===\n")

df_migrants_compare <- df_migrants %>%
  filter(Trajectory %in% c("Early Migrant", "Late Migrant")) %>%
  droplevels()

# Wilcoxon rank-sum test
wilcox_age_earlylate <- wilcox_test(
  df_migrants_compare,
  AgeMonths ~ Trajectory,
  detailed = TRUE
)

print(wilcox_age_earlylate)

# ============================================================
# ANNOTATION STATISTIQUE POUR EARLY vs LATE
# ============================================================

stat_earlylate <- wilcox_age_earlylate %>%
  mutate(
    p.signif = case_when(
      p <= 0.001  ~ "***",
      p <= 0.01   ~ "**",
      p <= 0.05   ~ "*",
      TRUE        ~ "ns"
    )
  )

y_max_earlylate <- max(df_migrants_compare$AgeMonths, na.rm = TRUE)

stat_earlylate <- stat_earlylate %>%
  mutate(
    y.position = y_max_earlylate + 10
  )

print(stat_earlylate)

# Taille d'effet : rank-biserial correlation
wilcox_effsize_earlylate <- wilcox_effsize(
  df_migrants_compare,
  AgeMonths ~ Trajectory
)

print(wilcox_effsize_earlylate)

# Export CSV
write_csv(as.data.frame(wilcox_age_earlylate), "Wilcoxon_Age_EarlyVsLate.csv")
write_csv(as.data.frame(wilcox_effsize_earlylate), "WilcoxonEffsize_Age_EarlyVsLate.csv")

# ============================================================
# F. VISUALISATION MIGRANTS : BOXPLOT ÂGE EARLY vs LATE
# ============================================================

cols_migrant <- c(
  "Early Migrant" = "#FDB863",
  "Late Migrant"  = "darkred"
)

p_age_migrant_type <- ggplot(
  df_migrants_compare,
  aes(x = Trajectory, y = AgeMonths, fill = Trajectory)
) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.5, size = 2, color = "black") +
  scale_fill_manual(values = cols_migrant) +
  labs(
    title    = "Age distribution: Early vs Late Migrants",
    subtitle = "Early = C1→C2→C2  |  Late = C1→C1→C2",
    x        = NULL,
    y        = "Age (months)"
  ) +
  stat_pvalue_manual(
    stat_earlylate,
    label = "p.signif",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    bracket.size = 0.6,
    size = 5,
    hide.ns = TRUE
  ) +
  expand_limits(y = max(stat_earlylate$y.position) + 10) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey40"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

print(p_age_migrant_type)

ggsave(
  "Type_Migrant_EarlyLate.tiff",
  plot = p_age_migrant_type,
  device = "tiff",
  width = 8,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

# ============================================================
# G. PANEL DES 15 VARIABLES SELON SURFACE ET TRAJECTOIRE
#     3 colonnes × 5 lignes
# ============================================================

# --- 1. Liste des 15 variables étudiées ---
variables_15 <- c(
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


variable_labels <- c(
  "Mean_Norm Gait Speed (m.s^{-1})" = "Norm Gait Speed (au)",
  "Mean_Norm Step length (ua)"      = "Norm Step Length (au)",
  "Mean_Norm WR (ua)"               = "Norm WR (au)",
  "Mean_Double support time (%)"    = "Double support time (%)",
  "Mean_Norm Cadence (ua)"          = "Norm Cadence (au)",
  "Mean_COM SPARC Magnitude (ua)"   = "SPARC (au)",
  "Mean_Norm StepWidth (ua)"        = "Norm StepWidth (au)",
  "Mean_MoS ML HS (%L0)"            = "MoS ML (%L0)",
  "Mean_MoS AP HS (%L0)"            = "MoS AP (%L0)",
  "SI_Stride length (m)"            = "S.I. Stride length (%)",
  "SI_Double support time (%)"      = "S.I. Double support time (%)",
  "SI_StepWidth (cm)"               = "S.I. StepWidth (%)",
  "CV_Norm StepWidth (ua)"          = "C.V. Norm StepWidth (%)",
  "Mean_GVI (ua)"                   = "GVI (au)",
  "CV_Gait speed (m.s^{-1})"        = "C.V. Gait speed (%)"
)

# --- 2. Vérification que les variables existent bien dans df_clust ---
missing_vars <- setdiff(variables_15, colnames(df_clust))

if (length(missing_vars) > 0) {
  stop(
    paste0(
      "Les variables suivantes sont absentes de df_clust :\n",
      paste(missing_vars, collapse = "\n")
    )
  )
}

# --- 3. Ordre des groupes à afficher ---
trajectory_order <- c(
  "Stable C1",
  "Late Migrant",
  "Early Migrant",
  "Transient",
  "Stable C2"
)

# --- 4. Couleurs des groupes ---
cols_trajectory <- c(
  "Stable C1"     = "#2C7FB8",
  "Late Migrant"  = "darkred",
  "Early Migrant" = "#FDB863",
  "Transient"     = "#9E9E9E",
  "Stable C2"     = "#31A354"
)

# --- 5. Préparation du tableau long ---
df_panel_15 <- df_clust %>%
  filter(Condition %in% c("Plat", "Medium", "High")) %>%
  left_join(
    df_all %>% select(Participant, Trajectory),
    by = "Participant"
  ) %>%
  mutate(
    Surface = case_when(
      Condition == "Plat"   ~ "Even",
      Condition == "Medium" ~ "Medium",
      Condition == "High"   ~ "High",
      TRUE ~ as.character(Condition)
    ),
    Surface = factor(Surface, levels = c("Even", "Medium", "High")),
    Trajectory = factor(Trajectory, levels = trajectory_order)
  ) %>%
  pivot_longer(
    cols = all_of(variables_15),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Variable = factor(Variable, levels = variables_15)
  ) %>%
  filter(!is.na(Trajectory), !is.na(Value))

# --- 6. Panel principal ---
p_panel_15 <- ggplot(
  df_panel_15,
  aes(x = Surface, y = Value, fill = Trajectory)
) +
  geom_boxplot(
    alpha = 0.85,
    outlier.shape = NA,
    color = "black",
    linewidth = 0.35,
    width = 0.65,
    position = position_dodge(width = 0.80)   
  ) +
  geom_jitter(
    position = position_jitterdodge(
      jitter.width = 0.08,
      dodge.width = 0.80                       
    ),
    alpha = 0.75,
    size = 0.55,
    color = "black"
  ) +
  facet_wrap(
    ~ Variable,
    ncol = 3,
    scales = "free_y",
    labeller = labeller(Variable = variable_labels)
  ) +
  scale_fill_manual(values = cols_trajectory, drop = FALSE) +
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    fill = "Trajectory"
  ) +
  theme_classic(base_size = 20) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      size = 20,
      face = "bold",
      color = "black",
      margin = margin(b = 4)
    ),
    axis.text.x = element_text(
      size = 18,
      face = "plain",
      color = "black"
    ),
    axis.text.y = element_text(
      size = 16,
      color = "black"
    ),
    axis.title = element_blank(),
    axis.line = element_line(
      color = "grey40",
      linewidth = 0.5
    ),
    axis.ticks = element_line(
      color = "grey40",
      linewidth = 0.5
    ),
    legend.position = "none",
    panel.spacing.x = grid::unit(1.2, "lines"),
    panel.spacing.y = grid::unit(1.4, "lines"),
    plot.margin = margin(10, 10, 10, 10)
  )

print(p_panel_15)

# ============================================================
# H. TABLEAU DESCRIPTIF — 15 VARIABLES PAR SURFACE ET GROUPE
#     Médiane [Q1 - Q3]
# ============================================================

trajectory_table_order <- c(
  "Stable C2",
  "Transient",
  "Early Migrant",
  "Late Migrant",
  "Stable C1"
)

trajectory_table_labels <- c(
  "Stable C2"     = "Consistent C2",
  "Transient"     = "Transient",
  "Early Migrant" = "Early Switchers",
  "Late Migrant"  = "Late Switchers",
  "Stable C1"     = "Consistent C1"
)

table_15_median_iqr_surface <- df_panel_15 %>%
  mutate(
    Trajectory = factor(Trajectory, levels = trajectory_table_order),
    Group_label = recode(as.character(Trajectory), !!!trajectory_table_labels),
    Group_label = factor(
      Group_label,
      levels = c(
        "Consistent C2",
        "Transient",
        "Early Switchers",
        "Late Switchers",
        "Consistent C1"
      )
    )
  ) %>%
  group_by(Surface, Variable, Group_label) %>%
  summarise(
    n = n(),
    Median = median(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    IQR = IQR(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Median_IQR = paste0(
      round(Median, 2),
      " [",
      round(Q1, 2),
      " - ",
      round(Q3, 2),
      "]"
    )
  ) %>%
  select(Surface, Variable, Group_label, Median_IQR) %>%
  pivot_wider(
    names_from = Group_label,
    values_from = Median_IQR
  ) %>%
  mutate(
    Variable = recode(as.character(Variable), !!!variable_labels)
  ) %>%
  arrange(
    Surface,
    match(Variable, variable_labels)
  )

print(table_15_median_iqr_surface)

# Export CSV avec séparateur virgule
write.csv(
  table_15_median_iqr_surface, "Table_15Variables_Median_IQR_by_Surface_and_Trajectory.csv")

# --- SAUVEGARDE ---
ggsave(
  "Panel_15Variables_Surface_Trajectory_MATLABstyle.tiff",
  plot = p_panel_15,
  device = "tiff",
  width = 18,
  height = 18,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

ggsave(
  "Panel_15Variables_Surface_Trajectory_MATLABstyle.pdf",
  plot = p_panel_15,
  width = 18,
  height = 18,
  units = "in"
)

message("\n=== TERMINÉ ===")
message("Fichiers créés :")
message("  - Trajectories_AllParticipants.csv")
message("  - AllParticipants_Groups.csv")
message("  - Table_Groups_AllParticipants.pdf / .png")
message("  - Kruskal_Age_Groups.csv")
message("  - KruskalEffsize_Age_Groups.csv")
message("  - PostHoc_Dunn_Age_Groups.csv")
message("  - Wilcoxon_Age_EarlyVsLate.csv")
message("  - WilcoxonEffsize_Age_EarlyVsLate.csv")
message("  - Groups_AllParticipants_Comparison.png")
message("  - Migrants_EarlyVsLate_Age.png")