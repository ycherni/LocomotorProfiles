# ============================================================
# TABLEAU VISUEL DES EFFECT SIZES
# 3 colonnes : Domain | Variable | Effect size
# ============================================================

# Packages
library(readr)
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------
# 1. IMPORT DES DONNÉES
# ------------------------------------------------------------
df <- read.csv(file.choose(), sep = ";", check.names = FALSE)

# ------------------------------------------------------------
# 2. MISE EN FORME
# ------------------------------------------------------------
df <- df %>%
  mutate(
    ES_category = case_when(
      EffectSize < 0.30 ~ "Small",
      EffectSize < 0.50 ~ "Medium",
      TRUE ~ "Large"
    ),
    Domain = factor(
      Domain,
      levels = c("Pace", "Rythm", "Postural control", "Variability", "Asymmetry")
    )
  ) %>%
  arrange(Domain)

# ordre des lignes : de haut en bas
df$Row <- rev(seq_len(nrow(df)))

# ------------------------------------------------------------
# 3. PRÉPARATION FORMAT "LONG" POUR LES 3 COLONNES
# ------------------------------------------------------------
df_plot <- bind_rows(
  df %>% transmute(Row, Column = "Domain", Value = as.character(Domain), ES_category = NA),
  df %>% transmute(Row, Column = "Variable", Value = as.character(Variable), ES_category = NA),
  df %>% transmute(Row, Column = "Effect size", Value = sprintf("%.2f", EffectSize), ES_category = ES_category)
)

df_plot$Column <- factor(df_plot$Column, levels = c("Domain", "Variable", "Effect size"))

# ------------------------------------------------------------
# 4. FIGURE
# ------------------------------------------------------------
p <- ggplot(df_plot, aes(x = Column, y = Row)) +
  
  # cases
  geom_tile(
    aes(fill = ES_category),
    color = "white",
    linewidth = 0.8,
    width = 0.98,
    height = 0.98
  ) +
  
  # texte dans chaque case
  geom_text(aes(label = Value), size = 3.8) +
  
  # couleurs : seules les cases "Effect size" seront colorées
  scale_fill_manual(
    values = c(
      "Small" = "#d9d9d9",
      "Medium" = "#fdae61",
      "Large" = "#d73027"
    ),
    na.value = "white",
    name = "Effect size"
  ) +
  
  scale_y_continuous(expand = c(0, 0)) +
  
  labs(
    title = "Effect sizes by gait domain and variable",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  )

print(p)

# ------------------------------------------------------------
# 5. EXPORT
# ------------------------------------------------------------
ggsave(
  filename = "EffectSize_table_visual.png",
  plot = p,
  width = 8,
  height = 7,
  dpi = 600
)