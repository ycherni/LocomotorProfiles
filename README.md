# Biomechanics Gait Analysis — Analyses données STP du projet SurfaceIRR

> **Projet MSc. Silvère De Freitas** — Analyse de la marche sur surfaces irrégulières chez différents groupes d'âge (Jeunes Enfants, Enfants, Adolescents, Adultes). 2025-2026
---

## 📋 Table des matières

1. [Vue d'ensemble du pipeline](#vue-densemble-du-pipeline)
2. [Prérequis & Dépendances](#prérequis--dépendances)
3. [Structure du répertoire](#structure-du-répertoire)
4. [Description détaillée des scripts](#description-détaillée-des-scripts)
5. [Flux de données entre les scripts](#flux-de-données-entre-les-scripts)
6. [Conventions de nommage](#conventions-de-nommage)
7. [Groupes d'âge](#groupes-dâge)

---

## Vue d'ensemble du pipeline

Le pipeline complet transforme des fichiers C3D bruts (données de capture de mouvement Vicon) en résultats statistiques. Il se compose de deux langages :

- **MATLAB** : traitement des données brutes, calculs biomécaniques, exports
- **R** : statistiques inférentielles, visualisations publication-grade, tableaux

[] = Les principales étapes du code
```
Fichiers C3D (Vicon)
        │
        ▼
[1] main.m ──────────────────────── Extraction brute, calculs par essai
        │
        ▼
[2] L0report.m ──────────────────── Renseigner la longueur de jambe (L0)
        │
        ▼
[3] Non_Dimensional_Normalization.m Normalisation non-dimensionnelle
        │
        ▼
[4] Equalize_N_Cycle.m ──────────── Égalisation du nombre de cycles
        │
        ├──► [5] MOS.m ──────────── Marges de stabilité (MoS)
        ├──► [6] SPARC_LDLJ.m ───── Fluidité du mouvement (SPARC/LDLJ)
        └──► [7] GVI.m ──────────── Gait Variability Index
                │
                ▼
        [8] SpatioTemporal_Analysis.m ── Agrégation de toutes les variables
                │
                └──► [9] Extract_Statistics_Table.m ── Préparation stats
                                │
                                ▼
                    ┌───────────────────────────────┐
                    │                               │
                    ▼                               ▼
          [10] Statistic_STP.R          [11] Clustering.m (MATLAB)
          (LMM, post-hocs,               (k-means, ACP)
           visualisations)                      │
                    │                           ▼
                    │              [12] Statistic_Clusters.R
                    │                           │
                    └───────────────────────────┘
                                │
                                ▼
                    Résultats / Figures / Tableaux
```

---

## Prérequis & Dépendances

### MATLAB
| Toolbox / Librairie | Utilisation |
|---|---|
| Signal Processing Toolbox | Filtrage Butterworth, `filtfilt` |
| Statistics and Machine Learning Toolbox | `kmeans`, `evalclusters`, `silhouette` |
| `ezc3d` / BTK | Lecture des fichiers C3D |
| `spm1dmatlab` | Analyses statistiques paramétriques sur courbes 1D |
| `ParticipantGroup.m` (local) | Affectation des participants aux groupes |

### R
| Package | Utilisation |
|---|---|
| `lme4`, `lmerTest` | Modèles linéaires mixtes (LMM) |
| `emmeans` | Moyennes marginales estimées, post-hocs |
| `effectsize`, `performance` | Tailles d'effet (η², R² Nakagawa) |
| `ggplot2`, `patchwork` | Visualisations |
| `gtsummary`, `gt` | Tableaux descriptifs publication |
| `fmsb` | Radar plots |
| `ggalluvial` | Diagrammes alluviaux (migrations) |
| `rstatix` | Tests statistiques (Wilcoxon, Kruskal-Wallis, etc.) |

---

## Structure du répertoire (sur mon ordinateur perso, ça peut donner un exemple d'organisation)

```
gaitAnalysisGUI/
├── Data/
│   ├── enfants/           # Fichiers C3D par groupe d'âge
│   ├── jeunes_enfants/
│   ├── adolescents/
│   └── adults/
├── functions/             # Fonctions MATLAB utilitaires
├── Libs/                  # Librairies
├── result/
│   ├── matfiles/ALL/      # Fichiers .mat par participant × condition
│   ├── MoS/               # Résultats marges de stabilité
│   ├── Smoothness/        # Résultats SPARC/LDLJ
│   ├── Kinematics/        # Matrices cinématiques
│   ├── Fig/               # Figures générées
│   └── Statistical_Analysis_LMM/
│       └── Prepared_Data/ # Données prêtes pour R
│
├── 0)  Participants_Characteristics.m
├── 1)  main.m
├── 2)  Association_Age_&_ParticipantGroup.txt
├── 3)  L0report.m
├── 4)  Non_Dimensional_Normalization.m
├── 5)  Equalize_N_Cycle.m
├── 6)  MOS.m
├── 7)  SPARC_LDLJ.m
├── 8)  GVI.m
├── 9)  SpatioTemporal_Analysis.m
├── 10) Extract_Statistics_Table.m
├── 11) Statistic_STP.R
├── 12) Statistic_PhysActivity.R
├── 13) Clustering.m
├── 14) TransitionClusters.R
├── 14bis) Statistic_Clusters.R
├── Age_Participant.m
├── ParticipantGroup.m
```

---

## Description détaillée des scripts

---

### `ParticipantGroup.m` *(fichier de configuration central)*

**Rôle :** Définit l'appartenance de chaque participant à un groupe d'âge. Fichier référencé par **tous les scripts** d'analyse.

| | Détail |
|---|---|
| **Entrées** | Aucune |
| **Sorties** | Variable `Group` (struct) avec 4 champs : `JeunesEnfants`, `Enfants`, `Adolescents`, `Adultes` — chacun contenant un cell array d'IDs (ex : `'CTL_01'`) |
| **Dépendances** | Aucune |

---

### `0) Participants_Characteristics.m`

**Rôle :** Crée et met à jour la base de données anthropométriques des participants (âge, sexe, taille, poids, longueur de jambe, IMC).

| | Détail |
|---|---|
| **Entrées** | `participants_metadata_MR.mat` (si existant) |
| **Sorties** | `participants_metadata_MR.mat`, `participants_metadata_MR.csv` |
| **Variables** | Participant, AgeGroup, AgeMonths, Sex, Height_cm, Weight_kg, L0_m, IMC |
| **Note** | À lancer une fois par cohorte ou à chaque ajout de participant. Calculer l'IMC automatiquement. |

---

### `Age_Participant.m`

**Rôle :** Calcule l'âge exact d'un participant (en années, mois et total de mois) à partir de sa date de naissance et de la date d'évaluation.

| | Détail |
|---|---|
| **Entrées** | Dates en dur dans le script (`dateNaissance`, `dateEvaluation`) |
| **Sorties** | Affichage console uniquement |
| **Dépendances** | Fonction `calculAge_anneeMois` (dans `functions/`) |
| **Usage** | Script utilitaire ponctuel — exécuter manuellement avant d'entrer un nouvel âge |

---

### `1) main.m`

**Rôle :** Script principal d'extraction et de traitement d'un essai C3D. C'est le point d'entrée du pipeline pour chaque enregistrement Vicon.

| | Détail |
|---|---|
| **Entrées** | Fichier(s) `.c3d` (sélection via interface graphique `getFile`) |
| **Sorties** | `result/matfiles/<NomParticipant>.mat` (structure `c` complète), fichiers Excel (`writeExcelMean`, `writeExcelEachTrial`) |
| **Calculs effectués** | EEI, paramètres spatio-temporels, cinématique, cinétique, moyenne gauche/droite |
| **Dépendances** | `loadEzc3d`, `getFile`, `eeiComputations`, `spatiotempComputations`, `kinematicsComputations`, `kineticsComputations`, `createEmptyIfNecessary`, `meanLegs` |
| **Note** | Traite un fichier à la fois. Répéter pour chaque participant × condition. |

---

### `3) L0report.m`

**Rôle :** Renseigne et maintient la longueur de jambe (L0, en mètres) pour chaque participant dans un fichier centralisé.

| | Détail |
|---|---|
| **Entrées** | `l0_participants.mat` (si existant), valeurs L0 saisies manuellement dans le script |
| **Sorties** | `result/matfiles/ALL/l0_participants.mat` (containers.Map : `ID → L0`) |
| **Dépendances** | Aucune |
| **Note** | À compléter à chaque nouveau participant. Les valeurs commentées sont les participants déjà traités. |

---

### `4) Non_Dimensional_Normalization.m`

**Rôle :** Ajoute les paramètres de marche normalisés non-dimensionnellement (méthode Hof et al., 1996) dans les fichiers `.mat` de chaque participant.

| | Détail |
|---|---|
| **Entrées** | `<ID>_<Condition>.mat` (fichiers existants), `l0_participants.mat` |
| **Sorties** | Fichiers `.mat` mis à jour avec les champs : `NormStepLength`, `NormCadence`, `NormWalkRatio`, `NormStepWidthHeel`, `NormWalkSpeed` |
| **Formules** | Hof et al. (1996) : normalisation par L0 et √(g·L0) |
| **Dépendances** | `l0_participants.mat`, fichiers `.mat` produits par `main.m` |
| **Précède** | `Equalize_N_Cycle.m` |

---

### `5) Equalize_N_Cycle.m`

**Rôle :** Égalise le nombre de cycles de marche entre les 3 conditions (Plat, Medium, High) pour chaque participant, avec une répartition équilibrée gauche/droite (cible 50/50).

| | Détail |
|---|---|
| **Entrées** | `<ID>_<Condition>.mat`, `l0_participants.mat` |
| **Sorties** | Fichiers `.mat` mis à jour (cycles réduits), `cycle_equalization_stats_balanced.mat`, `cycle_equalization_summary_balanced.csv` |
| **Méthode** | Sélection aléatoire avec seed fixe (reproductible), tolérance ±10% sur le ratio L/R |
| **Dépendances** | Fichiers `.mat` produits par `Non_Dimensional_Normalization.m` |
| **Précède** | `MOS.m`, `SPARC_LDLJ.m`, `GVI.m` |

---

### `6) MOS.m`

**Rôle :** Calcule les Marges de Stabilité (MoS) antéro-postérieures et médio-latérales pour chaque cycle de marche, à partir des fichiers C3D bruts.

| | Détail |
|---|---|
| **Entrées** | Fichiers C3D (`Data/<groupe>/`), `l0_participants.mat`, événements Vicon (HS, TO) |
| **Sorties** | `result/MoS/MoS_results_<ID>.csv`, `result/MoS/MoS_results_<ID>.mat` |
| **Calculs** | xCOM (centre de masse extrapolé), MoS_AP (mm et %L0), MoS_ML (mm et %L0) |
| **Algorithme HO** | Détection automatique du Heel-Off par seuil adaptatif sur la vitesse verticale du talon, filtrage Butterworth zéro-phase 6 Hz |
| **Dépendances** | BTK, `l0_participants.mat` |
| **Précède** | `SpatioTemporal_Analysis.m` |

---

### `7) SPARC_LDLJ.m`

**Rôle :** Calcule les indices de fluidité du mouvement (SPARC et LDLJ) sur la vitesse du centre de masse pelvien et du sternum, pour chaque essai.

| | Détail |
|---|---|
| **Entrées** | Fichiers C3D, événements Vicon (HS pour délimiter la fenêtre d'analyse) |
| **Sorties** | `result/Smoothness/Smoothness_TrialBased_<ID>.csv`, `.mat` |
| **Métriques** | SPARC (Spectral Arc Length), LDLJ (Log Dimensionless Jerk) — axes AP, ML, V et Magnitude |
| **Fenêtre** | Du 1er HeelStrike au dernier HeelStrike de l'essai |
| **Dépendances** | BTK, fonction locale `SpectralArcLength` (Balasubramanian et al. 2015) |
| **Précède** | `SpatioTemporal_Analysis.m` |

---

### `8) GVI.m`

**Rôle :** Calcule le Gait Variability Index (GVI, Gouelle 2013) pour chaque participant × surface, en utilisant les adultes sur surface plate comme référence (GVI = 100).

| | Détail |
|---|---|
| **Entrées** | Fichiers `<ID>_<Condition>.mat` (via `c.resultsAll.kin`) |
| **Sorties** | `GVI_AllSurfaces_Individual_<timestamp>.csv`, `GVI_AllSurfaces_ByGroup_<timestamp>.csv`, figures boxplots |
| **Calculs** | 9 paramètres spatio-temporels, paramètres alternatifs (Gouelle 2013), distance log-normalisée, score GVI |
| **Référence** | Adultes sur surface Plat → GVI normalisé à 100 |
| **Dépendances** | `ParticipantGroup.m`, fichiers `.mat`, `l0_participants.mat` |
| **Précède** | `SpatioTemporal_Analysis.m` |

---

### `9) SpatioTemporal_Analysis.m`

**Rôle :** Script central d'agrégation. Compile toutes les variables spatio-temporelles, MoS, SPARC/LDLJ et GVI en une structure unifiée par groupe × condition, et exporte les données pour R.

| | Détail |
|---|---|
| **Entrées** | Fichiers `.mat` (spatio-temporels), `MoS_results_<ID>.mat`, `Smoothness_TrialBased_<ID>.mat`, `GVI_AllSurfaces_Individual_*.csv` |
| **Sorties** | `SpatioTemporalDATA.mat` (structure MATLAB principale), `SpatioTemporal_ALL_<Condition>.csv`, `Comparaison_Groupes_SpatioTemporel.xlsx`, radar plots, nuages de points âge |
| **Variables produites** | Paramètres Mean_, CV_, SI_ pour chaque variable spatio-temporelle + MoS + Smoothness + GVI |
| **Dépendances** | `ParticipantGroup.m`, `Association_Age.m`, `Spatiotempocalc.m`, tous les modules précédents |
| **Précède** | `Extract_Statistics_Table.m`, `Clustering.m` |

---

### `10) Extract_Statistics_Table.m`

**Rôle :** Prépare et exporte les données en format long pour les analyses statistiques R. Ajoute les variables dérivées (StepTime, StanceTime, SwingTime) nécessaires au GVI.

| | Détail |
|---|---|
| **Entrées** | `SpatioTemporalDATA.mat` |
| **Sorties** | `Statistical_Analysis_LMM/Prepared_Data/DATA_all_prepared.mat`, `DATA_all_prepared.csv`, `ACP_Clustering_DATA.csv` |
| **Format sortie** | Format long : 1 ligne = 1 participant × 1 surface × toutes les variables |
| **Mapping** | Construit automatiquement le mapping Participant → AgeGroup |
| **Dépendances** | `SpatioTemporalDATA.mat` |
| **Précède** | `Statistic_STP.R`, `Clustering.m` |

---

### `11) Statistic_STP.R`

**Rôle :** Analyses statistiques complètes des paramètres spatio-temporels. C'est le script R principal.

| | Détail |
|---|---|
| **Entrées** | `ACP_Clustering_DATA.csv`, `participants_metadata.csv` |
| **Sorties** | `Table1_participants.pdf`, `LMM_ANOVA_and_Posthocs_COMPLET.xlsx`, `Synthese_Resultats_LMM_Discussion.xlsx`, boxplots par variable, radar plots, heatmaps, Gait Adaptation Score (GAS) |
| **Analyses** | (I) Stats descriptives population, (II) Descriptifs STP par groupe/surface, (III) Gait Adaptation Score, (IV) LMM + post-hocs sur toutes les variables |
| **Modèle LMM** | `Variable ~ Surface * AgeGroup + (1\|Participant)` — Type III, Satterthwaite, correction Holm |
| **Dépendances** | `lme4`, `lmerTest`, `emmeans`, `effectsize`, `ggplot2`, `gtsummary`, `fmsb` |

---

### `12) Statistic_PhysActivity.R`

**Rôle :** Analyse l'activité physique des participants (questionnaires PAQ-C, PAQ-A, GPAQ), normalisée par z-score intra-groupe pour permettre la comparaison entre groupes d'âge. Celui là de script est optionnel (uniquement si on utilise les données spatiotemporelles)

| | Détail |
|---|---|
| **Entrées** | Fichier CSV d'activité physique (sélection manuelle) |
| **Sorties** | `PhysicalActivity_Zscored.csv`, `Table_PhysActivity.pdf/.png`, boxplots, courbes de densité |
| **Calculs** | Z-score centré-réduit intra-groupe (Children : PAQ-C, Adolescent : PAQ-A, Adult : GPAQ) |
| **Dépendances** | `ggplot2`, `gtsummary`, `dplyr` |

---

### `13) Clustering.m`

**Rôle :** Analyse en Composantes Principales (ACP) et clustering k-means sur les paramètres de marche, pour identifier des profils de marcheurs.

| | Détail |
|---|---|
| **Entrées** | `SpatioTemporalDATA.mat` |
| **Sorties** | `DATA_FOR_R_GLOBAL_<timestamp>.csv`, `DATA_FOR_STATS_R_<Condition>_<timestamp>.csv`, figures ACP/clusters, heatmaps z-scores, indices de validité |
| **Méthode** | ACP commune (z-score), sélection k par vote multi-critères (Silhouette, Calinski-Harabász, Davies-Bouldin, Gap, ARI bootstrap), clustering sur nPC ≥ 70% variance |
| **Analyses** | Clustering global (toutes surfaces) + clustering par condition (Plat/Medium/High) |
| **Dépendances** | `SpatioTemporalDATA.mat`, `ParticipantGroup.m` |
| **Précède** | `Statistic_Clusters.R`, `TransitionClusters_Libre.R` |

---
### `14) TransitionClusters.R`

**Rôle :** Analyse étendue des trajectoires de migration entre clusters pour tous les participants (pas seulement les enfants), sur les 3 surfaces.

| | Détail |
|---|---|
| **Entrées** | `DATA_FOR_R_GLOBAL_<timestamp>.csv`, `participants_metadata.csv`, `PhysicalActivity_Zscored.csv` |
| **Sorties** | `Trajectories_AllParticipants.csv`, `AllParticipants_Groups.csv`, figures alluviales, boxplots Early vs Late migrants (`.tiff` haute qualité) |
| **Trajectoires** | Stable C1, Stable C2, Early Migrant (bascule dès Medium), Late Migrant (bascule sur High), Transient |
| **Analyses** | Kruskal-Wallis âge par groupe, post-hoc Dunn, Wilcoxon Early vs Late, Fisher sexe |
| **Dépendances** | `ggalluvial`, `rstatix`, `rcompanion`, `gtsummary` |

---

### `14bis) Statistic_Clusters.R`

**Rôle :** Caractérisation statistique et visualisation des clusters identifiés par le k-means MATLAB.

| | Détail |
|---|---|
| **Entrées** | `DATA_FOR_R_GLOBAL_<timestamp>.csv`, `participants_metadata.csv`, `PhysicalActivity_Zscored.csv` |
| **Sorties** | Tableaux descriptifs, tests Mann-Whitney entre clusters, χ², diagrammes alluviaux (migrations Plat→High), radar plots des profils de marche, analyses des migrations enfants |
| **Analyses** | (I) Stats inter-clusters, (II) Visualisations, (III) Radar plots, (IV) Migrations enfants C1→C2, (V) Comparaison 3 profils enfants |
| **Dépendances** | `rstatix`, `ggalluvial`, `fmsb`, `gtsummary`, `rcompanion` |

---

## Scripts optionnels / utilitaires

| `AIDE) Reconstruction_Data.m` | Reconstruction d'un marqueur disparu dans un C3D |
| `AIDE) Obtenir marqueurs.m` | Liste les marqueurs disponibles dans un C3D |
| `AIDE) Position_Marker_XYZ.m` | Export des positions filtrées d'un marqueur (ex: STRN) |

---

## Flux de données entre les scripts

```
                ┌─────────────────────────────────────────────┐
                │           FICHIERS C3D (Vicon)              │
                └────────────────────┬────────────────────────┘
                                     │
                              [1] main.m
                                     │
                         ┌───────────▼───────────┐
                         │  <ID>_<Cond>.mat       │
                         │  (structure c complète)│
                         └──┬──────────────┬──────┘
                            │              │
                   [3] L0report.m          │
                            │              │
              l0_participants.mat          │
                            │              │
               [4] Non_Dimensional_       │
                   Normalization.m ───────►│
                            │              │
               [5] Equalize_N_Cycle.m ────►│
                            │              │
              ┌─────────────┼──────────────┼─────────┐
              │             │              │         │
          [6] MOS.m   [7] SPARC_LDLJ.m  [8] GVI.m  │
              │             │              │         │
    MoS_results_  Smoothness_  GVI_Individual_        │
      <ID>.mat   TrialBased_   <timestamp>.csv        │
              │   <ID>.mat    │              │         │
              └─────────────►│◄─────────────┘         │
                             │                         │
                    [9] SpatioTemporal_Analysis.m ◄────┘
                             │
              ┌──────────────┼──────────────┐
              │              │              │
    SpatioTemporalDATA.mat   │    SpatioTemporal_ALL_*.csv
                             │
                   [10] Extract_Statistics_Table.m
                             │
                   ACP_Clustering_DATA.csv
                   DATA_all_prepared.csv
                             │
                   ┌─────────┴──────────┐
                   │                    │
                [11] Statistic_STP.R  [13] Clustering.m
                   │                    │
                 Figures,             DATA_FOR_R_GLOBAL.csv
                 Excel LMM            DATA_FOR_STATS_R_*.csv
                                        │
                             [14] TransitionClusters.R
                             [14bis] Statistic_Clusters.R
              
 
```

---

## Conventions de nommage

### Fichiers C3D
```
<ID>_<Surface>_<Essai>.c3d
ex : CTL_01_Plat_03.c3d
```

### Fichiers .mat par participant
```
<ID>_<Surface>.mat
ex : CTL_01_Plat.mat
```

### Identifiants participants
```
CTL_XX  (XX = numéro à deux chiffres)
ex : CTL_01, CTL_42
```

### Surfaces
| Code | Description |
|---|---|
| `Plat` | Surface plane (référence) |
| `Medium` | Surface irrégulière modérée |
| `High` | Surface irrégulière prononcée |

### Préfixes des variables spatio-temporelles
| Préfixe | Description |
|---|---|
| `Mean_` | Moyenne inter-cycles |
| `CV_` | Coefficient de variation (variabilité) |
| `SI_` | Symmetry Index (asymétrie gauche/droite) |

---

## Groupes d'âge

| Groupe | Âge approximatif | Questionnaire AP |
|---|---|---|
| Jeunes Enfants | 2–5ans et 11 mois (36–71 mois) | — |
| Enfants | 6–11ans et 11 mois ans (72–143 mois) | PAQ-C |
| Adolescents | 12–17ans et 11 mois (144–216 mois) | PAQ-A |
| Adultes | > 18ans (> 216 mois) | GPAQ-FR |

---

## Notes importantes

> ⚠️ **Ordre d'exécution :** Les scripts MATLAB doivent être exécutés dans l'ordre numérique (1 → 10). Chaque script dépend des fichiers produits par le précédent.

> ⚠️ **`ParticipantGroup.m` :** Ce fichier doit être mis à jour à chaque ajout de participant avant de lancer toute analyse.

> ⚠️ **Chemins absolus :** Les chemins de fichiers sont codés en dur dans les scripts. Les adapter à votre environnement local avant la première exécution.

> 💡 **Reproductibilité :** Le clustering MATLAB et l'égalisation des cycles utilisent un seed fixe (`RANDOM_SEED = 42`) pour garantir la reproductibilité des résultats.

> 💡 **Format export :** Les données sont exportées en `.csv` avec séparateur `;` (convention française). Vérifier le paramètre `Delimiter` lors de la lecture en R.
