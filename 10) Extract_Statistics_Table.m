%% ANALYSES STATISTIQUES AUTOMATISÉES - MODÈLES LINÉAIRES MIXTES (LMM)
% Design: Surface (mesures répétées) × Groupe d'âge (inter-sujets)
% Objectifs de correction:
% 1) Interaction: obtenir F/DF/p (pas de NaN) -> test global via coefTest
% 2) Reporter REML vs ML + DFMethod -> paramètres fixés et sauvegardés
% 3) Garder random intercept seul -> (1|Participant)
% 4) Clarifier FDR -> FDR appliquée séparément par "famille" d'effets (Surface / AgeGroup / Interaction)

clc; clear; close all;

%% =============== Chargement des données ===============
cd('C:\Users\silve\Desktop\DOCTORAT\UNIV MONTREAL\TRAVAUX-THESE\Surfaces_Irregulieres\Datas\Script\gaitAnalysisGUI\result');
load('SpatioTemporalDATA.mat');   % doit contenir SpatioTemporalDATA

% Dossier de sortie
stats_path = fullfile(pwd, 'Statistical_Analysis_LMM');
if ~exist(stats_path, 'dir'), mkdir(stats_path); end

%% =============== Configuration des variables ===============
surfaces = {'Plat', 'Medium', 'High'};
groups   = {'JeunesEnfants', 'Enfants', 'Adolescents', 'Adultes'};

variables_to_test = {
    % --- Paramètres spatio-temporelles ---
    'Mean_Single support time (%)'
    'Mean_ToeOff (%)'
    'Mean_Step length (m)'
    'Mean_Double support time (%)'
    'Mean_BaseOfSupport (cm)'
    'Mean_StepWidth (cm)'
    'Mean_Gait speed (m.s^{-1})'
    'Mean_Stride length (m)'
    'Mean_Stride time (s)'
    'Mean_WalkRatio'
    'Mean_Norm WR (ua)'
    'Mean_Cadence (step.min^{-1})'
    'Mean_Norm Step length (ua)'
    'Mean_Norm Cadence (ua)'
    'Mean_Norm StepWidth (ua)'
    'Mean_Norm Gait Speed (m.s^{-1})'

    % --- Variabilité ---
    'CV_Single support time (%)'
    'CV_ToeOff (%)'
    'CV_Step length (m)'
    'CV_Double support time (%)'
    'CV_BaseOfSupport (cm)'
    'CV_StepWidth (cm)'
    'CV_Gait speed (m.s^{-1})'
    'CV_Stride length (m)'
    'CV_Stride time (s)'
    'CV_WalkRatio'
    'CV_Norm WR (ua)'
    'CV_Cadence (step.min^{-1})'
    'CV_Norm Step length (ua)'
    'CV_Norm Cadence (ua)'
    'CV_Norm StepWidth (ua)'
    'CV_Norm Gait Speed (m.s^{-1})'

    % --- Stabilité dynamique ---
    'Mean_MoS AP HS (mm)'
    'Mean_MoS ML HS (mm)'
    'Mean_MoS AP Stance (mm)'
    'Mean_MoS ML Stance (mm)'
    'Mean_MoS AP HS (%L0)'
    'Mean_MoS ML HS (%L0)'
    'Mean_MoS AP Stance (%L0)'
    'Mean_MoS ML Stance (%L0)'

    % --- Smoothness ---
    'Mean_COM SPARC Magnitude (ua)'
    'Mean_COM LDLJ Magnitude (ua)'
    'Mean_STERN SPARC Magnitude (ua)'
    'Mean_STERN LDLJ Magnitude (ua)'

    % --- GVI ---
    'Mean_GVI (ua)'

    % --- Indices de symétrie ---
    'SI_Stride time (s)'
    'SI_Stride length (m)'
    'SI_Step length (m)'
    'SI_Double support time (%)'
    'SI_Single support time (%)'
    'SI_BaseOfSupport (cm)'
    'SI_StepWidth (cm)'
    'SI_WalkRatio'
    'SI_Norm WR (ua)'
    'SI_Cadence (step.min^{-1})'
    'SI_Norm Step length (ua)'
    'SI_Norm Cadence (ua)'
    'SI_Norm StepWidth (ua)'
};

%% =============== EXPORT des données (format long .csv) ===============
fprintf('=== EXPORT DATA FORMAT LONG ===\n');

% Fusion de toutes les conditions
DATA_all = [SpatioTemporalDATA.ALL.Plat;
            SpatioTemporalDATA.ALL.Medium;
            SpatioTemporalDATA.ALL.High];

% Harmoniser le facteur Surface
if ismember('Condition', DATA_all.Properties.VariableNames) && ~ismember('Surface', DATA_all.Properties.VariableNames)
    DATA_all.Properties.VariableNames{'Condition'} = 'Surface';
end

% Sécuriser colonnes minimales
if ~ismember('Participant', DATA_all.Properties.VariableNames)
    error('La colonne "Participant" est introuvable dans DATA_all.');
end
if ~ismember('Surface', DATA_all.Properties.VariableNames)
    error('La colonne "Surface" (ou "Condition") est introuvable dans DATA_all.');
end

% Convertir en categoricals
DATA_all.Participant = categorical(DATA_all.Participant);
DATA_all.Surface     = categorical(DATA_all.Surface, surfaces); % force l'ordre

% Mapping Participant -> AgeGroup
fprintf('Construction du mapping Participant -> AgeGroup...\n');
p2g = containers.Map('KeyType','char','ValueType','char');

for g = 1:numel(groups)
    gName = groups{g};
    if ~isfield(SpatioTemporalDATA, gName), continue; end

    for s = 1:numel(surfaces)
        surfName = surfaces{s};
        if ~isfield(SpatioTemporalDATA.(gName), surfName), continue; end

        T = SpatioTemporalDATA.(gName).(surfName);
        if isempty(T) || ~istable(T) || ~ismember('Participant', T.Properties.VariableNames), continue; end

        parts = unique(string(T.Participant));
        for k = 1:numel(parts)
            key = char(parts(k));
            if ~isKey(p2g, key)
                p2g(key) = gName;
            end
        end
    end
end

% Ajouter AgeGroup
DATA_all.AgeGroup = repmat({''}, height(DATA_all), 1);
parts_all = string(DATA_all.Participant);

for i = 1:numel(parts_all)
    key = char(parts_all(i));
    if isKey(p2g, key)
        DATA_all.AgeGroup{i} = p2g(key);
    else
        DATA_all.AgeGroup{i} = '';
    end
end

% Retirer lignes sans groupe
missingGroup = cellfun(@isempty, DATA_all.AgeGroup);
if any(missingGroup)
    warning('%.0f lignes sans AgeGroup (participants non mappés). Elles seront retirées.', sum(missingGroup));
    DATA_all(missingGroup, :) = [];
end

% Catégorielles avec ordre
DATA_all.AgeGroup = categorical(DATA_all.AgeGroup, groups, 'Ordinal', true);

% Vérifier distribution (Plat)
fprintf('\nDistribution des participants (surface Plat):\n');
for g = 1:numel(groups)
    n = sum(DATA_all.AgeGroup == groups{g} & DATA_all.Surface == 'Plat');
    fprintf('  %s: %d participants\n', groups{g}, n);
end

% =============== Sauvegarde des données préparées (DATA_all) ===============
fprintf('\nSauvegarde de DATA_all...\n');

prep_path = fullfile(stats_path, 'Prepared_Data');
if ~exist(prep_path, 'dir'), mkdir(prep_path); end

save(fullfile(prep_path, 'DATA_all_prepared.mat'), 'DATA_all', '-v7.3');
fprintf('✓ DATA_all_prepared.mat sauvegardé\n');

DATA_all_csv = DATA_all;
varsCat = varfun(@iscategorical, DATA_all_csv, 'OutputFormat', 'uniform');
catNames = DATA_all_csv.Properties.VariableNames(varsCat);
for k = 1:numel(catNames)
    DATA_all_csv.(catNames{k}) = string(DATA_all_csv.(catNames{k}));
end
writetable(DATA_all_csv, fullfile(prep_path, 'DATA_all_prepared.csv'));
fprintf('✓ DATA_all_prepared.csv sauvegardé\n');

%% AJOUT DES VARIABLES INCLUS DANS CALCUL DU GVI ============================================================
%  Ajout StepTime (s), StanceTime (s), SwingTime (s) dans un CSV
%  Méthode = identique à ton script GVI :
%   - StepTime(s)  = 60 / Cadence(steps/min)
%   - StanceTime(s)= StrideTime(s) * pctToeOff/100
%   - SwingTime(s) = StrideTime(s) - StanceTime(s)
%
%  IMPORTANT : dans ton CSV, pctToeOff est stocké sous
%  "Mean_pctToeOff" (en %). Le code l’utilise tel quel.
% ============================================================

clc; clear; close all;

folder = "C:\Users\silve\Desktop\DOCTORAT\UNIV MONTREAL\TRAVAUX-THESE\Surfaces_Irregulieres\Datas\Script\gaitAnalysisGUI\result\Statistical_Analysis_LMM\Prepared_Data";
inFile  = fullfile(folder, "DATA_all_prepared.csv");
outFile = fullfile(folder, "ACP_Clustering_DATA.csv");

% --- Lecture (CSV séparé par ; ) ---
T = readtable(inFile, ...
    "Delimiter",",", ...
    "TextType","string", ...
    "PreserveVariableNames",true);

% --- Récupérer les colonnes nécessaires ---
cadence   = T.("Mean_Cadence (step.min^{-1})");   % steps/min
strideT_s = T.("Mean_Stride time (s)");           % s
pctTO     = T.("Mean_ToeOff (%)");           % % (0-100)

% --- Calculs ---
Mean_StepTime_s  = 60 ./ cadence;
Mean_StanceTime_s = strideT_s .* (pctTO ./ 100);
Mean_SwingTime_s  = strideT_s - Mean_StanceTime_s;

% --- Nettoyage robuste (valeurs non finies / incohérentes) ---
badCad = ~isfinite(cadence) | cadence <= 0;
Mean_StepTime_s(badCad) = NaN;

badStride = ~isfinite(strideT_s) | strideT_s <= 0;
badPct = ~isfinite(pctTO) | pctTO < 0 | pctTO > 100;

Mean_StanceTime_s(badStride | badPct) = NaN;
Mean_SwingTime_s(badStride | badPct)  = NaN;

% Si swing/stance deviennent négatifs (incohérence), on met NaN
Mean_StanceTime_s(Mean_StanceTime_s < 0) = NaN;
Mean_SwingTime_s(Mean_SwingTime_s < 0)   = NaN;

% --- Ajouter les colonnes au tableau ---
T.Mean_StepTime_s   = Mean_StepTime_s;
T.Mean_StanceTime_s = Mean_StanceTime_s;
T.Mean_SwingTime_s  = Mean_SwingTime_s;

% --- Export (en gardant ; comme séparateur) ---
writetable(T, outFile, "Delimiter",";");

fprintf("✅ Colonnes ajoutées et exportées :\n%s\n", outFile);

%% Optionnel : aperçu rapide pour voir si paramètres en lien avec GVI présents
disp(T(1:10, ["Participant","Surface","AgeGroup", ...
              "Mean_Cadence (step.min^{-1})","Mean_Stride time (s)","Mean_pctToeOff", ...
              "StepTime_s","StanceTime_s","SwingTime_s"]));
