%% EXTRACTION VARIABLES SPATIO-TEMPORELLES PAR GROUPE D'ÂGE ET CONDITION (format.mat et .csv) + Visualisation
% extraction radar plot plus bas : 2ème partie du script
% extraction graphique valeurs en fonction de l'âge : 3ème partie du script

clc;
clear;
close all;

cd('XX') %.mat
addpath(genpath('XX')) %fonctions
addpath(genpath('XX')) % répertoire des codes

% Dossier où sont sauvegardés les résultats MoS et SPARC par participant
mos_dir = '\MoS';
smooth_dir = '\Smoothness';
gvi_dir = '\GVI\GVI_AllSurfaces_RefAdultsPlat'; 

% Chemin de sauvegarde
save_path = 'XX';
if ~exist(save_path, 'dir')
    mkdir(save_path);
end

% Attribution des participants dans des groupes
ParticipantGroup;
Condition = {'Plat', 'Medium', 'High'};
% === Dictionnaire de renommage des variables ===
oldNames = {'pctSimpleAppuie', 'pctToeOff', 'DoubleSupport','LargeurPas','vitFoulee','distFoulee', 'distPas', 'tempsFoulee', ...
            'NormWalkRatio','vitCadencePasParMinute','NormStepLength','NormCadence', 'stepWidthHeel', 'NormStepWidthHeel', 'NormWalkSpeed'};

newNames = {'Single support time (%)', 'ToeOff (%)','Double support time (%)','BaseOfSupport (cm)', ...
            'Gait speed (m.s^{-1})','Stride length (m)', 'Step length (m)', 'Stride time (s)', ...
            'Norm WR (ua)','Cadence (step.min^{-1})', ...
            'Norm Step length (ua)', 'Norm Cadence (ua)', 'StepWidth (cm)', 'Norm StepWidth (ua)', 'Norm Gait Speed (m.s^{-1})'};

renameMap = containers.Map(oldNames, newNames);

% MOS (raw)
renameMap('MoS_AP_HS_mm')   = 'MoS AP HS (mm)';
renameMap('MoS_AP_mean_mm') = 'MoS AP Stance (mm)';
renameMap('MoS_ML_HS_mm')   = 'MoS ML HS (mm)';
renameMap('MoS_ML_mean_mm') = 'MoS ML Stance (mm)';

% MOS (%L0) :
renameMap('MoS_AP_HS_pL0')   = 'MoS AP HS (%L0)';
renameMap('MoS_AP_mean_pL0') = 'MoS AP Stance (%L0)';
renameMap('MoS_ML_HS_pL0')   = 'MoS ML HS (%L0)';
renameMap('MoS_ML_mean_pL0') = 'MoS ML Stance (%L0)';

% Variables Smoothness - COM
renameMap('COM_SPARC_AP')        = 'COM SPARC AP (ua)';
renameMap('COM_SPARC_ML')        = 'COM SPARC ML (ua)';
renameMap('COM_SPARC_V')         = 'COM SPARC V (ua)';
renameMap('COM_SPARC_Magnitude') = 'COM SPARC Magnitude (ua)';
renameMap('COM_LDLJ_AP')         = 'COM LDLJ AP (ua)';
renameMap('COM_LDLJ_ML')         = 'COM LDLJ ML (ua)';
renameMap('COM_LDLJ_V')          = 'COM LDLJ V (ua)';
renameMap('COM_LDLJ_Magnitude')  = 'COM LDLJ Magnitude (ua)';

% Variables Smoothness - STERNUM
renameMap('STERN_SPARC_AP')        = 'STERN SPARC AP (ua)';
renameMap('STERN_SPARC_ML')        = 'STERN SPARC ML (ua)';
renameMap('STERN_SPARC_V')         = 'STERN SPARC V (ua)';
renameMap('STERN_SPARC_Magnitude') = 'STERN SPARC Magnitude (ua)';
renameMap('STERN_LDLJ_AP')         = 'STERN LDLJ AP (ua)';
renameMap('STERN_LDLJ_ML')         = 'STERN LDLJ ML (ua)';
renameMap('STERN_LDLJ_V')          = 'STERN LDLJ V (ua)';
renameMap('STERN_LDLJ_Magnitude')  = 'STERN LDLJ Magnitude (ua)';

% Variable GVI
renameMap('GVI') = 'GVI (ua)';

prefixes = {'Mean_', 'CV_'};

% Initialisation
SpatioTemporalDATA = struct();
groupNames = fieldnames(Group);

% Première phase : collecte des données individuelles (incluant médianes)
for g = 1:length(groupNames)
    groupName = groupNames{g};
    participants = Group.(groupName);
    fprintf('Traitement du groupe : %s\n', groupName);
    
    for iC = 1:length(Condition)
        cond = Condition{iC};
        fprintf('  Condition : %s\n', cond);
        
        % Préallocation d’un tableau de structs
        rowIdx = 1;
        recapRows = {};

        for iP = 1:length(participants)
            participant = participants{iP};
            fprintf('    Participant traité : %s\n', participant);
            file = [participant '_' cond '.mat'];

            if exist(file, 'file')
                data = load(file);
                stats = Spatiotempocalc(data.c.resultsAll.kin.Left, data.c.resultsAll.kin.Right);
                DATA.(participant).(cond).stats = stats;

                row = struct();
                row.Participant = string(participant);
                row.Condition = string(cond);
                row.AgeMonths = Association_Age(participant);

% ==== Récup MoS pour ce participant et cette condition ====
try
    mosAgg = get_mos_aggregates(participant, cond, mos_dir);
    
    % Bruts (mm) - toujours présents (NaN si manquant)
    row.Mean_MoS_AP_HS_mm   = mosAgg.MoS_AP_HS_mm;
    row.Mean_MoS_AP_mean_mm = mosAgg.MoS_AP_mean_mm;
    row.Mean_MoS_ML_HS_mm   = mosAgg.MoS_ML_HS_mm;
    row.Mean_MoS_ML_mean_mm = mosAgg.MoS_ML_mean_mm;

    % %L0 (toujours présents, NaN si manquant)
    row.Mean_MoS_AP_HS_pL0   = mosAgg.MoS_AP_HS_pL0;
    row.Mean_MoS_AP_mean_pL0 = mosAgg.MoS_AP_mean_pL0;
    row.Mean_MoS_ML_HS_pL0   = mosAgg.MoS_ML_HS_pL0;
    row.Mean_MoS_ML_mean_pL0 = mosAgg.MoS_ML_mean_pL0;

    % Garde aussi dans DATA
    DATA.(participant).(cond).MoS = mosAgg;

catch ME
    warning('Erreur MoS pour %s - %s : %s', participant, cond, ME.message);
    % Les champs restent NaN (déjà initialisés par défaut)
end
% ==== fin Récup MoS ===

% ==== Récup Smoothness pour ce participant et cette condition ====
try
    smoothAgg = get_smoothness_aggregates(participant, cond, smooth_dir);
    
    % COM - SPARC
    row.Mean_COM_SPARC_AP        = smoothAgg.COM_SPARC_AP;
    row.Mean_COM_SPARC_ML        = smoothAgg.COM_SPARC_ML;
    row.Mean_COM_SPARC_V         = smoothAgg.COM_SPARC_V;
    row.Mean_COM_SPARC_Magnitude = smoothAgg.COM_SPARC_Magnitude;
    
    % COM - LDLJ
    row.Mean_COM_LDLJ_AP         = smoothAgg.COM_LDLJ_AP;
    row.Mean_COM_LDLJ_ML         = smoothAgg.COM_LDLJ_ML;
    row.Mean_COM_LDLJ_V          = smoothAgg.COM_LDLJ_V;
    row.Mean_COM_LDLJ_Magnitude  = smoothAgg.COM_LDLJ_Magnitude;
    
    % STERNUM - SPARC
    row.Mean_STERN_SPARC_AP        = smoothAgg.STERN_SPARC_AP;
    row.Mean_STERN_SPARC_ML        = smoothAgg.STERN_SPARC_ML;
    row.Mean_STERN_SPARC_V         = smoothAgg.STERN_SPARC_V;
    row.Mean_STERN_SPARC_Magnitude = smoothAgg.STERN_SPARC_Magnitude;
    
    % STERNUM - LDLJ
    row.Mean_STERN_LDLJ_AP         = smoothAgg.STERN_LDLJ_AP;
    row.Mean_STERN_LDLJ_ML         = smoothAgg.STERN_LDLJ_ML;
    row.Mean_STERN_LDLJ_V          = smoothAgg.STERN_LDLJ_V;
    row.Mean_STERN_LDLJ_Magnitude  = smoothAgg.STERN_LDLJ_Magnitude;

    % Garde aussi dans DATA
    DATA.(participant).(cond).Smoothness = smoothAgg;

catch ME
    warning('Erreur Smoothness pour %s - %s : %s', participant, cond, ME.message);
    % Les champs restent NaN (déjà initialisés par défaut)
end
% ==== fin Récup Smoothness ===

% ==== Récup GVI pour ce participant et cette condition ====
try
    gvi_val = get_gvi_value(participant, cond, gvi_dir);
    row.Mean_GVI = gvi_val;
    
    % Garde aussi dans DATA
    DATA.(participant).(cond).GVI = gvi_val;
    
catch ME
    warning('Erreur GVI pour %s - %s : %s', participant, cond, ME.message);
    row.Mean_GVI = NaN;
end
% ==== fin Récup GVI ===

                % Nombre de cycles
                nCyclesLeft = size(data.c.resultsAll.kin.Left, 2);
                nCyclesRight = size(data.c.resultsAll.kin.Right, 2);
                row.NCycles_Left = nCyclesLeft;
                row.NCycles_Right = nCyclesRight;

                DATA.(participant).(cond).nCycles.Left = nCyclesLeft;
                DATA.(participant).(cond).nCycles.Right = nCyclesRight;

                statsFields = fieldnames(stats);
                for f = 1:length(statsFields)
                    fname = statsFields{f};

                    % Extraction
                    if contains(fname, '_Mean_Mean', 'IgnoreCase', true)
                        % On enlève le suffixe pour garder la racine (ex: pctToeOff)
                        shortName = regexprep(fname, '_Mean_Mean$', '', 'ignorecase');
                        row.(['Mean_' shortName]) = stats.(fname);
                    elseif contains(fname, '_CV_Mean', 'IgnoreCase', true)
                        shortName = regexprep(fname, '_CV_Mean$', '', 'ignorecase');
                        row.(['CV_' shortName]) = stats.(fname);
                    elseif contains(fname, '_SI', 'IgnoreCase', true)
                        shortName = regexprep(fname, '_SI$', '', 'ignorecase');
                        row.(['SI_' shortName]) = stats.(fname);
                    end
                end

       % On ajoute simplement la ligne à la suite des autres
        recapRows{end+1} = row;
        rowIdx = rowIdx + 1;

if ~isfield(SpatioTemporalDATA, 'ALL') || ~isfield(SpatioTemporalDATA.ALL, cond)
    SpatioTemporalDATA.ALL.(cond) = row; 
else
    SpatioTemporalDATA.ALL.(cond)(end+1) = row;
end

            else
                warning('Fichier manquant : %s', file);
            end
        end

        % Convertir en table une fois à la fin de la condition
recapData = struct2table(cell2mat(recapRows));
        SpatioTemporalDATA.(groupName).(cond) = recapData;
        fprintf('  -> Données du groupe %s pour la condition %s traitées.\n', groupName, cond);
    end
end


% Deuxième phase : Créer les matrices de comparaison par condition (incluant médianes)
disp('--- Début de la génération des matrices de comparaison par condition ---');
for iC = 1:length(Condition)
    cond = Condition{iC};
    
    % Collecter toutes les variables numériques communes
    allVariables = {};
    
    % Identifier les variables à partir du premier groupe qui a des données
    for g = 1:length(groupNames)
        groupName = groupNames{g};
        if isfield(SpatioTemporalDATA, groupName) && isfield(SpatioTemporalDATA.(groupName), cond)
            if ~isempty(SpatioTemporalDATA.(groupName).(cond))
                individualData = SpatioTemporalDATA.(groupName).(cond);
                numericCols = varfun(@isnumeric, individualData, 'OutputFormat', 'uniform');
                allVariables = individualData.Properties.VariableNames(numericCols);
                break;
            end
        end
    end
    
    if ~isempty(allVariables)
        % Créer les noms de colonnes : Mean_Var1, Median_Var1, Std_Var1, Mean_Var2, Median_Var2, Std_Var2...
        columnNames = {};
        for v = 1:length(allVariables)
            varName = allVariables{v};
            columnNames{end+1} = ['Mean_' varName];
            columnNames{end+1} = ['Std_' varName];
            columnNames{end+1} = ['Median_' varName];
        end
        
        % Initialiser la matrice de comparaison
        comparisonMatrix = array2table(NaN(length(groupNames), length(columnNames)), ...
            'VariableNames', columnNames, ...
            'RowNames', groupNames);
        
        % Remplir la matrice pour chaque groupe
        for g = 1:length(groupNames)
            groupName = groupNames{g};
            
            if isfield(SpatioTemporalDATA, groupName) && isfield(SpatioTemporalDATA.(groupName), cond)
                if ~isempty(SpatioTemporalDATA.(groupName).(cond))
                    individualData = SpatioTemporalDATA.(groupName).(cond);
                    
                    % Calculer statistiques pour chaque variable
                    for v = 1:length(allVariables)
                        varName = allVariables{v};
                        
                        if ismember(varName, individualData.Properties.VariableNames)
                            values = individualData.(varName);
                            cleanValues = values(~isnan(values));
                            
                            if ~isempty(cleanValues)
                                comparisonMatrix{groupName, ['Mean_' varName]} = mean(cleanValues);
                                comparisonMatrix{groupName, ['Std_' varName]} = std(cleanValues);
                                comparisonMatrix{groupName, ['Median_' varName]} = median(cleanValues);
                            end
                        end
                    end
                end
            end
        end
        
        % Sauvegarder la matrice de comparaison au niveau supérieur
        SpatioTemporalDATA.(cond) = comparisonMatrix;
        fprintf('  → Matrice de comparaison générée pour la condition : %s\n', cond);
    end
end

for iC = 1:length(Condition)
    cond = Condition{iC};
    if isfield(SpatioTemporalDATA.ALL, cond)
        SpatioTemporalDATA.ALL.(cond) = struct2table(SpatioTemporalDATA.ALL.(cond));
    end
end

% Export des matrices de comparaison (incluant médianes)
comparisonFile = fullfile(save_path, 'Comparaison_Groupes_SpatioTemporel.xlsx');
for iC = 1:length(Condition)
    cond = Condition{iC};
    if isfield(SpatioTemporalDATA, cond)
        comparisonMatrix = SpatioTemporalDATA.(cond);
        writetable(comparisonMatrix, comparisonFile, 'Sheet', cond, 'WriteRowNames', true);
    end
end

% === RENOMMAGE DES VARIABLES POUR LES TABLEAUX INDIVIDUELS (ALL) ===
for iC = 1:length(Condition)
    cond = Condition{iC};
    if isfield(SpatioTemporalDATA.ALL, cond)
        tableData = SpatioTemporalDATA.ALL.(cond);
        vars = tableData.Properties.VariableNames;

        for iV = 1:length(vars)
            varName = vars{iV};

            prefixes = {'Mean_', 'CV_', 'SI_'};
            matchedPrefix = '';
            baseName = varName;

            for p = 1:length(prefixes)
                if startsWith(varName, prefixes{p})
                    matchedPrefix = prefixes{p};
                    baseName = extractAfter(varName, matchedPrefix);
                    break;
                end
            end

            if isKey(renameMap, baseName)
                newBase = renameMap(baseName);
                tableData.Properties.VariableNames{iV} = [matchedPrefix newBase];
            end
        end

        SpatioTemporalDATA.ALL.(cond) = tableData;
    end
end

% === RENOMMAGE DES VARIABLES POUR CHAQUE GROUPE ===
groupList = {'JeunesEnfants', 'Enfants', 'Adolescents', 'Adultes'};

for iC = 1:length(Condition)
    cond = Condition{iC};
    
    for g = 1:length(groupList)
        groupName = groupList{g};
        
        if isfield(SpatioTemporalDATA, groupName) && isfield(SpatioTemporalDATA.(groupName), cond)
            tableData = SpatioTemporalDATA.(groupName).(cond);
            vars = tableData.Properties.VariableNames;

            for iV = 1:length(vars)
                varName = vars{iV};

                prefixes = {'Mean_', 'CV_', 'SI_'};
                matchedPrefix = '';
                baseName = varName;

                for p = 1:length(prefixes)
                    if startsWith(varName, prefixes{p})
                        matchedPrefix = prefixes{p};
                        baseName = extractAfter(varName, matchedPrefix);
                        break;
                    end
                end

                if isKey(renameMap, baseName)
                    newBase = renameMap(baseName);
                    tableData.Properties.VariableNames{iV} = [matchedPrefix newBase];
                end
            end

            % Remettre dans la structure renommée
            SpatioTemporalDATA.(groupName).(cond) = tableData;
        end
    end
end

% === EXPORT INDIVIDUEL DES VARIABLES PAR CSV ===
% Dossier de sortie
csv_export_path = fullfile(save_path, 'Matrice', 'CSV_Variables');
if ~exist(csv_export_path, 'dir')
    mkdir(csv_export_path);
end

% Groupes d'âge 
groupList = {'JeunesEnfants', 'Enfants', 'Adolescents', 'Adultes'};

% Base spatio-temporelle
originalNames = oldNames;
newNamesBase  = newNames;  % même ordre que oldNames

% Ajoute les variables MoS et Smoothness à exporter
mosTech = {'MoS_AP_HS_mm','MoS_AP_mean_mm','MoS_ML_HS_mm','MoS_ML_mean_mm', ...
           'MoS_AP_HS_pL0','MoS_AP_mean_pL0','MoS_ML_HS_pL0','MoS_ML_mean_pL0'};
mosReadable = {'MoS AP HS (mm)','MoS AP Stance (mm)','MoS ML HS (mm)','MoS ML Stance (mm)', ...
               'MoS AP HS (%L0)','MoS AP Stance (%L0)','MoS ML HS (%L0)','MoS ML Stance (%L0)'};

smoothTech = {'COM_SPARC_AP', 'COM_SPARC_ML', 'COM_SPARC_V', 'COM_SPARC_Magnitude', ...
    'COM_LDLJ_AP', 'COM_LDLJ_ML', 'COM_LDLJ_V', 'COM_LDLJ_Magnitude', ...
    'STERN_SPARC_AP', 'STERN_SPARC_ML', 'STERN_SPARC_V', 'STERN_SPARC_Magnitude', ...
    'STERN_LDLJ_AP', 'STERN_LDLJ_ML', 'STERN_LDLJ_V', 'STERN_LDLJ_Magnitude'};
smoothReadable = {'COM SPARC AP (ua)', 'COM SPARC ML (ua)', 'COM SPARC V (ua)', 'COM SPARC Magnitude (ua)', ...
    'COM LDLJ AP (ua)', 'COM LDLJ ML (ua)', 'COM LDLJ V (ua)', 'COM LDLJ Magnitude (ua)', ...
    'STERN SPARC AP (ua)', 'STERN SPARC ML (ua)', 'STERN SPARC V (ua)', 'STERN SPARC Magnitude (ua)', ...
    'STERN LDLJ AP (ua)', 'STERN LDLJ ML (ua)', 'STERN LDLJ V (ua)', 'STERN LDLJ Magnitude (ua)'};

% Ajoute GVI aux variables à exporter
gviTech = {'GVI'};
gviReadable = {'GVI (ua)'};

% Fusion avec les autres variables (après smoothness)
originalNames = [originalNames, mosTech, smoothTech, gviTech];
newNamesAll   = [newNamesBase, mosReadable, smoothReadable, gviReadable];

% Map OK (même longueur des deux côtés)
renameMapExport = containers.Map(originalNames, newNamesAll);

% Préfixes à traiter : Moyenne + CV
prefixes = {'Mean_', 'CV_'};

% Structure d'export par participant avec 1 ligne par participant
for p = 1:length(prefixes)
    prefix = prefixes{p};
    
    for iVar = 1:length(originalNames)
        varTech = originalNames{iVar};
        varNameReadable = renameMapExport(varTech);
        varFull = [prefix varNameReadable];  % ex: Mean_Gait speed (m.s^{-1})

        % Initialisation
        exportData = table();
        exportRowIndex = 1;
        
        % Obtenir tous les participants uniques de tous les groupes
        allParticipants = {};
        participantGroups = {};
        
        % Première passe : identifier tous les participants
        for g = 1:length(groupList)
            gName = groupList{g};
            
            % Vérifier si le groupe existe et a des données
            if isfield(SpatioTemporalDATA, gName)
                % Prendre n'importe quelle condition pour obtenir la liste des participants
                for iC = 1:length(Condition)
                    cond = Condition{iC};
                    if isfield(SpatioTemporalDATA.(gName), cond)
                        T = SpatioTemporalDATA.(gName).(cond);
                        if ~isempty(T) && any(strcmp(T.Properties.VariableNames, varFull))
                            groupParticipants = unique(T.Participant);
                            for i = 1:length(groupParticipants)
                                participantID = groupParticipants{i};
                                if ~ismember(participantID, allParticipants)
                                    allParticipants{end+1} = participantID;
                                    participantGroups{end+1} = gName;
                                end
                            end
                        end
                        break;
                    end
                end
            end
        end
        
        % Deuxième passe : collecter les données pour chaque participant
        for i = 1:length(allParticipants)
            participantID = allParticipants{i};
            gName = participantGroups{i};
            
            % Initialiser la ligne pour ce participant
            newRow = table();
            newRow.GroupeAge = {gName};
            newRow.Participant = {participantID};
            newRow.Plat = NaN;
            newRow.Medium = NaN;
            newRow.High = NaN;
            
            % Collecter les valeurs pour chaque condition
            for iC = 1:length(Condition)
                cond = Condition{iC};
                
                if isfield(SpatioTemporalDATA, gName) && isfield(SpatioTemporalDATA.(gName), cond)
                    T = SpatioTemporalDATA.(gName).(cond);
                    
                    if ~isempty(T) && any(strcmp(T.Properties.VariableNames, varFull))
                        % Trouver la ligne correspondant à ce participant
                        participantRows = strcmp(T.Participant, participantID);
                        
                        if any(participantRows)
                            value = T.(varFull)(participantRows);
                            if ~isempty(value) && ~isnan(value(1))
                                newRow.(cond) = value(1);
                            end
                        end
                    end
                end
            end
            
            % Ajouter la ligne au tableau d'export
            if exportRowIndex == 1
                exportData = newRow;
            else
                exportData = [exportData; newRow];
            end
            exportRowIndex = exportRowIndex + 1;
        end
        
        % Sauvegarder le fichier CSV
        if ~isempty(exportData)
            % Nettoyer le nom du fichier
            safeFileName = matlab.lang.makeValidName([prefix '_' varNameReadable]);
            csvPath = fullfile(csv_export_path, [safeFileName '.csv']);
            writetable(exportData, csvPath);
            fprintf(' Export format large : %s (%d participants)\n', csvPath, height(exportData));
        else
            fprintf('  Aucune donnée trouvée pour : %s\n', varFull);
        end
    end
end

% Résumé des exports
fprintf('\n=== RÉSUMÉ DES EXPORTS ===\n');
fprintf('Format: 1 ligne par participant avec colonnes Plat, Medium, High\n');
for g = 1:length(groupList)
    gName = groupList{g};
    if isfield(SpatioTemporalDATA, gName)
        % Compter les participants de ce groupe
        participantCount = 0;
        for iC = 1:length(Condition)
            cond = Condition{iC};
            if isfield(SpatioTemporalDATA.(gName), cond)
                T = SpatioTemporalDATA.(gName).(cond);
                if ~isempty(T)
                    participantCount = length(unique(T.Participant));
                    break;
                end
            end
        end
        fprintf('%s: %d participants\n', gName, participantCount);
    end
end

% Sauvegarder la structure finale
save(fullfile(save_path, 'SpatioTemporalDATA.mat'), 'SpatioTemporalDATA');

% Export des tableaux individuels (ALL) en format CSV pour chaque surface
for iC = 1:length(Condition)
    cond = Condition{iC};
    if isfield(SpatioTemporalDATA.ALL, cond)
        tableToExport = SpatioTemporalDATA.ALL.(cond);
        % Nom du fichier CSV
        csvFileName = fullfile(save_path, ['SpatioTemporal_ALL_' cond '.csv']);
        % Export en CSV
        writetable(tableToExport, csvFileName);
        fprintf('→ Export CSV réalisé pour : %s\n', cond);
    end
end

% Affichage des résultats
disp('=== RÉSULTATS ===');
disp('Structure SpatioTemporalDATA sauvegardée avec succès.');

fprintf('\n Plusieurs options désormais : Soit lancer Statisctics_SpatioTemporal_Parameters.m, soit Clustering.m, soit dresser des plots ci-dessous\n');

%% RADAR PLOTS 5 DOMAINES - INTER ET INTRA -GROUPES
clc; clear; close all;
cd('C:\Users\silve\Desktop\DOCTORAT\UNIV MONTREAL\TRAVAUX-THESE\Surfaces_Irregulieres\Datas\Script\gaitAnalysisGUI\result');
addpath(genpath('C:\Users\silve\Desktop\DOCTORAT\UNIV MONTREAL\TRAVAUX-THESE\Surfaces_Irregulieres\Datas\Script\gaitAnalysisGUI\functions'));
load('SpatioTemporalDATA.mat');

save_path = 'C:\Users\silve\Desktop\DOCTORAT\UNIV MONTREAL\TRAVAUX-THESE\Surfaces_Irregulieres\Datas\Script\gaitAnalysisGUI\result\Fig\SpatioTempo-DATA';
if ~exist(save_path, 'dir')
    mkdir(save_path);
end

conditions = {'Plat', 'Medium', 'High'};
groups = {'JeunesEnfants', 'Enfants', 'Adolescents', 'Adultes'};

% === RADAR INTER-GROUPES (compare les groupes d'âge) ===
fprintf('\n=== Génération des radar plots INTER-GROUPES ===\n');
for i = 1:length(conditions)
    condition = conditions{i};
    fig = radarGaitPlot_5Domains_Inter(SpatioTemporalDATA, condition, groups);
    
    set(fig, 'Units', 'pixels', 'Position', [100, 100, 1400, 1000]);
    filename = fullfile(save_path, sprintf('RadarPlot_5Domains_Inter_%s.png', condition));
    print(fig, filename, '-dpng', '-r300');
    fprintf('  ✅ %s\n', condition);
    close(fig);
end

% RADAR PLOTS 5 DOMAINES - INTRA-GROUPES
fprintf('\n=== Génération des radar plots INTRA-GROUPES ===\n');

condColors = {[0.2 0.4 1], [0 0.6 0], [1 0 0]}; % Plat, Medium, High

for g = 1:length(groups)
    groupName = groups{g};
    fig = radarGaitPlot_5Domains_Intra(SpatioTemporalDATA, groupName, conditions, condColors);
    
    set(fig, 'Units', 'pixels', 'Position', [100, 100, 1400, 1000]);
    filename = fullfile(save_path, sprintf('RadarPlot_5Domains_Intra_%s.png', groupName));
    print(fig, filename, '-dpng', '-r300');
    fprintf('  ✅ %s\n', groupName);
    close(fig);
end

fprintf('\n✅ TOTAL: 7 radar plots générés (3 inter + 4 intra)\n');
fprintf('📂 Sauvegardés dans: %s\n', save_path);

fprintf('\n💡 PROCHAINE ÉTAPE : Dresser le nuage de points');

%% NUAGE DE POINTS DE L'EVOLUTION DES PARAMETRES SPATIO-TEMPORELLES EN FONCTION DU TEMPS
clc; clear; close all;

cd('C:\Users\silve\Desktop\DOCTORAT\UNIV MONTREAL\TRAVAUX-THESE\Surfaces_Irregulieres\Datas\Script\gaitAnalysisGUI\result')
load('SpatioTemporalDATA.mat')

% Dossier de sortie
output_folder = fullfile( ...
    'C:\Users\silve\Desktop\DOCTORAT\UNIV MONTREAL\TRAVAUX-THESE\Surfaces_Irregulieres\Datas\Script\gaitAnalysisGUI\result\Fig', ...
    'SpatioTempo-DATA');
if ~exist(output_folder, 'dir')
    mkdir(output_folder);
end

% === VARIABLES COHERENTES AVEC LE RENOMMAGE ===
variables_to_plot = {
    % --- Moyennes spatio-temporelles ---
    'Mean_Single support time (%)'
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

    % --- MoS bruts (mm) ---
    'Mean_MoS AP HS (mm)'
    'Mean_MoS ML HS (mm)'
    'Mean_MoS AP Stance (mm)'
    'Mean_MoS ML Stance (mm)'


    % --- MoS normalisés (%L0) ---
    'Mean_MoS AP HS (%L0)'
    'Mean_MoS ML HS (%L0)'
    'Mean_MoS AP Stance (%L0)'
    'Mean_MoS ML Stance (%L0)'

    % --- Smoothness ---
    % Variables Smoothness - COM
    'Mean_COM SPARC Magnitude (ua)';
    'Mean_COM LDLJ Magnitude (ua)';
    % Variables Smoothness - STERNUM
    'Mean_STERN SPARC Magnitude (ua)';
    'Mean_STERN LDLJ Magnitude (ua)';

    % --- GVI ---
    'Mean_GVI (ua)'

    % --- Indices de symétrie ---
    'SI_Stride time (s)'
    'SI_Stride length (m)'
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

% Couleurs pour les 3 surfaces
color_map = containers.Map( ...
    {'Plat', 'Medium', 'High'}, ...
    {[0 0.447 0.741], [0 0.6 0], [0.85 0.1 0.1]});

% Fusionner toutes les conditions
DATA_all = [SpatioTemporalDATA.ALL.Plat;
            SpatioTemporalDATA.ALL.Medium;
            SpatioTemporalDATA.ALL.High];

% Tranches d'âge (mois)
tranches = [36 72; 72 144; 144 216; 216 432];
nTranches = size(tranches, 1);

for i = 1:numel(variables_to_plot)
    varname = variables_to_plot{i};

    % Sécurité
    if ~ismember(varname, DATA_all.Properties.VariableNames)
        warning('Variable manquante dans DATA_all : %s', varname);
        continue;
    end

    figure; hold on;

    % lignes de tranches
    for t = 1:nTranches
        xline(tranches(t,1), '--k', 'LineWidth', 1.0, 'HandleVisibility', 'off');
    end
    xline(tranches(end,2), '--k', 'LineWidth', 1.0, 'HandleVisibility', 'off');

    % Boucle surfaces
    for cond = {'Plat','Medium','High'}
        cond_name = cond{1};
        color = color_map(cond_name);

        data_cond = DATA_all(strcmp(DATA_all.Condition, cond_name), :);

        % scatter individuels
        scatter(data_cond.AgeMonths, data_cond.(varname), 22, ...
            'filled', ...
            'MarkerFaceColor', color, ...
            'MarkerEdgeColor', color, ...
            'MarkerFaceAlpha', 0.28, ...
            'MarkerEdgeAlpha', 0.28, ...
            'DisplayName', cond_name);

        % Moyennes par tranche
        moyennes = nan(nTranches,1);
        SD       = nan(nTranches,1);
        x_center = nan(nTranches,1);

        for it = 1:nTranches
            infB = tranches(it,1);
            supB = tranches(it,2);

            idx = data_cond.AgeMonths >= infB & data_cond.AgeMonths < supB;
            xvals = data_cond.AgeMonths(idx);
            yvals = data_cond.(varname)(idx);

            if ~isempty(yvals)
                moyennes(it) = mean(yvals,'omitnan');
                SD(it)       = std(yvals,'omitnan');
                x_center(it) = mean(xvals,'omitnan');
            end
        end

        % tracer moyennes
        errorbar(x_center, moyennes, SD, '-', ...
            'Color', color, 'LineWidth', 2, ...
            'Marker', 'o', 'MarkerFaceColor', color, ...
            'CapSize', 6, ...
            'DisplayName', ['Moyenne ' cond_name]);
    end

    xlabel('Âge (mois)', 'FontSize', 12);
    ylabel(varname, 'Interpreter','none', 'FontSize', 12);
    title(strrep(varname, '_', ' '), 'FontSize', 13);
    legend('Location','eastoutside');
    grid on; box on;

    % nom de fichier safe
    fname = regexprep(varname, '[^\w]', '_');
    saveas(gcf, fullfile(output_folder, [fname '_vs_Age.png']));
    close;
end

disp('✅ Figures générées.');

%% Fonction utilitaire
function mosAgg = get_mos_aggregates(participant, cond, mos_dir)
% Retourne des moyennes par condition pour les indicateurs MoS clés
% - suppose un fichier: mos_dir/MoS_results_<participant>.mat
% - lit la table MoS_data.results et filtre Surface==cond
% - retourne NaN si données manquantes (plus robuste qu'error)

    % Initialisation avec NaN par défaut
    mosAgg = struct();
    mosAgg.MoS_AP_HS_mm   = NaN;
    mosAgg.MoS_AP_mean_mm = NaN;
    mosAgg.MoS_ML_HS_mm   = NaN;
    mosAgg.MoS_ML_mean_mm = NaN;
    mosAgg.MoS_AP_HS_pL0   = NaN;
    mosAgg.MoS_AP_mean_pL0 = NaN;
    mosAgg.MoS_ML_HS_pL0   = NaN;
    mosAgg.MoS_ML_mean_pL0 = NaN;

    % Vérification du fichier
    f = fullfile(mos_dir, sprintf('MoS_results_%s.mat', participant));
    if ~exist(f,'file')
        warning('Fichier MoS introuvable: %s', f);
        return;
    end

    % Chargement
    try
        S = load(f, 'MoS_data');
    catch ME
        warning('Erreur de chargement pour %s: %s', participant, ME.message);
        return;
    end
    
    if ~isfield(S,'MoS_data') || ~isfield(S.MoS_data, 'results') || ~istable(S.MoS_data.results)
        warning('Structure MoS_data.results invalide pour %s', participant);
        return;
    end
    
    T = S.MoS_data.results;

    % Filtre par condition (Surface)
    if ~ismember('Surface', T.Properties.VariableNames)
        warning('La table MoS ne contient pas la colonne Surface pour %s', participant);
        return;
    end
    
    idx = strcmp(T.Surface, cond);
    T = T(idx, :);
    
    if isempty(T)
        warning('Aucun cycle MoS pour %s - %s', participant, cond);
        return;
    end

    % === Extraction des valeurs brutes (mm) ===
    % Vérifier les noms exacts dans votre table
    if ismember('MoS_Heel_Strike_AP', T.Properties.VariableNames)
        mosAgg.MoS_AP_HS_mm = mean(T.MoS_Heel_Strike_AP, 'omitnan');
    end
    
    if ismember('MoS_AP_Mean', T.Properties.VariableNames)
        mosAgg.MoS_AP_mean_mm = mean(T.MoS_AP_Mean, 'omitnan');
    end
    
    if ismember('MoS_Heel_Strike_ML', T.Properties.VariableNames)
        mosAgg.MoS_ML_HS_mm = mean(T.MoS_Heel_Strike_ML, 'omitnan');
    end
    
    if ismember('MoS_ML_Mean', T.Properties.VariableNames)
        mosAgg.MoS_ML_mean_mm = mean(T.MoS_ML_Mean, 'omitnan');
    end

    % === Extraction des versions %L0 (si disponibles) ===
    if ismember('MoS_Heel_Strike_AP_P', T.Properties.VariableNames)
        mosAgg.MoS_AP_HS_pL0 = mean(T.MoS_Heel_Strike_AP_P, 'omitnan');
    end
    
    if ismember('MoS_AP_Mean_P', T.Properties.VariableNames)
        mosAgg.MoS_AP_mean_pL0 = mean(T.MoS_AP_Mean_P, 'omitnan');
    end
    
    if ismember('MoS_Heel_Strike_ML_P', T.Properties.VariableNames)
        mosAgg.MoS_ML_HS_pL0 = mean(T.MoS_Heel_Strike_ML_P, 'omitnan');
    end
    
    if ismember('MoS_ML_Mean_P', T.Properties.VariableNames)
        mosAgg.MoS_ML_mean_pL0 = mean(T.MoS_ML_Mean_P, 'omitnan');
    end
end

function smoothAgg = get_smoothness_aggregates(participant, cond, smooth_dir)
% Retourne les moyennes par condition pour les indicateurs de smoothness
% - Lit le fichier: smooth_dir/Smoothness_TrialBased_<participant>.mat
% - Filtre par Surface==cond
% - Retourne NaN si données manquantes

    % Initialisation avec NaN par défaut
    smoothAgg = struct();
    
    % COM
    smoothAgg.COM_SPARC_AP        = NaN;
    smoothAgg.COM_SPARC_ML        = NaN;
    smoothAgg.COM_SPARC_V         = NaN;
    smoothAgg.COM_SPARC_Magnitude = NaN;
    smoothAgg.COM_LDLJ_AP         = NaN;
    smoothAgg.COM_LDLJ_ML         = NaN;
    smoothAgg.COM_LDLJ_V          = NaN;
    smoothAgg.COM_LDLJ_Magnitude  = NaN;
    
    % STERNUM
    smoothAgg.STERN_SPARC_AP        = NaN;
    smoothAgg.STERN_SPARC_ML        = NaN;
    smoothAgg.STERN_SPARC_V         = NaN;
    smoothAgg.STERN_SPARC_Magnitude = NaN;
    smoothAgg.STERN_LDLJ_AP         = NaN;
    smoothAgg.STERN_LDLJ_ML         = NaN;
    smoothAgg.STERN_LDLJ_V          = NaN;
    smoothAgg.STERN_LDLJ_Magnitude  = NaN;

    % Vérification du fichier
    f = fullfile(smooth_dir, sprintf('Smoothness_TrialBased_%s.mat', participant));
    if ~exist(f, 'file')
        warning('Fichier Smoothness introuvable: %s', f);
        return;
    end

    % Chargement
    try
        S = load(f, 'results');
    catch ME
        warning('Erreur de chargement pour %s: %s', participant, ME.message);
        return;
    end
    
    if ~isfield(S, 'results') || ~istable(S.results)
        warning('Structure results invalide pour %s', participant);
        return;
    end
    
    T = S.results;

    % Filtre par condition (Surface)
    if ~ismember('Surface', T.Properties.VariableNames)
        warning('La table Smoothness ne contient pas la colonne Surface pour %s', participant);
        return;
    end
    
    idx = strcmp(T.Surface, cond);
    T = T(idx, :);
    
    if isempty(T)
        warning('Aucun essai Smoothness pour %s - %s', participant, cond);
        return;
    end

    % === Extraction des moyennes pour chaque métrique COM ===
    if ismember('COM_SPARC_AP', T.Properties.VariableNames)
        smoothAgg.COM_SPARC_AP = mean(T.COM_SPARC_AP, 'omitnan');
    end
    if ismember('COM_SPARC_ML', T.Properties.VariableNames)
        smoothAgg.COM_SPARC_ML = mean(T.COM_SPARC_ML, 'omitnan');
    end
    if ismember('COM_SPARC_V', T.Properties.VariableNames)
        smoothAgg.COM_SPARC_V = mean(T.COM_SPARC_V, 'omitnan');
    end
    if ismember('COM_SPARC_Magnitude', T.Properties.VariableNames)
        smoothAgg.COM_SPARC_Magnitude = mean(T.COM_SPARC_Magnitude, 'omitnan');
    end
    
    if ismember('COM_LDLJ_AP', T.Properties.VariableNames)
        smoothAgg.COM_LDLJ_AP = mean(T.COM_LDLJ_AP, 'omitnan');
    end
    if ismember('COM_LDLJ_ML', T.Properties.VariableNames)
        smoothAgg.COM_LDLJ_ML = mean(T.COM_LDLJ_ML, 'omitnan');
    end
    if ismember('COM_LDLJ_V', T.Properties.VariableNames)
        smoothAgg.COM_LDLJ_V = mean(T.COM_LDLJ_V, 'omitnan');
    end
    if ismember('COM_LDLJ_Magnitude', T.Properties.VariableNames)
        smoothAgg.COM_LDLJ_Magnitude = mean(T.COM_LDLJ_Magnitude, 'omitnan');
    end

    % === Extraction des moyennes pour chaque métrique STERNUM ===
    if ismember('STERN_SPARC_AP', T.Properties.VariableNames)
        smoothAgg.STERN_SPARC_AP = mean(T.STERN_SPARC_AP, 'omitnan');
    end
    if ismember('STERN_SPARC_ML', T.Properties.VariableNames)
        smoothAgg.STERN_SPARC_ML = mean(T.STERN_SPARC_ML, 'omitnan');
    end
    if ismember('STERN_SPARC_V', T.Properties.VariableNames)
        smoothAgg.STERN_SPARC_V = mean(T.STERN_SPARC_V, 'omitnan');
    end
    if ismember('STERN_SPARC_Magnitude', T.Properties.VariableNames)
        smoothAgg.STERN_SPARC_Magnitude = mean(T.STERN_SPARC_Magnitude, 'omitnan');
    end
    
    if ismember('STERN_LDLJ_AP', T.Properties.VariableNames)
        smoothAgg.STERN_LDLJ_AP = mean(T.STERN_LDLJ_AP, 'omitnan');
    end
    if ismember('STERN_LDLJ_ML', T.Properties.VariableNames)
        smoothAgg.STERN_LDLJ_ML = mean(T.STERN_LDLJ_ML, 'omitnan');
    end
    if ismember('STERN_LDLJ_V', T.Properties.VariableNames)
        smoothAgg.STERN_LDLJ_V = mean(T.STERN_LDLJ_V, 'omitnan');
    end
    if ismember('STERN_LDLJ_Magnitude', T.Properties.VariableNames)
        smoothAgg.STERN_LDLJ_Magnitude = mean(T.STERN_LDLJ_Magnitude, 'omitnan');
    end
end

function gvi_value = get_gvi_value(participant, cond, gvi_dir)
% Retourne le GVI pour un participant et une surface donnée
% - Lit le fichier CSV GVI_AllSurfaces_Individual_*.csv le plus récent
% - Filtre par Participant et Surface
% - Retourne NaN si donnée manquante

    % Initialisation avec NaN par défaut
    gvi_value = NaN;
    
    % Recherche du fichier CSV le plus récent
    files = dir(fullfile(gvi_dir, 'GVI_AllSurfaces_Individual_*.csv'));
    
    if isempty(files)
        warning('Aucun fichier GVI trouvé dans : %s', gvi_dir);
        return;
    end
    
    % Trier par date et prendre le plus récent
    [~, idx] = max([files.datenum]);
    gvi_file = fullfile(gvi_dir, files(idx).name);
    
    % Chargement du fichier CSV
    try
        T = readtable(gvi_file);
    catch ME
        warning('Erreur de lecture du fichier GVI: %s', ME.message);
        return;
    end
    
    % Vérification des colonnes nécessaires
    if ~all(ismember({'Participant', 'Surface', 'GVI'}, T.Properties.VariableNames))
        warning('Le fichier GVI ne contient pas les colonnes requises');
        return;
    end
    
    % Filtrage par participant et surface
    idx = strcmp(T.Participant, participant) & strcmp(T.Surface, cond);
    
    if ~any(idx)
        warning('GVI non trouvé pour %s - %s', participant, cond);
        return;
    end
    
    % Extraction de la valeur GVI
    gvi_value = T.GVI(idx);
    
    % Si plusieurs lignes (ne devrait pas arriver), prendre la moyenne
    if numel(gvi_value) > 1
        warning('Plusieurs valeurs GVI pour %s - %s, moyenne prise', participant, cond);
        gvi_value = mean(gvi_value, 'omitnan');
    end
    
    gvi_value = gvi_value(1);
end