%% CLUSTERING SPATIO-TEMPOREL (k-means, nPC >= 70% var.)
% - ACP commune (z-score), clustering sur nPC_final (variance cumulée >= 70%)
% - Viz PC1–PC2 (gradient d'âge), mais partition apprise sur nPC_final
% - Sélection de k (WCSS, Sil, CH, DBI, Gap-1SE) + Stabilité (bootstrap ARI)
% - Rapports "littéraires" : z & unités réelles, Cohen's d, tests (ANOVA/Kruskal) + FDR

clc; clear; close all;

% === Dossier d'E/S ===
root_path = 'XX'; % vers dossier de résultats
cd(root_path)
load('SpatioTemporalDATA.mat') % attend SpatioTemporalDATA.ALL.(Plat|Medium|High)

outdir = fullfile(root_path, 'Fig', 'Clustering');
if ~exist(outdir, 'dir'); mkdir(outdir); end
ts = string(datetime('now','Format','yyyyMMdd_HHmm')); % horodatage

% === Domaines de clustering (5 catégories) ===
domains = struct();

domains.Pace = {
    'Norm Gait Speed',      'Norm Gait Speed (m.s^{-1})',     'Mean';
    'NormStepLength',    'Norm Step length (ua)',         'Mean';
    'NormWalkRatio', 'Norm WR (ua)',     'Mean'
};

domains.Rhythm = {
    'DoubleSupport',  'Double support time (%)',   'Mean';
    'NormCadence',    'Norm Cadence (ua)',         'Mean';
    'COM_SPARC_Magnitude','COM SPARC Magnitude (ua)', 'Mean'
};

domains.PosturalControl = {
    'Norm StepWidth (ua)',     'Norm StepWidth (ua)',         'Mean';
    'MoS_AP_HS_pL0',  'MoS AP HS (%L0)',           'Mean';
    'MoS_ML_HS_pL0',  'MoS ML HS (%L0)',           'Mean'
};

domains.Variability = {
    'GVI (ua)',       'GVI (ua)',                  'Mean';
    'vitFoulee',      'Gait speed (m.s^{-1})',     'CV' ;
    'Norm StepWidth (ua)',     'Norm StepWidth (ua)',         'CV' 
};

domains.Asymmetry = {
    'distFoulee',     'Stride length (m)',         'SI';
    'DoubleSupport',  'Double support time (%)',   'SI';
    'Norm StepWidth (ua)',     'Norm StepWidth (ua)',         'SI'
};


% === Mapping "nom technique" → "nom lisible" (ce que connaît la table) ===
nameMap = containers.Map;
nameMap('vitFoulee')               = 'Gait speed (m.s^{-1})';
nameMap('Norm Gait Speed')         = 'Norm Gait Speed (m.s^{-1})';
nameMap('distFoulee')              = 'Stride length (m)';
nameMap('NormStepLength')          = 'Norm Step length (ua)';
nameMap('tempsFoulee')             = 'Stride time (s)';
nameMap('vitCadencePasParMinute')  = 'Cadence (step.min^{-1})';
nameMap('NormCadence')             = 'Norm Cadence (ua)';
nameMap('NormWalkRatio')           = 'Norm WR (ua)';
nameMap('LargeurPas')              = 'StepWidth (cm)';
nameMap('Norm StepWidth (ua)')     = 'Norm StepWidth (ua)';
nameMap('DoubleSupport')           = 'Double support time (%)';
nameMap('MoS_AP_HS_pL0')           = 'MoS AP HS (%L0)';
nameMap('MoS_ML_HS_pL0')           = 'MoS ML HS (%L0)';
nameMap('COM_SPARC_Magnitude')     = 'COM SPARC Magnitude (ua)';
nameMap('GVI (ua)')                = 'GVI (ua)';
nameMap('ToeOff (%)')              = 'ToeOff (%)';

% === Construction automatique de allVars depuis les domaines ===
allVars = {};
domainNames = fieldnames(domains);

for d = 1:numel(domainNames)
    dVars = domains.(domainNames{d});
    for v = 1:size(dVars, 1)
        varTech = dVars{v,1};  % ex: 'vitFoulee'
        varType = dVars{v,3};  % 'Mean', 'SI', ou 'CV'
        
        % --- LIGNES CORRIGÉES (DÉCOMMENTÉES) ---
        baseName = nameMap(varTech);
        fullVarName = [varType '_' baseName];
        
        % Éviter les doublons et remplir la liste
        if ~ismember(fullVarName, allVars)
            allVars{end+1} = fullVarName; %#ok<SAGROW>
        end
        % ---------------------------------------
    end
end

fprintf('\n=== VARIABLES SÉLECTIONNÉES POUR LE CLUSTERING ===\n');
fprintf('Total : %d variables réparties en 5 domaines\n', numel(allVars));
for d = 1:numel(domainNames)
    dName = domainNames{d};
    nVars = size(domains.(dName), 1);
    fprintf('  - %s : %d variables\n', dName, nVars);
end
fprintf('\nListe complète :\n');
disp(allVars');

% === Version "affichage" des noms (sans unités) ===
allVars_disp = allVars;
for i = 1:numel(allVars_disp)
    % cas spécifique : CV_Gait speed (m.s^{-1}) -> CV_Stride speed
    if strcmp(allVars_disp{i}, 'CV_Gait speed (m.s^{-1})')
        allVars_disp{i} = 'CV_Stride speed';
        continue;
    end
    % supprimer les unités entre parenthèses partout ailleurs
    allVars_disp{i} = regexprep(allVars_disp{i}, '\s*\([^)]*\)', '');
    allVars_disp{i} = strtrim(regexprep(allVars_disp{i}, '\s+', ' '));
end

% === Conditions ===
conds = {'Plat','Medium','High'};

% === Concaténation des données ===
dataMat = [];
meta = table();  % Condition, Participant, AgeMonths

for iC = 1:numel(conds)
    cond = conds{iC};
    T = SpatioTemporalDATA.ALL.(cond);
    mask = all(~ismissing(T(:, allVars)), 2);
    Tsel = T(mask, :);
    X = Tsel{:, allVars};
    dataMat = [dataMat; X]; %#ok<AGROW>
    n = size(X,1);
    meta = [meta; table(repmat(string(cond),n,1), Tsel.Participant, Tsel.AgeMonths, ...
        'VariableNames',{'Condition','Participant','AgeMonths'})]; %#ok<AGROW>
end

% Groupes d'âge (catégories fixes)
meta.AgeGroup = arrayfun(@(m) age_group_from_months(m), meta.AgeMonths, 'UniformOutput', false);
meta.AgeGroup = string(meta.AgeGroup);

% === Normalisation & ACP commune ===
dataNorm = zscore(dataMat);
[coeff, score, ~, ~, explained] = pca(dataNorm);   % base commune

% Nombre de PCs à afficher (par exemple les 10 premières ou toutes si moins)
nPC_display = min(10, size(coeff, 2));

% === COURBE DE VARIANCE EXPLIQUÉE ===
fig_var = figure('Color','w');
cumVar = cumsum(explained);

yyaxis left
bar(explained, 'FaceColor', [0.2 0.4 0.8]); hold on;
ylabel('Explained variance (%)');

yyaxis right
plot(cumVar, '-o', 'Color', [0.8 0.2 0.2], 'LineWidth', 1.5);
ylabel('Cumulative explained variance (%)');

xlabel('Principal components (PC)');
title('Explained and cumulative explained variance - Global PCA');
grid on;

% Ligne du seuil 70 %
yline(70, '--k', '70%');

exportgraphics(fig_var, fullfile(outdir, "01_VarianceExpliquee_GLOBAL_"+ts+".png"), 'Resolution',300);

% === CERCLE DE CORRÉLATION (PC1-PC2) ===
fig_corr = figure('Color','w'); hold on; axis equal;
th = linspace(0, 2*pi, 100);
plot(cos(th), sin(th), 'k--'); % cercle unité
xlabel(sprintf('PC1 (%.1f%%)', explained(1)));
ylabel(sprintf('PC2 (%.1f%%)', explained(2)));
title('Correlation Plot PC1-PC2)');
grid on;

% Corrélations variables vs composantes
corr_vars = corr(dataNorm, score(:,1:2));

% Tracés des flèches
for i = 1:size(corr_vars,1)
    quiver(0,0, corr_vars(i,1), corr_vars(i,2), 0, 'LineWidth',1.2, 'MaxHeadSize',0.1);
    % ici on peut afficher la version sans unités
    text(corr_vars(i,1)*1.1, corr_vars(i,2)*1.1, allVars_disp{i}, ...
        'FontSize',9,'Interpreter','none');
end
xlim([-1.1 1.1]); ylim([-1.1 1.1]);

exportgraphics(fig_corr, fullfile(outdir, "02_CorrelationCircle_PC1PC2_"+ts+".png"), 'Resolution',300);

% === CHOIX DU NOMBRE DE PC POUR LE CLUSTERING (>= 70% de variance) ===
var_threshold = 70; %
cumVar = cumsum(explained);
nPC_final = find(cumVar >= var_threshold, 1, 'first');
if isempty(nPC_final), nPC_final = min(3, size(score,2)); end
nPC_final = max(2, nPC_final); % minimum 2 pour garder une structure
fprintf('nPC_final choisi: %d PCs (variance cumulée = %.1f%%)\n', nPC_final, cumVar(nPC_final));

% === HEATMAP DES LOADINGS (avec gras si |loading| >= 0.50) ===
fig_heatmap_loadings = figure('Color','w', 'Position', [100 100 600 520]);
loadings = corr(dataNorm, score(:, 1:nPC_final));
ax_load = axes('Parent', fig_heatmap_loadings);

nVars = numel(allVars_disp);

imagesc(ax_load, loadings);
colormap(ax_load, redblue(256));
clim(ax_load, [-1, 1]);
cb = colorbar(ax_load);
cb.Label.String = 'Correlation';

% Axes : étiquettes
set(ax_load, ...
    'XTick', 1:nPC_final, 'XTickLabel', {}, ...
    'YTick', 1:nVars,     'YTickLabel', allVars_disp, ...
    'FontSize', 10, 'TickLabelInterpreter', 'none');

ax_load.DataAspectRatio = [1 nVars/nPC_final 1];

for iCol = 1:nPC_final
    text(ax_load, iCol, nVars + 0.75, ...
        sprintf('PC%d', iCol), ...
        'HorizontalAlignment', 'center', 'VerticalAlignment', 'top', ...
        'FontSize', 10, 'FontWeight', 'bold', 'Interpreter', 'none');
    text(ax_load, iCol, nVars + 1.30, ...
        sprintf('(%.1f%%)', explained(iCol)), ...
        'HorizontalAlignment', 'center', 'VerticalAlignment', 'top', ...
        'FontSize', 9, 'Interpreter', 'none');
end
xlabel(ax_load, ''); 
ylabel(ax_load, 'Gait variables');

% Annotations textuelles avec gras conditionnel
for iRow = 1:nVars
    for iCol = 1:nPC_final
        val = loadings(iRow, iCol);
        txt = sprintf('%.2f', val);
        if abs(val) >= 0.50
            fw = 'bold';
        else
            fw = 'normal';
        end
        % Couleur du texte selon luminosité de fond
        if abs(val) > 0.65
            fc = 'w';   % fond saturé → texte blanc
        else
            fc = 'k';   % fond pâle → texte noir
        end
        text(ax_load, iCol, iRow, txt, ...
            'HorizontalAlignment', 'center', ...
            'VerticalAlignment',   'middle', ...
            'FontSize', 9, ...
            'FontWeight', fw, ...
            'Color', fc, ...
            'Interpreter', 'none');
    end
end

% Quadrillage des cellules
for iRow = 0:nVars
    yline(ax_load, iRow + 0.5, 'Color', [0.5 0.5 0.5], 'LineWidth', 0.4);
end
for iCol = 0:nPC_final
    xline(ax_load, iCol + 0.5, 'Color', [0.5 0.5 0.5], 'LineWidth', 0.4);
end
box(ax_load, 'on');
ax_load.LineWidth = 0.8;

exportgraphics(fig_heatmap_loadings, ...
    fullfile(outdir, "01b_Heatmap_Loadings_SelectedPCs_"+ts+".png"), 'Resolution', 300);


% === PAIR PLOTS DES PCs SELECTIONNEES ===
nPC_pairplot = min(5, nPC_final);

fig_pairplot = figure('Color','w', 'Position', [50 50 1400 1400]);

% Créer une grille de subplots
for i = 1:nPC_pairplot
    for j = 1:nPC_pairplot
        subplot(nPC_pairplot, nPC_pairplot, (i-1)*nPC_pairplot + j);
        
        if i == j
            % Diagonale : histogramme de la PC
            histogram(score(:,i), 30, 'FaceColor', [0.3 0.5 0.8], 'EdgeColor', 'none');
            title(sprintf('PC%d (%.1f%%)', i, explained(i)), 'FontSize', 10);
            ylabel('Frequency');
            grid on;
        else
            % Hors diagonale : scatter plot PC_j vs PC_i avec gradient d'âge
            scatter(score(:,j), score(:,i), 25, meta.AgeMonths/12, 'filled', 'MarkerFaceAlpha', 0.6);
            colormap(gca, parula);
            xlabel(sprintf('PC%d (%.1f%%)', j, explained(j)), 'FontSize', 9);
            ylabel(sprintf('PC%d (%.1f%%)', i, explained(i)), 'FontSize', 9);
            grid on;
            
            % Colorbar seulement pour le dernier subplot de chaque ligne
            if j == nPC_pairplot
                cb = colorbar('eastoutside');
                cb.Label.String = 'Age (years)';
                cb.FontSize = 8;
            end
        end
    end
end

sgtitle(sprintf('Pairwise plots of the first %d principal components with an age gradient', nPC_pairplot), ...
        'FontSize', 14, 'FontWeight', 'bold');

exportgraphics(fig_pairplot, fullfile(outdir, "02b_Pairplot_PCs_AgeGradient_"+ts+".png"), 'Resolution',300);

% Pour la visualisation, on conserve PC1–PC2 ; pour le clustering, on utilise 1:nPC_final
Xpcs_all_viz = score(:,1:2);
Xpcs_all     = score(:,1:nPC_final);

%% ========== (A) CLUSTERING PAR K-MEANS ANALYSE GLOBALE (toutes conditions) ==========
rng(42);

% --- Sélection robuste de k (vote multi-critères + bootstrap ARI) ---
kRange = 1:10;
[sil_vals,ch_vals,db_vals,gap_vals,gap_th] = criteria_curves(Xpcs_all,kRange);
ari_mean = bootstrap_ari(Xpcs_all,kRange,100);

sil_n = rescale(sil_vals,0,1);
ch_n  = rescale(ch_vals ,0,1);
db_n  = 1 - rescale(db_vals,0,1);
gap_n = rescale(gap_vals,0,1);
ari_n = rescale(ari_mean,0,1);
score_global = 0.30*sil_n + 0.20*ch_n + 0.20*db_n + 0.10*gap_n + 0.20*ari_n;
[~,best_idx] = max(score_global);
k_global = kRange(best_idx);

% --- Courbe du coude (WCSS) ---
WCSS_vals = zeros(size(kRange));
for kk = 1:numel(kRange)
    k = kRange(kk);
    [idx_tmp, Ctmp] = kmeans(Xpcs_all, k, ...
        'Replicates', 20, 'MaxIter', 300, 'Display', 'off');

    % distance de chaque point à SON centroïde
    d2 = sum((Xpcs_all - Ctmp(idx_tmp, :)).^2, 2);  % n×1
    WCSS_vals(kk) = sum(d2);
end

% === EXPORT DES VALEURS WCSS, SILHOUETTE ET ARI EN FONCTION DE k ===

Kselection_table = table();
Kselection_table.k = kRange(:);
Kselection_table.WCSS = WCSS_vals(:);
Kselection_table.Mean_Silhouette = sil_vals(:);
Kselection_table.ARI_bootstrap_mean = ari_mean(:);

% Réduction absolue du WCSS entre k-1 et k
Kselection_table.Delta_WCSS = [NaN; ...
    Kselection_table.WCSS(1:end-1) - Kselection_table.WCSS(2:end)];

% Réduction relative du WCSS en %
Kselection_table.Percent_WCSS_Reduction = [NaN; ...
    ((Kselection_table.WCSS(1:end-1) - Kselection_table.WCSS(2:end)) ./ ...
    Kselection_table.WCSS(1:end-1)) * 100];

% Ratio du gain par rapport au gain obtenu entre k = 1 et k = 2
gain_k2 = Kselection_table.Delta_WCSS(Kselection_table.k == 2);
Kselection_table.Gain_Ratio_vs_k2 = Kselection_table.Delta_WCSS ./ gain_k2;

disp('=== K-selection summary: WCSS, silhouette and ARI ===');
disp(Kselection_table);

% Export CSV
out_kselection = fullfile(outdir, "K_SELECTION_WCSS_SILHOUETTE_ARI_GLOBAL_" + ts + ".csv");
writetable(Kselection_table, out_kselection);

fprintf('✅ Export k-selection summary : %s\n', out_kselection);

fig_elbow = figure('Color','w');
plot(kRange, WCSS_vals, '-o', 'LineWidth',1.5);
xlabel('Number of clusters (k)');
ylabel('Within-cluster sum of squares (WCSS)');
title('Elbow method - Global data');
grid on;
exportgraphics(fig_elbow, fullfile(outdir, "03a_ElbowPlot_GLOBAL_"+ts+".png"), 'Resolution',300);

% Figures critères
fig_kcrit = figure('Color','w');
tiledlayout(3,2,'TileSpacing','compact','Padding','compact');
nexttile; plot(kRange,sil_vals,'-o'); grid on; title('Silhouette ↑'); xlabel('k');
nexttile; plot(kRange,ch_vals ,'-o'); grid on; title('Calinski–Harabasz ↑'); xlabel('k');
nexttile; plot(kRange,db_vals ,'-o'); grid on; title('Davies–Bouldin ↓'); xlabel('k');
nexttile; plot(kRange,gap_vals,'-o'); grid on; title('Gap ↑ (règle 1-SE)'); xlabel('k'); hold on; yline(gap_th,'k--','1-SE');
nexttile; plot(kRange,ari_mean,'-o'); grid on; title('Stabilité (ARI) ↑'); xlabel('k');
nexttile; plot(kRange,score_global,'-o'); hold on; plot(k_global, score_global(best_idx),'rp','MarkerFaceColor','r');
grid on; title(sprintf('Global score ↑ (k*=%d)',k_global)); xlabel('k');
exportgraphics(fig_kcrit, fullfile(outdir, "03b_Kselection_multiCriteria_GLOBAL_"+ts+".png"), 'Resolution',300);

% === ÉVALUATION COMPARATIVE (k=2→10) — TABLEAU SYNTHÉTIQUE ===
kRange_valid = 1:10;
validity = table('Size',[numel(kRange_valid) 7], ...
    'VariableNames', {'k','CH','Silhouette','DB','Gap','DNg','DNs'}, ...
    'VariableTypes', {'double','double','double','double','double','double','double'});

rng(42);
for ii = 1:numel(kRange_valid)
    k = kRange_valid(ii);
    if size(Xpcs_all,1) < k, continue; end
    idx = kmeans(Xpcs_all, k, 'Replicates',40,'MaxIter',400,'Display','off');
    try SilMean = mean(silhouette(Xpcs_all, idx)); catch, SilMean = NaN; end
    EC_ch  = evalclusters(Xpcs_all,'kmeans','CalinskiHarabasz','KList',k);
    EC_db  = evalclusters(Xpcs_all,'kmeans','DaviesBouldin','KList',k);
    EC_gap = evalclusters(Xpcs_all,'kmeans','gap','KList',k);

    % Distances inter/intra
    D = pdist2(Xpcs_all, Xpcs_all);
    intra = mean(arrayfun(@(c) mean(D(idx==c,idx==c),'all','omitnan'), 1:k));
    inter = mean(arrayfun(@(c) mean(D(idx==c,idx~=c),'all','omitnan'), 1:k));

    validity(ii,:) = {k, EC_ch.CriterionValues, SilMean, EC_db.CriterionValues, EC_gap.CriterionValues, inter, intra};
end

disp('=== Pertinence des clusters (2→10) ===');
disp(validity);

% Export CSV uniquement
out_csv = fullfile(outdir, "VALIDITY_INDEXES_GLOBAL_"+ts+".csv");
writetable(validity, out_csv);
fprintf('✅ Export CSV : %s\n', out_csv);

% --- Clustering final (global) sur nPC_final ---
[idxCluster_global, Cc_global_npc] = kmeans(Xpcs_all, k_global, ...
    'Replicates',50,'MaxIter',500,'Display','off');
Cc_global_pc12 = Cc_global_npc(:,1:2);  % centroïdes projetés sur PC1–PC2

% === TRAÇABILITÉ DES INDIVIDUS PAR CLUSTER ===
ClusterTrace = table();
ClusterTrace.Participant = meta.Participant;
ClusterTrace.Condition   = meta.Condition;
ClusterTrace.AgeMonths   = meta.AgeMonths;
ClusterTrace.AgeGroup    = meta.AgeGroup;
ClusterTrace.Cluster     = idxCluster_global;
ClusterTrace.PC1         = Xpcs_all_viz(:,1);
ClusterTrace.PC2         = Xpcs_all_viz(:,2);

% Tri par cluster
ClusterTrace = sortrows(ClusterTrace, "Cluster");

% === EXPORT COMPLET POUR R (GLOBAL) ===
% On crée une table qui combine Meta, le Cluster assigné, et les données brutes (dataMat)
FinalTable_Global = [meta, table(idxCluster_global, 'VariableNames', {'ClusterID'}), array2table(dataMat, 'VariableNames', allVars)];

% Export CSV
out_R_global = fullfile(outdir, "DATA_FOR_R_GLOBAL_" + ts + ".csv");
writetable(FinalTable_Global, out_R_global);
fprintf('✅ Export pour R (Global) : %s\n', out_R_global);

% Export CSV
out_trace = fullfile(outdir, "06_ClusterTraceability_GLOBAL_"+ts+".csv");
writetable(ClusterTrace, out_trace);
fprintf('✅ Export traçabilité participants : %s\n', out_trace);

% Définir les markers par condition
markerMap = containers.Map({'Plat', 'Medium', 'High'}, {'o', 's', '^'});
markerSizes = 60; % Taille des markers

fig_agegrad_shapes = figure('Color','w', 'Position', [100 100 780 650]); 
hold on;

% Tracer chaque condition avec sa forme spécifique
conds_unique = unique(meta.Condition);
for iC = 1:numel(conds_unique)
    cond = conds_unique(iC);
    idx_cond = meta.Condition == cond;

    % Nom à afficher dans la légende
    label_cond = char(cond);
    if strcmp(label_cond, 'Plat')
        label_cond = 'Even';
    end
    
    scatter(Xpcs_all_viz(idx_cond, 1), Xpcs_all_viz(idx_cond, 2), ...
            markerSizes, meta.AgeMonths(idx_cond)/12, ...
            'filled', markerMap(char(cond)), ...
            'MarkerFaceAlpha', 0.7, ...
            'DisplayName', label_cond);
end

colormap(parula); 
cb = colorbar; 
cb.Label.String = 'Age (years)';
cb.FontSize = 11;

xlabel(sprintf('PC1 (%.1f%%)', explained(1)), 'FontSize', 15); 
ylabel(sprintf('PC2 (%.1f%%)', explained(2)), 'FontSize', 15);
%title(sprintf('PC1–PC2 projection with age gradient and surface-specific markers\nClustering based on %d principal components, k=%d', ...
              %nPC_final, k_global), 'FontSize', 13);
ax = gca;
ax.FontSize = 13;
ax.LineWidth = 1.1;
ax.Box = 'on';

grid on;

% Tracer les contours des clusters
cols = lines(k_global);
for i = 1:k_global
    pts2 = Xpcs_all_viz(idxCluster_global==i, :);
    if size(pts2,1) >= 3
        K2 = convhull(pts2(:,1), pts2(:,2));
        plot(pts2(K2,1), pts2(K2,2), '-', 'Color', cols(i,:), ...
             'LineWidth', 1, 'HandleVisibility', 'off');
    end
end

% Centroïdes
plot(Cc_global_pc12(:,1), Cc_global_pc12(:,2), 'kx', ...
     'MarkerSize', 14, 'LineWidth', 3, 'DisplayName', 'Centroid');
for i = 1:k_global
    text(Cc_global_pc12(i,1), Cc_global_pc12(i,2), sprintf('  C%d', i), ...
         'FontWeight', 'bold', 'Color', 'k', 'VerticalAlignment', 'middle', ...
         'FontSize', 11);
end

% Légende pour les surfaces
legend('Location', 'best', 'FontSize', 13);

% === Export figure PC1-PC2 ===
out_pc12 = fullfile(outdir, "04_PC1PC2_AgeGradient_Shapes_GLOBAL_k" + string(k_global) + "_" + ts + ".png");

exportgraphics(fig_agegrad_shapes, out_pc12, 'Resolution', 300);

fprintf('✅ Figure PC1-PC2 exportée : %s\n', out_pc12);

% Annotations d'âge
%AgeStats_global = annotate_age_on_clusters(gca, Xpcs_all_viz, idxCluster_global, Cc_global_pc12, meta.AgeMonths);

%% Figure pour déterminer le nombre de cluster
% --- Courbe du coude (WCSS) + Silhouette + ARI sur UNE SEULE FIGURE ---
WCSS_vals = zeros(size(kRange));
for kk = 1:numel(kRange)
    k = kRange(kk);

    [idx_tmp, Ctmp] = kmeans(Xpcs_all, k, ...
        'Replicates', 20, 'MaxIter', 300, 'Display', 'off');

    % distance de chaque point à SON centroïde
    d2 = sum((Xpcs_all - Ctmp(idx_tmp, :)).^2, 2);  % n×1
    WCSS_vals(kk) = sum(d2);
end

fig_elbow_combo = figure('Color','w', 'Position',[100 100 650 720]);
tiledlayout(3,1,'TileSpacing','compact','Padding','compact');

kRange_part = kRange(kRange >= 2);
sil_vals_part = sil_vals(kRange >= 2);
ari_mean_part = ari_mean(kRange >= 2);

% 1) WCSS
nexttile;
plot(kRange, WCSS_vals, '-o', 'LineWidth',1.6); grid on;
ylabel('WCSS', 'FontSize', 13);
title('k-selection (Global): Elbow + Silhouette + Stability (ARI)');
xlim([min(kRange) max(kRange)]);
hold on;
xline(k_global,'--k',sprintf('k=%d',k_global), ...
    'FontSize',12, ...
    'LabelVerticalAlignment','bottom');

ax = gca;
ax.FontSize = 12;
ax.LineWidth = 1;
ax.Box = 'on';

% 2) Silhouette
nexttile;
plot(kRange_part, sil_vals_part, '-o', 'LineWidth',1.6); grid on;
ylabel('Mean silhouette', 'FontSize', 13);
xlim([2 max(kRange)]);
hold on;
xline(k_global,'--k','HandleVisibility','off');

ax = gca;
ax.FontSize = 12;
ax.LineWidth = 1;
ax.Box = 'on';

% 3) ARI bootstrap
nexttile;
plot(kRange_part, ari_mean_part, '-o', 'LineWidth',1.6); grid on;
ylabel('ARI', 'FontSize', 13);
xlabel('Number of clusters (k)');
xlim([2 max(kRange)]);
hold on;
xline(k_global,'--k','HandleVisibility','off');

ax = gca;
ax.FontSize = 12;
ax.LineWidth = 1;
ax.Box = 'on';

exportgraphics(fig_elbow_combo, fullfile(outdir, "03a_Elbow_Sil_ARI_GLOBAL_"+ts+".png"), 'Resolution',300);

%% SUITE CODE
exportgraphics(fig_agegrad_shapes, ...
               fullfile(outdir, "04_PC1PC2_AgeGradient_Shapes_GLOBAL_k"+k_global+"_"+ts+".png"), ...
               'Resolution', 300);

% --- Profils moyens: z & unités réelles, Cohen's d, tests + FDR ---
[meanZ_global, meanRAW_global, cohenD_global, statsTable_global] = ...
    cluster_profiles_and_stats(dataMat, dataNorm, idxCluster_global, allVars);

% --- Sauvegardes GLOBAL ---
writetable(AgeStats_global, fullfile(outdir, "Age_Stats_GLOBAL_"+ts+".csv"));
writetable(addvars(meanZ_global,(1:k_global)','Before',1,'NewVariableNames','ClusterID'), ...
           fullfile(outdir, "ProfilMoyen_zscore_GLOBAL_"+ts+".csv"), 'WriteRowNames',true);
writetable(addvars(meanRAW_global,(1:k_global)','Before',1,'NewVariableNames','ClusterID'), ...
           fullfile(outdir, "ProfilMoyen_unitesReelles_GLOBAL_"+ts+".csv"), 'WriteRowNames',true);
writetable(addvars(cohenD_global,(1:k_global)','Before',1,'NewVariableNames','ClusterID'), ...
           fullfile(outdir, "CohenD_GLOBAL_"+ts+".csv"), 'WriteRowNames',true);
writetable(statsTable_global, fullfile(outdir,"Stats_omnibus_GLOBAL_"+ts+".csv"));

% --- Heatmap z-score (UNE SEULE FIGURE, annotations intégrées) ---
fig_heat = figure('Color','w');
heatmap_data_global = table2array(meanZ_global);
clim = max(abs(heatmap_data_global(:)));
rowLabels = arrayfun(@(i) ...
    sprintf('C%d (n=%d, %.1f ans)', i, sum(idxCluster_global==i), AgeStats_global.AgeMonths_mean(i)/12), ...
    1:k_global, 'UniformOutput', false);

h = heatmap(allVars_disp, rowLabels, heatmap_data_global, ...
    'Title', sprintf('Mean z-score profiles - Global (k=%d, nPC=%d)', k_global, nPC_final), ...
    'XLabel','Spatiotemporal variables','YLabel','Clusters (taille, âge)', ...
    'Colormap', blueyellow(256), 'ColorLimits', [-clim, clim]);
h.FontSize = 10; h.CellLabelFormat = '%.2f'; h.GridVisible = 'on';
exportgraphics(fig_heat, fullfile(outdir, "05_Heatmap_Profils_GLOBAL_k"+k_global+"_"+ts+".png"), 'Resolution',300);

% --- Qualité & composition ---
[sil_each, CH_best, DB_best, GAP_best, EntTable_global] = ...
    quality_and_composition(Xpcs_all, idxCluster_global, meta, k_global, ch_vals, db_vals, gap_vals, kRange);
writetable( table((1:numel(sil_each))', sil_each, idxCluster_global, ...
    'VariableNames', {'Obs','Silhouette','Cluster'}) , ...
    fullfile(outdir,"Silhouette_indiv_GLOBAL_"+ts+".csv") );
writetable(EntTable_global, fullfile(outdir,"Qualite_Clusters_GLOBAL_"+ts+".csv"));
fprintf('\n=== QUALITE (GLOBAL) ===\n');
fprintf('nPC=%d | k*=%d | Sil=%.3f | CH=%.1f | DBI=%.3f | GAP=%.3f | ARI_boot=%.3f\n', ...
    nPC_final, k_global, mean(sil_each,'omitnan'), CH_best, DB_best, GAP_best, ari_mean(kRange==k_global));
print_cluster_composition(meta, idxCluster_global, conds);

% === TESTS STATISTIQUES ENTRE CLUSTERS ===
fprintf('\n=== Tests statistiques entre clusters (GLOBAL) ===\n');

cluster_labels = unique(idxCluster_global);
nClust = numel(cluster_labels);
pvals = nan(1, numel(allVars));
cohensD = nan(1, numel(allVars));
testType = strings(numel(allVars),1);

for v = 1:numel(allVars)
    data1 = dataMat(idxCluster_global == cluster_labels(1), v);
    data2 = dataMat(idxCluster_global == cluster_labels(2), v);

    % Test de normalité
    if all(~isnan(data1)) && all(~isnan(data2))
        isNorm = all([kstest((data1-mean(data1))/std(data1)) == 0, ...
                      kstest((data2-mean(data2))/std(data2)) == 0]);
    else
        isNorm = false;
    end

    % Test statistique
    if isNorm
        [~, p] = ttest2(data1, data2);
        testType(v) = "t-test";
    else
        p = ranksum(data1, data2);
        testType(v) = "Mann-Whitney";
    end
    pvals(v) = p;

    % Taille d'effet (Cohen's d)
    m1 = mean(data1, 'omitnan'); m2 = mean(data2, 'omitnan');
    s1 = std(data1, 'omitnan'); s2 = std(data2, 'omitnan');
    s_pooled = sqrt(((numel(data1)-1)*s1^2 + (numel(data2)-1)*s2^2) / ...
                    (numel(data1)+numel(data2)-2));
    cohensD(v) = (m1 - m2) / s_pooled;
end

% Correction Bonferroni
nTests = numel(pvals);
pvals_bonf = pvals * nTests;
pvals_bonf(pvals_bonf > 1) = 1; % borne max à 1

% Indicateurs de significativité
Sig = repmat("ns", numel(pvals), 1);
Sig(pvals_bonf < 0.05) = "*";
Sig(pvals_bonf < 0.01) = "**";
Sig(pvals_bonf < 0.001) = "***";

% Résumé tableau avec Bonferroni + type de test
StatsClusters = table(allVars_disp', testType, pvals', pvals_bonf', cohensD', Sig, ...
    'VariableNames', {'Variable','Test','p_value','p_Bonferroni','Cohens_d','Sig'});
disp(StatsClusters)

% Export CSV
writetable(StatsClusters, fullfile(outdir, "ClusterComparison_STATS_BONFERRONI_"+ts+".csv"));

fprintf('\n💡 Aller voir le fichier R pour stats inter-cluster');

%% ===================== FONCTIONS LOCALES =====================

function grp = age_group_from_months(m)
if isnan(m)
    grp = "NA";
elseif m < 72
    grp = "JeunesEnfants";
elseif m < 144
    grp = "Enfants";
elseif m < 216
    grp = "Adolescents";
else
    grp = "Adultes";
end
end

function [sil_vals,ch_vals,db_vals,gap_vals,gap_th] = criteria_curves(X,kRange)
sil_vals = zeros(numel(kRange),1);
ch_vals  = zeros(numel(kRange),1);
db_vals  = zeros(numel(kRange),1);
gap_vals = zeros(numel(kRange),1);
for ii = 1:numel(kRange)
    k = kRange(ii);
    idx_tmp = kmeans(X,k,'Replicates',10,'MaxIter',300,'Display','off');
    try
        sil_vals(ii) = mean(silhouette(X, idx_tmp, 'sqeuclidean'));
    catch
        sil_vals(ii) = NaN;
    end
    EC_ch  = evalclusters(X,'kmeans','CalinskiHarabasz','KList',k);
    EC_db  = evalclusters(X,'kmeans','DaviesBouldin'   ,'KList',k);
    EC_gap = evalclusters(X,'kmeans','gap'              ,'KList',k);
    ch_vals(ii)  = EC_ch.CriterionValues;
    db_vals(ii)  = EC_db.CriterionValues;
    gap_vals(ii) = EC_gap.CriterionValues;
end
gap_se = std(gap_vals,'omitnan');
gap_th = max(gap_vals,[],'omitnan') - gap_se;
end

function ari_mean = bootstrap_ari(X,kRange,B)
ari_mean = zeros(numel(kRange),1);
for ii=1:numel(kRange)
    k = kRange(ii);
    idx_ref = kmeans(X,k,'Replicates',50,'MaxIter',500,'Display','off');
    ARI = zeros(B,1);
    for b=1:B
        boot_idx = randsample(size(X,1), size(X,1), true);
        Xb = X(boot_idx,:);
        idx_b = kmeans(Xb,k,'Replicates',20,'MaxIter',300,'Display','off');
        ARI(b) = adjustedRandIndex(idx_ref(boot_idx), idx_b);
    end
    ari_mean(ii) = mean(ARI,'omitnan');
end
end

function [meanZ, meanRAW, cohenD, statsTable] = cluster_profiles_and_stats(dataRAW, dataZ, idx, allVars)
k = max(idx);
meanZ = array2table(zeros(k,numel(allVars)),'VariableNames',allVars,...
    'RowNames', "Cluster_"+string(1:k));
for i=1:k
    meanZ{i,:} = mean(dataZ(idx==i,:),1,'omitnan');
end
mu = mean(dataRAW,1,'omitnan'); sd = std(dataRAW,0,1,'omitnan');
meanRAW = meanZ;
for v=1:numel(allVars)
    meanRAW{:,v} = meanZ{:,v}.*sd(v) + mu(v);
end
cohenD = array2table(zeros(k,numel(allVars)),'VariableNames',allVars,...
    'RowNames', "Cluster_"+string(1:k));
for i=1;k
    Xc = dataRAW(idx==i,:);
    for v=1:numel(allVars)
        cohenD{i,v} = (mean(Xc(:,v),'omitnan') - mu(v)) / (sd(v)+eps);
    end
end
pvals = nan(1,numel(allVars));
for v=1:numel(allVars)
    y = dataRAW(:,v); g = idx;
    ok = ~isnan(y) & ~isnan(g);
    y = y(ok); g = g(ok);
    if numel(y) > 5 && numel(unique(g)) > 1
        ystd = (y - mean(y,'omitnan'))./std(y,[],'omitnan');
        try normok = (lillietest(ystd) == 0); catch, normok = false; end
        if normok
            pvals(v) = anova1(y,g,'off');
        else
            pvals(v) = kruskalwallis(y,g,'off');
        end
    end
end
[~,~,~,qvals] = fdr_bh(pvals, 0.05);
statsTable = table(allVars(:), pvals(:), qvals(:), ...
    'VariableNames', {'Variable','p_omnibus','q_FDR'});
end

function [sil_each, CH_best, DB_best, GAP_best, EntTable] = quality_and_composition(Xpcs, idx, meta_tbl, k, ch_vals, db_vals, gap_vals, kRange)
try
    sil_each = silhouette(Xpcs, idx);
catch
    sil_each = nan(size(idx));
end
CH_best  = ch_vals( kRange==k );
DB_best  = db_vals( kRange==k );
GAP_best = gap_vals(kRange==k );

ageCats = ["JeunesEnfants","Enfants","Adolescents","Adultes"];
ent_age = zeros(k,1);
ent_surf= zeros(k,1);
for c=1:k
    sub = meta_tbl(idx==c,:);
    countsA = arrayfun(@(a) sum(sub.AgeGroup==a), ageCats);
    pA = countsA/sum(countsA); pA = pA(pA>0);
    ent_age(c) = -sum(pA.*log2(pA));
    catsS = categorical(sub.Condition);
    countsS = countcats(catsS);
    pS = countsS/sum(countsS); pS = pS(pS>0);
    ent_surf(c) = -sum(pS.*log2(pS));
end
sil_byC = splitapply(@(x) mean(x,'omitnan'), sil_each, findgroups(idx));
EntTable = table((1:k)', sil_byC, ent_age, ent_surf, ...
    'VariableNames', {'Cluster','SilhouetteMean','EntropyAge','EntropySurface'});
end

function ARI = adjustedRandIndex(labelsTrue, labelsPred)
labelsTrue = labelsTrue(:); labelsPred = labelsPred(:);
n = numel(labelsTrue);
[~,~,l1] = unique(labelsTrue);
[~,~,l2] = unique(labelsPred);
k1 = max(l1); k2 = max(l2);
N = accumarray([l1 l2],1,[k1 k2]);
ni = sum(N,2); nj = sum(N,1);
sumComb = @(x) sum(x.*(x-1)/2);
t1 = sumComb(N(:));
t2 = sumComb(ni);
t3 = sumComb(nj);
t4 = n*(n-1)/2;
exp_index = (t2*t3)/t4;
max_index = (t2 + t3)/2;
ARI = (t1 - exp_index) / (max_index - exp_index + eps);
end

function [h, crit_p, adj_p, q] = fdr_bh(pvals,qlevel)
if nargin<2, qlevel = 0.05; end
p = pvals(:);
[ps, idx] = sort(p);
m = numel(p);
thresh = (1:m)'/m*qlevel;
is_sig = ps <= thresh;
k = find(is_sig,1,'last');
h = false(size(p));
if ~isempty(k), h(idx(1:k)) = true; end
if ~isempty(k), crit_p = ps(k); else, crit_p = NaN; end
adj_p = nan(size(p));
adj_p(idx) = ps .* m ./ (1:m)';
q = adj_p;
end

function AgeStats = annotate_age_on_clusters(ax, pcs, idxCluster, Cc, agesMonths)
k = size(Cc,1);
AgeStats = table('Size',[k 5], ...
    'VariableTypes', {'double','double','double','double','string'}, ...
    'VariableNames', {'Cluster','N','AgeMonths_mean','AgeMonths_sd','AgeYears_label'});
AgeStats.Cluster = (1:k)';

hold(ax, 'on');
dx = 0.12; dy = 0.12; halo = 0.05;

for i = 1:k
    idx = (idxCluster == i);
    ages_m = agesMonths(idx);
    n_i  = numel(ages_m);
    mu_m = mean(ages_m, 'omitnan');
    sd_m = std(ages_m, 'omitnan');

    AgeStats.N(i)              = n_i;
    AgeStats.AgeMonths_mean(i) = mu_m;
    AgeStats.AgeMonths_sd(i)   = sd_m;

    mu_y = mu_m/12; sd_y = sd_m/12;
    lbl = sprintf('%.1f %s %.1f ans', mu_y, char(177), sd_y);

    if all(isfinite(Cc(i,:)))
        x = Cc(i,1) + dx; y = Cc(i,2) + dy; txt = "← " + string(lbl);
        for ang = linspace(0, 2*pi, 8)
            text(ax, x + halo*cos(ang), y + halo*sin(ang), txt, ...
                'HorizontalAlignment','left','VerticalAlignment','bottom', ...
                'FontSize',12,'FontWeight','bold','Color','w','Clipping','on');
        end
        text(ax, x, y, txt, 'HorizontalAlignment','left','VerticalAlignment','bottom', ...
            'FontSize',12,'FontWeight','bold','Color','k','Clipping','on');
    end
end
end

function print_cluster_composition(meta_tbl, idxCluster, varargin)
clusters = unique(idxCluster(:))';
ageCats = ["JeunesEnfants","Enfants","Adolescents","Adultes"];
hasMultipleConds = numel(unique(meta_tbl.Condition)) > 1;

for c = clusters
    idx = (idxCluster == c);
    sub = meta_tbl(idx, :);
    countsAge = zeros(size(ageCats));
    for a = 1:numel(ageCats)
        countsAge(a) = sum(sub.AgeGroup == ageCats(a));
    end
    fprintf('Cluster %d:\n', c);
    fprintf('  Par groupe d''âge : ');
    for a = 1:numel(ageCats)
        fprintf('%s=%d%s', ageCats(a), countsAge(a), iff(a<numel(ageCats), ', ', ''));
    end
    fprintf('\n');
    if hasMultipleConds
        [uConds, ~, ic] = unique(sub.Condition);
        fprintf('  Conditions représentées : %d (', numel(uConds));
        for k = 1:numel(uConds)
            cnt = sum(ic==k);
            fprintf('%s=%d%s', uConds(k), cnt, iff(k<numel(uConds), ', ', ''));
        end
        fprintf(')\n');
    end
end
end

function s = iff(cond, a, b)
if cond, s = a; else, s = b; end
end

function cmap = blueyellow(n)
if nargin < 1, n = 256; end
r1 = linspace(0, 1, n/2)'; g1 = linspace(0, 1, n/2)'; b1 = ones(n/2, 1);
r2 = ones(n/2, 1); g2 = ones(n/2, 1); b2 = linspace(1, 0, n/2)';
cmap = [r1, g1, b1; r2, g2, b2];
end

function cmap = redblue(n)
    % Colormap rouge-blanc-bleu pour loadings
    if nargin < 1, n = 256; end
    
    % Rouge -> Blanc
    r1 = ones(n/2, 1);
    g1 = linspace(0, 1, n/2)';
    b1 = linspace(0, 1, n/2)';
    
    % Blanc -> Bleu
    r2 = linspace(1, 0, n/2)';
    g2 = linspace(1, 0, n/2)';
    b2 = ones(n/2, 1);
    
    cmap = [r1, g1, b1; r2, g2, b2];
end