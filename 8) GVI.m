%% ============================================================
%  GVI (Gouelle 2013) — TOUTES SURFACES (Plat / Medium / High)
%  - 1 GVI par individu ET par surface (donc 3 scores si 3 surfaces)
%  - Référence FIXE = Adultes (Plat) définis dans ParticipantGroup.m
%
%  Entrées .mat: variable 'c' avec c.resultsAll.kin.Left/Right (struct)
%    où chaque champ spatio-temporel est un vecteur (1 valeur = 1 cycle)
%
%  Sorties :
%   - CSV individuel (GVI par participant × surface)
%   - CSV résumé par groupe × surface
%   - Figures boxplots QC par surface
% ============================================================

clc; clear; close all;

%% === PATHS ===
mat_dir   = 'XX'; %.mat 
root_dir  = '';
func_dir  = fullfile(root_dir,'functions');

addpath(genpath(root_dir));
addpath(genpath(func_dir));
cd(mat_dir);

surfaces = {'Plat','Medium','High'};

outdir = fullfile(root_dir, 'result', 'Fig', 'GVI_AllSurfaces_RefAdultsPlat');
if ~exist(outdir,'dir'); mkdir(outdir); end
ts = char(datetime('now','Format','yyyyMMdd_HHmm'));

%% === GROUPES (référence) ===
ParticipantGroup;
assert(exist('Group','var')==1, 'ParticipantGroup.m doit créer une variable Group.');

refAdults = string(Group.Adultes(:));  % IDs adultes

% Map participant -> groupe
groupMap = containers.Map;
fns = fieldnames(Group);
for k = 1:numel(fns)
    gname = fns{k};
    ids = string(Group.(gname)(:));
    for j = 1:numel(ids)
        groupMap(char(ids(j))) = gname;
    end
end

%% === PARAMÈTRES GVI (9) ===
paramList = { ...
    'StepLength', ...
    'StrideLength', ...
    'StepTime', ...
    'StrideTime', ...
    'SwingTime', ...
    'StanceTime', ...
    'SingleSupportTime', ...
    'DoubleSupportTime', ...
    'StrideVelocity'};

% Basé sur Gouelle et al. 2013
cn18 = [
0.797 0.728 ... % StepLength (Mean, SD)
0.699 0.728 ... % StrideLength
0.930 0.817 ... % StepTime
0.901 0.836 ... % StrideTime
0.899 0.882 ... % Swing
0.919 0.852 ... % Stance
0.902 0.860 ... % Single
0.805 0.687 ... % Double
0.890 0.889];   % Velocity

cn = [cn18 cn18];
nParam = numel(paramList);
nAlt  = 2*nParam;        % 18
nCols = 2*nAlt;          % 36 = Left(18) + Right(18)

%% === LISTE DES FICHIERS (TOUTES SURFACES) ===
allFiles = [];
for s = 1:numel(surfaces)
    ff = dir(fullfile(mat_dir, sprintf('*_%s.mat', surfaces{s})));
    allFiles = [allFiles; ff]; 
end
if isempty(allFiles)
    error('Aucun fichier *_{Plat|Medium|High}.mat trouvé dans %s', mat_dir);
end
fprintf('Fichiers trouvés (toutes surfaces): %d\n', numel(allFiles));

%% === EXTRACTION + ALT PARAMS (36 colonnes) ===
X        = nan(numel(allFiles), nCols);
IDs      = strings(numel(allFiles),1);
AgeGrp   = strings(numel(allFiles),1);
Surface  = strings(numel(allFiles),1);

% QC cycles
nCyclesL_all = nan(numel(allFiles),1);
nCyclesR_all = nan(numel(allFiles),1);

for iF = 1:numel(allFiles)
    f = allFiles(iF).name;

    % attendu: "<ID>_<Surface>.mat"
    tok = regexp(f, '^(.*)_(Plat|Medium|High)\.mat$', 'tokens', 'once');
    if isempty(tok)
        warning('Nom inattendu (%s) -> skip', f);
        continue;
    end
    pid   = string(tok{1});
    surf  = string(tok{2});

    IDs(iF)     = pid;
    Surface(iF) = surf;

    AgeGrp(iF) = "NA";
    if isKey(groupMap, char(pid))
        AgeGrp(iF) = string(groupMap(char(pid)));
    end

    S = load(fullfile(mat_dir,f),'c');
    if ~isfield(S,'c') || ~isfield(S.c,'resultsAll') || ~isfield(S.c.resultsAll,'kin')
        warning('c.resultsAll.kin absent dans %s -> skip', f);
        continue;
    end
    kin = S.c.resultsAll.kin;

    if ~isfield(kin,'Left') || ~isfield(kin,'Right')
        warning('kin.Left/Right absent dans %s -> skip', f);
        continue;
    end

    L = kin.Left;
    R = kin.Right;

    seriesL = build_gvi_series_from_struct(L);
    seriesR = build_gvi_series_from_struct(R);

    nL = series_length(seriesL);
    nR = series_length(seriesR);
    nCyclesL_all(iF) = nL;
    nCyclesR_all(iF) = nR;

    if max([nL nR]) < 5
        warning('Pas assez de cycles (L=%d,R=%d) pour %s -> skip', nL, nR, f);
        continue;
    end

    altL = nan(1,nAlt);
    altR = nan(1,nAlt);

    for p = 1:nParam
        name = paramList{p};

        if isfield(seriesL,name)
            [mD,sD] = alternative_params(seriesL.(name));
            altL(2*(p-1)+1) = mD;
            altL(2*(p-1)+2) = sD;
        end
        if isfield(seriesR,name)
            [mD,sD] = alternative_params(seriesR.(name));
            altR(2*(p-1)+1) = mD;
            altR(2*(p-1)+2) = sD;
        end
    end

    X(iF,:) = [altL altR];
end

%% === FILTRAGE VALIDITÉ ===
minValidTotal = 24;  % ajustable (ex: 24 = 2/3 des colonnes)
valid = (IDs~="") & (Surface~="") & sum(~isnan(X),2) >= minValidTotal;

X      = X(valid,:);
IDs    = IDs(valid);
AgeGrp = AgeGrp(valid);
Surface = Surface(valid);
nCyclesL_all = nCyclesL_all(valid);
nCyclesR_all = nCyclesR_all(valid);

fprintf('Lignes valides (participant×surface): %d\n', numel(IDs));

%% === RÉFÉRENCE = Adultes Plat ===
idxRef = (AgeGrp=="Adultes") & (Surface=="Plat") & ismember(IDs, refAdults);
nRef = sum(idxRef);
fprintf(['Adultes référence (Plat): %d lignes\n'], nRef);
if nRef < 8
    warning('Référence Adultes faible (n=%d) -> prudence.', nRef);
end

%% === SCORE s, DISTANCE, LOG, NORMALISATION, GVI ===

X_L = X(:,1:18);
X_R = X(:,19:36);

% Score s par jambe
sL = sum(X_L .* cn18, 2, 'omitnan');
sR = sum(X_R .* cn18, 2, 'omitnan');

% Fusion des deux jambes AVANT log-distance
s_mean = (sL + sR) / 2;

% Référence Adultes Plat
s_ref = mean(s_mean(idxRef), 'omitnan');

% Distance
d = abs(s_mean - s_ref);

% Log-distance
g = log(d + 1e-12);

% Normalisation basée sur Adultes Plat
g_mu = mean(g(idxRef),'omitnan');
g_sd = std(g(idxRef),0,'omitnan');

if ~isfinite(g_sd) || g_sd < 1e-12
    g_sd = 1e-12;
end

% GVI final
GVI = 100 - 10*((g - g_mu)/g_sd);

%% === QC ADULTES PLAT (doit être ~100 ± 10) ===
fprintf('QC Adultes Plat GVI: mean=%.2f | sd=%.2f\n', ...
    mean(GVI(idxRef),'omitnan'), std(GVI(idxRef),0,'omitnan'));

%% === EXPORTS ===

% --- Export individuel (inchangé) ---
Tind = table(IDs, AgeGrp, Surface, ...
    GVI, ...
    nCyclesL_all, nCyclesR_all, ...
    'VariableNames', {'Participant','AgeGroup','Surface','GVI','nCyclesLeft','nCyclesRight'});

writetable(Tind, fullfile(outdir, sprintf('GVI_AllSurfaces_Individual_%s.csv', ts)));

% --- Résumé par groupe × surface (robuste) ---
Gage  = categorical(AgeGrp);
Gsurf = categorical(Surface);

GS = findgroups(Gage, Gsurf);

N   = splitapply(@numel, GVI, GS);
mG  = splitapply(@(x) mean(x,'omitnan'), GVI, GS);
sdG = splitapply(@(x) std(x,0,'omitnan'), GVI, GS);

mL  = splitapply(@(x) mean(x,'omitnan'), nCyclesL_all, GS);
mR  = splitapply(@(x) mean(x,'omitnan'), nCyclesR_all, GS);

% récupérer 1 label AgeGroup/Surface par groupe
agLab = splitapply(@(x) x(1), Gage,  GS);
sfLab = splitapply(@(x) x(1), Gsurf, GS);

Gage  = categorical(AgeGrp, {'JeunesEnfants','Enfants','Adolescents','Adultes'}, 'Ordinal', true);
Gsurf = categorical(Surface, {'Plat','Medium','High'}, 'Ordinal', true);

Tsum = table(string(agLab), string(sfLab), N, mG, sdG, mL, mR, ...
    'VariableNames', {'AgeGroup','Surface','N','GVI_Mean','GVI_SD','Mean_nCyclesLeft','Mean_nCyclesRight'});

writetable(Tsum, fullfile(outdir, sprintf('GVI_AllSurfaces_ByGroup_%s.csv', ts)));

%% === FIGURE UNIQUE : 3 boxplots par groupe (Plat/Medium/High) ===
fig = figure('Color','w'); hold on;

% Ordre souhaité groupes
ageOrder = {'JeunesEnfants','Enfants','Adolescents','Adultes'};
Gage = categorical(AgeGrp, ageOrder, 'Ordinal', true);

% Ordre surfaces
surfOrder = {'Plat','Medium','High'};
Gsurf = categorical(Surface, surfOrder, 'Ordinal', true);

% Ne garder que les lignes valides
idxP = ~isnan(GVI) & (Gage~='') & (Gsurf~='');
Y = GVI(idxP);
Gage = Gage(idxP);
Gsurf = Gsurf(idxP);

% Décalages horizontaux (3 boxplots côte à côte)
offsetMap = containers.Map(surfOrder, [-0.25, 0, +0.25]);
x = double(Gage);
xoff = zeros(size(x));
for i = 1:numel(surfOrder)
    ii = (Gsurf == surfOrder{i});
    xoff(ii) = offsetMap(surfOrder{i});
end
xplot = x + xoff;

% Couleurs (RGB) : bleu/vert/rouge
colPlat   = [0.00 0.45 0.74];
colMedium = [0.47 0.67 0.19];
colHigh   = [0.85 0.33 0.10];

% Tracer 3 boxcharts (un par surface) pour avoir la couleur par surface
for i = 1:numel(surfOrder)
    sf = surfOrder{i};
    ii = (Gsurf == sf);

    bc = boxchart(xplot(ii), Y(ii), ...
        'BoxWidth', 0.22, ...
        'MarkerStyle', '.', ...
        'MarkerColor', [0 0 0], ...
        'WhiskerLineColor', [0 0 0], ...
        'LineWidth', 1.0);

    switch sf
        case 'Plat'
            bc.BoxFaceColor = colPlat;
        case 'Medium'
            bc.BoxFaceColor = colMedium;
        case 'High'
            bc.BoxFaceColor = colHigh;
    end
end

% Mise en forme axes
set(gca,'XTick',1:numel(ageOrder),'XTickLabel',ageOrder,'FontSize',11);
xlim([0.5 numel(ageOrder)+0.5]);
ylabel('GVI');
xlabel('AgeGroup');
grid on; box on;

% Ligne 100
yline(100,'--','LineWidth',1.2);

% Légende (patchs factices)
h1 = plot(nan,nan,'s','MarkerFaceColor',colPlat,'MarkerEdgeColor','k');
h2 = plot(nan,nan,'s','MarkerFaceColor',colMedium,'MarkerEdgeColor','k');
h3 = plot(nan,nan,'s','MarkerFaceColor',colHigh,'MarkerEdgeColor','k');
legend([h1 h2 h3], {'Plat','Medium','High'}, 'Location','best');

title('GVI — Référence = Adultes Plat (100) — 3 surfaces par groupe');

exportgraphics(fig, fullfile(outdir, sprintf('GVI_Box_AllSurfaces_%s.png', ts)), 'Resolution', 300);
close(fig);

%% === FIGURES : boxplot + points individuels (1 figure par surface) ===
order = {'JeunesEnfants','Enfants','Adolescents','Adultes'};

for srf = 1:numel(surfaces)
    sf = string(surfaces{srf});
    idxS = (Surface==sf) & ~isnan(GVI);

    if ~any(idxS)
        warning('Aucune donnée valide pour la surface %s', sf);
        continue;
    end

    G = categorical(AgeGrp(idxS), order, 'Ordinal', true);
    Y = GVI(idxS);

    fig = figure('Color','w'); hold on;

    % Boxplot (sans outliers, car points individuels)
    boxplot(Y, G, 'Colors','k', 'Symbol','', 'Widths', 0.6);

    % Ligne 100
    yline(100,'--r','LineWidth',1.5);

    % Points individuels (jitter)
    jitterWidth = 0.18;
    groups = categories(G);

    for i = 1:numel(groups)
        ii = (G == groups{i}) & ~isnan(Y);
        n = sum(ii);
        if n==0, continue; end
        x = i + (rand(n,1)-0.5)*jitterWidth;
        scatter(x, Y(ii), 32, 'k', 'filled', ...
            'MarkerFaceAlpha', 0.55, 'MarkerEdgeAlpha', 0.55);
    end

    grid on; box on;
    set(gca,'FontSize',11);
    xlabel('AgeGroup');
    ylabel('GVI');
    title(sprintf('GVI (%s) — Référence = Adultes Plat (100)', sf));
    xlim([0.5 numel(groups)+0.5]);

    exportgraphics(fig, fullfile(outdir, sprintf('GVI_Box_%s_%s.png', sf, ts)), 'Resolution', 300);
    close(fig);
end

fprintf('\n✅ Terminé. Résultats dans: %s\n', outdir);

%% ====================== FONCTIONS LOCALES ======================

function series = build_gvi_series_from_struct(S)
% S = kin.Left ou kin.Right
% S est un struct array 1×N, chaque élément = 1 cycle.

    series = struct();

    % paramètres directement présents (un scalaire par cycle)
    series.StepLength     = getvec_cycles(S, {'distPas'});
    series.StrideLength   = getvec_cycles(S, {'distFoulee'});
    series.StrideTime     = getvec_cycles(S, {'tempsFoulee'});     % s
    series.StrideVelocity = getvec_cycles(S, {'vitFoulee'});       % m/s

    % cadence steps/min -> StepTime = 60/cadence
    cad = getvec_cycles(S, {'vitCadencePasParMin','vitCadencePasParMinute'});
    if ~isempty(cad) && ~all(isnan(cad))
        st = 60 ./ cad;
        st(~isfinite(st)) = NaN;
        series.StepTime = st(:);
    else
        series.StepTime = nan(size(series.StrideTime));
    end

    % pctToeOff (%) -> StanceTime = StrideTime * pctToeOff/100
    pctTO = getvec_cycles(S, {'pctToeOff'});
    stance = nan(size(series.StrideTime));
    if ~isempty(pctTO) && ~all(isnan(pctTO)) && ~all(isnan(series.StrideTime))
        stance = series.StrideTime .* (pctTO/100);
    end
    series.StanceTime = stance(:);

    % SwingTime = StrideTime - StanceTime
    series.SwingTime = series.StrideTime - series.StanceTime;

    % pctSimpleAppuie (%) -> SingleSupportTime
    pctSS = getvec_cycles(S, {'pctSimpleAppuie'});
    single = nan(size(series.StrideTime));
    if ~isempty(pctSS) && ~all(isnan(pctSS)) && ~all(isnan(series.StrideTime))
        single = series.StrideTime .* (pctSS/100);
    end
    series.SingleSupportTime = single(:);

    % DoubleSupportTime = Stance - SingleSupport
    series.DoubleSupportTime = series.StanceTime - series.SingleSupportTime;

    % nettoyage
    f = fieldnames(series);
    for k = 1:numel(f)
        v = series.(f{k});
        v = v(:);
        v(~isfinite(v)) = NaN;
        series.(f{k}) = v;
    end

    series.SwingTime(series.SwingTime < 0) = NaN;
    series.DoubleSupportTime(series.DoubleSupportTime < 0) = NaN;
    series.StepTime(series.StepTime < 0) = NaN;
    series.StrideTime(series.StrideTime < 0) = NaN;
end

function v = getvec_cycles(S, names)
% Retourne un vecteur colonne (N×1) : 1 valeur = 1 cycle
    v = [];

    for i = 1:numel(names)
        nm = names{i};
        if ~isfield(S, nm)
            continue;
        end

        % CAS 1 : struct array (1×N cycles)
        if isstruct(S) && numel(S) > 1
            try
                x = [S.(nm)];      % 1×N
            catch
                x = [];
            end

            if isempty(x) || ~isnumeric(x)
                v = nan(numel(S),1);
                return;
            end

            v = x(:);             % N×1
            return;
        end

        % CAS 2 : struct scalaire avec vecteur déjà stocké
        x = S.(nm);
        if isnumeric(x)
            if isvector(x)
                v = x(:);
            else
                v = nan(estimate_n_cycles(S),1);
            end
            return;
        else
            v = nan(estimate_n_cycles(S),1);
            return;
        end
    end

    % si aucun champ trouvé
    n = estimate_n_cycles(S);
    v = nan(n,1);
end

function n = estimate_n_cycles(S)
% Estime n cycles

    if isstruct(S) && numel(S) > 1
        n = numel(S);
        return;
    end

    n = 0;
    if ~isstruct(S); return; end

    f = fieldnames(S);
    for k = 1:numel(f)
        x = S.(f{k});
        if isnumeric(x) && isvector(x) && ~isempty(x)
            n = max(n, numel(x));
        end
    end

    if n == 0
        n = 1;
    end
end

function n = series_length(series)
% Retourne la longueur max des séries disponibles
    n = 0;
    f = fieldnames(series);
    for k = 1:numel(f)
        n = max(n, numel(series.(f{k})));
    end
end

function [meanDiff, sdDiff] = alternative_params(values)
% Paramètres alternatifs (Gouelle 2013):
% - normaliser la série par sa moyenne -> *100
% - calculer abs(diff) cycle-à-cycle
% - retourner mean(absDiff) et sd(absDiff)

    values = values(:);
    values = values(~isnan(values));

    if numel(values) < 5
        meanDiff = NaN; sdDiff = NaN; return;
    end

    m = mean(values);
    if ~isfinite(m) || abs(m) < 1e-12
        meanDiff = NaN; sdDiff = NaN; return;
    end

    v  = (values./m) * 100;
    dv = abs(diff(v));

    meanDiff = mean(dv, 'omitnan');
    sdDiff   = std(dv, 0, 'omitnan');

    if ~isfinite(sdDiff) || sdDiff < 1e-12
        sdDiff = 1e-12;
    end
end
