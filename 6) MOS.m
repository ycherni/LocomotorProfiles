%% === SCRIPT PRINCIPAL : Calcul des Marges de Stabilité (MoS) ===
% Ce script calcule les MoS à partir de fichiers C3D
% Adapté pour utiliser les fonctions d'extraction d'événements existantes

clear; clc; close all;

cd('XX') % Chemin vers map longueur de jambes
addpath(genpath('XX')); % BTK
addpath(genpath('XX')); % fonctions

%% === PARAMÈTRES À MODIFIER ===
sujet_id = 'XX';                              % ID du sujet à traiter
surfaces = {'Plat', 'Medium', 'High'};            % Surfaces étudiées
essais = 1:10;                                    % Numéros des essais
base_dir = 'XX';    % Dossier contenant les C3D
output_file_csv = sprintf('\\MoS_results_%s.csv', sujet_id);
output_file_mat = sprintf('\\MoS_results_%s.mat', sujet_id);

% Fréquence d'acquisition
freqVicon = 100;  % Fréquence Vicon (Hz)

% === Longueur de jambe L0 (pour normalisation) ===
load('l0_participants.mat', 'l0_map');   % structure containers.Map : key=participant, value=L0 (en m)
participant = sujet_id;

if isKey(l0_map, participant)
    l0 = l0_map(participant);
    L0_mm = l0 * 1000; % Passer de mètre à mm pour rester OK avec ce que calcule la MOS
    fprintf('Participant %s avec une L0 = %.1f mm\n', participant, L0_mm);
else
    error('Participant %s non trouvé dans l0_map.', participant); % <-- pas de "continue" ici
end

% === INITIALISATION DU TABLEAU DE RÉSULTATS ===
results = table();

% === BOUCLE DE TRAITEMENT ===
fprintf('🔄 Traitement du sujet %s...\n\n', sujet_id);

for surf_idx = 1:length(surfaces)
    surface = surfaces{surf_idx};
    
    for essai = essais
        % Construction du nom de fichier
        filename = sprintf('%s_%s_%02d.c3d', sujet_id, surface, essai);
        c3d_path = fullfile(base_dir, filename);
        
        % Vérification de l'existence du fichier
        if ~isfile(c3d_path)
            fprintf('Fichier non trouvé : %s\n', filename);
            continue;
        end
        
        fprintf('Traitement : %s\n', filename);
        
        try
            % Lecture du fichier C3D
            data = btkReadAcquisition(c3d_path);
            markers = btkGetMarkers(data);
            
            % Extraction directe des événements depuis Vicon
            fsrt_frame = btkGetFirstFrame(data);
            moments = btkGetEvents(data);
            
            % Extraction des frames d'événements
            % Left Heel Strike
            if isfield(moments, 'Left_Foot_Strike')
                Left_HS_frames = round(moments.Left_Foot_Strike * freqVicon - fsrt_frame + 1);
            else
                Left_HS_frames = [];
            end
            
            % Left Toe Off
            if isfield(moments, 'Left_Foot_Off')
                Left_TO_frames = round(moments.Left_Foot_Off * freqVicon - fsrt_frame + 1);
            else
                Left_TO_frames = [];
            end
            
            % Right Heel Strike
            if isfield(moments, 'Right_Foot_Strike')
                Right_HS_frames = round(moments.Right_Foot_Strike * freqVicon - fsrt_frame + 1);
            else
                Right_HS_frames = [];
            end
            
            % Right Toe Off
            if isfield(moments, 'Right_Foot_Off')
                Right_TO_frames = round(moments.Right_Foot_Off * freqVicon - fsrt_frame + 1);
            else
                Right_TO_frames = [];
            end
            
% CLIP DE SÉCURITÉ SUR LES INDICES (p.ex : si HS = 0)
fnm = fieldnames(markers);
N   = size(markers.(fnm{1}), 1);      % nb de frames dans les marqueurs
clip = @(v) max(1, min(v, N-1));      % borne à [1, N-1] (xCOM = N-1)

Left_HS_frames  = clip(Left_HS_frames);
Left_TO_frames  = clip(Left_TO_frames);
Right_HS_frames = clip(Right_HS_frames);
Right_TO_frames = clip(Right_TO_frames);

% Affichage des frame de chaque events
fprintf('   Événements bruts:\n');
fprintf('   Left HS: %s\n', mat2str(Left_HS_frames));
fprintf('   Left TO: %s\n', mat2str(Left_TO_frames));
fprintf('   Right HS: %s\n', mat2str(Right_HS_frames));
fprintf('   Right TO: %s\n', mat2str(Right_TO_frames));

% Traitement de chaque cycle de marche
% JAMBE GAUCHE
for i = 1:length(Left_HS_frames)
    
    hs = Left_HS_frames(i);

% Trouver le TO suivant ce HS
valid_TO = Left_TO_frames(Left_TO_frames > hs);
if isempty(valid_TO), continue; end
to = valid_TO(1);

% Estimer HO sur (hs,to)
ho = estimate_heel_off_pair(hs, to, markers, 'L');

% Vérification de cohérence
if ho <= hs || to <= ho
    fprintf('   Cycle L%d ignoré (ordre incohérent: HS=%d, HO=%d, TO=%d)\n', i, hs, ho, to);
    continue;
end
    
    fprintf('   Cycle Gauche %d : HS=%d, HO=%d, TO=%d\n', i, hs, ho, to);
    
    try
        mos = calculate_MoS(c3d_path, hs, ho, to, L0_mm);

%         === DIAGNOSTIQUES VISUELS (à laisser ON le temps du debug) ===
% try
%     plot_mos_diagnostics(c3d_path, hs, ho, to, freqVicon);
% catch ME_d
%     warning('Diagnostics MoS non affichés (%s).', ME_d.message);
% end

        
        % Ajout des résultats dans le tableau
        new_row = struct();
        new_row.Sujet = {sujet_id};
        new_row.Surface = {surface};
        new_row.Essai = essai;
        new_row.Cycle = i;
        new_row.Cote = {'Gauche'};
        new_row.HS_Frame = hs;
        new_row.HO_Frame = ho;
        new_row.TO_Frame = to;
        
        % Ajout des MoS
        fields = fieldnames(mos);
        for f = 1:length(fields)
            new_row.(fields{f}) = mos.(fields{f});
        end
        
        results = [results; struct2table(new_row)];
    catch ME
        fprintf('   Erreur cycle L%d : %s\n', i, ME.message);
    end
end

% JAMBE DROITE
for i = 1:length(Right_HS_frames)
    hs = Right_HS_frames(i);

valid_TO = Right_TO_frames(Right_TO_frames > hs);
if isempty(valid_TO), continue; end
to = valid_TO(1);

ho = estimate_heel_off_pair(hs, to, markers, 'R');

if ho <= hs || to <= ho
    fprintf('   Cycle R%d ignoré (ordre incohérent: HS=%d, HO=%d, TO=%d)\n', i, hs, ho, to);
    continue;
end
    
    fprintf('   Cycle Droit %d : HS=%d, HO=%d, TO=%d\n', i, hs, ho, to);
    
    try
        mos = calculate_MoS(c3d_path, hs, ho, to, L0_mm);

        % Ajout des résultats dans le tableau
        new_row = struct();
        new_row.Sujet = {sujet_id};
        new_row.Surface = {surface};
        new_row.Essai = essai;
        new_row.Cycle = i;
        new_row.Cote = {'Droit'};
        new_row.HS_Frame = hs;
        new_row.HO_Frame = ho;
        new_row.TO_Frame = to;
        
        % Ajout des MoS
        fields = fieldnames(mos);
        for f = 1:length(fields)
            new_row.(fields{f}) = mos.(fields{f});
        end
        
        results = [results; struct2table(new_row)];
    catch ME
        fprintf('    Erreur cycle R%d : %s\n', i, ME.message);
    end
end
            
            btkCloseAcquisition(data);
            fprintf('    Fichier traité avec succès\n\n');
            
        catch ME
            fprintf(' Erreur fichier %s : %s\n\n', filename, ME.message);
        end
    end
end

% === SAUVEGARDE ===
if height(results) > 0
    fprintf(' Sauvegarde des résultats...\n');
    
    % Sauvegarde CSV
    writetable(results, output_file_csv);
    fprintf(' CSV généré : %s\n', output_file_csv);
    
    % Sauvegarde MAT
    MoS_data = struct();
    MoS_data.results = results;           % Table complète
    MoS_data.sujet = sujet_id;            % ID du sujet
    MoS_data.surfaces = surfaces;         % Surfaces testées
    MoS_data.essais = essais;             % Essais testés
    MoS_data.date_traitement = datetime('now');  % Date du traitement
    MoS_data.freqVicon = freqVicon;       % Fréquence d'acquisition
    
    save(output_file_mat, 'MoS_data');
    fprintf(' MAT généré : %s\n', output_file_mat);
    
    fprintf(' Total : %d cycles traités\n', height(results));
else
    fprintf('  Aucun résultat à sauvegarder\n');
end

fprintf('\n PROCHAINE ÉTAPE : Lancer SPARC_LDLJ.m\n');

%% ========== FONCTIONS ==========

function ho = estimate_heel_off_pair(hs, to, markers, side)

heel_marker = [side 'HEE'];

% Sécurité
if ~isfield(markers, heel_marker) || ~(to > hs)
    ho = hs + 1; 
    return
end

% --- Extraction du marqueur talon ---
heel_z = markers.(heel_marker)(hs:to, 3);

% --- Gestion des valeurs manquantes ---
heel_z = fillmissing(heel_z, 'linear', 'EndValues', 'nearest');

% --- Lissage zéro-phase (Butterworth 6 Hz) pour éviter tout retard (zéro-lag) ---
fs = 100;                                  % fréquence d’échantillonnage Vicon
[bb, aa] = butter(2, 6/(fs/2), 'low');     % filtre passe-bas (ordre 2, fc=6 Hz)
if numel(heel_z) > 6                       % sécurité si assez d’échantillons
    heel_z = filtfilt(bb, aa, heel_z);     % filtrage zéro-phase
end

% --- Calcul de la vitesse verticale (mm/frame) ---
heel_vel_z = diff(heel_z);

stanceLen = to - hs;
if stanceLen < 15                          % stance trop court -> fallback
    ho = round(hs + 0.60*stanceLen);
    return
end

absMin = 12;                                % ≈120 ms @100 Hz
lo     = hs + max(round(0.35*stanceLen), absMin);
hi     = hs + round(0.90*stanceLen);

from_idx = max(1, lo - hs);
if from_idx > numel(heel_vel_z)
    ho = min(max(round(hs + 0.60*stanceLen), lo), hi);
    return
end

vz_seg = heel_vel_z(from_idx:end);
thr    = mean(vz_seg,'omitnan') + 0.8*std(vz_seg,0,'omitnan');

rel = find(vz_seg > thr, 1, 'first');
if isempty(rel)
    ho = round(hs + 0.60*stanceLen);       % fallback s’il n’y a pas de pic
else
    ho = hs + (from_idx - 1) + rel;        % index absolu
end

ho = min(max(ho, lo), hi);                 % clamp final [35%, 90%], ≥ hs+12
end

function positions = extract_marker_positions(file_path)
    % Extrait les positions des marqueurs depuis un fichier C3D
    c3d_data = btkReadAcquisition(file_path);
    markers = btkGetMarkers(c3d_data);
    marker_names = fieldnames(markers);
    
    positions = struct();
    for i = 1:length(marker_names)
        name = marker_names{i};
        positions.(name) = markers.(name);
    end
    
    btkCloseAcquisition(c3d_data);
end

function marker_names = get_marker_names(file_path)
    % Récupère la liste des noms de marqueurs
    c3d_data = btkReadAcquisition(file_path);
    markers = btkGetMarkers(c3d_data);
    marker_names = fieldnames(markers);
    btkCloseAcquisition(c3d_data);
end

function mos = calculate_MoS(file_path, heel_strike_frame, heel_off_frame, toe_off_frame, L0_mm)
    % Calcule les marges de stabilité (MoS)
    
    positions = extract_marker_positions(file_path);
    marker_names = get_marker_names(file_path);

    % GARDE INDICES (cohérent avec xCOM de longueur N-1)
    N = size(positions.LHEE,1);
    heel_strike_frame = min(max(heel_strike_frame,1), N-1);
    heel_off_frame    = min(max(heel_off_frame,1),    N-1);
    toe_off_frame     = min(max(toe_off_frame,1),     N-1);

    % Remplacement RM5 par RM51 si nécessaire
    if ~any(strcmp(marker_names, 'RM5')) && any(strcmp(marker_names, 'RM51'))
        positions.RM5 = positions.RM51;
    end
    if ~any(strcmp(marker_names, 'LM5')) && any(strcmp(marker_names, 'LM51'))
        positions.LM5 = positions.LM51;
    end
    
% Filtrage Butterworth 6 Hz (zéro phase)
fs = 100;              % <-- adapte/paramètre si nécessaire (FreqVicon)
fc = 6;                % coupure à 6 Hz
[b,a] = butter(2, fc/(fs/2), 'low');

% Filtrer toutes les trajectoires présentes dans 'positions'
fns = fieldnames(positions);
for ii = 1:numel(fns)
    M = positions.(fns{ii});
    if isnumeric(M) && size(M,1) >= 3*max(length(a),length(b))   % sécurité filtfilt
        positions.(fns{ii}) = filtfilt(b, a, M);
    end
end

    % Détermination du côté d'appui
    left_heel_z = positions.LHEE(heel_strike_frame, 3);
    right_heel_z = positions.RHEE(heel_strike_frame, 3);
    
    if left_heel_z < right_heel_z
        side = 'L';
        oppside = 'R';
    else
        side = 'R';
        oppside = 'L';
    end
    
    % Calcul du centre de masse (COM)
    COM_x = mean([positions.LPSI(:,1), positions.RPSI(:,1), ...
                  positions.LASI(:,1), positions.RASI(:,1)], 2);
    COM_y = mean([positions.LPSI(:,2), positions.RPSI(:,2), ...
                  positions.LASI(:,2), positions.RASI(:,2)], 2);
    COM_z = mean([positions.LPSI(:,3), positions.RPSI(:,3), ...
                  positions.LASI(:,3), positions.RASI(:,3)], 2);
    
    % Calcul des vitesses (en mm/s)
    velocities_x = diff(COM_x) * fs; 
    velocities_y = diff(COM_y) * fs;
    
    % Calcul de xCOM (centre de masse extrapolé)
    g = 9810;  % mm/s²
    ankle_marker = [side, 'ANK'];
    l_z = abs(positions.(ankle_marker)(1:end-1, 3) - COM_z(1:end-1));
    k = sqrt(g ./ (l_z + 1e-6));
    
    xCOM_x = COM_x(1:end-1) + velocities_x ./ k;
    xCOM_y = COM_y(1:end-1) + velocities_y ./ k;
    
    % Direction médio-latérale
    m5_marker_side = [side, 'M5'];
    m5_marker_opp = [oppside, 'M5'];
    
    if (positions.(m5_marker_side)(1, 1) - positions.(m5_marker_opp)(1, 1)) < 0
        directionML = -1;
    else
        directionML = 1;
    end
    
    % Extraction des positions des marqueurs
    heel_marker = [side, 'HEE'];
    toe_marker = [side, 'TOE'];
    
    HEEL_y = positions.(heel_marker)(:, 2);
    TOE_y = positions.(toe_marker)(:, 2);
    ANKLE_x = positions.(ankle_marker)(:, 1);
    M5_x = positions.(m5_marker_side)(:, 1);
    
    % Calcul MoS antéro-postérieur (AP)
    if HEEL_y(toe_off_frame) - HEEL_y(heel_strike_frame) > 0
        directionAP = 1;
    else
        directionAP = -1;
    end

    MoS_AP_heel = (HEEL_y(heel_strike_frame:heel_off_frame) - ...
               xCOM_y(heel_strike_frame:heel_off_frame)) * directionAP;
    MoS_AP_toe  = (TOE_y(heel_off_frame:toe_off_frame) - ...
               xCOM_y(heel_off_frame:toe_off_frame)) * directionAP;
    MoS_AP_Mean = mean([mean(MoS_AP_heel), mean(MoS_AP_toe)]);
    
    % Calcul MoS médio-latéral (ML)
    MoS_ML_ankle = (ANKLE_x(heel_strike_frame:heel_off_frame) - ...
                    xCOM_x(heel_strike_frame:heel_off_frame)) * directionML;
    MoS_ML_m5 = (M5_x(heel_off_frame:toe_off_frame) - ...
                 xCOM_x(heel_off_frame:toe_off_frame)) * directionML;
    mos_ml_total = [MoS_ML_ankle; MoS_ML_m5];
    
    % Création de la structure de résultats
    mos = struct();
    mos.MoS_AP_Heel_Min = min(MoS_AP_heel);
    mos.MoS_AP_Heel_Max = max(MoS_AP_heel);
    mos.MoS_AP_Heel_Mean = mean(MoS_AP_heel);
    mos.MoS_AP_Heel_SD = std(MoS_AP_heel);
    
    mos.MoS_AP_Toe_Min = min(MoS_AP_toe);
    mos.MoS_AP_Toe_Max = max(MoS_AP_toe);
    mos.MoS_AP_Toe_Mean = mean(MoS_AP_toe);
    mos.MoS_AP_Toe_SD = std(MoS_AP_toe);
    mos.MoS_AP_Mean = MoS_AP_Mean;
    
    mos.MoS_ML_Ankle_Min = min(MoS_ML_ankle);
    mos.MoS_ML_Ankle_Max = max(MoS_ML_ankle);
    mos.MoS_ML_Ankle_Mean = mean(MoS_ML_ankle);
    mos.MoS_ML_Ankle_SD = std(MoS_ML_ankle);
    
    mos.MoS_ML_M5_Min = min(MoS_ML_m5);
    mos.MoS_ML_M5_Max = max(MoS_ML_m5);
    mos.MoS_ML_M5_Mean = mean(MoS_ML_m5);
    mos.MoS_ML_M5_SD = std(MoS_ML_m5);
    
    mos.MoS_ML_Min = min(mos_ml_total);
    mos.MoS_ML_Max = max(mos_ml_total);
    mos.MoS_ML_Mean = mean(mos_ml_total);
    mos.MoS_ML_SD = std(mos_ml_total);
    
    mos.MoS_Heel_Strike_AP = MoS_AP_heel(1);
    mos.MoS_Heel_Strike_ML = mos_ml_total(1);

    % === Ajout %L0 (normalisation légère sur tous les SCALAIRES bruts en mm) ===
    if exist('L0_mm','var') && isnumeric(L0_mm) && numel(L0_mm)==1 && isfinite(L0_mm) && L0_mm>0
        mos.LegLength_mm = L0_mm;  % méta
        fns = fieldnames(mos);
        for i = 1:numel(fns)
            fn = fns{i};
            if endsWith(fn, '_P') || strcmp(fn, 'LegLength_mm'), continue; end
            val = mos.(fn);
            if isnumeric(val) && isscalar(val)
                mos.([fn '_P']) = 100 * (val / L0_mm);  % %L0
            end
        end
    end
end

function plot_mos_diagnostics(file_path, hs, ho, to, fs)
% Visualise COM/xCOM autour du HS, la cinématique talon et la MoS_AP,
% en REPRODUISANT le même pipeline que la détection de HO :
% - lissage movmean(5) sur heel_z
% - vitesse en mm/frame (pas de *fs)
% - fenêtre de recherche: lo = max(HS+12, HS+35% stance), hi = HS+90% stance
% - seuil: mean(vz_seg) + 0.8*std(vz_seg) sur [lo..TO]

% ---------- Lecture & pré-traitement ----------
acq     = btkReadAcquisition(file_path);
markers = btkGetMarkers(acq);
btkCloseAcquisition(acq);

% Harmonisation M5 (si besoin)
if ~isfield(markers,'RM5') && isfield(markers,'RM51'); markers.RM5 = markers.RM51; end
if ~isfield(markers,'LM5') && isfield(markers,'LM51'); markers.LM5 = markers.LM51; end

% Filtrage doux pour COM (n'influence pas la détection HO)
fc = 6;  [b,a] = butter(2, fc/(fs/2), 'low');
fn = fieldnames(markers);
for k = 1:numel(fn)
    M = markers.(fn{k});
    if isnumeric(M) && size(M,1) > 3*max(length(a),length(b))
        markers.(fn{k}) = filtfilt(b,a,M);
    end
end

% Bornes de sécurité
N   = size(markers.(fn{1}),1);
clip = @(v) max(1, min(v, N));
hs = clip(hs); ho = clip(ho); to = clip(to);
if ~(hs < ho && ho < to), warning('Ordre HS<HO<TO non respecté.'); end

% ---------- Côté d’appui au HS ----------
Lz = markers.LHEE(hs,3); Rz = markers.RHEE(hs,3);
if Lz < Rz, side='L'; opp='R'; else, side='R'; opp='L'; end
HEEL = [side 'HEE']; TOE = [side 'TOE']; ANK = [side 'ANK'];

% ---------- COM & xCOM ----------
COMx = mean([markers.LPSI(:,1), markers.RPSI(:,1), markers.LASI(:,1), markers.RASI(:,1)],2);
COMy = mean([markers.LPSI(:,2), markers.RPSI(:,2), markers.LASI(:,2), markers.RASI(:,2)],2);
COMz = mean([markers.LPSI(:,3), markers.RPSI(:,3), markers.LASI(:,3), markers.RASI(:,3)],2);

vx = [diff(COMx); 0] * fs;   % mm/s (affichage)
vy = [diff(COMy); 0] * fs;

g  = 9810; % mm/s^2
Lz_inst = abs(markers.(ANK)(:,3) - COMz);
k  = sqrt(g ./ max(Lz_inst,1e-6));

xCOMx = COMx + vx ./ k;
xCOMy = COMy + vy ./ k;

% ---------- Fenêtres ----------
w_pre  = round(0.08*fs);
w_post = round(0.25*fs);
idxA   = clip(hs-w_pre) : clip(hs+w_post);

% ---------- Talon : même pipeline que la détection ----------
heel_z_raw = markers.(HEEL)(hs:to,3);

% lissage identique (movmean 5)
if numel(heel_z_raw) >= 5
    heel_z = smoothdata(heel_z_raw, 'movmean', 5);
else
    heel_z = heel_z_raw;
end

% vitesse en mm/frame (PAS de *fs ici)
vz = diff(heel_z);

% fenêtre de recherche (identique)
stanceLen = to - hs;
absMin = 12;                                 % frames
lo     = hs + max(round(0.35*stanceLen), absMin);
hi     = hs + round(0.90*stanceLen);

from_idx = max(1, lo - hs);                  % index relatif dans vz
vz_seg   = vz(from_idx:end);

% seuil identique
thr = mean(vz_seg,'omitnan') + 0.8*std(vz_seg,0,'omitnan');

% ---------- MoS_AP (éviter le doublon au HO) ----------
MoS_AP_heel = markers.(HEEL)(hs:ho,2)  - xCOMy(hs:ho);
MoS_AP_toe  = markers.(TOE)(ho+1:to,2) - xCOMy(ho+1:to);
MoS_AP_full = [MoS_AP_heel; MoS_AP_toe];
x_stance    = hs:to;

% ---------- Figures ----------
fig = figure('Name',sprintf('Diagnostics MoS - %s [%s]', get_file_name(file_path), side),...
             'Color','w','Units','normalized','Position',[0.1 0.1 0.8 0.75]);

% (1) COM_y & xCOM_y autour du HS
subplot(3,1,1); hold on; grid on
plot(idxA, COMy(idxA), 'LineWidth',1.5);
plot(idxA, xCOMy(idxA), 'LineWidth',1.5);
xline(hs, 'k--', 'HS'); xline(ho, 'm--', 'HO'); xline(to, 'r--', 'TO');
title('COM_y vs xCOM_y (autour du HS)');
xlabel('Frame'); ylabel('mm');
legend({'COM_y','xCOM_y','HS','HO','TO'}, 'Location','best');

% (2) Talon Z (lissé) & v_z (mm/frame) + seuil et fenêtre
subplot(3,1,2); hold on; grid on
% position talon
plot(hs:to, heel_z, 'LineWidth',1.5); ylabel('Heel Z (mm)');
% vitesse (mm/frame)
yyaxis right
plot(hs:(to-1), vz, 'LineWidth',1.2);
yline(thr,'--','Threshold');
% bande de recherche lo..hi
yl = ylim;
patch([lo hi hi lo], [yl(1) yl(1) yl(2) yl(2)], [0.85 0.85 0.95], ...
      'FaceAlpha',0.25,'EdgeColor','none');
% marqueurs
xline(hs, 'k--', 'HS'); xline(ho, 'm--', 'HO'); xline(to, 'r--', 'TO');
title(sprintf('Talon Z (lissé) & v_z (mm/frame)  —  lo=%d, hi=%d, thr=%.2f', lo, hi, thr));
xlabel('Frame'); ylabel('v_z (mm/frame)');

% (3) MoS_AP sur la phase d’appui
subplot(3,1,3); hold on; grid on
plot(x_stance, MoS_AP_full, 'LineWidth',1.5);
xline(hs, 'k--', 'HS'); xline(ho, 'm--', 'HO'); xline(to, 'r--', 'TO');
f10 = min(10, numel(MoS_AP_heel));
if f10 > 1
    ylo = min(MoS_AP_full); yhi = max(MoS_AP_full);
    patch([hs hs+f10 hs+f10 hs],[ylo ylo yhi yhi],[0.9 0.9 0.9], ...
          'FaceAlpha',0.3, 'EdgeColor','none');
end
title('MoS_{AP} (HEEL puis TOE) — zone grisée = 10 frames post-HS');
xlabel('Frame'); ylabel('mm');
legend({'MoS_{AP}','HS','HO','TO'}, 'Location','best');

% ---------- Logs console ----------
mos_hs     = MoS_AP_heel(1);
mos_10mean = mean(MoS_AP_heel(1:max(1,f10)),'omitnan');
fprintf('[%s] %s  HS=%d, HO=%d, TO=%d | MoS_AP@HS=%.1f mm | mean(HS+10fr)=%.1f mm\n',...
    get_file_name(file_path), side, hs, ho, to, mos_hs, mos_10mean);
end

function nm = get_file_name(pathstr)
[~,nm,ext] = fileparts(pathstr);
nm = [nm ext];
end