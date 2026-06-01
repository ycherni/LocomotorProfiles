%% === CALCUL SPARC/LDLJ - COM vs STERNUM ===
% Segmentation de chaque essai du 1er au dernier HS
% Recommandation des calculs par Balasubramanian et al. 2015

clear; clc; close all;

% === CHEMINS ET PARAMÈTRES ===
addpath(genpath('C:\Users\silve\OneDrive - Universite de Montreal\Silvere De Freitas - PhD - NeuroBiomech\Scripts\btk'));    
addpath(genpath('C:\Users\silve\Desktop\DOCTORAT\UNIV MONTREAL\TRAVAUX-THESE\Surfaces_Irregulieres\Datas\Script\gaitAnalysisGUI\functions'));

sujet_id  = 'CTL_80';
surfaces  = {'Plat', 'Medium', 'High'};
essais    = 1:5; 
base_dir  = 'C:\Users\silve\Desktop\DOCTORAT\UNIV MONTREAL\TRAVAUX-THESE\Surfaces_Irregulieres\Datas\Script\gaitAnalysisGUI\Data\jeunes_enfants'; % Pensez à changer le répertoire d'âge ici!

freqVicon = 100;
fc_filter = 6;

output_csv = sprintf('C:\\Users\\silve\\Desktop\\DOCTORAT\\UNIV MONTREAL\\TRAVAUX-THESE\\Surfaces_Irregulieres\\Datas\\Script\\gaitAnalysisGUI\\result\\Smoothness\\Smoothness_TrialBased_%s.csv', sujet_id);
output_mat = sprintf('C:\\Users\\silve\\Desktop\\DOCTORAT\\UNIV MONTREAL\\TRAVAUX-THESE\\Surfaces_Irregulieres\\Datas\\Script\\gaitAnalysisGUI\\result\\Smoothness\\Smoothness_TrialBased_%s.mat', sujet_id);

% === INITIALISATION ===
results    = table();
log_errors = {};
fft_log    = table();  % journal FFT (N, K, df...) par essai
fprintf('🔄 Analyse de fluidité PAR ESSAI, du 1er au dernier HeelStrike, pour %s...\n\n', sujet_id);
fprintf('Colonne 1 : axe ML; colonne 2 : axe AP; colonne 3 : axe V\n');

% Filtre passe-bas
[b, a] = butter(2, fc_filter/(freqVicon/2), 'low');

% === BOUCLE DE TRAITEMENT ===
for surf_idx = 1:length(surfaces)
    surface = surfaces{surf_idx};
    
    for essai = essais
        filename = sprintf('%s_%s_%02d.c3d', sujet_id, surface, essai);
        c3d_path = fullfile(base_dir, filename);

        if ~isfile(c3d_path)
            msg = sprintf('❌ Fichier manquant : %s', filename);
            log_errors{end+1} = msg;
            fprintf('%s\n', msg);
            continue;
        end
        
        fprintf('📂 Traitement : %s\n', filename);
        
        try
            % Lecture C3D
            data    = btkReadAcquisition(c3d_path);
            markers = btkGetMarkers(data);

            % === COM PELVIEN ===
            try
                COM = calculate_pelvic_COM(markers);   % [N x 3]
            catch
                msg = sprintf('❌ %s : COM impossible (markers manquants ou invalides)', filename);
                log_errors{end+1} = msg;
                fprintf('%s\n', msg);
                btkCloseAcquisition(data);
                continue;
            end

            % === STERNUM ===
            if isfield(markers,'STRN')
                STERN = markers.STRN;     % [N x 3]
            else
                msg = sprintf('⚠️ %s : marqueur STRN absent, indices STERN mis à NaN', filename);
                log_errors{end+1} = msg;
                fprintf('%s\n', msg);
                STERN = NaN(size(COM));
            end

            % === FENÊTRE D'ANALYSE : du 1er HS au dernier HS ===
            n_frames   = size(COM, 1);
            fsrt_frame = btkGetFirstFrame(data);
            events     = btkGetEvents(data);

            Left_HS_frames  = [];
            Right_HS_frames = [];
            if isfield(events, 'Left_Foot_Strike')
                Left_HS_frames = round(events.Left_Foot_Strike * freqVicon - fsrt_frame + 1);
            end
            if isfield(events, 'Right_Foot_Strike')
                Right_HS_frames = round(events.Right_Foot_Strike * freqVicon - fsrt_frame + 1);
            end

            HS_all = sort([Left_HS_frames(:); Right_HS_frames(:)]);

            % Clip de sécurité dans [1, n_frames]
            HS_all = max(1, min(HS_all, n_frames));

            if numel(HS_all) >= 2
                start_frame = HS_all(1);
                end_frame   = HS_all(end);
            else
                start_frame = 1;
                end_frame   = n_frames;
            end

            if end_frame <= start_frame
                start_frame = 1;
                end_frame   = n_frames;
            end

            n_cycles = NaN;
            fprintf('   📏 Analyse entre 1er HS et dernier HS (frames %d à %d)\n', start_frame, end_frame);

            % === FILTRAGE POSITION ===
            COM_filt   = filtfilt(b, a, COM);
            STERN_filt = filtfilt(b, a, STERN);

            % === VITESSES ===
            vel_COM   = calculate_velocities(COM_filt,   freqVicon); 
            vel_STERN = calculate_velocities(STERN_filt, freqVicon);

            % === FFT SUR LA FENÊTRE 1er HS -> dernier HS (MAGNITUDE UNIQUEMENT) ===
            seg_COM     = vel_COM(start_frame:end_frame, :);
            v_mag_COM   = sqrt(sum(seg_COM.^2, 2));
            [~, ~, infoCOM] = compute_spectrum_with_info(v_mag_COM, freqVicon, 4); % mean retiré dans la fonction
            fft_log = [fft_log; make_fft_row(sujet_id, surface, essai, 'COM', start_frame, end_frame, infoCOM)];

            if ~all(isnan(vel_STERN(:)))
                seg_STERN   = vel_STERN(start_frame:end_frame, :);
                v_mag_STERN = sqrt(sum(seg_STERN.^2, 2));
                [~, ~, infoSTERN] = compute_spectrum_with_info(v_mag_STERN, freqVicon, 4); % mean retiré dans la fonction
                fft_log = [fft_log; make_fft_row(sujet_id, surface, essai, 'STERN', start_frame, end_frame, infoSTERN)];
            end

            % === INDICES DE FLUIDITÉ ===
            smooth_COM   = calculate_smoothness_indices(vel_COM,   start_frame, end_frame, freqVicon);
            smooth_STERN = calculate_smoothness_indices(vel_STERN, start_frame, end_frame, freqVicon);

            % Vérification scalaires
            fields_COM = fieldnames(smooth_COM);
            for f = 1:length(fields_COM)
                if ~isscalar(smooth_COM.(fields_COM{f}))
                    msg = sprintf('⚠️ %s : COM Champ %s non scalaire → NaN', filename, fields_COM{f});
                    log_errors{end+1} = msg;
                    fprintf('%s\n', msg);
                    smooth_COM.(fields_COM{f}) = NaN;
                end
            end

            fields_STERN = fieldnames(smooth_STERN);
            for f = 1:length(fields_STERN)
                if ~isscalar(smooth_STERN.(fields_STERN{f}))
                    msg = sprintf('⚠️ %s : STERN Champ %s non scalaire → NaN', filename, fields_STERN{f});
                    log_errors{end+1} = msg;
                    fprintf('%s\n', msg);
                    smooth_STERN.(fields_STERN{f}) = NaN;
                end
            end
     
            % === STOCKAGE ===
            new_row = struct();
            new_row.Sujet            = {sujet_id};
            new_row.Surface          = {surface};
            new_row.Essai            = essai;
            new_row.N_Cycles         = n_cycles;
            new_row.Start_Frame      = start_frame;
            new_row.End_Frame        = end_frame;
            new_row.Duration_frames  = end_frame - start_frame + 1;
            new_row.Duration_sec     = new_row.Duration_frames / freqVicon;

            for f = 1:length(fields_COM)
                fname = fields_COM{f};
                new_row.(sprintf('COM_%s', fname)) = smooth_COM.(fname);
            end

            for f = 1:length(fields_STERN)
                fname = fields_STERN{f};
                new_row.(sprintf('STERN_%s', fname)) = smooth_STERN.(fname);
            end

            results = [results; struct2table(new_row)];

            btkCloseAcquisition(data);
            fprintf('   ✅ Essai traité : durée %.1f sec\n\n', new_row.Duration_sec);
            
        catch ME
            msg = sprintf('❌ %s : %s', filename, ME.message);
            log_errors{end+1} = msg;
            fprintf('%s\n\n', msg);
            try, btkCloseAcquisition(data); end %#ok<TRYNC>
        end
    end
end

% === SAUVEGARDE ===
fprintf('💾 Sauvegarde...\n');

writetable(results, output_csv);

save(output_mat, 'results', 'log_errors', 'fft_log');

nTrials = height(results);
fprintf('📊 Total essais traités : %d\n', nTrials);
fprintf('\n💡 PROCHAINE ÉTAPE : Lancer SpatioTemporal_Analysis.m\n');

%% === VISUALISATION FFT COM (MAGNITUDE) – Essai voulu ===
plot_fft_COM_magnitude_trial(base_dir, sujet_id, surfaces, 3, freqVicon, fc_filter); % changer chiffre pour changer essai

%% === FIGURES COMPARATIVES COM vs STERN ===

% Familles d'indices et directions
families   = {'SPARC', 'LDLJ'};
directions = {'Magnitude', 'AP', 'ML', 'V'};

metrics_to_plot = {};

for f = 1:numel(families)
    for d = 1:numel(directions)
        metrics_to_plot{end+1} = sprintf('%s_%s', families{f}, directions{d});
    end
end

% Boucle sur toutes les métriques disponibles
for m = 1:numel(metrics_to_plot)
    metricName = metrics_to_plot{m};
    % On trace seulement si les colonnes existent vraiment
    comField   = ['COM_'   metricName];
    sternField = ['STERN_' metricName];

    if ismember(comField, results.Properties.VariableNames) && ...
       ismember(sternField, results.Properties.VariableNames)
        plot_COM_vs_STERN(results, metricName);
    else
        fprintf('⏭️  Skip %s (champs %s ou %s absents)\n', ...
            metricName, comField, sternField);
    end
end

%% ============================== FONCTIONS ==============================

function COM = calculate_pelvic_COM(markers)
    pelvis_markers = {};
    needed = {'LPSI','RPSI','LASI','RASI'};

    for i = 1:length(needed)
        if isfield(markers, needed{i})
            mk = markers.(needed{i});
            if ~isempty(mk) && size(mk,2) == 3
                pelvis_markers{end+1} = mk;
            end
        end
    end

    if length(pelvis_markers) < 2
        error('Not enough pelvic markers (minimum 2 requis)');
    end

    L = min(cellfun(@(x) size(x,1), pelvis_markers));
    for i = 1:length(pelvis_markers)
        pelvis_markers{i} = pelvis_markers{i}(1:L,:);
    end

    stack = cat(3, pelvis_markers{:});
    COM   = mean(stack, 3);
end

function vel = calculate_velocities(pos, fs)
    dt = 1/fs;
    vel = zeros(size(pos));
    for k = 1:size(pos,2)
        vel(:,k) = gradient(pos(:,k), dt);
    end
end

function smoothness = calculate_smoothness_indices(vel, i1, i2, fs)
    if all(isnan(vel(:)))
        smoothness.SPARC_AP        = NaN;
        smoothness.SPARC_ML        = NaN;
        smoothness.SPARC_V         = NaN;
        smoothness.SPARC_Magnitude = NaN;
        smoothness.LDLJ_AP         = NaN;
        smoothness.LDLJ_ML         = NaN;
        smoothness.LDLJ_V          = NaN;
        smoothness.LDLJ_Magnitude  = NaN;
        return;
    end

    Ts = 1/fs;
    ampThreshold = 0.05;
    fMaxHz       = 6;
    zeroPadIdx   = 4;
    params       = [ampThreshold, fMaxHz, zeroPadIdx];

    directions = {'ML','AP','V'};

    for d = 1:3
        v_seg = vel(i1:i2, d);

        % ===== SPARC =====
        speed = v_seg(:)';  % row vector, SANS abs(), SANS retrait de moyenne ici
        smoothness.(sprintf('SPARC_%s', directions{d})) = SpectralArcLength(speed, Ts, params);

        % ===== LDLJ (sans retrait de moyenne, sans abs) =====
        smoothness.(sprintf('LDLJ_%s', directions{d}))  = compute_LDLJ(v_seg, fs);
    end

    % ===== Magnitude vitesse =====
v_mag = sqrt(sum(vel(i1:i2,:).^2, 2));

    % SPARC magnitude
    % Le retrait de moyenne sera fait dans SpectralArcLength
    speed3D = v_mag(:)';  
    smoothness.SPARC_Magnitude = SpectralArcLength(speed3D, Ts, params);

    % LDLJ magnitude (sans retrait de moyenne, sans abs)
    smoothness.LDLJ_Magnitude  = compute_LDLJ(v_mag, fs);
end

function ldlj = compute_LDLJ(velocity, fs) 
    if length(velocity) < 10 || all(isnan(velocity))
        ldlj = NaN;
        return;
    end
    
    velocity = velocity(:);
    dt       = 1 / fs;
    
    acc  = gradient(velocity,dt);
    jerk = gradient(acc,dt);
    
    T      = (length(velocity) - 1) * dt;
    v_peak = max(abs(velocity));
    
    if v_peak < 1e-8 || T < 1e-8
        ldlj = NaN;
        return;
    end
    
    integral_j = sum(jerk.^2) * dt;
    D          = (T^5 / v_peak^2) * integral_j;
    
    ldlj = -log(D + eps);
end

function [freqs, Vn] = compute_spectrum(x, fs)
% FFT one-sided, mean retiré, normalisée par le max du spectre (style SPARC)
% zero-padding fixe +4 (cohérent SPARC)
    x = x(:);

    if numel(x) < 2 || all(isnan(x))
        freqs = NaN; 
        Vn    = NaN;
        return;
    end

    x = x - mean(x, 'omitnan'); % retrait DC

    N   = numel(x);
    Nfft = 2^(ceil(log2(N)) + 4); 

    Sp = abs(fft(x, Nfft));   % full spectrum
    denom = max(Sp);
    if denom < 1e-10, denom = 1e-10; end
    Sp = Sp / denom;          % normalisation par max (SPARC-like)

    Vn    = Sp(1:Nfft/2+1)';  % one-sided
    freqs = (0:Nfft/2)' * (fs / Nfft);
end

function [freqs, Vn, info] = compute_spectrum_with_info(x, fs, zeroPadPow2)
% FFT one-sided normalisée par max du spectre + log des tailles/résolution
% mean retiré ici pour cohérence avec SPARC/FFT
    x = x(:);
    N = length(x);

    if N < 2 || all(isnan(x))
        freqs = NaN; Vn = NaN;
        info = struct('N',N,'K',NaN,'df',NaN,'lenSpec',NaN);
        return;
    end

    x = x - mean(x, 'omitnan'); % retrait DC

    K = 2^(ceil(log2(N)) + zeroPadPow2);
    V = abs(fft(x, K));
    V = V(1:K/2+1);

    denom = max(V);
    if denom < 1e-10, denom = 1e-10; end
    Vn = V / denom;            % normalisation par max du spectre

    freqs = (0:K/2)' * (fs / K);

    info.N = N;
    info.K = K;
    info.df = fs / K;
    info.lenSpec = length(Vn);
end

function row = make_fft_row(sujet_id, surface, essai, signalName, start_frame, end_frame, info)
    row = table( string(sujet_id), string(surface), essai, string(signalName), ...
                 start_frame, end_frame, (end_frame-start_frame+1), ...
                 info.N, info.K, info.lenSpec, info.df, ...
                 'VariableNames', {'Sujet','Surface','Essai','Signal', ...
                                   'Start_Frame','End_Frame','Duration_frames', ...
                                   'N','K','LenSpectrum','df_Hz'} );
end

function plot_fft_COM_magnitude_trial(base_dir, sujet_id, surfaces, essai, fs, fc_filter)
% FFT de la MAGNITUDE de la vitesse COM (fenêtre 1er HS -> dernier HS)
% Visualisation Plat / Medium / High pour un essai donné

    [b, a] = butter(2, fc_filter/(fs/2), 'low');

    figure('Name', sprintf('FFT COM – Magnitude – Essai %02d', essai), ...
           'Color','w', 'Position',[200 200 900 600]);
    hold on; box on; grid on;

    colors = lines(numel(surfaces));
    leg = {};

    for s = 1:numel(surfaces)
        surface = surfaces{s};
        filename = sprintf('%s_%s_%02d.c3d', sujet_id, surface, essai);
        c3d_path = fullfile(base_dir, filename);

        if ~isfile(c3d_path)
            fprintf('❌ Manquant: %s\n', filename);
            continue;
        end

        try
            data    = btkReadAcquisition(c3d_path);
            markers = btkGetMarkers(data);

            COM = calculate_pelvic_COM(markers);
            COM_filt = filtfilt(b, a, COM);
            vel_COM  = calculate_velocities(COM_filt, fs);

            n_frames   = size(COM,1);
            fsrt_frame = btkGetFirstFrame(data);
            events     = btkGetEvents(data);

            LHS = []; RHS = [];
            if isfield(events,'Left_Foot_Strike')
                LHS = round(events.Left_Foot_Strike * fs - fsrt_frame + 1);
            end
            if isfield(events,'Right_Foot_Strike')
                RHS = round(events.Right_Foot_Strike * fs - fsrt_frame + 1);
            end

            HS_all = sort([LHS(:); RHS(:)]);
            HS_all = max(1, min(HS_all, n_frames));

            if numel(HS_all) >= 2
                start_frame = HS_all(1);
                end_frame   = HS_all(end);
            else
                start_frame = 1;
                end_frame   = n_frames;
            end

            if end_frame <= start_frame
                start_frame = 1;
                end_frame   = n_frames;
            end

            seg_COM = vel_COM(start_frame:end_frame, :);
            v_mag   = sqrt(sum(seg_COM.^2, 2));

            % IMPORTANT: mean retiré dans compute_spectrum -> ne pas le refaire ici
            [freqs, Vn] = compute_spectrum(v_mag, fs);

            idx = freqs <= 12;
            plot(freqs(idx), Vn(idx), 'LineWidth', 2, 'Color', colors(s,:));
            leg{end+1} = sprintf('%s (N=%d)', surface, length(v_mag));

            btkCloseAcquisition(data);

        catch ME
            warning('Erreur %s : %s', filename, ME.message);
            try, btkCloseAcquisition(data); end %#ok<TRYNC>
        end
    end

    xline(6, 'k--', 'LineWidth', 1.5, 'Alpha', 0.7);
    xlabel('Fréquence (Hz)', 'FontSize', 12);
    ylabel('Magnitude normalisée (max spectre)', 'FontSize', 12);
    title(sprintf('FFT COM – Magnitude vitesse – Essai %02d (1er HS → dernier HS)', essai), ...
          'FontSize', 13, 'FontWeight','bold');
    xlim([0 12]);
    ylim([0 inf]);
    legend(leg, 'Location','northeast');
    hold off;
end

% LOCAL FUNCTION FROM GITHUB: https://github.com/siva82kb/smoothness/blob/master/matlab/SpectralArcLength.m
function S = SpectralArcLength( speed, Ts, parameters )
% SPECTRALARCLENGTH computes the smoothness of the give movement speed
% profile using the spectral arc length method.
% The this function takes three inputs and provides one output.
% Inputs: { speed, Ts*, parameters* } ('*' optional parameters)
%         SPEED: Speed it the speed profile of the movement. This is a 1xN
%         row vecotr. N is the total number of points in the speed profile.
%         The function assumes that the movement speed profile is already
%         filtered and segemented.
%
%         TS*: Sampling time in seconds. (DEFAULT VALUE = 0.01sec) NOTE: IF
%         YOUR DATA WAS NOT SAMPLED AT 100HZ, YOU MUST ENTER THE
%         APPROPRIATE SAMPLING TIME FOR ACCURATE RESULTS.
%
%         PARAMETERS*: This contains the parameters to be used spectral arc
%         lenght computation. This is a 1x2 column vector. This input
%         argument is option
%           - PARAMETER(1): The amplitude threshold to be used to choose
%           the cut-off frequency. The default value is chosen to be 0.05.
%           - PARAMETER(2): Maximum cut-off frequency for the spectral arc
%           length calcualtion. (DEFAULT VALUE = 10HZ) NOTE: 20Hz IS USED
%           TO REPRESENT THE MAXIMUM FREQUENCY COMPONENT OF A MOVEMENT.
%           THIS WAS CHOSEN TO COVER BOTH NORMAL AND ABNORMAL MOTOR
%           BEAHVIOUR. YOU CAN USE A VALUE LOWER THAN 20Hz IF YOU ARE AWARE
%           OF THE MAXIMUM FREQUENCY COMPONENT IN THE MOVEMENT OF INTEREST.
%           - PARAMETER(3): Zero padding index. This parameter controls the
%           resolution of the movement spectrum calculated from the speed
%           profile. (DEFAULT VALUE = 4). NOTE: IT IS NOT ADVISABLE TO USE
%           VALUES LOWER THAN 4.
%
% Outputs: { S }
%          S: This is smoothness of the given movement.
%
% For any queries about the method or the code, or if you come across any
% bugs in the code, feel free to contact me at siva82kb@gmail.com
% Sivakumar Balasubramanian. July 02, 2014.

% Check input arguments.
if nargin == 0
    disp('Error! Input at least the movement speed profile for the smoothness calculation.');
    fprintf('\n');
    help('SpectralArcLength');
    return;
elseif nargin == 1
    % Default sampling time.
    Ts = 1/100; % 10ms.
elseif nargin == 2
    % Default parameters are use for the spectral arc length caclulations.
    parameters = [0.05, 10, 4];
end

% Check if the input argument are of the appropriate dimensions.
% Speed profile.
sz = size(speed);

% BUG FIX (minimum change):
% The original condition was inverted and rejected valid 1xN row vectors.
% This corrected condition throws an error ONLY when speed is NOT a 1xN row vector.
if ~((sz(1) == 1) && (sz(2) > 1))
    disp('Error! speed must be a row vector.');
    fprintf('\n');
    help('SpectralArcLength');
    return;
end

% Sampling time.
if ~isscalar(Ts)
    disp('Error! Ts must be a scalar.');
    fprintf('\n');
    help('SpectralArcLength');
    return;
end

% Parameters.
if length(parameters) ~= 3
    disp('Error! parameter is a vector with two elements.');
    fprintf('\n');
    help('SpectralArcLength');
    return;
end

% Calculate the spectrum of the speed profile.
speed = speed - mean(speed);

N = length(speed);
Nfft = 2^(ceil(log2(N))+parameters(3));
speedSpectrum = abs(fft( speed, Nfft ));

% Normalize spectrum with respect to the DC component.
% NOTE: keep their original normalization (by max) to minimize changes.
% Also build freq with a guaranteed length matching Nfft to avoid index mismatch.
freq = (0:Nfft-1) * ((1/Ts)/Nfft);
speedSpectrum = speedSpectrum'/max(speedSpectrum);

% Choose the spectrum that is always above the amplitude threshold, and
% within the cut-off frequency.
inxFc = find((freq(1:end) <= parameters(2)) & ...
             (speedSpectrum(1:end) >= parameters(1)), 1, 'last');

% SAFETY: if nothing found (or numerical mismatch), clamp index
if isempty(inxFc)
    inxFc = length(speedSpectrum);
elseif inxFc > length(speedSpectrum)
    inxFc = length(speedSpectrum);
end

% Calculate the spectral arc length.
% 1. select the spectrum of interest.
speedSpectrum = speedSpectrum(1:inxFc);

% 2. Calculate the incremental arc lengths.
if inxFc < 2
    S = NaN;
    return;
end
dArcLengths = sqrt((1/(inxFc-1))^2 + (diff(speedSpectrum)).^2);

% 4. Compute movement smoothness.
S = -sum(dArcLengths);

return;
end

function plot_COM_vs_STERN(results, metricFieldBase)
% Crée une figure comparant COM vs STERN pour une métrique donnée
% metricFieldBase = 'SPARC_Magnitude', 'SPARC_AP', 'LDLJ_ML', etc.
    
    % Noms complets des champs
    comField   = ['COM_'   metricFieldBase];
    sternField = ['STERN_' metricFieldBase];

    % Vérification de la présence des champs
    if ~ismember(comField, results.Properties.VariableNames) || ...
       ~ismember(sternField, results.Properties.VariableNames)
        warning('Champs %s ou %s introuvables dans results.', comField, sternField);
        return;
    end

    if ismember('RowType', results.Properties.VariableNames)
        trial_idx = results.RowType == "Trial";
    else
        trial_idx = true(height(results),1);
    end

    res_trial = results(trial_idx, :);

    % Surfaces dans l'ordre d'apparition parmi les essais
    surfaces = unique(res_trial.Surface, 'stable');
    nSurf    = numel(surfaces);

    % Couleurs COM vs STERN
    colCOM   = [0.2 0.4 0.8];
    colSTERN = [0.9 0.5 0.1];

    mCOM    = nan(nSurf,1);
    mSTERN  = nan(nSurf,1);
    sdCOM   = nan(nSurf,1);
    sdSTERN = nan(nSurf,1);

    dataCOM   = cell(nSurf,1);
    dataSTERN = cell(nSurf,1);

    % Récupère les données par surface
    for s = 1:nSurf
        idx = strcmp(res_trial.Surface, surfaces{s});

        yCOM   = res_trial.(comField)(idx);
        ySTERN = res_trial.(sternField)(idx);

        dataCOM{s}   = yCOM;
        dataSTERN{s} = ySTERN;

        mCOM(s)    = mean(yCOM,   'omitnan');
        mSTERN(s)  = mean(ySTERN, 'omitnan');
        sdCOM(s)   = std(yCOM,   'omitnan');
        sdSTERN(s) = std(ySTERN, 'omitnan');
    end

    % Création de la figure
    figure('Name', ['COM vs STERN - ' metricFieldBase], 'Color', 'w');
    hold on; box on;

    x        = 1:nSurf;
    barWidth = 0.35;

    % Barres des moyennes
    b1 = bar(x - barWidth/2, mCOM,   barWidth, 'FaceColor', colCOM,   'FaceAlpha', 0.6, 'EdgeColor', 'none');
    b2 = bar(x + barWidth/2, mSTERN, barWidth, 'FaceColor', colSTERN, 'FaceAlpha', 0.6, 'EdgeColor', 'none');

    % Barres d'erreur (±1 SD)
    for s = 1:nSurf
        if ~isnan(mCOM(s)) && ~isnan(sdCOM(s))
            plot([x(s)-barWidth/2 x(s)-barWidth/2], [mCOM(s)-sdCOM(s) mCOM(s)+sdCOM(s)], 'k-', 'LineWidth', 1);
        end
        if ~isnan(mSTERN(s)) && ~isnan(sdSTERN(s))
            plot([x(s)+barWidth/2 x(s)+barWidth/2], [mSTERN(s)-sdSTERN(s) mSTERN(s)+sdSTERN(s)], 'k-', 'LineWidth', 1);
        end
    end

    % Points individuels (essais)
    jitter = barWidth/3;

    for s = 1:nSurf
        yC = dataCOM{s};
        yS = dataSTERN{s};

        % On enlève les NaN pour éviter des points invisibles
        yC = yC(~isnan(yC));
        yS = yS(~isnan(yS));

        % COM – points
        if ~isempty(yC)
            xC = (x(s)-barWidth/2) + (rand(size(yC))-0.5) * jitter;
            scatter(xC, yC, 30, colCOM, 'filled', ...
                'MarkerFaceAlpha', 0.7, 'MarkerEdgeColor','none');
        end

        % STERN – points
        if ~isempty(yS)
            xS = (x(s)+barWidth/2) + (rand(size(yS))-0.5) * jitter;
            scatter(xS, yS, 30, colSTERN, 'filled', ...
                'MarkerFaceAlpha', 0.7, 'MarkerEdgeColor','none');
        end
    end

    % Axes & légendes
    set(gca, 'XTick', x, 'XTickLabel', surfaces, 'FontSize', 12);
    xlabel('Surface');
    ylabel(metricFieldBase, 'Interpreter','none');
    legend([b1 b2], {'COM', 'STERNUM'}, 'Location', 'best');
    title(sprintf('COM vs STERN - %s', metricFieldBase), 'Interpreter','none');

    hold off;
end