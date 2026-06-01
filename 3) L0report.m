%% Renseigner le L0 de l'ensemble des participants pour la suite du Script
% L0 = Longueur de jambe moyenne entre gauche et droite (en mètre)
clc, clear, close all;

% Chemin de sauvegarde
save_path = 'XXX';

% Charger la Map existante ou créer une nouvelle
full_filename = fullfile(save_path, 'l0_participants.mat');
if isfile(full_filename)
    load(full_filename, 'l0_map');
    fprintf('Fichier L0 existant chargé\n');
else
    l0_map = containers.Map();
    fprintf('Nouvelle Map L0 créée\n');
end

% === NOUVEAUX PARTICIPANTS (à ajouter) ===

l0_map('CTL_XX') = 0.XX ; 

% Sauvegarder dans le répertoire spécifié
save(full_filename, 'l0_map');

% Afficher un résumé
participants_list = keys(l0_map);
fprintf('Fichier l0_participants.mat mis à jour avec succès!\n');
fprintf('Chemin: %s\n', save_path);
fprintf('Nombre total de participants: %d\n', length(participants_list));
fprintf('Participants inclus: %s\n', strjoin(sort(participants_list), ', '));
fprintf('\n💡 PROCHAINE ÉTAPE : Lancer Non_Dimensional_Normalization.m après avoir vérifié si participant(s) dans ParticipantGroup.m\n');