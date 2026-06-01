clear all
clc
close all
cd ('C:\Users\silve\Desktop\DOCTORAT\UNIV MONTREAL\TRAVAUX-THESE\Surfaces_Irregulieres\Datas\Script\gaitAnalysisGUI\Data')
addpath(genpath('C:\Users\silve\Desktop\DOCTORAT\UNIV MONTREAL\TRAVAUX-THESE\Surfaces_Irregulieres\Datas\Script\gaitAnalysisGUI\functions\btk'));  

%% modif ou ajout de pistes cinématiques de marqueurs

acq=btkReadAcquisition('CTL_81_High_04.c3d'); % ouverture du fichier C3D

markers=btkGetMarkers(acq); % extraction de la cinematique des marqueurs

%% Marqueurs 2

[values, residuals]=btkGetPoint(acq,'C7');  % extraction de la cinematique du marqueur 'NOM_MARQUEUR' dont on veut la cinematique selon l'axe x
[values2, residuals2]=btkGetPoint(acq,'C7');% extraction de la cinematique du marqueur 'NOM_MARQUEUR2' dont on veut la cinematique selon l'axe y
[values3, residuals3]=btkGetPoint(acq,'C7');% extraction de la cinematique du marqueur 'NOM_MARQUEUR3' dont on veut la cinematique selon l'axe z


% on change les valeurs de la cinematique des marqueurs pour fitter avec celle du marqueur disparu. le fait de modifier ces valeurs ne change rien à la cinematique de NOM_MARQUEUR, etc. car on n'enregistre pas par dessus les originales, on ne fait qu'utiliser leur cinématique.

values=values+13.10; %rearrangement de la cinematique selon x pour fitter avec celle du marqueur disparu
values2=values2-104.2;%rearrangement de la cinematique selon y pour fitter avec celle du marqueur disparu
values3=values3-20.7;%rearrangement de la cinematique selon z pour fitter avec celle du marqueur disparu

valeurs(:,1)=values(:,1); % on construit un nouveau vecteur 'valeurs' qui a pour première colonne le vecteur selon x de NOM_MARQUEUR, ...
valeurs(:,2)=values2(:,2); % pour seconde colonne le vecteur selon y de NOM_MARQUEUR2
valeurs(:,3)=values3(:,3); % pour troisième colonne le vecteur selon z de NOM_MARQUEUR3


% [values4, residuals4]=btkGetPoint(acq,'RPSI'); % on extrait la cinématique du marqueur partiellement disparu que l'on sait bonne
% valeurs(600:664,:)=values4(600:664,:); % on remplace les premieres frames de la cinématique (que l'on a créée à partir des 3 marqueurs) par celle du marqueur réel. Ici les 120 premières frames sont correctes pour le marqueur.
[points, pointsInfo]=btkAppendPoint(acq, 'marker','CLAV', valeurs); % on crée le nouveau marqueur qui a donc les 120 premières frames à partir du réel puis une cinématique reconstruite.


%% ENREGISTREMENT DES DONNEES

btkWriteAcquisition(acq,'CTL_81_High_04_NEW.c3d'); % création du nouveau fichier C3D. 

%% verfication 
acq1=btkReadAcquisition('CTL_80_Plat_04_NEW.c3d'); % ouverture du fichier C3D
markers1=btkGetMarkers(acq1); % extraction de la cinematique des marqueurs

