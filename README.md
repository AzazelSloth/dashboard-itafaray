# i-Tafaray — Tableau de bord central One Health (Preuve de concept)

Application **R / Shiny** branchée sur un **jeu de données de démonstration synthétique**
(6 scénarios One Health : H5N1, peste, rage, Mpox, Ebola, contamination hydrique).
Aucune donnée réelle.

> ⚠️ **Données entièrement synthétiques.** Voir `../Jeu_de_donnees_POC/Scenarios_One_Health.md`.

## Contenu

- `app.R` — application Shiny (UI + serveur).
- `prepare_data.R` — lecture + jointure des 3 tables (signaux / evenement_sbe / alerte)
  via `id_signal`, et mise en forme au grain « signal ».
- `data_poc/` — les 3 fichiers de démonstration (`signaux_POC.xlsx`, `evenement_sbe_POC.xlsx`,
  `alerte_POC.xlsx`).

## Lancer l'application

1. Installer **R** (≥ 4.1) et, de préférence, **RStudio**.
2. Ouvrir `app.R`, puis **« Run App »**. Ou en console, depuis ce dossier : `shiny::runApp()`.
3. Au premier lancement, les packages manquants s'installent automatiquement
   (`shiny`, `shinydashboard`, `dplyr`, `ggplot2`, `DT`, `leaflet`, `scales`, `lubridate`,
   `readxl`, `stringr`).

## Pages

- **Vue d'ensemble** : volume par mois/secteur, niveau de risque (signaux évalués),
  pathogènes suspectés, répartition par signal.
- **Cartographie** : localisation des signaux (GPS), colorés par secteur — zoomer sur un
  fokontany de scénario fait apparaître la grappe inter-secteurs.
- **Par signal** : analyse d'un des 18 signaux prioritaires.
- **Alertes** : table des signaux ayant déclenché une alerte (dont les 6 scénarios).
- **Pipeline & qualité** : entonnoir collectés → vérifiés → évalués → alertes ; taux de
  vérification, doublons, délai détection → vérification.

## Passage à un export réel

`prepare_data.R` détecte les fichiers par préfixe (`signaux*`, `evenement*`, `alerte*`).
Quand Pivot fournira un **export réel de même structure (avec `id_signal` renseigné)**,
il suffira de déposer les fichiers dans `data_poc/` (ou d'ajuster `dirs` dans
`charger_donnees()`). La logique des indicateurs reste inchangée.

## Partager en ligne (optionnel)

`rsconnect::deployApp()` vers **shinyapps.io** pour un lien partageable aux autorités
(données synthétiques : publication sans risque).
