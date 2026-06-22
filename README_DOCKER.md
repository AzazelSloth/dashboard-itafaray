# i-Tafaray — Plateforme One Health (version Docker)

Tableau de bord de surveillance One Health (Une seule santé) — preuve de concept,
données de démonstration. Ce dossier est **autonome** : il contient l'application,
ses données et tout le nécessaire pour la lancer dans un conteneur Docker.

## Prérequis

- **Docker** installé (Docker Desktop sur Windows/Mac, ou Docker Engine sur Linux).
  Vérifier : `docker --version`

## Lancer la plateforme (2 commandes)

Depuis ce dossier, dans un terminal :

```bash
docker compose up -d --build
```

La première fois, la construction prend ~5–10 min (téléchargement de R et compilation
des paquets). Ensuite, ouvrez votre navigateur sur :

```link
http://localhost:3838
```

Pour arrêter :

```bash
docker compose down
```

## Authentification

L'authentification est retiree temporairement du projet. L'application demarre
donc directement sans ecran de connexion.

## Variante sans docker compose

```bash
docker build -t itafaray .
docker run --rm -p 3838:3838 itafaray
```

## Contenu du dossier

```text
app.R                     application Shiny
prepare_data.R            chargement / préparation des données
i18n_setup.R              moteur multilingue (FR / EN / MG)
xroad_bridge.R            pont vers les données réelles X-Road (optionnel)
data_poc/                 données de démonstration + climat
translations/             dictionnaire de traduction
report/                   modèle du rapport HTML interactif
Dockerfile                recette de construction de l'image
docker-compose.yml        lancement en un service
```

## Notes

- **Port** : l'app écoute sur `3838`. Pour utiliser un autre port hôte, modifiez
  `docker-compose.yml` (ex. `"8080:3838"`) puis ouvrez `http://localhost:8080`.
- **Données réelles X-Road** : le bouton « Données réelles » ne fonctionne que depuis
  un réseau autorisé à joindre le Security Server. Sinon, l'app bascule automatiquement
  sur les données de démonstration.
- **Langues** : bascule FR / EN / MG en haut de l'interface (le malgache est une
  traduction provisoire, à relire par un locuteur natif).
- **Mise en production (serveur + HTTPS)** : pour un déploiement exposé sur Internet
  avec nom de domaine et certificat, voir la configuration nginx/HTTPS du projet
  principal (`DEPLOIEMENT_AWS.md`).
- **Reduction des CVE** : reconstruisez regulierement l'image
  (`docker compose build --pull --no-cache`) pour recuperer les correctifs Ubuntu/Rocker,
  car une partie des CVE visibles dans Docker Hub provient de la base systeme et non du
  code applicatif.
