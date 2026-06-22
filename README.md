# i-Tafaray

Application `R / Shiny` pour le tableau de bord One Health.

## Fichiers utiles au fonctionnement

- `app.R` : application principale.
- `prepare_data.R` : chargement et préparation des données.
- `i18n_setup.R` : gestion des traductions.
- `xroad_bridge.R` : accès aux données réelles X-Road.
- `ingest_xroad.R` : alimentation optionnelle du cache X-Road.
- `extract_climate_mdg.R` : génération optionnelle des données climatiques.
- `credentials.sqlite` : base d'authentification.
- `data_poc/` : données de démonstration utilisées par défaut.
- `translations/` : dictionnaire multilingue.
- `report/` : template de rapport HTML.
- `www/` : logos et ressources statiques.
- `Dockerfile` et `docker-compose.yml` : build et lancement du projet.
- `.github/workflows/` : pipeline CI/CD.

## Lancement

```bash
docker compose up -d --build
```

Puis ouvrir `http://localhost:3838`.

## Notes

- Le mode démonstration fonctionne avec les fichiers présents dans `data_poc/`.
- Le mode X-Road repose sur `xroad_bridge.R` et peut aussi exploiter un cache produit par `ingest_xroad.R`.
- Les vues climat utilisent le fichier `data_poc/climate_ifanadiana.csv`. Le script `extract_climate_mdg.R` sert uniquement à le régénérer.
