# Workflow OpenFn - cache X-Road

Le fichier `itafaray-xroad-cache.yaml` est un Project Spec OpenFn v2 (schema 4.0)
importable depuis les parametres d'un projet OpenFn. Son cron `*/15 * * * *`
rafraichit le cache toutes les 15 minutes.

## Configuration apres import

1. Creer un credential HTTP OpenFn avec les proprietes suivantes :

   ```json
   {
     "baseUrl": "https://dashboard.example.org/xroad-ingest/",
     "token": "LE_MEME_SECRET_QUE_OPENFN_INGEST_TOKEN"
   }
   ```

2. Affecter ce credential a l'etape `Executer ingest_xroad.R`.
3. Dans le fichier `.env` du serveur, definir le meme secret dans
   `OPENFN_INGEST_TOKEN`.
4. Exposer le service local `127.0.0.1:8000` derriere le reverse proxy HTTPS sur
   le chemin ou sous-domaine utilise dans `baseUrl`. Si un chemin est utilise,
   conserver le `/` final dans `baseUrl`. Ne pas transmettre le jeton en HTTP
   non chiffre.
5. Lancer `docker compose up -d --build`, puis effectuer une execution manuelle
   du workflow avant de laisser le cron actif.

La frequence se change dans `cron_expression`. Exemples :

- toutes les 5 minutes : `*/5 * * * *` ;
- toutes les 30 minutes : `*/30 * * * *` ;
- toutes les heures : `0 * * * *`.

OpenFn execute les crons en UTC. Ici la periodicite de 15 minutes ne depend pas
du fuseau horaire.

Exemple Nginx correspondant au `baseUrl` ci-dessus :

```nginx
location /xroad-ingest/ {
    proxy_pass http://127.0.0.1:8000/;
    proxy_set_header Authorization $http_authorization;
    proxy_set_header Host $host;
}
```
