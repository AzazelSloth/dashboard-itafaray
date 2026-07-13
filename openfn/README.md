# Workflow OpenFn - cache X-Road

Le fichier `itafaray-xroad-cache.yaml` est un Project Spec OpenFn v2 (schema 4.0)
importable depuis les parametres d'un projet OpenFn. Son cron `*/15 * * * *`
rafraichit le cache toutes les 15 minutes. Il est importe desactive afin de
permettre l'affectation du credential et une premiere execution manuelle.

## Configuration apres import

1. Creer un credential HTTP OpenFn avec les proprietes suivantes :

   ```json
   {
     "baseUrl": "https://dashboard-itafaray.onehealthsismada.org/xroad-ingest/",
     "token": "LE_MEME_SECRET_QUE_OPENFN_INGEST_TOKEN"
   }
   ```

2. Affecter ce credential a l'etape `Executer ingest_xroad.R`.
3. Dans le fichier `.env` du serveur, definir le meme secret dans
   `OPENFN_INGEST_TOKEN`.
4. Exposer le service local `127.0.0.1:8000` derriere le reverse proxy HTTPS avec
   les deux routes Nginx ci-dessous. Ne pas transmettre le jeton en HTTP non
   chiffre.
5. Deployer `app` et `ingest-api`, puis verifier l'endpoint public :

   ```bash
   curl -i https://dashboard-itafaray.onehealthsismada.org/xroad-ingest/health
   ```

6. Effectuer une execution manuelle du workflow avec l'input `{}`. Activer le
   cron uniquement apres une execution reussie.

La frequence se change dans `cron_expression`. Exemples :

- toutes les 5 minutes : `*/5 * * * *` ;
- toutes les 30 minutes : `*/30 * * * *` ;
- toutes les heures : `0 * * * *`.

OpenFn execute les crons en UTC. Ici la periodicite de 15 minutes ne depend pas
du fuseau horaire.

Configuration Nginx a ajouter dans le bloc HTTPS de
`dashboard-itafaray.onehealthsismada.org`, avant le `location /` de Shiny :

```nginx
location = /xroad-ingest/health {
    proxy_pass http://127.0.0.1:8000/health;

    proxy_http_version 1.1;
    proxy_set_header Host              $host;
    proxy_set_header X-Real-IP         $remote_addr;
    proxy_set_header X-Forwarded-For   $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto $scheme;

    proxy_connect_timeout 5s;
    proxy_read_timeout 30s;
    proxy_buffering off;
}

location = /xroad-ingest/ingest {
    limit_except POST {
        deny all;
    }

    proxy_pass http://127.0.0.1:8000/ingest;

    proxy_http_version 1.1;
    proxy_set_header Host              $host;
    proxy_set_header Authorization     $http_authorization;
    proxy_set_header X-Real-IP         $remote_addr;
    proxy_set_header X-Forwarded-For   $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto $scheme;

    proxy_connect_timeout 5s;
    proxy_read_timeout 600s;
    proxy_send_timeout 600s;
    proxy_buffering off;
}
```

Le reverse proxy de `openfn.onehealthsismada.org` ne doit pas etre modifie : les
jobs OpenFn appellent le domaine du dashboard en HTTPS.
