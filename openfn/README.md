# Workflow OpenFn - cache X-Road

Le fichier `itafaray-xroad-cache.yaml` utilise le schema d'import de workflow
attendu par Lightning v2.16.7 (`jobs`, `triggers`, `edges`). Il est importable
directement depuis `Workflows > New Workflow > Import workflow`.
Son cron `*/3 * * * *`
rafraichit le cache toutes les trois minutes. Il est importe desactive afin de
permettre l'affectation du credential et une premiere execution manuelle.

## Configuration apres import

1. Creer un credential HTTP OpenFn avec les proprietes suivantes :

   ```json
   {
     "baseUrl": "https://dashboard-itafaray.onehealthsismada.org/xroad-ingest/",
     "token": "LE_MEME_SECRET_QUE_OPENFN_INGEST_TOKEN",
     "xApiKey": "LA_MEME_VALEUR_QUE_LE_SECRET_GITHUB_X_API_KEY"
   }
   ```

2. Affecter ce credential a l'etape `Executer ingestion XRoad`.
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

## Cle API X-Road

Le job OpenFn lit `xApiKey` depuis son credential et l'envoie dans le header
HTTP `X-API-KEY` a l'API d'ingestion. Celle-ci verifie le bearer token et cette
cle avant de lancer `ingest_xroad.R`. Le script lit ensuite `X_API_KEY` dans
l'environnement du conteneur et l'envoie a X-Road sous le meme nom de header.

1. Dans GitHub, creer le secret d'environnement ou de depot nomme exactement
   `X_API_KEY` (`Settings > Secrets and variables > Actions`).
2. Relancer le workflow de deploiement. GitHub Actions mappe ce secret vers
   `X_API_KEY` dans le `.env` du serveur; la valeur n'est jamais versionnee.
3. Ajouter la propriete `xApiKey` au credential OpenFn, avec exactement la meme
   valeur que le secret GitHub `X_API_KEY`, puis affecter ce credential au job.
4. Lancer manuellement le workflow OpenFn et verifier que le cache est rafraichi.

Les routes completes utilisees par l'ingestion sont configurables avec la
variable GitHub Actions `XROAD_API_PATHS`. Sa valeur par defaut est :

```text
/api/v1/signaux,/api/v1/evenement,/api/v1/alertes
```

Les endpoints `/count` et les vues filtrees (secteur, triage, verification et
niveau de risque) restent disponibles pour des usages cibles, mais ne sont pas
necessaires au cache complet.

La frequence se change dans `cron_expression`. Exemples :

- toutes les minutes : `* * * * *` ;
- toutes les 5 minutes : `*/5 * * * *` ;
- toutes les 30 minutes : `*/30 * * * *` ;
- toutes les heures : `0 * * * *`.

OpenFn execute les crons en UTC. Ici la periodicite d'une minute ne depend pas
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
    proxy_set_header X-API-KEY         $http_x_api_key;
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
