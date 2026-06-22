# Mise à jour automatique du dashboard depuis X-Road

Objectif : le tableau de bord i-Tafaray (hébergé sur AWS) doit refléter les nouvelles
données dès qu'elles arrivent sur X-Road, sans intervention manuelle.

## Principe retenu : ingestion programmée → cache → lecture auto-rafraîchie

On **n'interroge pas** X-Road à chaque ouverture de page. À la place :

1. Un script (`ingest_xroad.R`) récupère les données via le Security Server et écrit un
   **cache** (`xroad_cache.rds`) avec un horodatage.
2. Le dashboard, en mode « Données réelles », **lit ce cache** et se **rafraîchit tout seul**
   dès qu'il change (toute session ouverte se met à jour sans recharger la page).
3. Si X-Road est momentanément indisponible, le dernier cache reste affiché ; s'il n'y a
   aucun cache, l'app bascule sur les données de démonstration.

Avantages : robuste aux coupures X-Road, rapide (pas de latence réseau à chaque page),
charge maîtrisée sur X-Road. FHIR ne « pousse » pas les données : on est en récupération.

## 2 décisions à prendre (côté Pivot / UGD)

1. **Joignabilité réseau.** Le serveur AWS peut-il atteindre le Security Server ?
   - **Oui** (route réseau / VPN / IP autorisée) → on planifie `ingest_xroad.R` directement
     sur le serveur AWS.
   - **Non** → on exécute `ingest_xroad.R` sur une **machine interne** au réseau autorisé,
     puis on **synchronise le cache** (`xroad_cache.rds`) vers AWS (par ex. `aws s3 cp`,
     `scp`, ou un petit point d'API). Le dashboard lit le cache déposé sur AWS.
2. **Fréquence.** Cadence de rafraîchissement souhaitée. Pour de la surveillance,
   **15 à 60 min** suffisent (valeur par défaut proposée : 30 min).

## Déploiement AWS — accès direct au Security Server (cas retenu)

Le serveur AWS joint directement le Security Server : l'ingestion tourne **sur AWS**, aucune
synchro externe nécessaire. Comme R et les paquets vivent dans le **conteneur** (pas sur l'hôte),
deux montages propres, au choix :

**Point commun : un volume partagé pour le cache.** Le conteneur de l'app et l'ingestion
doivent voir le **même** fichier de cache, sur un **volume persistant** (qui survit aux
redéploiements). On expose le même chemin via `XROAD_CACHE_PATH`.

### Option A — service `ingest` dans docker-compose (recommandé : déployé via le CI/CD)

Dans `docker-compose.yml`, ajouter un volume nommé, le brancher sur l'app, et ajouter un
service d'ingestion qui réutilise la même image :

```yaml
volumes:
  itafaray_data:

services:
  app:
    # … config existante …
    environment:
      - XROAD_CACHE_PATH=/data/xroad_cache.rds
    volumes:
      - itafaray_data:/data

  ingest:
    image: itafaray-poc:1.1          # même image que l'app
    container_name: itafaray-ingest
    restart: unless-stopped
    working_dir: /app
    env_file: .env                    # secrets X-Road (NON versionnés)
    environment:
      - XROAD_CACHE_PATH=/data/xroad_cache.rds
    volumes:
      - itafaray_data:/data
    # Récupère toutes les 30 min (1800 s) — ajuster INGEST_INTERVAL au besoin
    command: ["sh","-c","while true; do Rscript /app/ingest_xroad.R; sleep ${INGEST_INTERVAL:-1800}; done"]
```

Avantage : tout part par GitHub → CI/CD, rien à configurer à la main sur le serveur (hormis
le fichier `.env` des secrets X-Road).

### Option B — cron de l'hôte + `docker exec` (si le déploiement n'utilise pas ce compose)

Si l'app tourne déjà comme conteneur nommé `itafaray` avec un volume monté sur `/data` :

```cron
*/30 * * * * docker exec -e XROAD_CACHE_PATH=/data/xroad_cache.rds itafaray Rscript /app/ingest_xroad.R >> /var/log/itafaray_ingest.log 2>&1
```

Dans les deux cas, l'app détecte le changement du cache et se rafraîchit toute seule.

## Mise en place serveur (détails)

**Variable d'environnement** — pointer le cache vers un **volume persistant** monté dans le
conteneur (et non l'image Docker, qui est reconstruite à chaque déploiement) :

```bash
XROAD_CACHE_PATH=/data/xroad_cache.rds
```
Le dashboard et le script lisent/écrivent ce même chemin. Monter `/data` comme volume Docker.

**Secrets X-Road** — l'identité client, la passphrase, etc. ne doivent **pas** être dans le
dépôt : les fournir en variables d'environnement sur le serveur (cf. `xroad_bridge.R`).

**Planification (exemple cron, toutes les 30 min)** :
```cron
*/30 * * * * cd /app && XROAD_CACHE_PATH=/data/xroad_cache.rds /usr/bin/Rscript ingest_xroad.R >> /var/log/itafaray_ingest.log 2>&1
```
(ou un timer systemd équivalent). Le script écrit le cache de façon **atomique** et ne
l'écrase pas en cas d'échec.

**Si ingestion sur machine interne** — ajouter après le `Rscript` une étape de synchro, ex. :
```bash
aws s3 cp /data/xroad_cache.rds s3://<bucket>/xroad_cache.rds   # puis récupéré côté AWS
```

## Comportement visible

Un badge en haut du dashboard indique la source et l'heure de dernière mise à jour :
« X-Road actif · maj JJ/MM HH:MM ». En cas d'indisponibilité : « X-Road indisponible — démo ».

## Optimisations futures (optionnelles)

- **Synchro incrémentale** : interroger `Observation?_lastUpdated=gt<dernier_sync>` pour ne
  récupérer que les nouveautés (utile si le volume grandit).
- **Événementiel (temps quasi réel)** : si le serveur HAPI FHIR de Pivot expose des
  **FHIR Subscriptions / webhooks**, déclencher l'ingestion à l'arrivée d'un cas plutôt
  qu'au cron.

## Fichiers concernés (dans ce dépôt)

- `ingest_xroad.R` — script d'ingestion (à planifier).
- `xroad_bridge.R` — fonction `charger_xroad()` (récupération FHIR via le Security Server).
- `app.R` — lecture du cache + auto-rafraîchissement + badge de mise à jour.
- Le cache (`data_poc/xroad_cache.rds`, `*_meta.txt`) est en `.gitignore` / `.dockerignore` :
  il vit sur le serveur, pas dans le dépôt ni l'image.
