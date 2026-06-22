#!/bin/bash
# =====================================================================
#  i-Tafaray — Initialisation du certificat TLS (Let's Encrypt)
#  À exécuter UNE SEULE FOIS, après avoir renseigné .env (DOMAIN, EMAIL).
#  Crée un certificat temporaire, démarre nginx, puis demande le vrai
#  certificat à Let's Encrypt et recharge nginx.
#
#  Usage :  ./init-letsencrypt.sh           (certificat de production)
#           STAGING=1 ./init-letsencrypt.sh (certificat de test, sans quota)
# =====================================================================
set -e

if [ ! -f .env ]; then
  echo "ERREUR : fichier .env manquant. Faites : cp .env.example .env puis éditez-le."
  exit 1
fi
# Charge DOMAIN et EMAIL depuis .env
set -a; . ./.env; set +a

if [ -z "$DOMAIN" ] || [ -z "$EMAIL" ]; then
  echo "ERREUR : DOMAIN et EMAIL doivent être renseignés dans .env."
  exit 1
fi

CONF=./certbot/conf
WWW=./certbot/www
LIVE="$CONF/live/$DOMAIN"
STAGING_ARG=""
if [ "${STAGING:-0}" != "0" ]; then STAGING_ARG="--staging"; fi

echo "### Domaine : $DOMAIN   |   E-mail : $EMAIL"
mkdir -p "$WWW" "$LIVE"

# 1) Paramètres TLS recommandés
if [ ! -e "$CONF/options-ssl-nginx.conf" ] || [ ! -e "$CONF/ssl-dhparams.pem" ]; then
  echo "### Téléchargement des paramètres TLS recommandés..."
  curl -s https://raw.githubusercontent.com/certbot/certbot/main/certbot-nginx/src/certbot_nginx/_internal/tls_configs/options-ssl-nginx.conf > "$CONF/options-ssl-nginx.conf" || true
  curl -s https://raw.githubusercontent.com/certbot/certbot/main/certbot/certbot/ssl-dhparams.pem > "$CONF/ssl-dhparams.pem" || true
fi

# 2) Certificat temporaire (auto-signé) pour permettre à nginx de démarrer
echo "### Création d'un certificat temporaire pour $DOMAIN..."
openssl req -x509 -nodes -newkey rsa:2048 -days 1 \
  -keyout "$LIVE/privkey.pem" -out "$LIVE/fullchain.pem" \
  -subj "/CN=$DOMAIN" >/dev/null 2>&1

# 3) Démarrage de nginx (utilise le certificat temporaire)
echo "### Démarrage de nginx..."
docker compose up -d nginx

# 4) Suppression du certificat temporaire
echo "### Suppression du certificat temporaire..."
docker compose run --rm --entrypoint "rm -rf /etc/letsencrypt/live/$DOMAIN \
  /etc/letsencrypt/archive/$DOMAIN /etc/letsencrypt/renewal/$DOMAIN.conf" certbot

# 5) Demande du vrai certificat à Let's Encrypt
echo "### Demande du certificat Let's Encrypt..."
docker compose run --rm --entrypoint "certbot certonly --webroot -w /var/www/certbot \
  $STAGING_ARG --email $EMAIL -d $DOMAIN --rsa-key-size 2048 --agree-tos --no-eff-email --force-renewal" certbot

# 6) Rechargement de nginx avec le vrai certificat
echo "### Rechargement de nginx..."
docker compose exec nginx nginx -s reload

echo "### Terminé. Lancez maintenant :  docker compose up -d"
