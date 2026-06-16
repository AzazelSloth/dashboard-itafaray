# Déploiement Docker — i-Tafaray (serveur AWS Pivot)

L'application est packagée dans une **image Docker autoportante** : elle contient R, tous les
packages, le code (`app.R`, `prepare_data.R`), les données de démonstration (`data_poc/`) et la
base d'identifiants (`credentials.sqlite`). Rien d'autre à installer sur le serveur que Docker.

---

## 0. Pré-requis : Docker Desktop

Installer **Docker Desktop** (https://www.docker.com/products/docker-desktop/ ou
`brew install --cask docker`), puis **lancer l'application** une première fois (l'icône baleine
doit être active). Vérifier : `docker --version`.

## 1. Construire l'image (sur ta machine)

Depuis le dossier `Dashboard_POC_iTafaray/` :

```bash
docker build -t itafaray-poc:1.0 .
```

> **Mac Apple Silicon (M1/M2/M3)** : les serveurs AWS sont en général en architecture amd64.
> Construire alors l'image pour cette cible, sinon elle ne démarrera pas sur le serveur :
>
> ```bash
> docker build --platform linux/amd64 -t itafaray-poc:1.0 .
> ```

Tester en local avant d'envoyer :

```bash
docker run --rm -p 3838:3838 itafaray-poc:1.0
# Puis ouvrir http://localhost:3838
```

Comptes : `admin` / `itafaray2026` · `pivot` / `pivot2026` · `cirad` / `cirad2026`
(passphrase de la base : `itafaray-poc-2026`).

---

## 2. Envoyer l'image au serveur AWS — deux options

### Option A — fichier image (simple, sans registre)

```bash
# Exporter l'image en archive compressée
docker save itafaray-poc:1.0 | gzip > itafaray-poc_1.0.tar.gz

# Transférer vers le serveur (adapter user/clé/hôte)
scp -i ma-cle.pem itafaray-poc_1.0.tar.gz ubuntu@SERVEUR_AWS:/home/ubuntu/

# Sur le serveur : charger puis lancer
docker load < itafaray-poc_1.0.tar.gz
docker run -d --restart unless-stopped -p 3838:3838 --name itafaray itafaray-poc:1.0
```

### Option B — registre AWS ECR (recommandé si Pivot utilise ECR)

```bash
# Sur ta machine, après `aws ecr get-login-password ... | docker login ...`
docker tag itafaray-poc:1.0 <ID>.dkr.ecr.<region>.amazonaws.com/itafaray-poc:1.0
docker push <ID>.dkr.ecr.<region>.amazonaws.com/itafaray-poc:1.0

# Sur le serveur : docker pull <...>/itafaray-poc:1.0 puis docker run (idem option A)
```

---

## 3. Mettre derrière HTTPS (production)

Le conteneur écoute en **HTTP sur le port 3838**. En production il doit passer derrière un
reverse proxy TLS (nginx, ou un ALB AWS) :

- ALB / Nginx en façade (443, certificat) → redirige vers `localhost:3838`.
- N'expose pas 3838 directement sur Internet ; restreins-le au reverse proxy.

Exemple de bloc nginx :

```nginx
location / {
    proxy_pass http://127.0.0.1:3838;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";   # nécessaire pour le WebSocket Shiny
    proxy_read_timeout 3600s;
}
```

---

## 4. Persister les identifiants / journaux (optionnel)

`shinymanager` écrit les journaux d'accès dans `credentials.sqlite`. Pour que ces journaux
survivent à un redémarrage du conteneur, monte le fichier depuis l'hôte :

```bash
# Pré-requis : copier credentials.sqlite sur le serveur, ex. /opt/itafaray/credentials.sqlite
docker run -d --restart unless-stopped -p 3838:3838 \
  -v /opt/itafaray/credentials.sqlite:/app/credentials.sqlite \
  --name itafaray itafaray-poc:1.0
```

---

## 5. Points à durcir avant la production

- **Changer les mots de passe par défaut** et régénérer `credentials.sqlite` (script `deploy.R`,
  bloc `create_db`).
- **Externaliser la passphrase** : dans `app.R`, remplacer la valeur en dur par
  `Sys.getenv("ITAFARAY_PASSPHRASE")` et la passer au `docker run` via `-e ITAFARAY_PASSPHRASE=...`.
- **Données réelles** : remplacer le contenu de `data_poc/` (jeu synthétique) une fois l'accès et
  la clé `id_signal` en place, puis reconstruire l'image.
- **Mise à jour** : rebuild de l'image avec un nouveau tag (`itafaray-poc:1.1`), re-`save`/`push`,
  `docker stop itafaray && docker rm itafaray`, puis `docker run` la nouvelle version.
