# Déploiement i-Tafaray sur AWS EC2 (Docker + HTTPS)

Guide pas-à-pas pour mettre la plateforme en ligne sur une instance EC2, derrière
un reverse proxy nginx avec certificat HTTPS (Let's Encrypt) renouvelé automatiquement.

Architecture : `Internet → nginx (443/TLS) → app Shiny (3838, réseau interne Docker)`.
Trois conteneurs : **app** (Shiny), **nginx** (proxy + HTTPS), **certbot** (certificats).

---

## 1. Prérequis

- Un **nom de domaine** (ex. `itafaray.exemple.mg`) dont vous pouvez modifier le DNS.
- Un compte AWS.
- Le dossier du projet `Dashboard_POC_iTafaray/` (cette application).

---

## 2. Créer l'instance EC2

1. Console AWS → **EC2 → Launch instance**.
2. **AMI** : Ubuntu Server 24.04 LTS (ou 22.04 LTS).
3. **Type** : `t3.medium` recommandé (2 vCPU, 4 Go RAM). Minimum `t3.small` (2 Go) pour un usage léger.
4. **Stockage** : 20 Go gp3.
5. **Paire de clés** : créez/choisissez une clé SSH (`.pem`).
6. **Groupe de sécurité** — règles entrantes :

   | Type  | Port | Source            | Usage              |
   |-------|------|-------------------|--------------------|
   | SSH   | 22   | Votre IP (/32)    | Administration     |
   | HTTP  | 80   | 0.0.0.0/0         | Redirection + ACME |
   | HTTPS | 443  | 0.0.0.0/0         | Accès application  |

7. Lancez l'instance et notez son **IP publique**.

## 3. Pointer le domaine vers l'instance

Chez votre fournisseur DNS, créez un enregistrement **A** :
`itafaray.exemple.mg → <IP publique EC2>`. Attendez la propagation (`ping votre-domaine`).

> Le certificat HTTPS ne pourra être délivré que si le domaine résout déjà vers l'instance.

## 4. Installer Docker sur l'instance

Connectez-vous puis installez Docker + le plugin Compose :

```bash
ssh -i votre-cle.pem ubuntu@<IP-publique>

sudo apt-get update
sudo apt-get install -y ca-certificates curl
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
sudo chmod a+r /etc/apt/keyrings/docker.asc
echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] \
  https://download.docker.com/linux/ubuntu $(. /etc/os-release && echo $VERSION_CODENAME) stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update
sudo apt-get install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

# Utiliser docker sans sudo (reconnexion nécessaire ensuite)
sudo usermod -aG docker $USER
newgrp docker
docker --version && docker compose version
```

## 5. Copier le projet sur l'instance

Depuis votre poste (dans le dossier qui contient `Dashboard_POC_iTafaray/`) :

```bash
scp -i votre-cle.pem -r Dashboard_POC_iTafaray ubuntu@<IP-publique>:~/itafaray
```

Puis sur l'instance :

```bash
cd ~/itafaray
```

## 6. Configurer et lancer

```bash
# 1) Renseigner le domaine et l'e-mail
cp .env.example .env
nano .env          # DOMAIN=itafaray.exemple.mg   EMAIL=contact@exemple.mg

# 2) Construire l'image de l'application
docker compose build         # ~5–10 min la première fois (compilation des packages R)

# 3) Obtenir le certificat HTTPS (une seule fois)
chmod +x init-letsencrypt.sh
./init-letsencrypt.sh        # astuce : STAGING=1 ./init-letsencrypt.sh pour tester sans quota

# 4) Démarrer toute la pile
docker compose up -d
```

Ouvrez ensuite **https://votre-domaine** : l'écran de connexion i-Tafaray apparaît.

> Identifiants POC (à changer pour une mise en production réelle) :
> `admin / itafaray2026`, `pivot / pivot2026`, `cirad / cirad2026`.

---

## 7. Exploitation

```bash
docker compose ps                 # état des conteneurs
docker compose logs -f app        # logs de l'application
docker compose logs -f nginx      # logs du proxy
docker compose restart app        # redémarrer l'app
docker compose down               # tout arrêter
```

**Mettre à jour l'application** (après modification du code) :

```bash
# recopier les fichiers modifiés (scp) puis :
docker compose build app
docker compose up -d app
```

**Renouvellement HTTPS** : automatique (le conteneur `certbot` tente un renouvellement
toutes les 12 h, nginx se recharge toutes les 6 h). Rien à faire.

**Sauvegarde** : le fichier `credentials.sqlite` (comptes/mots de passe) et le dossier
`certbot/conf/` (certificats) sont à sauvegarder si vous voulez pouvoir restaurer à l'identique.

---

## 8. Données réelles X-Road

Le bouton « Données réelles (X-Road) » ne fonctionne que depuis un réseau autorisé à
joindre le Security Server. Sur une instance EC2 publique, il faut soit que le SS soit
accessible, soit une liaison réseau dédiée (VPN / peering). À défaut, l'application
bascule automatiquement sur les données de démonstration — aucun blocage.

## 9. Coût indicatif

Une instance `t3.medium` à la demande coûte ~30 USD/mois (hors trafic), réductible avec
un plan Savings Plan. `t3.small` (~15 USD/mois) convient pour une démonstration légère.

---

## Dépannage

- **nginx ne démarre pas / erreur de certificat** : vérifiez que le DNS pointe bien vers
  l'instance, puis relancez `./init-letsencrypt.sh`.
- **« too many certificates / rate limit »** : utilisez d'abord `STAGING=1 ./init-letsencrypt.sh`
  pour valider la chaîne, puis relancez en production.
- **Page blanche / déconnexions** : c'est généralement le WebSocket ; il est déjà géré dans
  `nginx/templates/app.conf.template` (`Upgrade`/`Connection`). Vérifiez les logs nginx.
- **Build lent** : normal la première fois (packages R compilés). Les builds suivants sont mis en cache.
