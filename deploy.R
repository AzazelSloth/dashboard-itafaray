# =====================================================================
#  Déploiement / mise à jour sur shinyapps.io — i-Tafaray (PoC)
#  À exécuter depuis le dossier de l'application (Dashboard_POC_iTafaray).
#  Ce script est SÛR À RELANCER : il ne reconfigure pas un compte déjà
#  enregistré et ne réinstalle rien d'inutile.
# =====================================================================

# 1) Installer rsconnect seulement s'il manque
if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")

# 2) Configurer le compte UNIQUEMENT la première fois (placeholders à remplir).
#    Récupérer name / token / secret dans : shinyapps.io > Account > Tokens > Show
if (length(rsconnect::accounts()$name) == 0) {
  rsconnect::setAccountInfo(
    name   = "VOTRE_COMPTE",
    token  = "VOTRE_TOKEN",
    secret = "VOTRE_SECRET"
  )
}

# 3) Pré-générer la base d'identifiants si elle n'existe pas encore
#    (sinon on conserve celle déjà présente, pour des comptes stables).
if (!file.exists("credentials.sqlite")) {
  library(shinymanager)
  credentials <- data.frame(
    user = c("admin", "pivot", "cirad"),
    password = c("itafaray2026", "pivot2026", "cirad2026"),
    admin = c(TRUE, FALSE, FALSE),
    comment = c("Administrateur", "ONG Pivot", "CIRAD"),
    stringsAsFactors = FALSE
  )
  create_db(credentials_data = credentials,
            sqlite_path = "credentials.sqlite",
            passphrase = "itafaray-poc-2026")
}

# 4) Déployer / mettre à jour l'application (tout le dossier courant)
rsconnect::deployApp(
  appDir   = ".",
  appName  = "itafaray-poc",
  appTitle = "i-Tafaray — Tableau de bord One Health (PoC)",
  forceUpdate = TRUE
)

# Après déploiement, l'URL publique reste :
#   https://sa-epr.shinyapps.io/itafaray-poc/
