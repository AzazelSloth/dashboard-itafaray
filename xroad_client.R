# =====================================================================
#  Client X-Road (MG / UGD) — récupération de données via le Security Server
#  Protocole REST X-Road (message protocol "r1"). Stack : R.
#
#  Principe : l'application N'APPELLE PAS le producteur directement.
#  Elle envoie une requête HTTP à SON PROPRE Security Server, qui relaie
#  (TLS mutuel) vers le service du producteur. L'app ne gère donc pas
#  l'authentification réseau : tout se joue entre Security Servers.
#
#  Pré-requis (côté gouvernance/technique X-Road, à faire avec l'UGD) :
#   - votre sous-système consommateur est enregistré sur le Security Server ;
#   - le service producteur existe et vous y avez les droits d'accès ;
#   - vous connaissez l'identifiant complet du service (voir ci-dessous).
# =====================================================================

# install.packages(c("httr2", "jsonlite"))
library(httr2)
library(jsonlite)

# ---- 1) Configuration ------------------------------------------------
# Adresse de VOTRE Security Server (souvent une IP/host interne, port 80/443).
XROAD_SECURITY_SERVER <- "https://ss.operator.xroad.digital.gov.mg"

# Identité du CONSOMMATEUR (votre sous-système) — sert à l'en-tête X-Road-Client.
# Format : INSTANCE/CLASSE_MEMBRE/CODE_MEMBRE/SOUS-SYSTEME
CLIENT <- list(
  instance     = "MG",            # instance X-Road Madagascar (ex. "MG")
  member_class = "GOV",           # classe de membre
  member_code  = "UGD-MANAGEMENT",         # code du membre consommateur
  subsystem    = "management-client"       # votre sous-système
)

# Identité du SERVICE PRODUCTEUR à interroger.
# Le producteur est un serveur HAPI FHIR : service_code = "hapifhir",
# et le `path` est le type de ressource FHIR (Observation / Location / Provenance).
SERVICE <- list(
  instance     = "MG",
  member_class = "GOV",
  member_code  = "ONGMedicalePivot",   # producteur (Pivot)
  subsystem    = "SBE",                # sous-système producteur
  service_code = "hapifhir"            # serveur FHIR exposé via X-Road
)

# ---- 2) Fonction générique d'appel REST X-Road -----------------------
#   path  = type de ressource FHIR (ex. "Observation", "Location", "Provenance")
#   query = paramètres de recherche FHIR (ex. list(`_count` = 10))
xroad_get <- function(path = "Observation", query = list()) {
  client_hdr <- paste(CLIENT$instance, CLIENT$member_class,
                      CLIENT$member_code, CLIENT$subsystem, sep = "/")
  # URL X-Road REST : {SS}/r1/{instance}/{classe}/{membre}/{soussys}/{service}[/{version}]/{path}
  svc <- c(SERVICE$instance, SERVICE$member_class, SERVICE$member_code,
           SERVICE$subsystem, SERVICE$service_code)
  if (!is.null(SERVICE$version) && nzchar(SERVICE$version)) svc <- c(svc, SERVICE$version)
  url <- paste0(XROAD_SECURITY_SERVER, "/r1/", paste(svc, collapse = "/"),
                if (nzchar(path)) paste0("/", path) else "")

  req <- request(url) |>
    req_headers(
      "X-Road-Client" = client_hdr,
      "Accept"        = "application/json"
    ) |>
    req_url_query(!!!query) |>
    req_timeout(30) |>
    req_error(is_error = \(resp) FALSE)   # on gère l'erreur nous-mêmes

  resp <- req_perform(req)
  if (resp_status(resp) >= 400) {
    stop(sprintf("X-Road %d : %s", resp_status(resp), resp_body_string(resp)))
  }
  fromJSON(resp_body_string(resp), simplifyVector = TRUE)
}


# ---- 3) Exemples ----------------------------------------------------
# Récupérer un Bundle FHIR d'Observations (limiter la taille avec _count) :
obs <- xroad_get("Observation", query = list(`_count` = 10))
str(obs, max.level = 2)

# Autres ressources exposées :
# loc  <- xroad_get("Location",   query = list(`_count` = 50))
# prov <- xroad_get("Provenance", query = list(`_count` = 10))

# Lire une ressource précise par id :
# obs1 <- xroad_get("Observation/123")
