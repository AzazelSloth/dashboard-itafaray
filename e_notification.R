# =====================================================================
#  e_notification.R — Envoi d'alertes via la passerelle E-Notification UGD
#  (SMS / WhatsApp / Email). Réalise le volet « notifications d'alertes »
#  du Livrable 2 (tableaux de bord & alertes intelligentes).
#
#  SÉCURITÉ
#   - Aucun identifiant n'est codé en dur : tout vient de variables
#     d'environnement (fichier .env, NON versionné). Voir .env.example.
#   - Envoi réel DÉSACTIVÉ par défaut : tant que EN_ENABLED != "true",
#     la fonction fait un « essai à blanc » (dry-run) et n'envoie rien.
#   - EN_TEST_TARGET : si renseigné, TOUS les envois sont forcés vers ce
#     seul numéro/email de test (garde-fou pour les démonstrations).
# =====================================================================

# Chargement local du .env (en production Docker, les variables sont déjà
# injectées dans l'environnement ; on ne réécrase rien d'existant).
en_load_dotenv <- function(path = ".env") {
  if (!file.exists(path)) return(invisible())
  lignes <- readLines(path, warn = FALSE)
  for (l in lignes) {
    l <- trimws(l)
    if (l == "" || startsWith(l, "#") || !grepl("=", l)) next
    cle <- trimws(sub("=.*$", "", l))
    val <- sub("^[^=]*=", "", l)
    val <- trimws(val); val <- gsub('^"|"$', "", val)
    if (nzchar(cle) && !nzchar(Sys.getenv(cle))) {
      do.call(Sys.setenv, stats::setNames(list(val), cle))
    }
  }
  invisible()
}
en_load_dotenv()

EN_BASE        <- Sys.getenv("EN_BASE_URL", "https://e-notification-gateway.tsirylab.com/api/v1")
EN_ENABLED     <- tolower(Sys.getenv("EN_ENABLED", "false")) %in% c("true", "1", "oui", "yes")
EN_TEST_TARGET <- trimws(Sys.getenv("EN_TEST_TARGET", ""))

# Configuration lue à chaud (permet de changer le .env sans relancer le code)
en_cfg <- function() {
  list(
    user     = Sys.getenv("EN_USER"),
    pass     = Sys.getenv("EN_PASS"),
    service  = Sys.getenv("EN_SERVICE_ID"),
    agent    = Sys.getenv("EN_AGENT_ID"),
    category = Sys.getenv("EN_CATEGORY_ID"),
    channels = list(
      SMS      = Sys.getenv("EN_CH_SMS"),
      WhatsApp = Sys.getenv("EN_CH_WHATSAPP"),
      Email    = Sys.getenv("EN_CH_EMAIL")
    )
  )
}

# Configuration minimale présente ? (pour griser le bouton si non configuré)
en_configure <- function() {
  cfg <- en_cfg()
  all(nzchar(c(cfg$user, cfg$pass, cfg$service, cfg$agent, cfg$category)))
}

# --- Étape 1 : jeton d'accès (valable 30 min) -----------------------
en_token <- function() {
  cfg <- en_cfg()
  req <- httr2::request(paste0(EN_BASE, "/auth/token")) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(list(username = cfg$user, password = cfg$pass)) |>
    httr2::req_timeout(30)
  resp <- httr2::req_perform(req)
  body <- jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
  tok <- tryCatch(body$data$access_token, error = function(e) NULL)
  if (is.null(tok) || !nzchar(tok)) stop("Jeton d'accès introuvable dans la réponse.")
  tok
}

# --- Étape 2 : envoi -------------------------------------------------
#  title    : titre / objet
#  message  : contenu
#  targets  : vecteur de destinataires (numéros +261... ou emails)
#  canaux   : sous-ensemble de c("SMS","WhatsApp","Email")
#  priority : "LOW" | "NORMAL" | "HIGH" | "URGENT"
#  Retour   : list(ok, dry, status, message, id, targets)
envoyer_notification <- function(title, message, targets,
                                 canaux = c("SMS"), priority = "NORMAL") {
  cfg <- en_cfg()
  canaux <- intersect(canaux, names(cfg$channels))
  if (length(canaux) == 0) return(list(ok = FALSE, dry = FALSE,
    status = "ERREUR", message = "Aucun canal sélectionné."))

  if (!requireNamespace("httr2", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    return(list(ok = FALSE, dry = FALSE, status = "ERREUR",
      message = "Packages httr2 / jsonlite requis pour l'envoi."))
  }

  channel_ids <- unlist(cfg$channels[canaux], use.names = FALSE)
  channel_ids <- channel_ids[nzchar(channel_ids)]
  if (length(channel_ids) == 0) return(list(ok = FALSE, dry = FALSE,
    status = "ERREUR", message = "Identifiants de canal non configurés (.env)."))

  # Garde-fou : si un numéro de test est défini, on force tous les envois dessus
  if (nzchar(EN_TEST_TARGET)) targets <- EN_TEST_TARGET
  targets <- targets[nzchar(trimws(targets))]
  if (length(targets) == 0) return(list(ok = FALSE, dry = FALSE,
    status = "ERREUR", message = "Aucun destinataire."))

  # Essai à blanc tant que l'envoi réel n'est pas explicitement activé
  if (!EN_ENABLED) {
    return(list(ok = TRUE, dry = TRUE, status = "ESSAI À BLANC",
      message = paste0("Aucun message envoyé (EN_ENABLED désactivé). ",
                       "Prêt à envoyer via ", paste(canaux, collapse = "+"),
                       " à : ", paste(targets, collapse = ", ")),
      id = NA_character_, targets = targets))
  }

  if (!en_configure()) return(list(ok = FALSE, dry = FALSE,
    status = "ERREUR", message = "Identifiants E-Notification manquants (.env)."))

  out <- tryCatch({
    token <- en_token()
    payload <- list(
      title = title, message = message, priority = priority,
      serviceId = cfg$service, agentId = cfg$agent, categoryId = cfg$category,
      targetType = "EXTERNAL_CONTACTS",
      channelIds = as.list(channel_ids),
      targets = as.list(targets)
    )
    req <- httr2::request(paste0(EN_BASE, "/service-notification/notifications")) |>
      httr2::req_headers("Authorization" = paste("Bearer", token),
                         "Content-Type" = "application/json") |>
      httr2::req_body_json(payload) |>
      httr2::req_timeout(30)
    resp <- httr2::req_perform(req)
    body <- jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
    list(ok = TRUE, dry = FALSE, status = body$status %||% "PROCESSING",
         message = paste0("Notification transmise (",
                          paste(canaux, collapse = "+"), ") à : ",
                          paste(targets, collapse = ", ")),
         id = body$id %||% NA_character_, targets = targets)
  }, error = function(e) {
    list(ok = FALSE, dry = FALSE, status = "ERREUR",
         message = paste0("Échec de l'envoi : ", conditionMessage(e)),
         id = NA_character_, targets = targets)
  })
  out
}

# petit opérateur de repli
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
