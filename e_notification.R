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
#   - EN_TEST_TARGET : si renseigné (typiquement en dev), TOUS les envois
#     sont forcés vers cette ou ces cibles de test. S'il est absent ou
#     vide (pré-prod / prod), les destinataires saisis dans l'interface
#     sont utilisés tels quels.
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

en_env_value <- function(key, default = "") {
  val <- Sys.getenv(key, default)
  val <- trimws(val)
  val <- gsub("^[\"']|[\"']$", "", val)
  if (identical(val, "\"\"") || identical(val, "''")) val <- ""
  val
}

en_flag <- function(key, default = FALSE) {
  raw <- en_env_value(key, if (default) "true" else "false")
  tolower(raw) %in% c("true", "1", "oui", "yes")
}

en_targets_from_input <- function(value) {
  if (length(value) == 0 || all(is.na(value))) return(character(0))
  raw <- paste(value, collapse = ",")
  raw <- gsub("[\r\n]+", ",", raw)
  parts <- unlist(strsplit(raw, "[,;]+", perl = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  unique(parts[nzchar(parts)])
}

# Configuration lue à chaud (permet de changer le .env sans relancer le code)
en_cfg <- function() {
  list(
    base_url = en_env_value("EN_BASE_URL", "https://e-notification-gateway.tsirylab.com/api/v1"),
    enabled  = en_flag("EN_ENABLED", FALSE),
    test_target = en_targets_from_input(en_env_value("EN_TEST_TARGET", "")),
    user     = en_env_value("EN_USER"),
    pass     = en_env_value("EN_PASS"),
    service  = en_env_value("EN_SERVICE_ID"),
    agent    = en_env_value("EN_AGENT_ID"),
    category = en_env_value("EN_CATEGORY_ID"),
    channels = list(
      SMS      = en_env_value("EN_CH_SMS"),
      WhatsApp = en_env_value("EN_CH_WHATSAPP"),
      Email    = en_env_value("EN_CH_EMAIL")
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
  req <- httr2::request(paste0(cfg$base_url, "/auth/token")) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(list(username = cfg$user, password = cfg$pass)) |>
    httr2::req_timeout(30) |>
    httr2::req_error(is_error = function(resp) FALSE)   # on lit même les réponses 4xx
  resp <- httr2::req_perform(req)
  txt  <- httr2::resp_body_string(resp)
  body <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
  # Cherche le jeton à plusieurs emplacements possibles
  tok <- NULL
  if (!is.null(body)) {
    cand <- list(
      tryCatch(body$data$access_token, error = function(e) NULL),
      tryCatch(body$access_token,      error = function(e) NULL),
      tryCatch(body$data$accessToken,  error = function(e) NULL),
      tryCatch(body$accessToken,       error = function(e) NULL),
      tryCatch(body$data$token,        error = function(e) NULL),
      tryCatch(body$token,             error = function(e) NULL))
    for (x in cand) if (!is.null(x) && length(x) == 1 && nzchar(x)) { tok <- x; break }
  }
  if (is.null(tok)) {
    st  <- httr2::resp_status(resp)
    msg <- if (!is.null(body) && !is.null(body$message)) body$message else substr(txt, 1, 200)
    stop(paste0("Jeton introuvable (HTTP ", st, "). Réponse : ", msg))
  }
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
  if (length(cfg$test_target) > 0) targets <- cfg$test_target
  targets <- en_targets_from_input(targets)
  if (length(targets) == 0) return(list(ok = FALSE, dry = FALSE,
    status = "ERREUR", message = "Aucun destinataire."))

  # Essai à blanc tant que l'envoi réel n'est pas explicitement activé
  if (!isTRUE(cfg$enabled)) {
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
    req <- httr2::request(paste0(cfg$base_url, "/service-notification/notifications")) |>
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
