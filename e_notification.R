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

en_resp_message <- function(resp, default = "") {
  txt <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  txt <- gsub("<[^>]+>", " ", txt)
  txt <- gsub("[\r\n\t ]+", " ", txt)
  txt <- trimws(txt)
  if (!nzchar(txt)) default else substr(txt, 1, 300)
}

en_pick_first <- function(...) {
  vals <- list(...)
  for (x in vals) {
    if (!is.null(x) && length(x) == 1 && !is.na(x) && nzchar(trimws(as.character(x)))) {
      return(as.character(x))
    }
  }
  NULL
}

en_status_is_final <- function(status) {
  st <- toupper(trimws(as.character(status %||% "")))
  nzchar(st) && !(st %in% c("PENDING", "PROCESSING", "QUEUED", "ACCEPTED", "RECEIVED", "IN_PROGRESS"))
}

en_status_tracking_cfg <- function() {
  wait_sec <- suppressWarnings(as.numeric(en_env_value("EN_STATUS_WAIT_SEC", "12")))
  poll_sec <- suppressWarnings(as.numeric(en_env_value("EN_STATUS_POLL_SEC", "3")))
  if (is.na(wait_sec) || wait_sec < 0) wait_sec <- 12
  if (is.na(poll_sec) || poll_sec <= 0) poll_sec <- 3
  list(wait_sec = wait_sec, poll_sec = poll_sec)
}

en_extract_status <- function(body) {
  en_pick_first(
    tryCatch(body$data$status, error = function(e) NULL),
    tryCatch(body$data$notification$status, error = function(e) NULL),
    tryCatch(body$notification$status, error = function(e) NULL),
    tryCatch(body$data$state, error = function(e) NULL),
    tryCatch(body$state, error = function(e) NULL),
    tryCatch(body$status, error = function(e) NULL)
  )
}

en_extract_id <- function(body) {
  en_pick_first(
    tryCatch(body$data$id, error = function(e) NULL),
    tryCatch(body$data$notification$id, error = function(e) NULL),
    tryCatch(body$notification$id, error = function(e) NULL),
    tryCatch(body$id, error = function(e) NULL)
  )
}

en_get_notification_status <- function(notification_id, token) {
  cfg <- en_cfg()
  endpoints <- c(
    paste0(cfg$base_url, "/service-notification/notifications/", notification_id),
    paste0(cfg$base_url, "/service-notification/notifications/", notification_id, "/status")
  )
  last_error <- NULL
  for (url in endpoints) {
    req <- httr2::request(url) |>
      httr2::req_headers("Authorization" = paste("Bearer", token),
                         "Accept" = "application/json") |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_timeout(20)
    resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
    if (inherits(resp, "error")) {
      last_error <- conditionMessage(resp)
      next
    }
    st <- httr2::resp_status(resp)
    if (st == 404) next
    if (st >= 400) {
      last_error <- paste0("HTTP ", st, " sur ", url, ". Réponse : ", en_resp_message(resp, "réponse vide"))
      next
    }
    txt <- httr2::resp_body_string(resp)
    body <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(body)) {
      last_error <- paste0("Réponse JSON illisible sur ", url)
      next
    }
    status <- en_extract_status(body)
    id <- en_extract_id(body) %||% notification_id
    if (is.null(status)) {
      last_error <- paste0("Statut absent dans la réponse de suivi sur ", url)
      next
    }
    return(list(
      ok = TRUE,
      id = id,
      status = status,
      final = en_status_is_final(status),
      raw = body,
      message = paste0("Statut actuel : ", status)
    ))
  }
  list(ok = FALSE, id = notification_id, status = NULL, final = FALSE,
       raw = NULL, message = last_error %||% "Endpoint de suivi introuvable")
}

en_wait_final_status <- function(notification_id, token) {
  cfg <- en_status_tracking_cfg()
  deadline <- Sys.time() + cfg$wait_sec
  last <- list(ok = FALSE, id = notification_id, status = NULL, final = FALSE,
               raw = NULL, message = "Suivi non disponible")
  repeat {
    last <- en_get_notification_status(notification_id, token)
    if (isTRUE(last$ok) && isTRUE(last$final)) return(last)
    if (Sys.time() >= deadline) return(last)
    Sys.sleep(min(cfg$poll_sec, max(0, as.numeric(deadline - Sys.time(), units = "secs"))))
  }
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
  st   <- httr2::resp_status(resp)
  txt  <- httr2::resp_body_string(resp)
  body <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
  if (st >= 500) {
    stop(paste0(
      "Passerelle E-Notification indisponible (HTTP ", st, ") sur /auth/token. ",
      "Réponse : ", en_resp_message(resp, "réponse vide")
    ))
  }
  if (st %in% c(401, 403)) {
    stop(paste0(
      "Authentification E-Notification refusée (HTTP ", st, "). ",
      "Vérifiez EN_USER et EN_PASS. Réponse : ", en_resp_message(resp, "réponse vide")
    ))
  }
  if (st >= 400) {
    stop(paste0(
      "Échec d'authentification E-Notification (HTTP ", st, ") sur /auth/token. ",
      "Réponse : ", en_resp_message(resp, "réponse vide")
    ))
  }
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
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_timeout(30)
    resp <- httr2::req_perform(req)
    st <- httr2::resp_status(resp)
    if (st >= 400) {
      stop(paste0(
        "Passerelle E-Notification a refusé l'envoi (HTTP ", st, ") sur /service-notification/notifications. ",
        "Réponse : ", en_resp_message(resp, "réponse vide")
      ))
    }
    body <- jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
    notification_id <- en_extract_id(body) %||% NA_character_
    status_initial <- en_extract_status(body) %||% "PROCESSING"
    status_final <- status_initial
    status_message <- paste0("Notification transmise (",
                             paste(canaux, collapse = "+"), ") à : ",
                             paste(targets, collapse = ", "))
    if (!is.na(notification_id) && nzchar(notification_id) && !en_status_is_final(status_initial)) {
      suivi <- en_wait_final_status(notification_id, token)
      if (isTRUE(suivi$ok) && nzchar(suivi$status %||% "")) {
        status_final <- suivi$status
        if (isTRUE(suivi$final)) {
          status_message <- paste0("Notification suivie jusqu'au statut final ",
                                   status_final, " (id ", notification_id, ") à : ",
                                   paste(targets, collapse = ", "))
        } else {
          status_message <- paste0("Notification acceptée par la passerelle (id ",
                                   notification_id, ") ; statut actuel : ",
                                   status_final, " à : ", paste(targets, collapse = ", "))
        }
      } else {
        status_message <- paste0("Notification acceptée par la passerelle (id ",
                                 notification_id, ") ; suivi final indisponible. À : ",
                                 paste(targets, collapse = ", "))
      }
    } else if (!is.na(notification_id) && nzchar(notification_id)) {
      status_message <- paste0("Notification transmise avec statut ",
                               status_final, " (id ", notification_id, ") à : ",
                               paste(targets, collapse = ", "))
    }
    list(ok = TRUE, dry = FALSE, status = status_final,
         message = status_message,
         id = notification_id, targets = targets)
  }, error = function(e) {
    list(ok = FALSE, dry = FALSE, status = "ERREUR",
         message = paste0("Échec de l'envoi : ", conditionMessage(e)),
         id = NA_character_, targets = targets)
  })
  out
}

# petit opérateur de repli
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
