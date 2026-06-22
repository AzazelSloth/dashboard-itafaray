# =====================================================================
#  Pont X-Road -> tableau de bord i-Tafaray
#  Récupère les données réelles FHIR (via le Security Server) et les mappe
#  au schéma attendu par le dashboard (cf. prepare_data.R).
#
#  État du flux réel exposé par Pivot (juin 2026) :
#    - Observation : « Nombre de cas » (LOINC 75323-6) et « Nombre de décès »
#      (57823-5), datées, rattachées à une Location (sujet).
#    - Location : id + nom (PAS de GPS ni de hiérarchie district).
#    => On peut alimenter cas / décès par lieu + date. Les attributs One Health
#       (type de signal, secteur, risque, alertes, GPS) ne sont pas encore
#       exposés : ils sont mis à des valeurs par défaut en attendant.
#
#  Réseau : ne fonctionne que là où le Security Server est joignable
#  (réseau interne / poste autorisé), pas depuis shinyapps.io.
# =====================================================================

charger_xroad <- function() {
  env_flag_xroad <- function(name, default = FALSE) {
    val <- trimws(Sys.getenv(name, unset = if (default) "true" else "false"))
    tolower(val) %in% c("1", "true", "yes", "on")
  }

  if (!requireNamespace("httr2", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Packages httr2 / jsonlite requis pour la connexion X-Road.")
  }

  SS         <- Sys.getenv("XROAD_BASE_URL", "https://ss.operator.xroad.digital.gov.mg")
  CLIENT_HDR <- Sys.getenv("XROAD_CLIENT_HEADER", "MG/GOV/UGD-MANAGEMENT/management-client")
  SVC        <- Sys.getenv("XROAD_SERVICE_PATH", "MG/GOV/ONGMedicalePivot/SBE/hapifhir")
  INSECURE   <- env_flag_xroad("XROAD_ALLOW_INSECURE_TLS", FALSE)

  if (INSECURE) {
    warning("La verification TLS X-Road est desactivee via XROAD_ALLOW_INSECURE_TLS=true.")
  }

  fetch <- function(resource, count = 1000) {
    url <- paste0(SS, "/r1/", SVC, "/", resource, "?_count=", count)
    req <- httr2::request(url) |>
      httr2::req_headers("X-Road-Client" = CLIENT_HDR, "Accept" = "application/json") |>
      httr2::req_timeout(60)
    if (isTRUE(INSECURE)) req <- httr2::req_options(req, ssl_verifypeer = 0L, ssl_verifyhost = 0L)
    resp <- httr2::req_perform(req)
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
  }

  obs <- fetch("Observation")
  loc <- fetch("Location")
  if (is.null(obs$entry) || is.null(obs$entry$resource) || nrow(obs$entry$resource) == 0)
    stop("Aucune Observation reçue de X-Road.")

  # Table id de Location -> nom (fokontany)
  locmap <- if (!is.null(loc$entry) && !is.null(loc$entry$resource))
    stats::setNames(loc$entry$resource$name, loc$entry$resource$id) else character(0)

  r    <- obs$entry$resource
  type <- r$code$text
  val  <- suppressWarnings(as.numeric(r$valueQuantity$value))
  dte  <- as.Date(r$effectiveDateTime)
  subj <- sub("^Location/", "", r$subject$reference)
  fok  <- ifelse(subj %in% names(locmap), unname(locmap[subj]), subj)

  long <- data.frame(fokontany = fok, date = dte, type = type, val = val,
                     id = r$id, stringsAsFactors = FALSE)

  cas <- long |> dplyr::filter(type == "Nombre de cas") |>
    dplyr::group_by(fokontany, date) |>
    dplyr::summarise(Nombre_cas = sum(val, na.rm = TRUE), id_signal = dplyr::first(id), .groups = "drop")
  dec <- long |> dplyr::filter(type == "Nombre de décès") |>
    dplyr::group_by(fokontany, date) |>
    dplyr::summarise(Nombre_deces = sum(val, na.rm = TRUE), .groups = "drop")

  m <- dplyr::full_join(cas, dec, by = c("fokontany", "date"))
  m$Nombre_cas   <- ifelse(is.na(m$Nombre_cas),   0, m$Nombre_cas)
  m$Nombre_deces <- ifelse(is.na(m$Nombre_deces), 0, m$Nombre_deces)
  m$id_signal    <- ifelse(is.na(m$id_signal), paste0("xr-", seq_len(nrow(m))), m$id_signal)

  niv <- c("Très élevé", "Haute", "Modéré", "Faible", "Non évalué")
  data.frame(
    id_signal            = m$id_signal,
    secteur              = "Non précisé",
    code                 = "",
    signal               = "Cas / décès (FHIR)",
    fokontany            = m$fokontany,
    commune              = NA_character_,
    district             = "Ifanadiana",
    lat = NA_real_, lon = NA_real_,
    date_de_survenue     = m$date,
    date_detection       = m$date,
    date_verification    = as.Date(NA),
    delai_verif          = NA_real_,
    Nombre_cas           = m$Nombre_cas,
    Nombre_deces         = m$Nombre_deces,
    niveau_risque        = factor("Non évalué", levels = niv),
    classification_event = "Non précisé",
    classe_source = NA_character_, is_verifie = NA_character_, pertinence = NA_character_,
    doublon = NA_character_, is_trie = NA_character_, veracite = NA_character_,
    date_triage = as.Date(NA), date_evaluation = as.Date(NA),
    q1 = NA_character_, q2 = NA_character_, q3 = NA_character_,
    a_ete_evalue = FALSE, a_une_alerte = FALSE, alerte_label = NA_character_,
    stringsAsFactors = FALSE
  )
}
