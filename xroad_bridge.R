# =====================================================================
#  Pont X-Road -> tableau de bord i-Tafaray
#  Récupère les données réelles FHIR (via le Security Server) et les mappe
#  au schéma attendu par le dashboard (cf. prepare_data.R).
#
#  État du flux réel exposé par Pivot (juin 2026, modèle révisé) :
#    - Observation : 1 Observation = 1 SIGNAL (id signal-XXX, daté, rattaché à
#      un Location en sujet). Le code de l'Observation porte le type de signal
#      (ex. SEC_1). Les variables du signal sont portées par un tableau
#      "component" (clé-valeur) : case_name (code), types_de_signaux (secteur),
#      Nombre_cas, Nombre_deces, date_detection, is_trie, is_verifie,
#      code_fkt_survenue (fokontany), etc.
#    => On reconstitue le signal complet. Risque/alertes/GPS non exposés :
#       valeurs par défaut. Libellé FR et secteur canonique via REF18 si présent.
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
  if (is.null(obs$entry) || is.null(obs$entry$resource) || NROW(obs$entry$resource) == 0)
    stop("Aucune Observation reçue de X-Road.")
  r <- obs$entry$resource

  # --- Nouveau modèle FHIR (juin 2026) : 1 Observation = 1 signal, dont les
  #     variables sont portées par un tableau "component" (clé-valeur). ---
  comps <- r$component
  if (is.data.frame(comps)) comps <- list(comps)      # cas d'une seule Observation
  if (is.null(comps)) stop("Format X-Road inattendu : composants (component) absents.")
  n <- length(comps)

  # Carte propriété -> valeur pour un signal
  prop_map <- function(comp) {
    if (is.null(comp) || is.null(comp$code) || is.null(comp$code$coding)) return(character(0))
    props <- vapply(comp$code$coding, function(d) {
      cc <- d$code; if (is.null(cc)) NA_character_ else as.character(cc)[1] }, character(1))
    vi <- if (!is.null(comp$valueInteger)) comp$valueInteger else rep(NA, length(props))
    vs <- if (!is.null(comp$valueString))  comp$valueString  else rep(NA_character_, length(props))
    stats::setNames(ifelse(!is.na(vi), as.character(vi), as.character(vs)), props)
  }
  G <- function(pm, key, default = NA_character_) if (key %in% names(pm)) unname(pm[[key]]) else default
  oui_non <- function(x){ x <- tolower(as.character(x)); if (length(x)==0 || is.na(x) || x=="") NA_character_
                          else if (x %in% c("oui","yes","true","1")) "Oui" else "Non" }
  to_date <- function(x){ suppressWarnings(as.Date(substr(as.character(x), 1, 10))) }

  obs_date <- as.Date(r$effectiveDateTime)
  obs_id   <- as.character(r$id)
  obs_loc  <- sub("^Location/", "", as.character(r$subject$reference))
  sig_code_obs <- vapply(seq_len(n), function(i){
    cd <- tryCatch(r$code$coding[[i]]$code, error = function(e) NULL)
    if (is.null(cd)) NA_character_ else as.character(cd)[1] }, character(1))

  rows <- lapply(seq_len(n), function(i){
    pm  <- prop_map(comps[[i]])
    code <- G(pm, "case_name", sig_code_obs[i])
    sec_raw <- tolower(G(pm, "types_de_signaux", ""))
    secteur <- if (grepl("environ", sec_raw)) "Environnement"
               else if (grepl("animal", sec_raw)) "Animal"
               else if (grepl("humain", sec_raw)) "Humain"
               else switch(substr(code, 1, 3), SEC = "Environnement", SAC = "Animal",
                           SHC = "Humain", "Non précisé")
    fok  <- G(pm, "code_fkt_survenue", G(pm, "user_fok_code", obs_loc[i]))
    dsurv <- if (i <= length(obs_date) && !is.na(obs_date[i])) obs_date[i] else to_date(G(pm, "date_detection"))
    data.frame(
      id_signal = if (i <= length(obs_id) && !is.na(obs_id[i]) && nzchar(obs_id[i])) obs_id[i] else paste0("xr-", i),
      code = code, secteur = secteur, fokontany = fok,
      date_de_survenue = dsurv, date_detection = to_date(G(pm, "date_detection")),
      Nombre_cas   = suppressWarnings(as.numeric(G(pm, "Nombre_cas", "0"))),
      Nombre_deces = suppressWarnings(as.numeric(G(pm, "Nombre_deces", "0"))),
      is_trie = oui_non(G(pm, "is_trie")), is_verifie = oui_non(G(pm, "is_verifie")),
      stringsAsFactors = FALSE)
  })
  m <- do.call(rbind, rows)
  m$Nombre_cas[is.na(m$Nombre_cas)]     <- 0
  m$Nombre_deces[is.na(m$Nombre_deces)] <- 0

  # Libellé français + secteur canonique depuis REF18 si disponible
  signal_lbl <- m$code
  if (exists("REF18") && is.data.frame(REF18) && all(c("code","signal","secteur") %in% names(REF18))) {
    lut_sig <- stats::setNames(REF18$signal,  REF18$code)
    lut_sec <- stats::setNames(REF18$secteur, REF18$code)
    signal_lbl <- ifelse(m$code %in% names(lut_sig), unname(lut_sig[m$code]), m$code)
    m$secteur  <- ifelse(m$code %in% names(lut_sec), unname(lut_sec[m$code]), m$secteur)
  }

  niv <- c("Très élevé", "Haute", "Modéré", "Faible", "Non évalué")
  data.frame(
    id_signal            = m$id_signal,
    secteur              = m$secteur,
    code                 = m$code,
    signal               = signal_lbl,
    fokontany            = m$fokontany,
    commune              = NA_character_,
    district             = "Ifanadiana",
    lat = NA_real_, lon = NA_real_,
    date_de_survenue     = m$date_de_survenue,
    date_detection       = m$date_detection,
    date_verification    = as.Date(NA),
    delai_verif          = NA_real_,
    Nombre_cas           = m$Nombre_cas,
    Nombre_deces         = m$Nombre_deces,
    niveau_risque        = factor("Non évalué", levels = niv),
    classification_event = "Non précisé",
    classe_source = NA_character_, is_verifie = m$is_verifie, pertinence = NA_character_,
    doublon = NA_character_, is_trie = m$is_trie, veracite = NA_character_,
    date_triage = as.Date(NA), date_evaluation = as.Date(NA),
    q1 = NA_character_, q2 = NA_character_, q3 = NA_character_,
    a_ete_evalue = FALSE, a_une_alerte = FALSE, alerte_label = NA_character_,
    stringsAsFactors = FALSE
  )
}
