# =====================================================================
#  Pont X-Road -> tableau de bord i-Tafaray
#  Récupère les données réelles FHIR (via le Security Server) et les mappe
#  au schéma attendu par le dashboard (cf. prepare_data.R).
#
#  Modèle réel exposé par Pivot (juin 2026) — TOUT est une Observation :
#    - SIGNAL   : id "signal-XXX", 1 seul identifier (…/signal), code système
#                 …/signaux. Ses variables sont dans un tableau "component"
#                 (clé-valeur) : case_name (code), types_de_signaux (secteur),
#                 Nombre_cas, Nombre_deces, date_detection, is_trie, is_verifie,
#                 code_fkt_survenue (fokontany), etc.
#    - ÉVÈNEMENT (évaluation) : identifier …/evenement présent (2 identifiers :
#                 …/signal + …/evenement), rattaché au signal par la valeur
#                 d'identifier …/signal (et derivedFrom = Observation/signal-XXX).
#                 Porte niveau_risque, classification_event(_name),
#                 risque_mortal_morbid / risque_propagation / mesure_control,
#                 date_evaluation, reponse_initiale_name, et surtout
#                 eval_orientee_alertes ("oui" => le signal a une ALERTE).
#
#  => a_une_alerte  = un évènement lié a eval_orientee_alertes == "oui".
#     niveau_risque = niveau de l'évènement (alerte prioritaire).
#     reponse_initiale = réponse initiale saisie à l'évaluation.
#
#  Système jeune : données irrégulières (certains évènements sans niveau,
#  certains signaux sans case_type, derivedFrom parfois auto-référent).
#  Le parsing est donc défensif : on classe par l'identifier …/evenement et
#  on joint sur la valeur d'identifier …/signal, en lisant les champs là où
#  ils existent.
#
#  Réseau : ne fonctionne que là où le Security Server est joignable
#  (réseau interne / poste autorisé), pas depuis shinyapps.io.
# =====================================================================

charger_xroad <- function() {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

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

  fetch_raw <- function(resource, count = 3000) {
    url <- paste0(SS, "/r1/", SVC, "/", resource, "?_count=", count)
    req <- httr2::request(url) |>
      httr2::req_headers("X-Road-Client" = CLIENT_HDR, "Accept" = "application/json") |>
      httr2::req_timeout(90)
    if (isTRUE(INSECURE)) req <- httr2::req_options(req, ssl_verifypeer = 0L, ssl_verifyhost = 0L)
    resp <- httr2::req_perform(req)
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
  }

  bund <- fetch_raw("Observation")
  entries <- bund$entry
  if (is.null(entries) || length(entries) == 0) stop("Aucune Observation reçue de X-Road.")
  resources <- lapply(entries, function(e) e$resource)
  resources <- Filter(function(r) identical(r$resourceType, "Observation"), resources)
  if (length(resources) == 0) stop("Aucune Observation exploitable reçue de X-Road.")

  # ------------------------------------------------------------------ helpers
  first_coding_code <- function(x) {
    v <- tryCatch(x$coding[[1]]$code, error = function(e) NULL)
    if (is.null(v) || !length(v)) NA_character_ else as.character(v)[1]
  }

  comp_kv <- function(res) {
    comp <- res$component
    if (is.null(comp) || !length(comp)) return(list())
    out <- list()
    for (cp in comp) {
      code <- tryCatch(cp$code$coding[[1]]$code, error = function(e) NULL)
      if (is.null(code) || !length(code) || is.na(code[1])) next
      code <- as.character(code)[1]
      v <- NULL
      for (f in c("valueString", "valueCode", "valueInteger", "valueBoolean", "valueDecimal", "valueDateTime"))
        if (!is.null(cp[[f]]) && length(cp[[f]])) { v <- cp[[f]]; break }
      if (is.null(v) && !is.null(cp$valueCodeableConcept))
        v <- tryCatch(cp$valueCodeableConcept$coding[[1]]$code,
                      error = function(e) cp$valueCodeableConcept$text)
      out[[code]] <- if (is.null(v) || !length(v)) NA else v[[1]]
    }
    out
  }

  ident_val <- function(res, suffix) {
    ids <- res$identifier
    if (is.null(ids) || !length(ids)) return(NA_character_)
    for (id in ids) {
      sys <- id$system
      if (!is.null(sys) && length(sys) && grepl(paste0(suffix, "$"), sys[1])) {
        v <- id$value
        return(if (is.null(v) || !length(v)) NA_character_ else as.character(v)[1])
      }
    }
    NA_character_
  }

  G <- function(pm, key, default = NA_character_) {
    v <- pm[[key]]
    if (is.null(v) || !length(v) || is.na(v[1])) default else as.character(v)[1]
  }
  oui_non <- function(x) {
    x <- tolower(as.character(x))
    if (length(x) == 0 || is.na(x) || x == "") NA_character_
    else if (x %in% c("oui", "yes", "true", "1")) "Oui" else "Non"
  }
  num0 <- function(x) suppressWarnings(as.numeric(as.character(x)))
  to_date <- function(x) {
    x <- as.character(x)
    if (length(x) == 0 || is.na(x) || x == "") return(as.Date(NA))
    if (grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", x)) as.Date(x, format = "%d/%m/%Y")
    else suppressWarnings(as.Date(substr(x, 1, 10)))
  }
  to_when <- function(x) {
    x <- as.character(x %||% NA)
    if (is.na(x) || x == "") return(as.POSIXct(NA))
    suppressWarnings(as.POSIXct(sub("Z$", "", substr(x, 1, 19)), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  }
  norm_niv <- function(x) {
    x <- tolower(trimws(as.character(x)))
    if (length(x) == 0 || is.na(x) || x == "") return("Non évalué")
    if (grepl("tr[eè]s", x) && grepl("[eé]lev", x)) return("Très élevé")
    if (grepl("haut|[eé]lev", x)) return("Haute")
    if (grepl("mod[eé]r", x)) return("Modéré")
    if (grepl("faibl", x)) return("Faible")
    "Non évalué"
  }
  coa <- function(a, b, d) { out <- a; out[is.na(out)] <- b[is.na(out)]; out[is.na(out)] <- d; out }
  # Le flux réel code les signaux SHC_/SAC_/SEC_ ; le dashboard (REF18, démo)
  # utilise H/A/E. On aligne pour que Par signal, Indicateurs et REF18 matchent.
  norm_code <- function(code) {
    if (is.null(code) || !length(code) || is.na(code)) return(NA_character_)
    u <- toupper(code)
    m <- regmatches(u, regexec("^(SHC|SAC|SEC)_?([0-9]+)$", u))[[1]]
    if (length(m) == 3) return(paste0(switch(m[2], SHC = "H", SAC = "A", SEC = "E"), m[3]))
    as.character(code)
  }
  # Les 18 signaux prioritaires (nomenclature du dashboard). Tout code hors de
  # cette liste n'est pas un signal (évaluation, alerte, secteur, instance…).
  VALID_CODES <- c(paste0("H", 1:5), paste0("A", 1:4), paste0("E", 1:9))
  # Le type du signal est porté par Observation.code ; case_name est un
  # identifiant d'instance (SEC_107…), à n'utiliser qu'en dernier recours.
  pick_code <- function(obs_code, case_name) {
    for (cc in c(obs_code, case_name)) {
      nc <- norm_code(cc)
      if (!is.na(nc) && nc %in% VALID_CODES) return(nc)
    }
    norm_code(obs_code %||% case_name)
  }
  # Référentiel des 18 signaux (code -> secteur, libellé) — repli quand REF18
  # (global de l'app) n'existe pas, ex. lors de l'ingestion via ingest_xroad.R.
  REF18_LOCAL <- data.frame(
    code    = c(paste0("A", 1:4), paste0("E", 1:9), paste0("H", 1:5)),
    secteur = c(rep("Animal", 4), rep("Environnement", 9), rep("Humain", 5)),
    signal  = c(
      "Avortements en série (ruminants)", "Mortalité animale inexpliquée",
      "Maladie groupée des animaux", "Augmentation de vecteurs (puces)",
      "Changement couleur/goût/odeur de l'eau", "Feux multiples",
      "Odeur inhabituelle (pollution)", "Trace d'exploitation forestière",
      "Déchets cumulés", "Cadavres d'animaux sauvages", "Maladies des plants",
      "Espèces envahissantes", "Tarissement puits / rivières",
      "Signes/symptômes similaires chez ≥2 personnes", "Décès groupés humains",
      "Morsures / griffures multiples", "Absentéisme scolaire",
      "Déplacements anormaux de population"),
    stringsAsFactors = FALSE)

  # ------------------------------------------------- séparation signaux / évènements
  is_event <- vapply(resources, function(r) {
    if (!is.na(ident_val(r, "/evenement")) || !is.na(ident_val(r, "/alerte"))) return(TRUE)
    if (grepl("^(event|evenement|evaluation|alerte)", tolower(r$id %||% ""))) return(TRUE)
    pm <- comp_kv(r)
    !is.null(pm[["eval_orientee_alertes"]]) || !is.null(pm[["niveau_risque"]])
  }, logical(1))
  sig_res  <- resources[!is_event]
  evt_res  <- resources[is_event]
  if (length(sig_res) == 0) stop("Aucun signal (Observation) reçu de X-Road.")

  # ------------------------------------------------- évènements -> résumé par signal
  sev_rank <- c("Très élevé" = 4, "Haute" = 3, "Modéré" = 2, "Faible" = 1, "Non évalué" = 0)
  ev_rows <- lapply(evt_res, function(r) {
    pm  <- comp_kv(r)
    niv <- G(pm, "niveau_risque", NA)
    data.frame(
      jk           = ident_val(r, "/signal"),
      ev_niveau    = norm_niv(niv),
      ev_eval      = !is.na(niv) && nzchar(niv),
      ev_class     = G(pm, "classification_event", NA),
      ev_class_nm  = G(pm, "classification_event_name", NA),
      ev_alerte    = tolower(G(pm, "eval_orientee_alertes", "")) %in% c("oui", "yes", "true", "1"),
      ev_reponse   = G(pm, "reponse_initiale_name", NA),
      ev_q1        = oui_non(G(pm, "risque_mortal_morbid")),
      ev_q2        = oui_non(G(pm, "risque_propagation")),
      ev_q3        = oui_non(G(pm, "mesure_control")),
      ev_date_eval = to_date(G(pm, "date_evaluation")),
      ev_when      = to_when(r$issued %||% r$meta$lastUpdated),
      stringsAsFactors = FALSE)
  })
  EV <- if (length(ev_rows)) do.call(rbind, ev_rows) else data.frame()
  ev_summary <- data.frame()
  if (nrow(EV) > 0) {
    EV <- EV[!is.na(EV$jk), , drop = FALSE]
    EV$sev <- unname(sev_rank[EV$ev_niveau]); EV$sev[is.na(EV$sev)] <- 0
    parts <- split(EV, EV$jk)
    ev_summary <- do.call(rbind, lapply(names(parts), function(k) {
      s    <- parts[[k]]
      pref <- s[s$ev_alerte, , drop = FALSE]      # priorité aux évènements porteurs d'alerte
      pool <- if (nrow(pref) > 0) pref else s
      pool <- pool[order(-pool$sev, -as.numeric(pool$ev_when)), , drop = FALSE]
      rep_row   <- pool[1, ]
      rep_class <- if (!is.na(rep_row$ev_class_nm)) rep_row$ev_class_nm else rep_row$ev_class
      al_rep    <- s[s$ev_alerte & !is.na(s$ev_reponse), , drop = FALSE]
      data.frame(
        jk                   = k,
        a_une_alerte         = any(s$ev_alerte),
        a_ete_evalue         = any(s$ev_eval),
        niveau_risque        = rep_row$ev_niveau,
        classification_event = rep_class,
        alerte_label         = if (any(s$ev_alerte)) rep_class else NA_character_,
        reponse_initiale     = if (nrow(al_rep) > 0) al_rep$ev_reponse[1] else rep_row$ev_reponse,
        q1 = rep_row$ev_q1, q2 = rep_row$ev_q2, q3 = rep_row$ev_q3,
        date_evaluation      = rep_row$ev_date_eval,
        stringsAsFactors = FALSE)
    }))
  }

  # ------------------------------------------------- signaux -> table de base
  sig_rows <- lapply(sig_res, function(r) {
    pm       <- comp_kv(r)
    sid      <- ident_val(r, "/signal")
    obs_code <- first_coding_code(r$code)
    code     <- pick_code(obs_code, G(pm, "case_name", NA))
    sec_raw  <- tolower(G(pm, "types_de_signaux", ""))
    sec_ltr  <- if (is.na(code) || !nzchar(code)) "" else substr(toupper(code), 1, 1)
    secteur  <- if (grepl("environ", sec_raw)) "Environnement"
                else if (grepl("animal", sec_raw)) "Animal"
                else if (grepl("humain", sec_raw)) "Humain"
                else switch(sec_ltr, H = "Humain", A = "Animal", E = "Environnement", "Non précisé")
    subj_ref <- tryCatch(r$subject$reference, error = function(e) NA_character_)
    # Affichage : on privilégie le nom du lieu (lieu de survenue) au code fokontany.
    fok      <- G(pm, "lieu_de_survenue",
                  G(pm, "code_fkt_survenue", G(pm, "user_fok_code",
                    sub("^Location/", "", (subj_ref %||% NA)))))
    classif  <- { nm <- G(pm, "classification_event_name", NA)
                  if (!is.na(nm)) nm else G(pm, "classification_event", NA) }
    # Coordonnées : composant "gps" ("lat lon alt acc") ou gps_latitude/gps_longitude
    lat <- suppressWarnings(as.numeric(G(pm, "gps_latitude", NA)))
    lon <- suppressWarnings(as.numeric(G(pm, "gps_longitude", NA)))
    if (is.na(lat) || is.na(lon)) {
      g <- G(pm, "gps", NA)
      if (!is.na(g)) {
        pp <- suppressWarnings(as.numeric(strsplit(trimws(g), "[[:space:]]+")[[1]]))
        if (length(pp) >= 2) { if (is.na(lat)) lat <- pp[1]; if (is.na(lon)) lon <- pp[2] }
      }
    }
    data.frame(
      jk                = if (!is.na(sid)) sid else (r$id %||% NA_character_),
      id_signal         = if (!is.null(r$id)) as.character(r$id) else paste0("xr-", sid),
      code              = code,
      secteur           = secteur,
      fokontany         = fok,
      lat               = lat,
      lon               = lon,
      date_de_survenue  = to_date(r$effectiveDateTime %||% G(pm, "date_detection")),
      date_detection    = to_date(G(pm, "date_detection")),
      date_verification = to_date(G(pm, "date_verification")),
      delai_verif       = num0(G(pm, "diff_date_detection_verification", NA)),
      Nombre_cas        = num0(G(pm, "Nombre_cas", "0")),
      Nombre_deces      = num0(G(pm, "Nombre_deces", "0")),
      is_trie           = oui_non(G(pm, "is_trie")),
      is_verifie        = oui_non(G(pm, "is_verifie")),
      pertinence        = G(pm, "pertinence", NA),
      doublon           = G(pm, "doublon", NA),
      veracite          = G(pm, "veracite", NA),
      classe_source     = G(pm, "classe_source", NA),
      classif_sig       = classif,
      date_triage       = to_date(G(pm, "date_triage")),
      when              = to_when(r$issued %||% r$meta$lastUpdated),
      stringsAsFactors  = FALSE)
  })
  m <- do.call(rbind, sig_rows)
  # 1 ligne par signal : garder la version la plus récente
  m <- m[order(m$jk, as.numeric(m$when)), , drop = FALSE]
  m <- m[!duplicated(m$jk, fromLast = TRUE), , drop = FALSE]
  m$Nombre_cas[is.na(m$Nombre_cas)]     <- 0
  m$Nombre_deces[is.na(m$Nombre_deces)] <- 0
  # Ne garder que les vrais signaux (18 codes) ; le reste (évaluations, alertes,
  # secteurs, instances) est écarté du grain "signal".
  m <- m[!is.na(m$code) & m$code %in% VALID_CODES, , drop = FALSE]
  if (nrow(m) == 0) stop("Aucun signal valide (18 codes) après filtrage X-Road.")

  # ------------------------------------------------- jointure évènements -> signaux
  if (nrow(ev_summary) > 0) {
    m <- merge(m, ev_summary, by = "jk", all.x = TRUE)
  } else {
    m$a_une_alerte <- FALSE; m$a_ete_evalue <- FALSE
    m$niveau_risque <- NA_character_; m$classification_event <- NA_character_
    m$alerte_label <- NA_character_; m$reponse_initiale <- NA_character_
    m$q1 <- NA_character_; m$q2 <- NA_character_; m$q3 <- NA_character_
    m$date_evaluation <- as.Date(NA)
  }

  # Libellé français + secteur canonique : REF18 (global de l'app) si présent,
  # sinon le référentiel local (cas de l'ingestion hors app).
  RF <- if (exists("REF18") && is.data.frame(REF18) && all(c("code", "signal", "secteur") %in% names(REF18)))
          REF18 else REF18_LOCAL
  lut_sig <- stats::setNames(RF$signal,  RF$code)
  lut_sec <- stats::setNames(RF$secteur, RF$code)
  signal_lbl <- ifelse(m$code %in% names(lut_sig), unname(lut_sig[m$code]), m$code)
  m$secteur  <- ifelse(m$code %in% names(lut_sec), unname(lut_sec[m$code]), m$secteur)

  niv <- c("Très élevé", "Haute", "Modéré", "Faible", "Non évalué")
  data.frame(
    id_signal            = m$id_signal,
    secteur              = m$secteur,
    code                 = m$code,
    signal               = signal_lbl,
    fokontany            = ifelse(is.na(m$fokontany) | m$fokontany == "", "Non précisé", m$fokontany),
    commune              = NA_character_,
    district             = "Ifanadiana",
    lat = m$lat, lon = m$lon,
    date_de_survenue     = m$date_de_survenue,
    date_detection       = m$date_detection,
    date_verification    = m$date_verification,
    delai_verif          = m$delai_verif,
    Nombre_cas           = m$Nombre_cas,
    Nombre_deces         = m$Nombre_deces,
    niveau_risque        = factor(ifelse(is.na(m$niveau_risque), "Non évalué", as.character(m$niveau_risque)), levels = niv),
    classification_event = coa(m$classification_event, m$classif_sig, "Non précisé"),
    classe_source        = m$classe_source,
    is_verifie           = m$is_verifie,
    pertinence           = m$pertinence,
    doublon              = m$doublon,
    is_trie              = m$is_trie,
    veracite             = m$veracite,
    date_triage          = m$date_triage,
    date_evaluation      = m$date_evaluation,
    q1 = m$q1, q2 = m$q2, q3 = m$q3,
    a_ete_evalue         = ifelse(is.na(m$a_ete_evalue), FALSE, m$a_ete_evalue),
    a_une_alerte         = ifelse(is.na(m$a_une_alerte), FALSE, m$a_une_alerte),
    alerte_label         = m$alerte_label,
    reponse_initiale     = m$reponse_initiale,
    stringsAsFactors = FALSE
  )
}
