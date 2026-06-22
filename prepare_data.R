# =====================================================================
#  i-Tafaray — Préparation des données pour le tableau de bord
#  Lit les 3 tables (signaux / evenement_sbe / alerte), les joint via
#  id_signal, et renvoie une table au grain « signal » prête à l'emploi.
#  Fonctionne sur le jeu de démonstration (dossier data_poc) comme sur
#  un futur export réel de même structure.
# =====================================================================

required <- c("readxl", "dplyr", "stringr")
missing_pkgs <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    paste0(
      "Packages R manquants: ", paste(missing_pkgs, collapse = ", "),
      ". Reconstruisez l'image Docker ou installez-les explicitement avant le lancement."
    ),
    call. = FALSE
  )
}
suppressPackageStartupMessages({ library(readxl); library(dplyr); library(stringr) })

NIVEAUX_RISQUE <- c("Très élevé", "Haute", "Modéré", "Faible", "Non évalué")

.clean_names <- function(df) {
  n <- tolower(str_replace_all(names(df), "[^A-Za-z0-9]+", "_"))
  names(df) <- make.unique(str_replace_all(n, "^_|_$", ""), sep = "_")
  df
}
.parse_date <- function(x) {
  x <- as.character(x); x[x %in% c("---", "", "NA")] <- NA
  as.Date(x)
}
.num <- function(x) { x <- as.character(x); x[x %in% c("---", "", "NA")] <- NA; suppressWarnings(as.numeric(x)) }
.txt <- function(x) { x <- as.character(x); x[x %in% c("---", "", "NA", "—")] <- NA; x }

charger_donnees <- function(dirs = c("data_poc", "../Jeu_de_donnees_POC", "data", ".")) {
  trouver <- function(prefixe) {
    for (d in dirs) {
      if (!dir.exists(d)) next
      f <- list.files(d, pattern = paste0("^", prefixe, ".*\\.xlsx$"),
                      full.names = TRUE, ignore.case = TRUE)
      if (length(f) > 0) return(f[which.max(file.info(f)$mtime)])
    }
    stop(paste0("Fichier introuvable pour « ", prefixe,
                " » (cherché dans : ", paste(dirs, collapse = ", "), ")."))
  }
  sig <- .clean_names(read_excel(trouver("signaux"),       guess_max = 10000))
  evt <- .clean_names(read_excel(trouver("evenement"),     guess_max = 10000))
  alr <- .clean_names(read_excel(trouver("alerte"),        guess_max = 10000))

  # Évaluation (1 ligne par signal)
  eval_min <- evt %>%
    transmute(id_signal = as.character(id_signal),
              eval_niveau = .txt(niveau_risque),
              eval_classification = .txt(classification_event),
              eval_q1 = .txt(risque_mortal_morbid),
              eval_q2 = .txt(risque_propagation),
              eval_q3 = .txt(mesure_control),
              eval_date = .parse_date(date_evaluation)) %>%
    filter(!is.na(id_signal)) %>% distinct(id_signal, .keep_all = TRUE)

  # Alertes (résumé par signal)
  alr_min <- alr %>%
    mutate(id_signal = as.character(id_signal)) %>%
    filter(!is.na(id_signal), id_signal != "---") %>%
    group_by(id_signal) %>%
    summarise(nb_alertes = n(),
              alerte_label = paste(unique(na.omit(alerte_label)), collapse = " ; "),
              .groups = "drop")

  signaux_clean <- sig %>%
    mutate(id_signal = as.character(id_signal)) %>%
    left_join(eval_min, by = "id_signal") %>%
    left_join(alr_min,  by = "id_signal") %>%
    transmute(
      id_signal = id_signal,
      secteur   = secteur,
      code      = code_signaux,
      signal    = signaux_label,
      fokontany = .txt(user_fokontany),
      commune   = .txt(user_commune),
      district  = .txt(user_district),
      lat = .num(lat), lon = .num(lon),
      date_de_survenue = .parse_date(date_de_survenue),
      date_detection   = .parse_date(date_detection),
      date_verification = .parse_date(date_verification),
      delai_verif = .num(diff_date_detection_verification),
      Nombre_cas = .num(nombre_cas), Nombre_deces = .num(nombre_deces),
      niveau_risque = factor(ifelse(is.na(eval_niveau), "Non évalué", eval_niveau),
                             levels = NIVEAUX_RISQUE),
      classification_event = coalesce(.txt(classification_event), eval_classification, "Non précisé"),
      classe_source = .txt(classe_source),
      is_verifie = .txt(is_verifie), pertinence = .txt(pertinence), doublon = .txt(doublon),
      is_trie = .txt(is_trie), veracite = .txt(veracite),
      date_triage = .parse_date(date_triage), date_evaluation = eval_date,
      q1 = eval_q1, q2 = eval_q2, q3 = eval_q3,
      a_ete_evalue = !is.na(eval_niveau),
      a_une_alerte = !is.na(nb_alertes) & nb_alertes > 0,
      alerte_label = alerte_label
    ) %>%
    mutate(secteur = ifelse(is.na(secteur) | secteur == "", "Non précisé", secteur),
           fokontany = ifelse(is.na(fokontany), "Non précisé", fokontany),
           Nombre_cas = ifelse(is.na(Nombre_cas), 0, Nombre_cas),
           Nombre_deces = ifelse(is.na(Nombre_deces), 0, Nombre_deces))

  signaux_clean
}
