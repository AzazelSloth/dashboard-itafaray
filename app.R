# =====================================================================
#  i-Tafaray — Tableau de bord central One Health
#  PREUVE DE CONCEPT — données de DÉMONSTRATION (synthétiques)
#  Branché sur le jeu de données à 6 scénarios (H5N1, Peste, Rage,
#  Mpox, Ebola, contamination hydrique) — voir Scenarios_One_Health.md
# 
#  Lancement :  shiny::runApp()   (depuis ce dossier)
# =====================================================================

required <- c("shiny", "shinydashboard", "dplyr", "ggplot2", "DT",
              "leaflet", "scales", "lubridate", "readxl", "stringr", "echarts4r",
              "shinymanager", "gridExtra", "jsonlite")
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
suppressPackageStartupMessages({
  library(shiny); library(shinydashboard); library(dplyr); library(ggplot2)
  library(DT); library(leaflet); library(scales); library(lubridate); library(echarts4r)
  library(shinymanager); library(gridExtra)
})

## ---- Internationalisation custom (FR clé · EN · MG) ----
source("i18n_setup.R")

source("prepare_data.R")
# Pont X-Road (données réelles) — point d'intégration, optionnel
if (file.exists("xroad_bridge.R")) source("xroad_bridge.R")
# Module d'envoi d'alertes (E-Notification UGD : SMS / WhatsApp / Email)
if (file.exists("e_notification.R")) source("e_notification.R")

DATA <- tryCatch(
  charger_donnees(),
  error = function(e) { stop(paste0(
    "Impossible de charger les données.\n", conditionMessage(e),
    "\nPlacez signaux_*.xlsx / evenement_sbe_*.xlsx / alerte_*.xlsx dans 'data_poc/'.")) }
)
DATA_SIM <- DATA   # jeu de démonstration (synthétique) — référence pour la bascule de source

## ---- Cache X-Road (alimenté par ingest_xroad.R, relu en auto-refresh) ----
# En production, pointer XROAD_CACHE_PATH vers un volume persistant monté dans le conteneur.
XROAD_CACHE <- Sys.getenv("XROAD_CACHE_PATH", unset = "data_poc/xroad_cache.rds")
xroad_cache_mtime <- function()
  if (file.exists(XROAD_CACHE)) as.numeric(file.info(XROAD_CACHE)$mtime) else 0
lire_cache_xroad <- function() {
  if (!file.exists(XROAD_CACHE)) return(NULL)
  tryCatch(readRDS(XROAD_CACHE), error = function(e) NULL)
}

## ---- Données climatiques du district (extraites de MDG via extract_climate_mdg.R) ----
CLIMATE <- local({
  cands <- c("data_poc/climate_ifanadiana.csv", "data/climate_ifanadiana.csv", "climate_ifanadiana.csv")
  f <- cands[file.exists(cands)][1]
  if (is.na(f)) return(NULL)
  cl <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8"),
                 error = function(e) NULL)
  if (is.null(cl)) return(NULL)
  cl$date <- as.Date(cl$date)
  # Reconstituer l'observé depuis normale + anomalie si la colonne directe est vide
  if (!"pluie_mm" %in% names(cl)) cl$pluie_mm <- NA_real_
  if (!"temp_moy" %in% names(cl)) cl$temp_moy <- NA_real_
  if (all(c("pluie_normale", "pluie_anomalie_pct") %in% names(cl)))
    cl$pluie_mm <- ifelse(is.na(cl$pluie_mm) & !is.na(cl$pluie_normale) & !is.na(cl$pluie_anomalie_pct),
                          cl$pluie_normale * (1 + cl$pluie_anomalie_pct / 100), cl$pluie_mm)
  if (all(c("temp_normale", "temp_anomalie") %in% names(cl)))
    cl$temp_moy <- ifelse(is.na(cl$temp_moy) & !is.na(cl$temp_normale) & !is.na(cl$temp_anomalie),
                          cl$temp_normale + cl$temp_anomalie, cl$temp_moy)
  cl
})

## ---- Données environnementales / faune — patrouilles SMART (couche autonome) ----
SMART_ENV <- local({
  cands <- c("data_poc/smart_env.csv", "data/smart_env.csv", "smart_env.csv")
  f <- cands[file.exists(cands)][1]
  if (is.na(f)) return(NULL)
  s <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8"),
                error = function(e) NULL)
  if (is.null(s) || nrow(s) == 0) return(NULL)
  s$date <- suppressWarnings(as.Date(s$date))
  for (cc in c("lat", "lon", "n_bonne_sante", "n_malades", "n_morts"))
    if (cc %in% names(s)) s[[cc]] <- suppressWarnings(as.numeric(s[[cc]]))
  # --- Typologie explicite des menaces (par ordre de priorité) ---
  nz <- function(x) !is.na(x) & nzchar(x)
  s$menace_type <- with(s, ifelse(
    categorie1 %in% c("Braconnage", "Partie d'animal") |
      categorie2 %in% c("Piégeage", "Chasse active", "Collecte faune"),
      "Chasse / braconnage",
    ifelse(type_signe %in% c("Coupe de machette", "Bois de chauffage", "Bois travaillé"),
      "Exploitation forestière",
    ifelse(type_signe %in% "Ordures",
      "Pollution / déchets",
    ifelse(categorie2 %in% "Divagation d'animaux domestiques" | nz(animaux_rencontres),
      "Contact domestique",
    ifelse(categorie1 %in% "Circulation dans l'AP" | type_signe %in% "Voix",
      "Présence humaine", NA_character_))))))
  # Quels types comptent comme « menace » (présence humaine incluse, choix utilisateur)
  MENACE_INCLUT_PRESENCE <- TRUE
  types_menace <- c("Chasse / braconnage", "Exploitation forestière",
                    "Pollution / déchets", "Contact domestique",
                    if (MENACE_INCLUT_PRESENCE) "Présence humaine")
  # Types pertinents pour le One Health (risque d'émergence / contact inter-espèces)
  oh_menace <- c("Chasse / braconnage", "Exploitation forestière", "Contact domestique", "Pollution / déchets")
  s$est_menace   <- !is.na(s$menace_type) & s$menace_type %in% types_menace
  s$oh_pertinent <- !is.na(s$menace_type) & s$menace_type %in% oh_menace
  s$contact_dom  <- (s$categorie2 %in% "Divagation d'animaux domestiques") | nz(s$animaux_rencontres)
  s$annee <- ifelse(is.na(s$date), NA_character_, format(s$date, "%Y"))
  s
})
CORR_ENV <- local({
  cands <- c("data_poc/correspondance_sbe_env.csv", "data/correspondance_sbe_env.csv", "correspondance_sbe_env.csv")
  f <- cands[file.exists(cands)][1]
  if (is.na(f)) return(NULL)
  tryCatch(utils::read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8"), error = function(e) NULL)
})
# Palette des catégories d'observation SMART
COL_ENVCAT <- c("Observation directe" = "#5E8B6A", "Circulation dans l'AP" = "#35618E",
                "Braconnage" = "#9E2A2B", "Partie d'animal" = "#C2703D")
# Palette des types de menace (typologie One Health)
COL_MENACE <- c("Chasse / braconnage" = "#9E2A2B", "Exploitation forestière" = "#C2703D",
                "Contact domestique" = "#B07A3C", "Pollution / déchets" = "#8A6D3B",
                "Présence humaine" = "#9AA3AB")
ENV_SECTEURS <- if (!is.null(SMART_ENV))
  c("Tous", sort(unique(SMART_ENV$secteur[nzchar(SMART_ENV$secteur)]))) else "Tous"
ENV_ANNEES <- if (!is.null(SMART_ENV))
  c("Toutes", sort(unique(SMART_ENV$annee[!is.na(SMART_ENV$annee)]), decreasing = TRUE)) else "Toutes"
# Année par défaut du sélecteur SMART : la plus récente (plutôt que « Toutes »)
ENV_ANNEE_DEFAUT <- if (!is.null(SMART_ENV) && any(!is.na(SMART_ENV$annee)))
  as.character(max(suppressWarnings(as.integer(SMART_ENV$annee)), na.rm = TRUE)) else "Toutes"

## ---- Palettes (sobres / institutionnelles) ----
INK <- "#26333F"; ACCENT <- "#1e3a5f"
COL_SECTEUR <- c("Humain" = "#35618E", "Animal" = "#B07A3C",
                 "Environnement" = "#5E8B6A", "Non précisé" = "#9AA3AB")
COL_RISQUE  <- c("Très élevé" = "#9E2A2B", "Haute" = "#D26A3D",
                 "Modéré" = "#E8C14A", "Faible" = "#9FB1BC", "Non évalué" = "#C9CDD2")
SECTEURS <- intersect(names(COL_SECTEUR), unique(DATA$secteur))
DRANGE <- range(DATA$date_de_survenue, na.rm = TRUE)
REF18  <- DATA %>% distinct(secteur, code, signal) %>% arrange(secteur, code)
SEV <- c("Non évalué" = 0, "Faible" = 1, "Modéré" = 2, "Haute" = 3, "Très élevé" = 4)
RISK_LAB <- setNames(names(SEV), as.character(SEV))
translate_signal <- function(x, lang = "fr") i18n_vec(x, lang)
format_signal_label <- function(code, signal, lang = "fr", sep = " — ")
  paste0(code, sep, translate_signal(signal, lang))
translate_alert <- function(x, lang = "fr") i18n_vec(x, lang)
asset_version <- function(path) {
  info <- suppressWarnings(file.info(path))
  if (is.na(info$mtime[1])) return("0")
  format(as.POSIXct(info$mtime[1], tz = "UTC"), "%Y%m%d%H%M%S")
}
QR_CODE_SRC <- paste0("itafaray-qr.png?v=", asset_version("www/itafaray-qr.png"))
# Ventilation complète d'un vecteur de niveaux de risque -> phrase qui totalise l'effectif
repartition_risque <- function(x, lang = "fr") {
  ord <- names(sort(SEV, decreasing = TRUE))
  rk  <- table(factor(as.character(x), levels = ord)); rk <- rk[rk > 0]
  if (length(rk) == 0) return("")
  conj <- if (lang == "en") "and" else if (lang == "mg") "sy" else "et"
  parts <- vapply(seq_along(rk), function(i) {
    n <- as.integer(rk[i]); lvl <- tolower(names(rk)[i]); lt <- i18n_lookup(lvl, lang)
    if (lang == "en")      paste0(n, " at ", lt, " risk")
    else if (lang == "mg") paste0(n, " misy risika ", lt)
    else                   paste0(n, " à risque ", lvl)
  }, character(1))
  if (length(parts) > 1)
    parts <- paste0(paste(parts[-length(parts)], collapse = ", "), " ", conj, " ", parts[length(parts)])
  parts
}

# Narratif « contexte » du rapport, dans la langue active (FR/EN/MG)
report_contexte <- function(lg, d, na, al, periode_lbl) {
  if (nrow(d) == 0) {
    return(switch(lg,
      en = sprintf("No validated signal was recorded over the selected period (%s). The 12-month trend is shown below as surveillance context.", periode_lbl),
      mg = sprintf("Tsy nisy famantarana voamarina voarakitra nandritra ny vanim-potoana voafidy (%s). Aseho etsy ambany ny fironana 12 volana ho fanampim-pahalalana.", periode_lbl),
      sprintf("Aucun signal validé n'a été enregistré sur la période sélectionnée (%s). La tendance des douze derniers mois est présentée ci-dessous à titre de contexte de surveillance.", periode_lbl)))
  }
  st <- d %>% count(secteur, name = "n")
  dom <- st$secteur[which.max(st$n)]; domp <- round(100 * max(st$n) / sum(st$n))
  susp <- d %>% filter(a_une_alerte, classification_event != "Non précisé") %>%
    count(classification_event, sort = TRUE) %>% head(3) %>% pull(classification_event)
  foks <- d %>% filter(a_une_alerte) %>% distinct(fokontany) %>% pull(fokontany) %>% head(4)
  repart <- repartition_risque(al$niveau_risque, lg)
  sec <- i18n_lookup(tolower(dom), lg); n <- nrow(d)
  if (lg == "en") {
    p1 <- sprintf("During the period (%s), the event-based surveillance (EBS) system recorded %d validated signals, most of which fall under the %s sector (%d%%). ", periode_lbl, n, sec, domp)
    p2 <- if (na > 0) paste0(sprintf("%d signals triggered an alert (%s). ", na, repart),
            if (length(susp)) sprintf("The main suspicions concern %s. ", paste(susp, collapse = ", ")) else "",
            if (length(foks)) sprintf("The most affected fokontany are %s. ", paste(foks, collapse = ", ")) else "")
          else "No alert was triggered over the period; the situation remains under routine surveillance. "
    p3 <- "In line with the One Health approach, the cross-analysis of animal, environmental and human signals aims to exploit potential sentinel signals: for several priority zoonoses (avian influenza, Rift Valley fever, plague, Ebola), animal events may precede human cases and provide lead time for investigation and response."
  } else if (lg == "mg") {
    p1 <- sprintf("Nandritra ny vanim-potoana (%s), ny rafitra fanaraha-maso mifototra amin'ny tranga (SBE) dia nandrakitra famantarana voamarina %d, ka ny ankamaroany dia ao amin'ny sehatra %s (%d%%). ", periode_lbl, n, sec, domp)
    p2 <- if (na > 0) paste0(sprintf("Famantarana %d no niteraka fampandrenesana (%s). ", na, repart),
            if (length(susp)) sprintf("Ny ahiahy lehibe dia mahakasika ny %s. ", paste(susp, collapse = ", ")) else "",
            if (length(foks)) sprintf("Ny fokontany voakasika indrindra dia %s. ", paste(foks, collapse = ", ")) else "")
          else "Tsy nisy fampandrenesana nandritra ny vanim-potoana ; mbola eo ambany fanaraha-maso mahazatra ny toe-draharaha. "
    p3 <- "Mifanaraka amin'ny fomba One Health, ny fampifandraisana ny famantarana biby, tontolo iainana sy olombelona dia mikendry ny hampiasa famantarana mialoha : ho an'ny zoonose maromaro (gripa vorona, tazo Rift Valley, pesta, Ebola), ny tranga amin'ny biby dia mety hialoha ny tranga amin'ny olombelona ka manome fotoana ho an'ny fanadihadiana sy ny valiny."
  } else {
    p1 <- sprintf("Au cours de la période (%s), le système de surveillance à base évènementielle (SBE) a enregistré %d signaux validés, dont la majorité relève du secteur %s (%d %%). ", periode_lbl, n, sec, domp)
    p2 <- if (na > 0) paste0(sprintf("%d signaux ont déclenché une alerte (%s). ", na, repart),
            if (length(susp)) sprintf("Les principales suspicions concernent %s. ", paste(susp, collapse = ", ")) else "",
            if (length(foks)) sprintf("Les fokontany les plus concernés sont %s. ", paste(foks, collapse = ", ")) else "")
          else "Aucune alerte n'a été déclenchée sur la période ; la situation reste sous surveillance de routine. "
    p3 <- "Conformément à l'approche One Health, le croisement de signaux animaux, environnementaux et humains vise à exploiter d'éventuels signaux sentinelles : pour plusieurs zoonoses prioritaires (grippe aviaire, fièvre de la Vallée du Rift, peste, Ebola), des événements animaux peuvent précéder les cas humains et offrir un temps d'anticipation pour l'investigation et la réponse."
  }
  paste0(p1, p2, p3)
}

# Recommandations du rapport, dans la langue active
report_reco <- function(lg) {
  bul <- c(
    "Investiguer en priorité les grappes inter-secteurs classées à risque élevé.",
    "Accélérer le triage et la vérification des signaux en attente afin de réduire les délais.",
    "Maintenir la coordination One Health entre santé humaine, animale et environnementale.",
    "Assurer la remontée et le partage des données via la plateforme nationale interopérable (UGD / X-Road).")
  paste(paste0("-  ", vapply(bul, function(b) i18n_lookup(b, lg), character(1))), collapse = "\n")
}

# Grappes One Health inter-secteurs (fonction pure — réutilisée par l'app et le rapport).
# Renvoie un data.frame (0 ligne si pas d'alerte / pas de grappe).
oh_clusters <- function(d) {
  d <- d %>% dplyr::filter(!is.na(date_de_survenue))
  if (nrow(d) == 0) return(data.frame())
  anchors <- d %>% dplyr::filter(a_une_alerte) %>%
    dplyr::transmute(a_fok = fokontany, a_date = date_de_survenue, a_id = id_signal)
  if (nrow(anchors) == 0) return(data.frame())
  cl <- anchors %>%
    dplyr::inner_join(d, by = c("a_fok" = "fokontany")) %>%
    dplyr::filter(date_de_survenue >= a_date - 14, date_de_survenue <= a_date + 3) %>%
    dplyr::group_by(a_id, a_fok, a_date) %>%
    dplyr::summarise(debut = min(date_de_survenue), fin = max(date_de_survenue),
              nb = dplyr::n(), nsec = dplyr::n_distinct(secteur),
              secteurs = paste(sort(unique(secteur)), collapse = " + "),
              humain = any(secteur == "Humain"), animal = any(secteur == "Animal"),
              env = any(secteur == "Environnement"),
              cas = sum(Nombre_cas), deces = sum(Nombre_deces),
              sev = max(SEV[as.character(niveau_risque)]),
              suspicion = paste(setdiff(unique(classification_event), "Non précisé"), collapse = " ; "),
              t_nh = suppressWarnings(min(date_de_survenue[secteur != "Humain"])),
              t_h  = suppressWarnings(min(date_de_survenue[secteur == "Humain"])),
              .groups = "drop") %>%
    dplyr::filter(nsec >= 2) %>%
    dplyr::mutate(avance = ifelse(humain & (animal | env) &
                             is.finite(as.numeric(t_nh)) & is.finite(as.numeric(t_h)),
                           as.numeric(t_h - t_nh), NA_real_),
           risque = RISK_LAB[as.character(sev)]) %>%
    dplyr::arrange(a_fok, dplyr::desc(nb), debut)
  if (nrow(cl) == 0) return(cl)
  keep <- rep(TRUE, nrow(cl)); kept <- list()
  for (i in seq_len(nrow(cl))) {
    f <- cl$a_fok[i]; dte <- cl$debut[i]; prev <- kept[[f]]
    if (!is.null(prev) && any(abs(as.numeric(dte - prev)) < 14)) keep[i] <- FALSE
    else kept[[f]] <- c(prev, dte)
  }
  cl[keep, ] %>% dplyr::rename(fokontany = a_fok) %>% dplyr::arrange(dplyr::desc(sev), dplyr::desc(nb))
}

# Rapport HTML interactif (auto-contenu) — assemble les widgets et rend le template Rmd.
report_html_render <- function(file, d, d_all, lg, periode_lbl) {
  Tr <- function(k) i18n_lookup(k, lg)
  na  <- sum(d$a_une_alerte)
  al  <- d %>% dplyr::filter(a_une_alerte) %>% dplyr::arrange(dplyr::desc(date_de_survenue))
  gr  <- oh_clusters(d)

  ## Date de référence du bulletin = max de la source active (démo ou réel)
  rmax <- { mx <- suppressWarnings(max(d_all$date_de_survenue, na.rm = TRUE))
            if (is.finite(mx)) mx else suppressWarnings(max(d$date_de_survenue, na.rm = TRUE)) }
  rcur <- lubridate::floor_date(rmax, "month")

  ## --- Tendance (12 derniers mois, barres empilées par secteur) ---
  full_start <- lubridate::floor_date(rcur %m-% months(11), "month")
  dd <- d_all %>% dplyr::filter(date_de_survenue >= full_start) %>%
    dplyr::mutate(mois = lubridate::floor_date(date_de_survenue, "month")) %>%
    dplyr::count(mois, secteur) %>% dplyr::arrange(mois)
  secs <- sort(unique(dd$secteur)); cols <- unname(COL_SECTEUR[secs])
  dd$secteur <- factor(i18n_vec(dd$secteur, lg), levels = i18n_vec(secs, lg))
  r_trend <- dd %>% dplyr::group_by(secteur) %>% echarts4r::e_charts(mois) %>%
    echarts4r::e_bar(n, stack = "secteur", barWidth = "60%") %>%
    echarts4r::e_color(cols) %>%
    echarts4r::e_tooltip(trigger = "axis") %>%
    echarts4r::e_legend(top = 4) %>%
    echarts4r::e_y_axis(name = Tr("Signaux"), minInterval = 1) %>%
    echarts4r::e_grid(left = 48, right = 18, top = 44, bottom = 36)

  ## --- Niveau de risque (barres colorées) ---
  dr <- d %>% dplyr::filter(niveau_risque != "Non évalué") %>% droplevels() %>%
    dplyr::count(niveau_risque)
  r_risk <- if (nrow(dr) > 0) {
    dr <- dr %>% dplyr::mutate(lab = i18n_vec(as.character(niveau_risque), lg),
                               color = unname(COL_RISQUE[as.character(niveau_risque)]))
    dr %>% echarts4r::e_charts(lab) %>%
      echarts4r::e_bar(n, legend = FALSE) %>%
      echarts4r::e_add_nested("itemStyle", color) %>%
      echarts4r::e_legend(show = FALSE) %>%
      echarts4r::e_tooltip() %>%
      echarts4r::e_y_axis(minInterval = 1) %>%
      echarts4r::e_grid(left = 40, right = 18, top = 18, bottom = 30)
  } else NULL

  ## --- Carte (signaux + grappes + alertes) ---
  dm <- d %>% dplyr::filter(!is.na(lat), !is.na(lon))
  r_map <- if (nrow(dm) > 0) {
    pal <- leaflet::colorFactor(unname(COL_SECTEUR[SECTEURS]), domain = SECTEURS)
    dm <- dm %>% dplyr::mutate(signal_label = format_signal_label(code, signal, lg))
    m <- leaflet::leaflet(dm, height = 460) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::addCircleMarkers(~lon, ~lat, radius = 5, stroke = FALSE, fillOpacity = 0.7,
        color = ~pal(secteur), group = "Signaux",
        popup = ~paste0("<b>", format_signal_label(code, signal, lg), "</b><br>",
                        "Fokontany : ", fokontany, "<br>",
                        "Date : ", format(date_de_survenue, "%d/%m/%Y"), "<br>",
                        "Risque : ", niveau_risque))
    if (nrow(gr) > 0) {
      grc <- gr %>% dplyr::left_join(FOK_CENTROIDS, by = "fokontany") %>% dplyr::filter(!is.na(lat))
      if (nrow(grc) > 0)
        m <- m %>% leaflet::addCircles(data = grc, lng = ~lon, lat = ~lat,
          radius = ~pmin(2600, 1000 + 110 * nb), color = "#1e3a5f", weight = 2, opacity = 0.9,
          dashArray = "6,5", fillColor = "#38bdf8", fillOpacity = 0.08, group = "Grappes One Health",
          popup = ~paste0("<b>Grappe One Health — ", fokontany, "</b><br>",
                          "Secteurs croisés : ", secteurs, "<br>Signaux : ", nb,
                          ifelse(!is.na(avance), paste0("<br>Avance de détection : ", round(avance), " j"), "")))
    }
    da <- dm %>% dplyr::filter(a_une_alerte)
    if (nrow(da) > 0)
      m <- m %>% leaflet::addCircleMarkers(data = da, lng = ~lon, lat = ~lat, radius = 10,
        stroke = TRUE, weight = 2.5, color = "#9E2A2B", fillOpacity = 0, opacity = 1, group = "Alertes",
        popup = ~paste0("<b>ALERTE — ", format_signal_label(code, signal, lg), "</b><br>",
                        "Fokontany : ", fokontany, "<br>Niveau de risque : ", niveau_risque))
    m %>% leaflet::addLegend("bottomright", pal = pal, values = SECTEURS, title = Tr("Secteur")) %>%
      leaflet::addLayersControl(overlayGroups = c("Signaux", "Grappes One Health", "Alertes"),
        options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
      leaflet::hideGroup("Grappes One Health")
  } else NULL

  ## --- Tableau des alertes (interactif) + colonne Croisement One Health ---
  crossmap <- if (nrow(gr) > 0) stats::setNames(
    paste0(gr$secteurs, ifelse(is.na(gr$avance), "", paste0("  ·  +", round(gr$avance), " j"))),
    gr$a_id) else character(0)
  tabdf <- al %>% dplyr::transmute(
    Date = format(date_de_survenue, "%d/%m/%Y"),
    Secteur = i18n_vec(secteur, lg),
    Signal = format_signal_label(code, signal, lg),
    Fokontany = fokontany,
    Suspicion = ifelse(classification_event == "Non précisé", "—", classification_event),
    Risque = i18n_vec(as.character(niveau_risque), lg),
    Croisement = ifelse(id_signal %in% names(crossmap), unname(crossmap[id_signal]), "—"))
  names(tabdf) <- c(Tr("Date"), Tr("Secteur"), Tr("Signal"), Tr("Fokontany"),
                    Tr("Suspicion"), Tr("Risque"), Tr("Croisement One Health"))
  r_tab <- DT::datatable(tabdf, rownames = FALSE, class = "stripe hover compact",
    options = list(pageLength = 15, dom = "tip", scrollX = TRUE))

  ## --- Rendu du template ---
  env <- new.env(parent = globalenv())
  env$Tr       <- Tr
  env$meta     <- list(periode = periode_lbl, arrete = format(rmax, "%d/%m/%Y"))
  env$kpi      <- list(signaux = nrow(d),
                       risque  = sum(as.character(d$niveau_risque) %in% c("Très élevé", "Haute")),
                       alertes = na, fokontany = dplyr::n_distinct(d$fokontany))
  env$contexte <- report_contexte(lg, d, na, al, periode_lbl)
  env$reco     <- report_reco(lg)
  env$r_trend  <- r_trend; env$r_risk <- r_risk; env$r_map <- r_map; env$r_tab <- r_tab

  tmpl   <- normalizePath(file.path("report", "rapport_html.Rmd"), mustWork = TRUE)
  outdir <- tempfile("itaf_rep_"); dir.create(outdir)
  rmarkdown::render(tmpl, output_dir = outdir, output_file = "rapport.html",
                    envir = env, quiet = TRUE,
                    intermediates_dir = outdir, knit_root_dir = outdir)
  file.copy(file.path(outdir, "rapport.html"), file, overwrite = TRUE)
}

# Info-bulle : titre de graphique + icône « i » révélant une explication au survol
titre_info <- function(titre, txt, pos = "left") {
  cls <- if (pos == "right") "info-bulle ib-right" else "info-bulle"
  # Texte nettoyé (sans balises, espaces normalisés) -> un seul noeud-texte,
  # traduisible automatiquement par le moteur i18n côté client.
  txt_plain <- trimws(gsub("\\s+", " ", gsub("<[^>]+>", "", txt)))
  shiny::tagList(
    titre,
    shiny::tags$span(class = cls,
                     shiny::icon("circle-info"),
                     shiny::tags$span(class = "ib-txt", txt_plain)))
}
# Titre de box avec bouton d'export PNG dans l'en-tête (hors zone de tracé).
titre_dl <- function(titre, txt, chart_id, fname, pos = "left") {
  cls <- if (pos == "right") "info-bulle ib-right" else "info-bulle"
  txt_plain <- trimws(gsub("\\s+", " ", gsub("<[^>]+>", "", txt)))
  shiny::tagList(
    titre,
    shiny::tags$span(class = cls,
                     shiny::icon("circle-info"),
                     shiny::tags$span(class = "ib-txt", txt_plain)),
    shiny::tags$a(class = "chart-dl", href = "#",
                  title = i18n$t("Exporter en PNG"),
                  onclick = sprintf("exportEChart('%s','%s'); return false;", chart_id, fname),
                  shiny::icon("download")))
}
FOK_CENTROIDS <- DATA %>% group_by(fokontany) %>%
  summarise(lat = mean(lat, na.rm = TRUE), lon = mean(lon, na.rm = TRUE), .groups = "drop")
.maxdate  <- max(DATA$date_de_survenue, na.rm = TRUE)
.cur_start <- lubridate::floor_date(.maxdate, "month")

theme_it <- theme_minimal(base_size = 13) +
  theme(text = element_text(color = INK),
        axis.text = element_text(color = "#5A6672"),
        panel.grid.major = element_line(color = "#E7EAEE", linewidth = 0.4),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom", legend.title = element_blank())

## ---- Scénarios de démonstration (calage automatique) ----
SCENARIOS <- list(
  "Peste — Antanandava (oct. 2025)" = list(fok = "Antanandava",
                                           deb = as.Date("2025-10-01"), fin = as.Date("2025-10-31"),
                                           desc = "Prolifération de puces et mortalité de rats (animal) précèdent de ~1 semaine les cas puis les décès humains, au même fokontany."),
  "Grippe aviaire H5N1 — Kelilalina (nov. 2025)" = list(fok = "Kelilalina",
                                                        deb = as.Date("2025-11-01"), fin = as.Date("2025-11-30"),
                                                        desc = "Mortalité d'oiseaux sauvages puis de volailles avant l'apparition de cas humains grippaux."),
  "Rage — Maroharatra (févr. 2026)" = list(fok = "Maroharatra",
                                           deb = as.Date("2026-02-01"), fin = as.Date("2026-02-28"),
                                           desc = "Mortalité de chauves-souris suivie de morsures multiples — exposition rage."),
  "Mpox — Ranomafana (mars 2026)" = list(fok = "Ranomafana",
                                         deb = as.Date("2026-03-01"), fin = as.Date("2026-03-31"),
                                         desc = "Mortalité de rongeurs / primates puis cas humains avec éruption cutanée."),
  "Contamination hydrique — Antaralava (avril 2026)" = list(fok = "Antaralava",
                                                            deb = as.Date("2026-04-01"), fin = as.Date("2026-04-30"),
                                                            desc = "Altération d'une source d'eau (environnement) suivie de cas humains groupés sur la même source."),
  "Ebola — Vohitrarivo (mai 2026)" = list(fok = "Vohitrarivo",
                                          deb = as.Date("2026-05-01"), fin = as.Date("2026-05-31"),
                                          desc = "Mortalité de chauves-souris / primates (réservoir) avant des décès humains avec fièvre hémorragique.")
)

## =====================================================================
##  UI
## =====================================================================
## ---- Identifiants + base SQLite chiffrée (mode admin & journaux de connexion) ----
credentials <- data.frame(
  user = c("admin", "pivot", "cirad"),
  password = c("itafaray2026", "pivot2026", "cirad2026"),
  admin = c(TRUE, FALSE, FALSE),
  comment = c("Administrateur", "ONG Pivot", "CIRAD"),
  stringsAsFactors = FALSE
)
db_path <- "credentials.sqlite"
passphrase <- "itafaray-poc-2026"   # à externaliser (variable d'environnement) en production
# Authentification (shinymanager) : FALSE = accès libre (désactivée). TRUE = réactiver.
AUTH_ENABLED <- FALSE
if (AUTH_ENABLED && !file.exists(db_path)) {
  create_db(credentials_data = credentials, sqlite_path = db_path, passphrase = passphrase)
}

ui <- dashboardPage(
  title = i18n_lookup("Tableau de bord iTafaray", I18N_DEFAULT),
  skin = "blue",
  dashboardHeader(
    title = tags$span(
      class = "itafaray-header-title",
      tags$span(
        class = "itafaray-brand",
        tags$span(class = "mg-white", "i"),
        tags$span(class = "mg-red", "Tafa"),
        tags$span(class = "mg-green", "ray")
      ),
      tags$span(class = "itafaray-separator", HTML("&nbsp;&middot;&nbsp;")),
      tags$span(class = "itafaray-subtitle", "One Health Madagascar")
    ), titleWidth = 300,
    # Bandeau institutionnel dans l'en-tête : Primature puis ministères de tutelle.
    # Pastille blanche pour que les logos ressortent sur le bleu foncé.
    tags$li(class = "dropdown",
      tags$div(style = "display:flex; align-items:center; height:50px; padding:6px 12px;",
        tags$div(style = "display:flex; align-items:center; gap:10px;
                          background:#fff; border-radius:6px; padding:4px 10px;",
          lapply(c("madagascar.png", "sante.png", "elevage.png", "environnement.png"),
                 function(f) tags$img(src = file.path("logos", f),
                                      style = "height:30px; width:auto; object-fit:contain;",
                                      alt = "Logo institutionnel"))))),
    lang_switcher_ui()
  ),
  dashboardSidebar(
    width = 260,
    tags$div(style = "margin:10px 12px 4px; padding:9px 12px;
                       background:rgba(255,255,255,.06); border-left:3px solid #38bdf8;
                         color:rgba(255,255,255,.75); font-size:11.5px; line-height:1.4;",
             i18n$t("Source des données — bascule démo / réel (X-Road).")),
    tags$div(style = "margin:0 12px 8px;",
      radioButtons("data_source", label = NULL,
                   choices = c("Démonstration (synthétique)" = "sim",
                               "Données réelles (X-Road)"     = "reel"),
                   selected = "sim"),
      uiOutput("data_source_badge")),
    sidebarMenu(
      id = "tabs",
      menuItem(i18n$t("Accueil"), tabName = "accueil", icon = icon("house")),
      menuItem(i18n$t("Synthèse"), tabName = "synthese", icon = icon("gauge-high")),
      tags$li(tags$hr(style = "border:0; border-top:1px solid rgba(255,255,255,.16); margin:9px 16px;")),
      menuItem(i18n$t("Vue d'ensemble"), tabName = "vue", icon = icon("gauge")),
      menuItem(i18n$t("Par signal"), tabName = "signal", icon = icon("layer-group")),
      menuItem(i18n$t("Cartographie"), tabName = "carte", icon = icon("map-location-dot")),
      menuItem(i18n$t("Climat & environnement"), tabName = "climat", icon = icon("cloud-sun-rain")),
      menuItem(i18n$t("Environnement (SMART)"), tabName = "smartenv", icon = icon("tree")),
      menuItem(i18n$t("Alertes"), tabName = "alertes", icon = icon("triangle-exclamation")),
      menuItem(i18n$t("Indicateurs (18 signaux)"), tabName = "indic", icon = icon("table-cells")),
      menuItem(i18n$t("One Health (croisements)"), tabName = "onehealth", icon = icon("diagram-project")),
      menuItem(i18n$t("Pipeline & qualité"), tabName = "qualite", icon = icon("circle-check")),
      menuItem(i18n$t("À propos"), tabName = "apropos", icon = icon("circle-info"))
    ),
    conditionalPanel(
      condition = "input.tabs != 'synthese' && input.tabs != 'accueil'",
      sliderInput("dates", "Période :", min = DRANGE[1], max = DRANGE[2],
                  value = c(DRANGE[1], DRANGE[2]), timeFormat = "%d/%m/%Y"),
      selectInput("fokontany", "Fokontany (district d'Ifanadiana) :",
                  choices = c("Tous", sort(unique(na.omit(DATA$fokontany)))), selected = "Tous"),
      checkboxInput("verif_only", "Signaux vérifiés uniquement", value = FALSE)
    ),
    # Bandeau de logos partenaires (même ordre que l'onglet « À propos »)
    tags$div(style = "margin:16px 12px 12px; background:#fff; border-radius:8px; padding:10px 8px;",
      tags$div(style = "display:grid; grid-template-columns:repeat(3,1fr); gap:8px;
                         align-items:center; justify-items:center;",
        lapply(c("afd.png", "banque_mondiale.png", "madagascar.png",
                 "sante.png", "elevage.png", "environnement.png",
                 "africam_prezode.png", "cirad.png", "pivot.png"),
               function(f) tags$img(src = file.path("logos", f),
                                    style = paste0("max-height:",
                                                   if (f == "afd.png") "22px" else "32px",
                                                   "; max-width:100%; object-fit:contain;"),
                                    alt = "Logo partenaire"))))
  ),
  dashboardBody(
    lang_switcher_css(),
    lang_switcher_js(),
    tags$head(
      tags$link(rel = "stylesheet",
                href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css"),
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;500;600;700&display=swap"),
      tags$style(HTML("
      body, .content-wrapper, .main-sidebar, .box, .small-box, .box-title, .kpi,
      .form-control, .btn, .sidebar-menu, table.dataTable, h1, h2, h3, h4, h5 {
        font-family:'IBM Plex Sans',-apple-system,'Segoe UI',Roboto,Arial,sans-serif !important; }
      .skin-blue .main-header .logo,
      .skin-blue .main-header .navbar { background:#1e3a5f !important; }
      .skin-blue .main-header .logo { color:#FFFFFF; font-weight:700; letter-spacing:.3px; border-bottom:0; }
      .skin-blue .main-header .logo:hover { background:#1e3a5f !important; }
      .itafaray-header-title { display:inline-flex; align-items:center; gap:0; white-space:nowrap; }
      .itafaray-brand { display:inline-flex; align-items:baseline; gap:0; font-weight:800; letter-spacing:.2px; line-height:1; }
      .itafaray-brand .mg-white { color:#FFFFFF; text-shadow:0 0 0.5px rgba(15,23,42,.35); }
      .itafaray-brand .mg-red { color:#FC3D32; }
      .itafaray-brand .mg-green { color:#007E3A; }
      .itafaray-separator, .itafaray-subtitle { color:rgba(255,255,255,.92); font-weight:600; }
      .skin-blue .main-header .navbar .sidebar-toggle { color:#cbd5e1; }
      .skin-blue .main-header .navbar .sidebar-toggle:hover { background:rgba(255,255,255,.08); color:#fff; }
      .skin-blue .main-sidebar { background:#1e3a5f !important; }
      .skin-blue .sidebar a { color:rgba(255,255,255,.85); }
      .skin-blue .sidebar-menu>li>a { color:rgba(255,255,255,.55); border-left:3px solid transparent;
        font-size:13px; padding:10px 16px; }
      .skin-blue .sidebar-menu>li>a:hover { background:rgba(255,255,255,.07); color:rgba(255,255,255,.9); }
      .skin-blue .sidebar-menu>li.active>a { background:rgba(255,255,255,.1); color:#FFFFFF;
        border-left:3px solid #38bdf8; font-weight:500; }
      .skin-blue .sidebar label, .skin-blue .sidebar .control-label {
        color:rgba(255,255,255,.55) !important; font-size:11px; }
      .skin-blue .sidebar .form-control, .skin-blue .sidebar .selectize-input {
        background:rgba(255,255,255,.08) !important; border:1px solid rgba(255,255,255,.15) !important; color:#fff !important; }
      .skin-blue .sidebar .selectize-dropdown { background:#1e3a5f !important; color:#fff !important; }
      .skin-blue .sidebar .selectize-dropdown .option:hover,
      .skin-blue .sidebar .selectize-dropdown .option.active {
        background:rgba(255,255,255,.1) !important; color:#7dd3fc !important; }
      .skin-blue .sidebar .checkbox label { color:rgba(255,255,255,.85) !important; }
      .skin-blue .sidebar .radio label { color:rgba(255,255,255,.85) !important; font-size:12px; }
      .ds-badge { display:inline-block; margin-top:4px; padding:2px 9px; border-radius:10px;
        font-size:10.5px; font-weight:600; }
      html, body, .wrapper, .content-wrapper, .right-side,
      section.content, .tab-content, .tab-pane {
        background:#E2E8F0 !important; }
      .content-wrapper { min-height:100vh !important; }
      .box { border:1px solid #E3E7EB; border-top:3px solid #1e3a5f; border-radius:5px;
        box-shadow:0 1px 2px rgba(20,40,60,.05); }
      .box>.box-header { padding:12px 15px; }
      .box>.box-header .box-title { font-size:15px; font-weight:600; color:#26333F; }
      .box.box-solid>.box-header, .box.box-solid.box-primary>.box-header,
      .box.box-solid.box-success>.box-header, .box.box-solid.box-info>.box-header,
      .box.box-solid.box-warning>.box-header, .box.box-solid.box-danger>.box-header {
        background:#FFFFFF; color:#26333F; border-bottom:1px solid #EDF0F3; }
      .box.box-solid { border:1px solid #E3E7EB; border-top:3px solid #1e3a5f; }
      .box.box-solid.box-danger { border-top-color:#9E2A2B; }
      .box.box-solid.box-warning { border-top-color:#1e3a5f; }
      .box.box-solid.box-success { border-top-color:#5E8B6A; }
      .small-box { border-radius:5px; box-shadow:0 1px 2px rgba(20,40,60,.06); }
      .small-box h3 { font-weight:700; font-size:30px; }
      .small-box .icon { opacity:.16; }
      .small-box.bg-green  { background:#1e3a5f !important; color:#fff !important; }
      .small-box.bg-aqua   { background:#2F6E78 !important; color:#fff !important; }
      .small-box.bg-blue   { background:#35618E !important; color:#fff !important; }
      .small-box.bg-red    { background:#9E2A2B !important; color:#fff !important; }
      .small-box.bg-yellow { background:#B98A2E !important; color:#fff !important; }
      .small-box.bg-teal   { background:#2F6E78 !important; color:#fff !important; }
      .small-box.bg-orange { background:#C2703D !important; color:#fff !important; }
      .irs--shiny .irs-line { background:rgba(255,255,255,.15); border:0; height:6px; top:27px; border-radius:6px; }
      .irs--shiny .irs-bar { background:#38bdf8; border:0; height:6px; top:27px; }
      .irs--shiny .irs-handle { width:16px; height:16px; top:22px; border:2px solid #38bdf8;
        background:#fff; box-shadow:0 1px 2px rgba(0,0,0,.3); }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
        background:#1e3a5f; color:#38bdf8; border:1px solid #38bdf8; font-weight:600; }
      .irs--shiny .irs-min, .irs--shiny .irs-max, .irs--shiny .irs-grid-text {
        color:rgba(255,255,255,.45); background:transparent; }
      .synth-head { background:#FFFFFF; border:1px solid #E3E7EB; border-radius:8px;
        padding:16px 20px; box-shadow:0 1px 3px rgba(20,40,60,.06); margin-bottom:14px; }
      .synth-title { font-size:19px; font-weight:700; color:#1F2D3D; }
      .synth-sub { color:#7A8593; font-size:13px; margin-top:2px; }
      .synth-msg { margin:12px 0 0 0; padding-left:18px; color:#26333F; line-height:1.7; }
      .synth-head .form-group { margin-bottom:0; }
      .kpi { background:#FFFFFF; border:1px solid #E3E7EB; border-radius:8px; padding:16px 18px;
        box-shadow:0 1px 3px rgba(20,40,60,.06); border-top:3px solid #1e3a5f; margin-bottom:14px; }
      .kpi-val { font-size:32px; font-weight:700; color:#1F2D3D; line-height:1; }
      .kpi-lab { font-size:12px; text-transform:uppercase; letter-spacing:.04em; color:#8A93A0; margin-top:6px; }
      .kpi-cap { font-size:12px; color:#5A6672; margin-top:6px; }
      .kpi-alert { border-top-color:#9E2A2B; } .kpi-warn { border-top-color:#C2703D; }
      .kpi-info { border-top-color:#2F6E78; }
      .box>.box-header { overflow:visible; }
      .box>.box-header .box-title { overflow:visible; }
      .info-bulle { position:relative; display:inline-block; margin-left:7px;
        color:#dbe7f2; cursor:help; font-size:13px; vertical-align:middle; }
      .info-bulle:hover { color:#fff; }
      .info-bulle .ib-txt { visibility:hidden; opacity:0; transition:opacity .15s ease;
        position:absolute; z-index:9999; left:0; top:155%; width:290px;
        background:#1e3a5f; color:#fff; padding:10px 13px; border-radius:7px;
        font-size:12px; font-weight:400; line-height:1.55; text-transform:none;
        letter-spacing:normal; box-shadow:0 10px 26px rgba(15,30,50,.32); white-space:normal; }
      .info-bulle:hover .ib-txt { visibility:visible; opacity:1; }
      .info-bulle .ib-txt::after { content:''; position:absolute; bottom:100%; left:13px;
        border:6px solid transparent; border-bottom-color:#1e3a5f; }
      .info-bulle.ib-right .ib-txt { left:auto; right:0; }
      .info-bulle.ib-right .ib-txt::after { left:auto; right:13px; }
      .chart-dl { float:right; margin-left:10px; color:#fff; cursor:pointer;
        font-size:13px; opacity:.75; transition:opacity .15s ease; vertical-align:middle; }
      .chart-dl:hover { opacity:1; color:#fff; }
      td.details-control { cursor:pointer; text-align:center; width:30px; }
      .oh-toggle { display:inline-block; width:18px; height:18px; line-height:16px; text-align:center;
        border:1px solid #1e3a5f; border-radius:4px; color:#1e3a5f; font-weight:700; font-size:13px; }
      td.details-control:hover .oh-toggle { background:#1e3a5f; color:#fff; }
      .oh-childwrap:hover { background:#EAF1F7 !important; }
      .home-shell { padding:8px 4px 0; }
      .home-hero {
        position:relative; overflow:hidden; margin-bottom:18px; border-radius:24px;
        padding:34px 34px 28px; color:#fff;
        background:linear-gradient(135deg, #153554 0%, #1e3a5f 46%, #2F6E78 100%);
        box-shadow:0 24px 60px rgba(20,40,60,.18);
      }
      .home-hero::before, .home-hero::after {
        content:''; position:absolute; border-radius:999px; pointer-events:none;
        background:rgba(255,255,255,.08);
      }
      .home-hero::before { width:280px; height:280px; top:-110px; right:-70px; }
      .home-hero::after { width:220px; height:220px; bottom:-110px; left:-80px; }
      .home-grid {
        display:grid; grid-template-columns:minmax(0, 1.45fr) minmax(280px, .9fr);
        gap:28px; align-items:center; position:relative; z-index:1;
      }
      .home-eyebrow {
        display:inline-flex; align-items:center; gap:8px; margin-bottom:14px; padding:7px 12px;
        border:1px solid rgba(255,255,255,.18); border-radius:999px; background:rgba(255,255,255,.08);
        font-size:12px; font-weight:600; letter-spacing:.08em; text-transform:uppercase;
      }
      .home-title { margin:0; font-size:42px; line-height:1.05; font-weight:700; letter-spacing:-.03em; }
      .home-copy { margin:18px 0 0; max-width:700px; font-size:17px; line-height:1.75; color:rgba(255,255,255,.88); }
      .home-pillars { display:grid; grid-template-columns:repeat(3, minmax(0, 1fr)); gap:12px; margin-top:26px; }
      .home-pillar {
        min-height:116px; padding:16px 16px 14px; border-radius:18px;
        background:rgba(255,255,255,.1); border:1px solid rgba(255,255,255,.12);
        backdrop-filter:blur(6px);
      }
      .home-pillar i { font-size:18px; color:#BAE6FD; }
      .home-pillar h3 { margin:14px 0 8px; font-size:16px; font-weight:600; color:#fff; }
      .home-pillar p { margin:0; font-size:13px; line-height:1.6; color:rgba(255,255,255,.78); }
      .home-qr-card {
        justify-self:end; width:min(100%, 360px); padding:18px; border-radius:24px;
        background:rgba(247,250,252,.96); box-shadow:0 18px 44px rgba(15,23,42,.22);
      }
      .home-qr-card img {
        display:block; width:100%; height:auto; border-radius:18px; background:#fff;
      }
      .home-qr-title { margin:16px 0 6px; color:#1e3a5f; font-size:24px; font-weight:700; text-align:center; }
      .home-qr-copy { margin:0; color:#52606D; font-size:13px; line-height:1.6; text-align:center; }
      .home-strip {
        display:grid; grid-template-columns:repeat(3, minmax(0, 1fr)); gap:14px; margin-bottom:18px;
      }
      .home-strip-card {
        padding:18px 20px; border-radius:18px; background:#FFFFFF; border:1px solid #D8E0E8;
        box-shadow:0 10px 30px rgba(20,40,60,.07);
      }
      .home-strip-kicker { color:#1e3a5f; font-size:11px; font-weight:700; letter-spacing:.08em; text-transform:uppercase; }
      .home-strip-card p { margin:10px 0 0; color:#4C5A67; line-height:1.7; font-size:14px; }
      @media (max-width: 1100px) {
        .home-grid { grid-template-columns:1fr; }
        .home-qr-card { justify-self:start; max-width:340px; }
      }
      @media (max-width: 768px) {
        .home-hero { padding:24px 20px 22px; border-radius:20px; }
        .home-title { font-size:34px; }
        .home-copy { font-size:15px; }
        .home-pillars, .home-strip { grid-template-columns:1fr; }
        .home-qr-card { max-width:none; width:100%; }
      }
    ")),
      tags$script(HTML("
        window.exportEChart = function(id, name){
          try {
            if (typeof echarts === 'undefined') { return; }
            var host = document.getElementById(id);
            if (!host) { return; }
            var inst = echarts.getInstanceByDom(host);
            if (!inst) { var c = host.querySelector('div'); if (c) inst = echarts.getInstanceByDom(c); }
            if (!inst) { return; }
            var url = inst.getDataURL({ type: 'png', pixelRatio: 2, backgroundColor: '#FFFFFF' });
            var a = document.createElement('a');
            a.href = url; a.download = (name || 'graphique') + '.png';
            document.body.appendChild(a); a.click(); document.body.removeChild(a);
          } catch (e) { console.error('exportEChart', e); }
        };
      "))
    ),
    tabItems(
      tabItem("accueil",
              tags$div(class = "home-shell",
                       tags$section(class = "home-hero",
                                    tags$div(class = "home-grid",
                                             tags$div(
                                               tags$div(class = "home-eyebrow",
                                                        tags$i(class = "fa-solid fa-satellite-dish"),
                                                        i18n$t("Plateforme nationale de surveillance")),
                                               tags$h1(class = "home-title", HTML("iTafaray &middot; One Health Madagascar")),
                                               tags$p(class = "home-copy",
                                                      i18n$t("iTafaray centralise les signaux humains, animaux et environnementaux pour offrir une lecture partag\u00e9e des risques sanitaires \u00e0 Madagascar, acc\u00e9l\u00e9rer la d\u00e9tection pr\u00e9coce et faciliter la coordination entre acteurs One Health.")),
                                               tags$div(class = "home-pillars",
                                                        tags$div(class = "home-pillar",
                                                                 tags$i(class = "fa-solid fa-user-group"),
                                                                 tags$h3(i18n$t("Sant\u00e9 humaine")),
                                                                 tags$p(i18n$t("Suivi des signaux communautaires, des cas suspects et des foyers n\u00e9cessitant une action rapide."))),
                                                        tags$div(class = "home-pillar",
                                                                 tags$i(class = "fa-solid fa-paw"),
                                                                 tags$h3(i18n$t("Sant\u00e9 animale")),
                                                                 tags$p(i18n$t("Croisement des \u00e9v\u00e9nements animaux pour mieux rep\u00e9rer les zoonoses prioritaires et leurs signaux sentinelles."))),
                                                        tags$div(class = "home-pillar",
                                                                 tags$i(class = "fa-solid fa-leaf"),
                                                                 tags$h3(i18n$t("Environnement")),
                                                                 tags$p(i18n$t("Int\u00e9gration des facteurs climatiques et environnementaux pour contextualiser les menaces et anticiper les tensions."))))),
                                             tags$aside(class = "home-qr-card",
                                                        tags$img(src = QR_CODE_SRC,
                                                                 alt = "Code QR iTafaray"),
                                                        tags$h2(class = "home-qr-title", i18n$t("Scannez-moi !")),
                                                        tags$p(class = "home-qr-copy",
                                                               i18n$t("Acc\u00e9dez rapidement \u00e0 la plateforme depuis un appareil mobile ou partagez l'entr\u00e9e du tableau de bord avec vos partenaires."))))),
                       tags$div(class = "home-strip",
                                tags$div(class = "home-strip-card",
                                         tags$div(class = "home-strip-kicker", i18n$t("Vision")),
                                         tags$p(i18n$t("Une porte d'entr\u00e9e claire pour comprendre le r\u00f4le de la plateforme avant d'explorer les tableaux, cartes et alertes."))),
                                tags$div(class = "home-strip-card",
                                         tags$div(class = "home-strip-kicker", i18n$t("Coordination")),
                                         tags$p(i18n$t("Une lecture commune des signaux pour les acteurs de la sant\u00e9 humaine, animale et environnementale."))),
                                tags$div(class = "home-strip-card",
                                         tags$div(class = "home-strip-kicker", i18n$t("Acc\u00e8s rapide")),
                                         tags$p(i18n$t("Le code QR facilite le partage du tableau de bord pendant les r\u00e9unions, revues de situation et activit\u00e9s terrain."))))),
              ),
      tabItem("synthese",
              tags$div(class = "synth-head",
                       fluidRow(
                         column(8,
                                tags$div(class = "synth-title", i18n$t("Synthèse — Comité de pilotage One Health")),
                                tags$div(class = "synth-sub", textOutput("s_period", inline = TRUE)),
                                uiOutput("s_msg")),
                         column(4,
                                selectInput("exec_period", i18n$t("Période"), width = "100%",
                                            choices = c("Mois en cours", "3 derniers mois", "12 derniers mois", "Tout"),
                                            selected = "3 derniers mois"),
                                fluidRow(
                                  column(7, style = "padding-right:5px;",
                                         downloadButton("dl_report", i18n$t("Générer le rapport"),
                                                        class = "btn-block",
                                                        style = "background:#1e3a5f; color:#fff; border:0; font-weight:600; margin-top:2px;")),
                                  column(5, style = "padding-left:5px;",
                                         selectInput("report_format", NULL, width = "100%",
                                                     choices = c("PDF" = "pdf", "HTML interactif" = "html"),
                                                     selected = "pdf"))
                                ),
                                actionButton("tour", i18n$t("Visite guidée"), icon = icon("circle-question"),
                                             class = "btn-block btn-sm",
                                             style = "margin-top:6px; background:transparent; color:#1e3a5f; border:1px solid #1e3a5f; font-weight:600;"))
                       )
              ),
              uiOutput("s_kpis"),
              fluidRow(
                box(title = titre_info("Alertes prioritaires sur la période",
                      "Signaux ayant déclenché une alerte sur la période, avec secteur, fokontany, suspicion et niveau de risque. La colonne <b>Croisement One Health</b> indique les secteurs croisés autour de l'alerte (même fokontany, fenêtre de 14 jours) et, le cas échéant, l'<b>avance de détection</b> gagnée grâce au signal animal ou environnemental sur le premier cas humain — c'est tout l'intérêt de croiser les trois secteurs. Cliquez le <b>+</b> à gauche pour <b>déplier</b> les signaux de la grappe, ou une ligne pour la chaîne de décision complète."),
                    width = 12,
                    status = "danger", solidHeader = TRUE, DTOutput("s_alertes"))
              ),
              fluidRow(
                box(title = titre_info("Tendance de l'activité",
                      "Nombre de signaux validés par mois et par secteur (humain, animal, environnement) sur les 12 derniers mois glissants. Le curseur sous le graphique se cale automatiquement sur la période choisie en haut de page — faites-le glisser pour explorer une autre fenêtre."),
                    width = 8,
                    status = "primary", solidHeader = TRUE, echarts4rOutput("s_trend", height = 300)),
                box(title = titre_info("Niveau de risque sur la période",
                      "Répartition des signaux <b>évalués</b> selon le niveau de risque attribué au triage (très élevé, haute, modéré, faible). Les signaux non encore évalués ne sont pas comptés.",
                      pos = "right"),
                    width = 4,
                    status = "warning", solidHeader = TRUE, plotOutput("s_risk", height = 300))
              )
      ),
      tabItem("vue",
              fluidRow(
                valueBoxOutput("kpi_total", 3), valueBoxOutput("kpi_verif", 3),
                valueBoxOutput("kpi_eval", 3), valueBoxOutput("kpi_alertes", 3)
              ),
              fluidRow(
                box(title = titre_info("Volume de signaux par mois et par secteur",
                      "Évolution mensuelle du nombre de signaux, ventilée par secteur. Le curseur sous le graphique permet de zoomer sur une fenêtre temporelle ; survolez un point pour le détail."),
                    width = 8,
                    status = "primary", solidHeader = TRUE, echarts4rOutput("p_volume", height = 330)),
                box(title = titre_info("Niveau de risque (signaux évalués)",
                      "Répartition des signaux évalués au triage par niveau de risque. Seuls les signaux ayant fait l'objet d'une évaluation figurent ici.",
                      pos = "right"),
                    width = 4,
                    status = "warning", solidHeader = TRUE, plotOutput("p_risque", height = 330))
              ),
              fluidRow(
                box(title = titre_dl("Pathogènes / événements suspectés",
                      "Décompte des signaux par type d'événement ou pathogène suspecté, renseigné lors du triage. Donne un aperçu des menaces dominantes sur la période et les filtres en cours.",
                      "p_events", "i-Tafaray_pathogenes"),
                    width = 6,
                    status = "danger", solidHeader = TRUE, echarts4rOutput("p_events", height = 280)),
                box(title = titre_dl("Répartition par signal",
                      "Nombre de signalements pour chacun des 18 signaux prioritaires de la nomenclature One Health.",
                      "p_signaux", "i-Tafaray_repartition_signal", pos = "right"),
                    width = 6,
                    status = "info", solidHeader = TRUE, echarts4rOutput("p_signaux", height = 280))
              )
      ),
      tabItem("carte",
              box(title = titre_info("Localisation des signaux, alertes et grappes One Health",
                    "Chaque point est un signal, coloré par secteur. Un <b>anneau rouge</b> entoure les signaux ayant déclenché une <b>alerte</b>. Un <b>cercle pointillé</b> délimite chaque <b>grappe One Health</b> inter-secteurs (taille proportionnelle au nombre de signaux). Le sélecteur en haut à droite affiche ou masque chaque couche ; cliquez un élément pour son détail."),
                  width = 12, status = "success", solidHeader = TRUE,
                  fluidRow(
                    column(7, selectInput("carte_nav",
                              "Naviguer vers une alerte ou une grappe One Health :",
                              choices = c("— Vue d'ensemble —" = ""), width = "100%")),
                    column(5, tags$div(style = "margin-top:25px;",
                              actionButton("carte_reset", "Vue d'ensemble",
                                           icon = icon("expand"),
                                           class = "btn-sm",
                                           style = "background:#1e3a5f;color:#fff;border:0;")))),
                  leafletOutput("carte", height = 560))
      ),
      tabItem("climat",
              fluidRow(
                box(title = titre_info("Pluviométrie — district d'Ifanadiana",
                      "Cumul de pluie mensuel (barres) comparé à la normale climatologique (ligne). Source : plateforme climatique MDG. Une saison anormalement sèche ou humide modifie les risques (vecteurs, maladies hydriques, feux)."),
                    width = 6, status = "primary", solidHeader = TRUE,
                    echarts4rOutput("clim_pluie", height = 300)),
                box(title = titre_info("Température — district d'Ifanadiana",
                      "Température moyenne mensuelle (ligne) comparée à la normale. Les anomalies chaudes/froides influencent la dynamique des pathogènes et des vecteurs.",
                      pos = "right"),
                    width = 6, status = "warning", solidHeader = TRUE,
                    echarts4rOutput("clim_temp", height = 300))
              ),
              fluidRow(
                box(title = titre_info("Risque incendie (FWI) — district d'Ifanadiana",
                      "Indice météorologique de risque de feu (Fire Weather Index) comparé à sa normale. À relier aux signaux environnementaux « feux » (E2)."),
                    width = 6, status = "danger", solidHeader = TRUE,
                    echarts4rOutput("clim_fwi", height = 300)),
                box(title = titre_dl("Profil saisonnier — signaux environnementaux vs climat",
                      "Pour chaque mois de l'année : nombre de signaux environnementaux (barres) et FWI moyen (ligne). L'alignement par mois calendaire suggère les liens saison sèche / risque de feu → signaux. <i>Signaux synthétiques, climat réel — superposition à visée illustrative.</i>",
                      "clim_overlay", "i-Tafaray_profil_saisonnier", pos = "right"),
                    width = 6, status = "success", solidHeader = TRUE,
                    echarts4rOutput("clim_overlay", height = 300))
              )
      ),
      tabItem("smartenv",
              tags$div(class = "synth-head",
                fluidRow(
                  column(8,
                    tags$div(class = "synth-title", i18n$t("Surveillance environnementale & faune — patrouilles SMART")),
                    tags$div(class = "synth-sub", textOutput("env_sub", inline = TRUE)),
                    tags$div(class = "synth-msg", style = "padding-left:0;",
                      i18n$t("Couche autonome : données de patrouilles SMART en aire protégée (Nord-Est de Madagascar), distinctes du district d'Ifanadiana et de sa période."))),
                  column(4,
                    selectInput("env_secteur", i18n$t("Secteur"), width = "100%",
                                choices = ENV_SECTEURS, selected = "Tous"),
                    selectInput("env_annee", i18n$t("Année"), width = "100%",
                                choices = ENV_ANNEES, selected = ENV_ANNEE_DEFAUT))
                )
              ),
              uiOutput("env_kpis"),
              fluidRow(
                box(title = titre_info("Localisation des observations SMART",
                      "Chaque point est une observation de patrouille, colorée par catégorie (observation directe, circulation, braconnage, partie d'animal). Les observations de menace sont entourées d'un anneau rouge."),
                    width = 8, status = "success", solidHeader = TRUE,
                    leafletOutput("env_map", height = 430)),
                box(title = titre_info("Répartition par catégorie d'observation",
                      "Volume d'observations par catégorie principale relevée lors des patrouilles.", pos = "right"),
                    width = 4, status = "primary", solidHeader = TRUE,
                    echarts4rOutput("env_cat", height = 430))
              ),
              fluidRow(
                box(title = titre_dl("Activité de patrouille par mois",
                      "Nombre d'observations par mois ; en rouge, la part de menaces (braconnage, pièges, coupe de bois…). Reflète la pression sur l'aire protégée dans le temps.",
                      "env_trend", "i-Tafaray_smart_tendance"),
                    width = 8, status = "primary", solidHeader = TRUE,
                    echarts4rOutput("env_trend", height = 300)),
                box(title = titre_info("Menaces relevées",
                      "Répartition des observations par type de menace : Chasse / braconnage, Exploitation forestière, Contact domestique (faune–bétail), Pollution / déchets et Présence humaine. Tous ces types sont comptés comme menaces ; le KPI « part de menaces » et les anneaux rouges de la carte suivent cette typologie.", pos = "right"),
                    width = 4, status = "danger", solidHeader = TRUE,
                    echarts4rOutput("env_menaces", height = 300))
              ),
              fluidRow(
                box(title = titre_info("Lecture One Health — correspondance avec les signaux SBE",
                      "Mise en correspondance des observations SMART avec les signaux SBE One Health (santé humaine, animale, environnement), selon la table de correspondance fournie. Indique le degré de couverture (directe, partielle, indirecte, non couvert)."),
                    width = 7, status = "success", solidHeader = TRUE,
                    DTOutput("env_corr")),
                box(title = titre_info("Faune & contact homme–animal",
                      "Espèces les plus observées et indicateurs de contact faune sauvage / animaux domestiques (divagation de zébus, chiens) — situations à risque de transmission inter-espèces."),
                    width = 5, status = "warning", solidHeader = TRUE,
                    echarts4rOutput("env_especes", height = 220),
                    uiOutput("env_faune"))
              )
      ),
      tabItem("signal",
              box(title = titre_dl("Vue d'ensemble — activité par signal et par mois",
                    "Carte de chaleur croisant les 18 signaux (lignes) et les mois (colonnes) : plus la cellule est foncée, plus le nombre de signaux est élevé. Repère d'un coup d'œil les montées d'activité par type de signal.",
                    "p_heat_temps", "i-Tafaray_heatmap_mois"),
                  width = 12,
                  status = "primary", solidHeader = TRUE,
                  echarts4rOutput("p_heat_temps", height = 430)),
              box(title = titre_dl("Vue d'ensemble — activité par signal et par fokontany",
                    "Carte de chaleur croisant les 18 signaux (lignes) et les fokontany (colonnes) : plus la cellule est foncée, plus le nombre de signaux est élevé. Repère les zones de concentration par type de signal.",
                    "p_heat_geo", "i-Tafaray_heatmap_fokontany"),
                  width = 12,
                  status = "success", solidHeader = TRUE,
                  echarts4rOutput("p_heat_geo", height = 430))
      ),
      tabItem("indic",
              box(title = "Indicateurs par signal — les 18 signaux prioritaires", width = 12,
                  status = "primary", solidHeader = TRUE,
                  helpText("Une ligne par signal prioritaire. Volumes, cas, décès, évaluations et alertes calculés sur la période et les filtres sélectionnés. La colonne « En grappe One Health » compte les signalements de ce signal impliqués dans une grappe inter-secteurs — sa fréquence d'usage dans les alertes One Health."),
                  DTOutput("t_indic"))
      ),
      tabItem("onehealth",
              fluidRow(
                valueBoxOutput("oh_total", 4), valueBoxOutput("oh_zoo", 4), valueBoxOutput("oh_env", 4)
              ),
              box(title = "Indicateurs One Health — grappes inter-secteurs", width = 12,
                  status = "warning", solidHeader = TRUE,
                  helpText("Autour de chaque alerte, regroupement des signaux du même fokontany sur les 14 jours précédents : on retient les grappes croisant au moins deux secteurs — le croisement intersectoriel au cœur du One Health. « Avance détection » = nombre de jours entre le 1er signal animal/environnemental et le 1er signal humain de la grappe."),
                  DTOutput("t_oh"))
      ),
      tabItem("alertes",
              box(title = titre_info("Alertes (signaux ayant déclenché une alerte)",
                    "Détail de tous les signaux passés en alerte, avec la chaîne de décision (triage, vérification, évaluation du risque). Cliquez une ligne pour afficher le parcours complet du signal."),
                  width = 12,
                  status = "danger", solidHeader = TRUE, DTOutput("t_alertes")),
              box(title = "Notifier l'autorité (E-Notification UGD)",
                  width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE,
                  helpText("Sélectionnez une alerte dans le tableau ci-dessus, choisissez le canal et le ou les destinataires, puis cliquez « Notifier ». Vous pouvez saisir plusieurs destinataires séparés par des virgules ou des points-virgules. En dev, une cible de test (EN_TEST_TARGET) peut forcer les envois. En pré-prod / prod, si cette variable est absente, les destinataires saisis ici sont utilisés. Tant que l'envoi réel n'est pas activé (EN_ENABLED), il s'agit d'un essai à blanc : rien n'est envoyé."),
                  fluidRow(
                    column(4, checkboxGroupInput("en_canaux", "Canal", inline = TRUE,
                                                 choices = c("SMS", "WhatsApp", "Email"),
                                                 selected = "SMS")),
                    column(5, textInput("en_dest", "Destinataire(s) (numéro +261… ou email)",
                                        placeholder = "+261340000000, +261320000000, autorite@example.org")),
                    column(3, br(), actionButton("en_send", "Notifier",
                                                 icon = icon("paper-plane"), class = "btn-warning"))
                  ),
                  tags$div(style = "margin-top:6px;",
                           tags$b("Aperçu du message :"),
                           verbatimTextOutput("en_apercu")),
                  uiOutput("en_statut")
              )
      ),
      tabItem("qualite",
              fluidRow(valueBoxOutput("q_verif", 4), valueBoxOutput("q_doublon", 4), valueBoxOutput("q_delai", 4)),
              fluidRow(
                box(title = titre_info("Pipeline de la surveillance",
                      "Entonnoir du circuit de l'information : du signal collecté jusqu'à l'alerte, en passant par le tri, la vérification et l'évaluation. Visualise le volume retenu à chaque étape."),
                    width = 6, status = "primary",
                    solidHeader = TRUE, plotOutput("p_funnel", height = 300)),
                box(title = titre_info("Délai détection → vérification (jours)",
                      "Distribution du nombre de jours écoulés entre la détection d'un signal et sa vérification. Indicateur de réactivité du système : plus la distribution est resserrée à gauche, plus le système est rapide.",
                      pos = "right"),
                    width = 6,
                    status = "primary", solidHeader = TRUE, plotOutput("p_delai", height = 300))
              )
      ),
      tabItem("apropos",
              box(width = 12, status = "info", solidHeader = TRUE,
                  title = i18n$t("À propos d'i-Tafaray"),
                  tags$div(style = "font-size:14px; line-height:1.65; color:#26333F; text-align:justify;",
                    tags$h4(style = "color:#1e3a5f; margin-top:4px; font-weight:600; text-align:left;",
                            i18n$t("L'approche One Health (Une seule santé)")),
                    tags$p(i18n$t("One Health reconnaît que les santés humaine, animale et environnementale sont interdépendantes. La plupart des maladies émergentes sont d'origine animale (zoonoses) et leur apparition est liée aux contacts exacerbés entre hommes et animaux, aux pressions sur les écosystèmes et au changement climatique. Surveiller les trois secteurs — homme, animal, environnement — simultanément permet de détecter plus tôt et d'agir de façon précoce et coordonnée.")),
                    tags$h4(style = "color:#1e3a5f; font-weight:600;",
                            i18n$t("Le projet AFRICAM Madagascar")),
                    tags$p(i18n$t("AFRICAM Madagascar, porté par l'initiative PREZODE et coordonné par le CIRAD avec le financement de l'AFD, vise à renforcer la surveillance des maladies zoonotiques prioritaires selon une approche One Health intégrée. Dans le district d'Ifanadiana, il développe un système de surveillance associant santé humaine, animale, et de la faune et de l'environnement. La réalisation technique est confiée à l'ONG Pivot.")),
                    tags$h4(style = "color:#1e3a5f; font-weight:600;",
                            i18n$t("La surveillance à base évènementielle (SBE)")),
                    tags$p(i18n$t("La SBE repère les évènements inhabituels liés à la santé humaine, animale ou environnementale et potentiellement indicateurs d'émergence, au plus près du terrain — agents communautaires, centres de santé de base, districts. Chaque signal est collecté par les agents de terrain (application CommCare), trié, vérifié puis évalué selon son niveau de risque, et peut déclencher une alerte. i-Tafaray, plateforme digitale One Health gérée par l'Unité de Gouvernance Digitale (UGD), centralise les signaux des trois secteurs et révèle, par leur croisement dans l'espace et le temps, des menaces qu'aucun secteur ne verrait seul.")),
                    tags$h4(style = "color:#1e3a5f; font-weight:600;",
                            i18n$t("L'appui de la Banque mondiale")),
                    tags$p(i18n$t("Le Projet de préparation et de réponse aux pandémies (PPSB), financé par la Banque mondiale, soutient la mise en place d'un système de notification électronique, en temps réel, interopérable et interconnecté, conforme au Règlement sanitaire international (RSI). i-Tafaray contribue à cet objectif via l'interopérabilité des données (standard FHIR, échange par X-Road).")))),
              box(width = 12, status = "info", solidHeader = TRUE, title = i18n$t("Partenaires"),
                  tags$div(style = "background:#fff; border-radius:8px; padding:14px 8px;
                                    display:flex; flex-wrap:wrap; gap:16px;
                                    align-items:center; justify-content:center;",
                    # Bailleurs (tailles ajustées individuellement)
                    tags$div(style = "display:flex; flex-wrap:wrap; gap:22px; align-items:center;",
                      tags$img(src = "logos/afd.png",
                               style = "height:42px; width:auto; object-fit:contain;", alt = "AFD"),
                      tags$img(src = "logos/banque_mondiale.png",
                               style = "height:66px; width:auto; object-fit:contain;", alt = "Banque mondiale")),
                    tags$div(style = "width:1px; align-self:stretch; min-height:52px;
                                      background:#D9DEE4; margin:0 4px;"),
                    # Gouvernement (Primature)
                    tags$img(src = "logos/madagascar.png",
                             style = "height:54px; width:auto; object-fit:contain;", alt = "Primature"),
                    tags$div(style = "width:1px; align-self:stretch; min-height:52px;
                                      background:#D9DEE4; margin:0 4px;"),
                    # Ministères & partenaires de mise en œuvre
                    tags$div(style = "display:flex; flex-wrap:wrap; gap:22px; align-items:center;",
                      lapply(c("sante.png", "elevage.png", "environnement.png",
                               "africam_prezode.png", "cirad.png", "pivot.png"),
                             function(f) tags$img(src = file.path("logos", f),
                                                  style = "height:54px; width:auto; object-fit:contain;",
                                                  alt = "Partenaire"))))),
              box(width = 12, status = "info", solidHeader = TRUE,
                  title = i18n$t("À propos de cette démonstration"),
                  tags$div(style = "font-size:14px; line-height:1.6; color:#26333F; text-align:justify;",
                    tags$p(i18n$t("Les données affichées sont entièrement synthétiques : un fond de signaux de routine dans lequel sont insérés six scénarios One Health (H5N1, peste, rage, Mpox, Ebola, contamination hydrique). Les trois tables — Signaux, Événements, Alertes — sont reliées par l'identifiant id_signal.")),
                    tags$p(tags$i(i18n$t("Outil : R / R Shiny — solution open source.")))))
      )
    )
  )
)

## =====================================================================
##  Guide de la plateforme (fenêtre modale — présentation des modules)
## =====================================================================
.guide_item <- function(ic, titre, desc) {
  tags$div(style = "display:flex; align-items:flex-start; gap:11px; margin-bottom:11px;",
    tags$div(style = "flex:0 0 30px; height:30px; border-radius:7px; background:#1e3a5f;
                       color:#fff; display:flex; align-items:center; justify-content:center;",
             icon(ic)),
    tags$div(
      tags$div(style = "font-weight:600; color:#26333F;", titre),
      tags$div(style = "color:#5A6672; font-size:13px;", desc)))
}
show_guide_modal <- function() {
  showModal(modalDialog(
    title = tags$div(style = "color:#1e3a5f; font-weight:700;",
                     icon("compass"), " ", i18n$t("Guide de la plateforme i-Tafaray")),
    easyClose = TRUE, size = "l", footer = modalButton(i18n$t("Fermer")),
    tags$div(style = "font-size:14px; line-height:1.55;",
      tags$p(i18n$t("Plateforme de surveillance One Health (Une seule santé). Elle réunit les signaux humains, animaux et environnementaux pour révéler, par leur croisement, des menaces qu'aucun secteur ne verrait seul.")),
      tags$div(style = "font-size:11px; text-transform:uppercase; letter-spacing:.05em;
                         color:#8A93A0; margin:14px 0 8px;", i18n$t("Les modules")),
      fluidRow(
        column(6,
          .guide_item("gauge-high", i18n$t("Synthèse"),
                      i18n$t("Vue d'ensemble pour le comité : chiffres clés, tendance, alertes prioritaires.")),
          .guide_item("gauge", i18n$t("Vue d'ensemble"),
                      i18n$t("Volumes par mois et secteur, niveau de risque, pathogènes suspectés.")),
          .guide_item("map-location-dot", i18n$t("Cartographie"),
                      i18n$t("Localisation des signaux, anneaux d'alerte et cercles de grappes One Health.")),
          .guide_item("triangle-exclamation", i18n$t("Alertes"),
                      i18n$t("Liste des alertes et leur chaîne de décision, du signalement à l'action."))),
        column(6,
          .guide_item("table-cells", i18n$t("Indicateurs"),
                      i18n$t("Les 18 signaux prioritaires et leurs mesures (volumes, cas, décès, alertes).")),
          .guide_item("diagram-project", "One Health",
                      i18n$t("Grappes inter-secteurs et avance de détection gagnée sur le 1er cas humain.")),
          .guide_item("circle-check", i18n$t("Pipeline & qualité"),
                      i18n$t("Du signal collecté à l'alerte : délais de vérification, doublons, taux de tri.")))),
      tags$hr(style = "margin:10px 0;"),
      tags$div(style = "font-size:11px; text-transform:uppercase; letter-spacing:.05em;
                         color:#8A93A0; margin-bottom:8px;", i18n$t("Sur la page Synthèse")),
      tags$ul(style = "color:#5A6672; font-size:13px; padding-left:18px; margin:0;",
        tags$li(tags$b("Période"), " : sélecteur en haut à droite (mois, 3 mois, 12 mois, tout)."),
        tags$li(tags$b("Croisement One Health"), " : dans le tableau des alertes, secteurs croisés et avance de détection."),
        tags$li(tags$b("« + »"), " : déplie les signaux de la grappe ; ", tags$b("clic sur la ligne"),
                " : ouvre la chaîne de décision."),
        tags$li(tags$b("Rapport officiel"), " : génère le bulletin PDF prêt à partager."))
    )
  ))
}

## =====================================================================
##  SERVER
## =====================================================================
server <- function(input, output, session) {
  res_auth <- if (AUTH_ENABLED)
    secure_server(check_credentials(db_path, passphrase = passphrase)) else NULL

  ## ---- Changement de langue (FR / EN / MG) — swap DOM côté client ----
  current_lang <- reactiveVal(I18N_DEFAULT)
  # Applique la langue par défaut à l'ouverture.
  observe({
    current_lang(I18N_DEFAULT)
    session$sendCustomMessage("i18n_set_lang", I18N_DEFAULT)
  }, priority = 1000)
  observeEvent(input$lang, {
    if (!input$lang %in% I18N_LANGS) return()
    current_lang(input$lang)
    session$sendCustomMessage("i18n_set_lang", input$lang)
  }, ignoreInit = TRUE)

  ## ---- Source des données : démonstration (synthétique) ou réel (X-Road) ----
  # Presque tous les onglets passent par base_filtree()/filtree()/exec_data() ;
  # il suffit donc de commuter le jeu de données actif ici, en amont.
  # Cache X-Road relu automatiquement dès que le fichier change (alimenté par ingest_xroad.R).
  # Toute session ouverte se met ainsi à jour sans rechargement de page.
  xroad_cache <- reactivePoll(60000, session,
    checkFunc = xroad_cache_mtime, valueFunc = lire_cache_xroad)

  DATA_ACTIVE <- reactive({
    if (identical(input$data_source, "reel")) {
      d <- xroad_cache()                       # 1. cache local (rafraîchi en continu)
      if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) {
        d <- tryCatch(                         # 2. pas de cache -> appel direct si la machine joint X-Road
          if (exists("charger_xroad")) charger_xroad() else stop("Pont X-Road non disponible."),
          error = function(e) e)
      }
      if (inherits(d, "error") || is.null(d) || !is.data.frame(d) || nrow(d) == 0) {
        showNotification(                      # 3. repli sur la démonstration
          paste0("Données réelles X-Road indisponibles — affichage des données de démonstration. ",
                 if (inherits(d, "error")) conditionMessage(d) else ""),
          type = "warning", duration = 7)
        return(DATA_SIM)
      }
      d
    } else DATA_SIM
  })
  # Indique la source réellement affichée
  output$data_source_badge <- renderUI({
    reel <- identical(input$data_source, "reel")
    ok   <- reel && isTRUE(nrow(DATA_ACTIVE()) > 0) && !identical(DATA_ACTIVE(), DATA_SIM)
    if (!reel) {
      tags$span(class = "ds-badge", style = "background:#5E8B6A;color:#fff;", i18n$t("Démo active"))
    } else if (ok) {
      maj <- if (file.exists(XROAD_CACHE)) format(file.info(XROAD_CACHE)$mtime, "%d/%m %H:%M") else NA
      tags$span(class = "ds-badge", style = "background:#2B6CB0;color:#fff;",
                paste0(i18n$t("X-Road actif"), if (!is.na(maj)) paste0(" · ", i18n$t("maj"), " ", maj) else ""))
    } else {
      tags$span(class = "ds-badge", style = "background:#C2703D;color:#fff;", i18n$t("X-Road indisponible — démo"))
    }
  })

  ## ---- Guide de la plateforme (fenêtre modale) ----
  observeEvent(input$tour, show_guide_modal())

  ## Bornes de dates calées sur la SOURCE ACTIVE (démo ou X-Road réel),
  ## sinon les données réelles (datées après la démo) seraient toutes filtrées.
  act_max <- reactive({
    mx <- suppressWarnings(max(DATA_ACTIVE()$date_de_survenue, na.rm = TRUE))
    if (is.finite(mx)) mx else .maxdate
  })
  act_curstart <- reactive(lubridate::floor_date(act_max(), "month"))

  # Bascule de source -> recale le curseur de dates du menu sur la plage active
  observeEvent(input$data_source, {
    rng <- suppressWarnings(range(DATA_ACTIVE()$date_de_survenue, na.rm = TRUE))
    if (all(is.finite(rng))) {
      if (rng[1] == rng[2]) rng <- c(rng[1] - 1, rng[2] + 1)
      updateSliderInput(session, "dates", min = rng[1], max = rng[2], value = c(rng[1], rng[2]))
    }
  }, ignoreInit = TRUE)

  ## ---- Synthèse exécutive (comité de pilotage) ----
  exec_data <- reactive({
    end <- act_max()
    start <- switch(input$exec_period,
                    "Mois en cours"     = act_curstart(),
                    "3 derniers mois"   = end %m-% months(3),
                    "12 derniers mois"  = end %m-% months(12),
                    min(DATA_ACTIVE()$date_de_survenue, na.rm = TRUE))
    DATA_ACTIVE() %>% filter(date_de_survenue >= start, date_de_survenue <= end,
                    is.na(doublon) | doublon != "Oui")
  })
  output$s_period <- renderText({
    lg <- current_lang()
    paste0(i18n_lookup("Période :", lg), " ", tolower(i18n_lookup(input$exec_period, lg)),
           " ", i18n_lookup("— données arrêtées au", lg), " ", format(act_max(), "%d/%m/%Y"))
  })
  output$s_msg <- renderUI({
    lg <- current_lang()
    d <- exec_data()
    na <- sum(d$a_une_alerte)
    al <- d %>% filter(a_une_alerte)
    susp <- al %>% filter(classification_event != "Non précisé") %>%
      count(classification_event, sort = TRUE) %>% head(3) %>% pull(classification_event)
    tags$ul(class = "synth-msg",
            tags$li(paste0(nrow(d), " ", i18n_lookup("signaux validés sur la période, dont", lg),
                           " ", na, " ", i18n_lookup("ayant déclenché une alerte.", lg))),
            if (na > 0) tags$li(paste0(i18n_lookup("Répartition des alertes :", lg), " ",
                                       repartition_risque(al$niveau_risque, lg), ".")) else NULL,
            if (length(susp) > 0)
              tags$li(HTML(paste0(i18n_lookup("Principales suspicions :", lg),
                                  " <b>", paste(susp, collapse = "</b>, <b>"), "</b>.")))
            else NULL)
  })
  output$s_kpis <- renderUI({
    d <- exec_data()
    kpi <- function(val, lab, cap, cls = "") {
      column(3, tags$div(class = paste("kpi", cls),
                         tags$div(class = "kpi-val", val),
                         tags$div(class = "kpi-lab", lab),
                         tags$div(class = "kpi-cap", cap)))
    }
    fluidRow(
      kpi(nrow(d), "Signaux", "validés sur la période"),
      kpi(sum(as.character(d$niveau_risque) %in% c("Très élevé", "Haute")),
          "À risque élevé", "signaux évalués (TE + haute)", "kpi-warn"),
      kpi(sum(d$a_une_alerte), "Alertes", "déclenchées", "kpi-alert"),
      kpi(dplyr::n_distinct(d$fokontany), "Fokontany", "concernés", "kpi-info")
    )
  })
  output$s_trend <- renderEcharts4r({
    lg <- current_lang()
    full_start <- floor_date(act_curstart() %m-% months(11), "month")
    dd <- DATA_ACTIVE() %>% filter(date_de_survenue >= full_start) %>%
      mutate(mois = floor_date(date_de_survenue, "month")) %>% count(mois, secteur) %>% arrange(mois)
    secs <- sort(unique(dd$secteur)); cols <- unname(COL_SECTEUR[secs])
    dd$secteur <- factor(i18n_vec(dd$secteur, lg), levels = i18n_vec(secs, lg))
    # Le curseur cadre automatiquement la fenêtre choisie en haut de page
    zstart <- switch(input$exec_period,
                     "Mois en cours"   = floor_date(act_max(), "month"),
                     "3 derniers mois" = floor_date(act_max() %m-% months(2), "month"),
                     full_start)
    zend <- floor_date(act_max(), "month")
    dd %>% group_by(secteur) %>% e_charts(mois) %>%
      e_bar(n, stack = "secteur", barWidth = "60%") %>%
      e_color(cols) %>%
      e_tooltip(trigger = "axis", axisPointer = list(type = "shadow")) %>%
      e_legend(top = 4) %>% e_y_axis(name = i18n_lookup("Signaux", lg), minInterval = 1) %>%
      e_x_axis(axisLabel = list(hideOverlap = TRUE)) %>%
      e_toolbox(right = 8, top = 2) %>%
      e_toolbox_feature(feature = "saveAsImage", title = i18n_lookup("Exporter en PNG", lg),
                        name = "i-Tafaray_tendance", backgroundColor = "#FFFFFF", pixelRatio = 2) %>%
      e_datazoom(type = "slider", startValue = format(zstart, "%Y-%m-%d"),
                 endValue = format(zend, "%Y-%m-%d"), bottom = 6, height = 18) %>%
      e_grid(left = 48, right = 18, top = 44, bottom = 64)
  })
  output$s_risk <- renderPlot({
    lg <- current_lang()
    d <- exec_data() %>% filter(niveau_risque != "Non évalué") %>% droplevels()
    shiny::validate(shiny::need(nrow(d) > 0, "Aucun signal évalué sur la période."))
    d %>% count(niveau_risque) %>%
      ggplot(aes(niveau_risque, n, fill = niveau_risque)) +
      geom_col(width = 0.7) + geom_text(aes(label = n), vjust = -0.3, size = 4) +
      scale_fill_manual(values = COL_RISQUE, guide = "none") +
      scale_x_discrete(labels = function(x) i18n_vec(x, lg)) +
      labs(x = NULL, y = NULL) + theme_it
  })
  s_al_data <- reactive({
    exec_data() %>% filter(a_une_alerte) %>% arrange(desc(date_de_survenue))
  })
  # Détail HTML (sous-ligne dépliable) : liste chronologique des signaux d'une grappe
  oh_detail_html <- function(df) {
    df <- df[order(df$date_de_survenue), ]
    rows <- vapply(seq_len(nrow(df)), function(i) {
      s <- as.character(df$secteur[i]); col <- COL_SECTEUR[[s]]
      if (is.null(col) || is.na(col)) col <- "#9AA3AB"
      sprintf(paste0("<tr>",
        "<td style='padding:2px 14px;color:#5A6672;white-space:nowrap;'>%s</td>",
        "<td style='padding:2px 14px;white-space:nowrap;'>",
        "<span style='display:inline-block;width:9px;height:9px;border-radius:50%%;background:%s;margin-right:7px;'></span>%s</td>",
        "<td style='padding:2px 14px;'>%s</td>",
        "<td style='padding:2px 14px;color:#5A6672;'>%s</td></tr>"),
        format(df$date_de_survenue[i], "%d/%m/%Y"), col, s,
        paste0(df$code[i], " &mdash; ", df$signal[i]), as.character(df$niveau_risque[i]))
    }, character(1))
    paste0("<div class='oh-childwrap' style='padding:4px 14px 10px 46px;background:#F4F7FA;cursor:pointer;'>",
      "<div style='font-size:11px;text-transform:uppercase;letter-spacing:.04em;color:#8A93A0;margin:4px 0;'>",
      "Signaux de la grappe &mdash; du 1<sup>er</sup> signal sentinelle au cas humain</div>",
      "<table style='border-collapse:collapse;font-size:12px;'>", paste(rows, collapse = ""), "</table>",
      "<div style='font-size:11px;color:#1e3a5f;margin-top:7px;font-weight:600;'>",
      "&rsaquo; Cliquez pour ouvrir la fiche grappe One Health (chronologie + signaux)</div></div>")
  }
  # Pour chaque alerte : grappe inter-secteurs de son fokontany (fenêtre 14 j amont)
  grappe_par_alerte <- function(d) {
    d <- d %>% filter(!is.na(date_de_survenue))
    anchors <- d %>% filter(a_une_alerte) %>%
      transmute(a_id = id_signal, a_fok = fokontany, a_date = date_de_survenue)
    if (nrow(anchors) == 0) return(NULL)
    members <- anchors %>%
      inner_join(d, by = c("a_fok" = "fokontany")) %>%
      filter(date_de_survenue >= a_date - 14, date_de_survenue <= a_date + 3)
    summ <- members %>% group_by(a_id) %>%
      summarise(nsec = n_distinct(secteur),
                secteurs = paste(sort(unique(secteur)), collapse = " + "),
                humain = any(secteur == "Humain"), nonh = any(secteur != "Humain"),
                t_nh = suppressWarnings(min(date_de_survenue[secteur != "Humain"])),
                t_h  = suppressWarnings(min(date_de_survenue[secteur == "Humain"])),
                .groups = "drop") %>%
      mutate(avance = ifelse(humain & nonh & is.finite(as.numeric(t_nh)) & is.finite(as.numeric(t_h)),
                             as.numeric(t_h - t_nh), NA_real_))
    det <- members %>% group_by(a_id) %>%
      group_modify(~ data.frame(details = oh_detail_html(.x), stringsAsFactors = FALSE)) %>%
      ungroup()
    summ %>% left_join(det, by = "a_id") %>%
      select(a_id, nsec, secteurs, avance, details)
  }
  # Rend la cellule « Croisement One Health » sous forme de chips de secteur colorés
  oh_cell <- function(secteurs, nsec, avance) {
    if (is.na(nsec) || nsec < 2) return("<span style='color:#9AA3AB;'>&mdash;</span>")
    secs <- strsplit(secteurs, " \\+ ")[[1]]
    secs <- secs[order(secs == "Humain")]   # animal / environnement d'abord, humain en bout
    chips <- vapply(secs, function(s) {
      col <- COL_SECTEUR[[s]]; if (is.null(col) || is.na(col)) col <- "#9AA3AB"
      lbl <- if (s == "Environnement") "Env." else s
      sprintf(paste0("<span style='display:inline-block;padding:1px 8px;margin:1px;border-radius:10px;",
                     "background:%s;color:#fff;font-size:11px;font-weight:600;white-space:nowrap;'>%s</span>"),
              col, lbl)
    }, character(1))
    out <- paste(chips, collapse = "<span style='color:#9AA3AB;'> &rsaquo; </span>")
    if (!is.na(avance) && avance > 0)
      out <- paste0(out, " <span style='display:inline-block;padding:1px 8px;margin-left:5px;border-radius:10px;",
                    "background:#1e3a5f;color:#fff;font-size:11px;font-weight:700;'>+", round(avance), " j</span>")
    out
  }
  output$s_alertes <- renderDT({
    g  <- grappe_par_alerte(exec_data())
    d0 <- s_al_data()
    if (!is.null(g)) d0 <- d0 %>% left_join(g, by = c("id_signal" = "a_id"))
    else d0 <- d0 %>% mutate(nsec = NA_integer_, secteurs = NA_character_,
                             avance = NA_real_, details = NA_character_)
    d <- d0 %>%
      mutate(croise = mapply(oh_cell, secteurs, nsec, avance, USE.NAMES = FALSE),
             details = ifelse(is.na(details),
                              "<div style='padding:6px 14px 6px 46px;color:#9AA3AB;font-size:12px;'>Aucune grappe inter-secteurs associée.</div>",
                              details)) %>%
      transmute(` ` = "", Date = format(date_de_survenue, "%d/%m/%Y"), Secteur = secteur,
                Signal = format_signal_label(code, signal, current_lang()), Fokontany = fokontany,
                Suspicion = ifelse(classification_event == "Non précisé", "—", classification_event),
                Risque = as.character(niveau_risque),
                `Croisement One Health` = croise, Alerte = translate_alert(alerte_label, current_lang()),
                .details = details)
    datatable(
      d, rownames = FALSE, selection = "none", escape = FALSE,
      callback = JS(
        "table.on('click','td.details-control',function(e){",
        "  e.stopPropagation();",
        "  var row=table.row($(this).closest('tr')), tog=$(this).find('.oh-toggle');",
        "  if(row.child.isShown()){ row.child.hide(); tog.text('+'); }",
        "  else {",
        "    var ridx=row.index();",
        "    row.child(row.data()[9]).show(); tog.text(String.fromCharCode(8722));",
        "    $(row.child()).find('td').off('click.ohc').on('click.ohc',function(ev){",
        "      ev.stopPropagation();",
        "      if($(this).find('.oh-childwrap').length){",
        "        Shiny.setInputValue('s_oh_click', ridx+1, {priority:'event'});",
        "      }",
        "    });",
        "  }",
        "});",
        "table.on('click','tbody td:not(.details-control)',function(){",
        "  var row=table.row($(this).closest('tr'));",
        "  Shiny.setInputValue('s_alertes_click', row.index()+1, {priority:'event'});",
        "});"),
      options = list(
        pageLength = 8, dom = "tip",
        columnDefs = list(
          list(targets = 0, orderable = FALSE, className = "details-control",
               render = JS("function(data,type,row){return \"<span class='oh-toggle'>+</span>\";}")),
          list(visible = FALSE, targets = 9),
          list(targets = 7, orderable = FALSE))))
  })
  s_al_sel <- reactive({
    i <- input$s_alertes_click
    shiny::validate(shiny::need(!is.null(i) && i >= 1,
                                "Cliquez sur une alerte pour afficher sa chaîne de décision."))
    s_al_data()[i, ]
  })
  output$s_al_summary <- renderUI({ alerte_summary_ui(s_al_sel()) })
  output$s_al_chain   <- renderDT({ alerte_chain_dt(s_al_sel()) })
  observeEvent(input$s_alertes_click, {
    i <- input$s_alertes_click
    if (is.null(i) || i < 1) return()
    a <- s_al_data()[i, ]
    showModal(modalDialog(
      title = paste0("Chaîne de décision — ", format_signal_label(a$code, a$signal, current_lang()), " · ", a$fokontany),
      uiOutput("s_al_summary"), DTOutput("s_al_chain"),
      size = "l", easyClose = TRUE, footer = modalButton("Fermer")
    ))
  }, ignoreInit = TRUE)

  ## Clic sur une sous-ligne dépliée -> fiche « grappe One Health » (même popup que l'onglet One Health)
  s_oh_sig <- reactive({
    i <- input$s_oh_click
    shiny::validate(shiny::need(!is.null(i) && i >= 1, ""))
    a <- s_al_data()[i, ]
    exec_data() %>%
      filter(fokontany == a$fokontany,
             date_de_survenue >= a$date_de_survenue - 14,
             date_de_survenue <= a$date_de_survenue + 3) %>%
      arrange(date_de_survenue)
  })
  s_oh_sel <- reactive({
    d <- s_oh_sig(); shiny::validate(shiny::need(nrow(d) > 0, ""))
    a   <- s_al_data()[input$s_oh_click, ]
    tnh <- suppressWarnings(min(d$date_de_survenue[d$secteur != "Humain"]))
    th  <- suppressWarnings(min(d$date_de_survenue[d$secteur == "Humain"]))
    has_h <- any(d$secteur == "Humain"); has_nh <- any(d$secteur != "Humain")
    data.frame(
      fokontany = a$fokontany,
      debut = min(d$date_de_survenue), fin = max(d$date_de_survenue),
      secteurs = paste(sort(unique(as.character(d$secteur))), collapse = " + "),
      nb = nrow(d), cas = sum(d$Nombre_cas), deces = sum(d$Nombre_deces),
      risque = unname(RISK_LAB[as.character(max(SEV[as.character(d$niveau_risque)]))]),
      suspicion = paste(setdiff(unique(d$classification_event), "Non précisé"), collapse = " ; "),
      avance = if (has_h && has_nh && is.finite(as.numeric(tnh)) && is.finite(as.numeric(th)))
                 as.numeric(th - tnh) else NA_real_,
      stringsAsFactors = FALSE)
  })
  output$s_oh_summary  <- renderUI({ oh_summary_ui(s_oh_sel()) })
  output$s_oh_timeline <- renderPlot({ oh_timeline_plot(s_oh_sig()) })
  output$s_oh_signals  <- renderDT({ oh_signals_dt(s_oh_sig()) })
  observeEvent(input$s_oh_click, {
    i <- input$s_oh_click
    if (is.null(i) || i < 1) return()
    g <- s_oh_sel()
    showModal(modalDialog(
      title = paste0("Grappe One Health — fokontany ", g$fokontany),
      uiOutput("s_oh_summary"),
      plotOutput("s_oh_timeline", height = 240),
      DTOutput("s_oh_signals"),
      size = "l", easyClose = TRUE, footer = modalButton("Fermer")
    ))
  }, ignoreInit = TRUE)

  ## ---- Rapport officiel (PDF avec graphiques) ----
  output$dl_report <- downloadHandler(
    filename = function() {
      ext <- if (identical(input$report_format, "html")) "html" else "pdf"
      paste0("i-Tafaray_Rapport_Synthese_", format(Sys.Date(), "%Y%m%d"), ".", ext)
    },
    content = function(file) {
      ## --- Variante HTML interactive (carte + tableau + graphiques) ---
      if (identical(input$report_format, "html")) {
        lg2 <- current_lang()
        periode_lbl2 <- tolower(i18n_lookup(input$exec_period, lg2))
        report_html_render(file, exec_data(), DATA_ACTIVE(), lg2, periode_lbl2)
        return(invisible(NULL))
      }
      d   <- exec_data()
      na  <- sum(d$a_une_alerte)
      al  <- d %>% dplyr::filter(a_une_alerte)
      nte <- sum(as.character(al$niveau_risque) == "Très élevé")
      nh  <- sum(as.character(al$niveau_risque) == "Haute")
      nte_all <- sum(as.character(d$niveau_risque) == "Très élevé")
      nh_all  <- sum(as.character(d$niveau_risque) == "Haute")
      nf  <- dplyr::n_distinct(d$fokontany)
      lg <- current_lang()
      T  <- function(k) i18n_lookup(k, lg)
      periode_lbl <- tolower(i18n_lookup(input$exec_period, lg))

      ## --- Texte analytique (contexte / situation) + recommandations ---
      contexte   <- report_contexte(lg, d, na, al, periode_lbl)
      contexte_w <- paste(strwrap(contexte, width = 104), collapse = "\n")
      reco_txt   <- report_reco(lg)

      ## --- Figures ---
      dd <- DATA_ACTIVE() %>% filter(date_de_survenue >= (act_curstart() %m-% months(11))) %>%
        mutate(mois = floor_date(date_de_survenue, "month")) %>% count(mois, secteur)
      g_trend <- ggplot(dd, aes(mois, n, fill = secteur)) +
        geom_col(width = 22) +
        scale_fill_manual(values = COL_SECTEUR, name = NULL,
                          labels = function(x) i18n_vec(x, lg)) +
        scale_x_date(date_labels = "%b %y", date_breaks = "2 months") +
        scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
        labs(x = NULL, y = T("Signaux")) + theme_it +
        theme(legend.position = "top", legend.title = ggplot2::element_blank(),
              legend.key.size = grid::unit(4, "mm"),
              legend.text = ggplot2::element_text(size = 8))
      dr <- d %>% filter(niveau_risque != "Non évalué") %>% droplevels() %>% count(niveau_risque)
      g_risk <- if (nrow(dr) > 0)
        ggplot(dr, aes(niveau_risque, n, fill = niveau_risque)) +
          geom_col(width = .7) + geom_text(aes(label = n), vjust = -0.4, size = 3.4) +
          scale_fill_manual(values = COL_RISQUE, guide = "none") +
          scale_x_discrete(labels = function(x) i18n_vec(x, lg)) +
          scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.16))) +
          labs(x = NULL, y = NULL) + theme_it
        else NULL

      ## --- Tableau alertes ---
      alertes <- d %>% filter(a_une_alerte) %>% arrange(desc(date_de_survenue)) %>%
        transmute(Date = format(date_de_survenue, "%d/%m/%Y"), Secteur = secteur,
                  Signal = format_signal_label(code, signal, lg, sep = " - "), Fokontany = fokontany,
                  Suspicion = ifelse(classification_event == "Non précisé", "-", classification_event),
                  Risque = as.character(niveau_risque),
                  Alerte = translate_alert(alerte_label, lg)) %>% head(12)
      names(alertes) <- c(T("Date"), T("Secteur"), T("Signal"), T("Fokontany"), T("Suspicion"), T("Risque"), T("Alerte"))
      tt <- gridExtra::ttheme_minimal(base_size = 8, padding = grid::unit(c(4, 3), "mm"),
              core = list(fg_params = list(hjust = 0, x = 0.04)),
              colhead = list(bg_params = list(fill = "#1e3a5f"),
                             fg_params = list(col = "#ffffff", fontface = "bold", hjust = 0, x = 0.04)))
      tab <- if (nrow(alertes) > 0) gridExtra::tableGrob(alertes, rows = NULL, theme = tt)
             else grid::textGrob(T("Aucune alerte sur la période."), x = 0.06, hjust = 0,
                                 gp = grid::gpar(col = "#5A6672"))

      ## --- Helpers de mise en page ---
      TXT <- function(x, y, label, size = 9, col = "#26333F", face = "plain", hjust = 0, vjust = 1, lh = 1.4)
        grid::grid.text(label, grid::unit(x, "npc"), grid::unit(y, "npc"), hjust = hjust, vjust = vjust,
                        gp = grid::gpar(fontsize = size, col = col, fontface = face, lineheight = lh))
      RULE <- function(y, x0 = 0.06, x1 = 0.94, col = "#1e3a5f", lwd = 1.1)
        grid::grid.lines(grid::unit(c(x0, x1), "npc"), grid::unit(c(y, y), "npc"),
                         gp = grid::gpar(col = col, lwd = lwd))
      BAND <- function(y0, y1, fill = "#1e3a5f")
        grid::grid.rect(grid::unit(0.5, "npc"), grid::unit((y0 + y1) / 2, "npc"),
                        width = grid::unit(1, "npc"), height = grid::unit(y1 - y0, "npc"),
                        gp = grid::gpar(fill = fill, col = NA))
      KPI <- function(xc, yc, w, h, val, lab) {
        grid::grid.rect(grid::unit(xc, "npc"), grid::unit(yc, "npc"),
                        width = grid::unit(w, "npc"), height = grid::unit(h, "npc"),
                        gp = grid::gpar(fill = "#F4F7FA", col = "#D9DEE4"))
        grid::grid.text(val, grid::unit(xc, "npc"), grid::unit(yc + h * 0.16, "npc"),
                        gp = grid::gpar(fontsize = 19, fontface = "bold", col = "#1e3a5f"))
        grid::grid.text(lab, grid::unit(xc, "npc"), grid::unit(yc - h * 0.26, "npc"),
                        gp = grid::gpar(fontsize = 8, col = "#5A6672"))
      }
      FIG <- function(g, x, y, w, h) {
        grid::pushViewport(grid::viewport(x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
                                          width = grid::unit(w, "npc"), height = grid::unit(h, "npc"),
                                          just = c("left", "bottom")))
        grid::grid.draw(ggplot2::ggplotGrob(g)); grid::popViewport()
      }
      TABFIG <- function(tg, yTop, x0 = 0.06, x1 = 0.94) {
        h <- grid::grobHeight(tg)
        grid::pushViewport(grid::viewport(x = grid::unit((x0 + x1) / 2, "npc"),
                                          y = grid::unit(yTop, "npc") - 0.5 * h,
                                          width = grid::unit(x1 - x0, "npc"), height = h))
        grid::grid.draw(tg); grid::popViewport()
      }

      grDevices::pdf(file, width = 8.27, height = 11.69, onefile = TRUE, encoding = "ISOLatin1")
      on.exit(grDevices::dev.off(), add = TRUE)

      ## ===================== PAGE 1 =====================
      BAND(0.905, 1)
      TXT(0.06, 0.963, paste0(T("BULLETIN DE SURVEILLANCE"), "  -  One Health"), 15, "#FFFFFF", "bold", 0, 0.5)
      TXT(0.06, 0.927, paste0("i-Tafaray - ", T("Plateforme nationale Une seule santé - République de Madagascar")),
          8.5, "#CFE0EE", "plain", 0, 0.5)
      TXT(0.94, 0.963, T("RAPPORT DE SYNTHÈSE"), 10.5, "#9FD3EE", "bold", 1, 0.5)
      TXT(0.94, 0.927, paste0(T("Période :"), " ", periode_lbl, "   |   ", T("arrêté au"), " ",
                              format(act_max(), "%d/%m/%Y")), 8, "#CFE0EE", "plain", 1, 0.5)

      TXT(0.06, 0.875, paste0("1.  ", T("CONTEXTE ET ANALYSE DE LA SITUATION")), 11.5, "#1e3a5f", "bold", 0, 1)
      RULE(0.856)
      TXT(0.06, 0.846, contexte_w, 9.5, "#26333F", "plain", 0, 1, 1.5)

      TXT(0.06, 0.655, paste0("2.  ", T("CHIFFRES CLÉS DE LA PÉRIODE")), 11.5, "#1e3a5f", "bold", 0, 1)
      RULE(0.637)
      KPI(0.155, 0.578, 0.19, 0.075, nrow(d), T("Signaux validés"))
      KPI(0.385, 0.578, 0.19, 0.075, nte_all + nh_all, T("À risque élevé"))
      KPI(0.615, 0.578, 0.19, 0.075, na, T("Alertes"))
      KPI(0.845, 0.578, 0.19, 0.075, nf, T("Fokontany"))

      TXT(0.06, 0.515, paste0("3.  ", T("TENDANCE DE L'ACTIVITÉ (12 DERNIERS MOIS)")), 11.5, "#1e3a5f", "bold", 0, 1)
      RULE(0.497)
      FIG(g_trend, 0.05, 0.11, 0.90, 0.37)
      TXT(0.06, 0.095, T("Figure 1. Nombre de signaux validés par mois et par secteur (humain, animal, environnement)."),
          8, "#5A6672", "plain", 0, 1)
      RULE(0.05, 0.06, 0.94, "#D9DEE4", 0.8)
      TXT(0.06, 0.033, paste0("i-Tafaray - ", T("Données de démonstration (synthétiques). Document généré automatiquement.")),
          7.5, "#8A93A0", "plain", 0, 0.5)
      TXT(0.94, 0.033, "Page 1 / 2", 7.5, "#8A93A0", "plain", 1, 0.5)

      ## ===================== PAGE 2 =====================
      grid::grid.newpage()
      BAND(0.95, 1)
      TXT(0.06, 0.975, paste0("i-Tafaray - ", T("Bulletin de surveillance One Health (suite)")), 10.5, "#FFFFFF", "bold", 0, 0.5)
      TXT(0.94, 0.975, paste0(T("Période :"), " ", periode_lbl), 8, "#CFE0EE", "plain", 1, 0.5)

      TXT(0.06, 0.925, paste0("4.  ", T("NIVEAU DE RISQUE")), 11.5, "#1e3a5f", "bold", 0, 1)
      RULE(0.907)
      if (!is.null(g_risk)) FIG(g_risk, 0.06, 0.66, 0.88, 0.225)
      else TXT(0.06, 0.88, T("Aucun signal évalué sur la période."), 9.5, "#5A6672", "plain", 0, 1)
      TXT(0.06, 0.645, T("Figure 2. Répartition des signaux évalués par niveau de risque."),
          8, "#5A6672", "plain", 0, 1)

      TXT(0.06, 0.59, paste0("5.  ", T("ALERTES PRIORITAIRES")), 11.5, "#1e3a5f", "bold", 0, 1)
      RULE(0.572)
      TABFIG(tab, 0.555)
      th_npc  <- grid::convertHeight(grid::grobHeight(tab), "npc", valueOnly = TRUE)
      sec6_y  <- 0.555 - th_npc - 0.045
      TXT(0.06, sec6_y, paste0("6.  ", T("RECOMMANDATIONS")), 11.5, "#1e3a5f", "bold", 0, 1)
      RULE(sec6_y - 0.018)
      TXT(0.06, sec6_y - 0.030, reco_txt, 9.5, "#26333F", "plain", 0, 1, 1.65)
      RULE(0.05, 0.06, 0.94, "#D9DEE4", 0.8)
      TXT(0.06, 0.033, paste0("i-Tafaray - ", T("Surveillance intégrée One Health - Comité de pilotage.")),
          7.5, "#8A93A0", "plain", 0, 0.5)
      TXT(0.94, 0.033, "Page 2 / 2", 7.5, "#8A93A0", "plain", 1, 0.5)
    }
  )

  base_filtree <- reactive({
    rng <- if (length(input$dates) == 2) input$dates else DRANGE
    # Si le curseur (calé sur une autre source) ne recouvre pas les données
    # actives, on prend leur propre plage (évite un affichage vide en mode réel).
    drng <- suppressWarnings(range(DATA_ACTIVE()$date_de_survenue, na.rm = TRUE))
    if (all(is.finite(drng)) && (rng[2] < drng[1] || rng[1] > drng[2])) rng <- drng
    fok <- if (is.null(input$fokontany)) "Tous" else input$fokontany
    d <- DATA_ACTIVE() %>% filter(date_de_survenue >= rng[1], date_de_survenue <= rng[2])
    if (fok != "Tous") d <- d %>% filter(fokontany == fok)
    d
  })
  filtree <- reactive({
    d <- base_filtree() %>% filter(is.na(doublon) | doublon != "Oui")
    if (isTRUE(input$verif_only)) d <- d %>% filter(is_verifie == "Oui")
    d
  })
  
  ## KPI
  output$kpi_total  <- renderValueBox(valueBox(nrow(filtree()), "Signaux", icon = icon("bell"), color = "green"))
  output$kpi_verif  <- renderValueBox({
    d <- base_filtree(); pct <- if (nrow(d)) round(100 * mean(d$is_verifie == "Oui", na.rm = TRUE)) else 0
    valueBox(paste0(pct, "%"), "Vérifiés", icon = icon("check"), color = "aqua")
  })
  output$kpi_eval   <- renderValueBox(valueBox(sum(filtree()$a_ete_evalue), "Évalués", icon = icon("clipboard-check"), color = "blue"))
  output$kpi_alertes<- renderValueBox(valueBox(sum(filtree()$a_une_alerte), "Alertes", icon = icon("triangle-exclamation"), color = "red"))
  
  ## Volume (echarts4r interactif + slider temporel)
  output$p_volume <- renderEcharts4r({
    lg <- current_lang()
    d <- filtree(); shiny::validate(shiny::need(nrow(d) > 0, "Aucun signal."))
    dd <- d %>% mutate(mois = floor_date(date_de_survenue, "month")) %>%
      count(mois, secteur) %>% arrange(mois)
    secs <- sort(unique(dd$secteur)); cols <- unname(COL_SECTEUR[secs])
    dd$secteur <- factor(i18n_vec(dd$secteur, lg), levels = i18n_vec(secs, lg))
    dd %>% group_by(secteur) %>% e_charts(mois) %>%
      e_bar(n, stack = "secteur", barWidth = "60%") %>%
      e_color(cols) %>%
      e_tooltip(trigger = "axis", axisPointer = list(type = "shadow")) %>%
      e_legend(top = 4) %>%
      e_y_axis(name = i18n_lookup("Signaux", lg), minInterval = 1) %>%
      e_toolbox(right = 8, top = 2) %>%
      e_toolbox_feature(feature = "saveAsImage", title = i18n_lookup("Exporter en PNG", lg),
                        name = "i-Tafaray_volume", backgroundColor = "#FFFFFF", pixelRatio = 2) %>%
      e_datazoom(type = "slider", bottom = 6, height = 18) %>%
      e_grid(left = 48, right = 18, top = 44, bottom = 64) %>%
      e_x_axis(axisLabel = list(hideOverlap = TRUE))
  })
  
  ## Risque (évalués)
  output$p_risque <- renderPlot({
    lg <- current_lang()
    d <- filtree() %>% filter(niveau_risque != "Non évalué") %>% droplevels()
    shiny::validate(shiny::need(nrow(d) > 0, "Aucun signal évalué pour ces filtres."))
    d %>% count(niveau_risque) %>%
      ggplot(aes(niveau_risque, n, fill = niveau_risque)) +
      geom_col(width = 0.7) + geom_text(aes(label = n), vjust = -0.3, size = 4) +
      scale_fill_manual(values = COL_RISQUE, guide = "none") +
      scale_x_discrete(labels = function(x) i18n_vec(x, lg)) +
      labs(x = NULL, y = NULL) + theme_it
  })
  
  ## Pathogènes suspectés
  output$p_events <- renderEcharts4r({
    lg <- current_lang()
    d <- filtree() %>% filter(classification_event != "Non précisé")
    shiny::validate(shiny::need(nrow(d) > 0, "Aucun événement classé pour ces filtres."))
    d %>% count(classification_event, sort = TRUE) %>% head(10) %>% arrange(n) %>%
      e_charts(classification_event) %>%
      e_bar(n, name = i18n_lookup("Signalements", lg), legend = FALSE) %>%
      e_flip_coords() %>% e_color("#9E2A2B") %>%
      e_tooltip(trigger = "axis") %>% e_legend(show = FALSE) %>%
      e_x_axis(minInterval = 1) %>% e_grid(top = 8, bottom = 16, left = 130, right = 16)
  })

  ## Répartition par signal
  output$p_signaux <- renderEcharts4r({
    lg <- current_lang()
    d <- filtree(); shiny::validate(shiny::need(nrow(d) > 0, "Aucun signal."))
    d %>% mutate(signal_label = translate_signal(signal, lg)) %>%
      count(signal_label, sort = TRUE) %>% head(12) %>% arrange(n) %>%
      e_charts(signal_label) %>%
      e_bar(n, name = i18n_lookup("Signalements", lg), legend = FALSE) %>%
      e_flip_coords() %>% e_color("#35618E") %>%
      e_tooltip(trigger = "axis") %>% e_legend(show = FALSE) %>%
      e_x_axis(minInterval = 1) %>% e_grid(top = 8, bottom = 16, left = 150, right = 16)
  })
  
  ## Carte
  output$carte <- renderLeaflet({
    lg <- current_lang()
    d <- filtree() %>% filter(!is.na(lat), !is.na(lon))
    shiny::validate(shiny::need(nrow(d) > 0, "Aucun signal géolocalisé."))
    pal <- colorFactor(unname(COL_SECTEUR[SECTEURS]), domain = SECTEURS)
    gr <- tryCatch(oh(), error = function(e) NULL)   # grappes inter-secteurs (peut être vide)
    d <- d %>% mutate(signal_label = format_signal_label(code, signal, lg))

    m <- leaflet(d) %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(~lon, ~lat, radius = 5, stroke = FALSE, fillOpacity = 0.7,
                       color = ~pal(secteur), group = "Signaux",
                       popup = ~paste0("<b>", signal_label, "</b><br>",
                                       "Date : ", format(date_de_survenue, "%d/%m/%Y"), "<br>",
                                       "Fokontany : ", fokontany, "<br>",
                                       "Événement : ", classification_event, "<br>",
                                       "Cas : ", Nombre_cas, " — Décès : ", Nombre_deces, "<br>",
                                       "Risque : ", niveau_risque))

    ## Cercles autour des grappes One Health
    if (!is.null(gr) && nrow(gr) > 0) {
      grc <- gr %>% left_join(FOK_CENTROIDS, by = "fokontany") %>% filter(!is.na(lat))
      if (nrow(grc) > 0)
        m <- m %>% addCircles(
          data = grc, lng = ~lon, lat = ~lat,
          radius = ~pmin(2600, 1000 + 110 * nb),
          color = "#1e3a5f", weight = 2, opacity = 0.9, dashArray = "6,5",
          fillColor = "#38bdf8", fillOpacity = 0.08, group = "Grappes One Health",
          popup = ~paste0("<b>Grappe One Health — ", fokontany, "</b><br>",
                          "Période : ", format(debut, "%d/%m/%Y"), " – ",
                          format(fin, "%d/%m/%Y"), "<br>",
                          "Secteurs croisés : ", secteurs, "<br>",
                          "Signaux : ", nb, " — Risque max : ", risque,
                          ifelse(!is.na(avance),
                                 paste0("<br>Avance de détection : ", round(avance), " j"), ""),
                          ifelse(nchar(suspicion) > 0, paste0("<br>Suspicion : ", suspicion), "")))
    }

    ## Anneau rouge sur les signaux ayant déclenché une alerte
    da <- d %>% filter(a_une_alerte)
    if (nrow(da) > 0)
      m <- m %>% addCircleMarkers(
        data = da, lng = ~lon, lat = ~lat, radius = 10, stroke = TRUE, weight = 2.5,
        color = "#9E2A2B", fillOpacity = 0, opacity = 1, group = "Alertes",
        popup = ~paste0("<b>ALERTE — ", signal_label, "</b><br>",
                        "Date : ", format(date_de_survenue, "%d/%m/%Y"), "<br>",
                        "Fokontany : ", fokontany, "<br>",
                        "Niveau de risque : ", niveau_risque, "<br>",
                        "Action : ", translate_alert(alerte_label, lg)))

    m %>%
      addLegend("bottomright", pal = pal, values = SECTEURS, title = "Secteur") %>%
      addLayersControl(overlayGroups = c("Signaux", "Grappes One Health", "Alertes"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("Grappes One Health")
  })
  ## Cibles de navigation (grappes + alertes) pour le sélecteur de la carte
  carte_targets <- reactive({
    d  <- filtree() %>% filter(!is.na(lat), !is.na(lon))
    gr <- tryCatch(oh(), error = function(e) NULL)
    grc <- if (!is.null(gr) && nrow(gr) > 0)
      gr %>% left_join(FOK_CENTROIDS, by = "fokontany") %>% filter(!is.na(lat)) else NULL
    da <- d %>% filter(a_une_alerte) %>% arrange(desc(date_de_survenue))
    list(grc = grc, da = da)
  })
  observe({
    tg <- carte_targets()
    ch <- list("— Vue d'ensemble —" = "")
    if (!is.null(tg$grc) && nrow(tg$grc) > 0)
      ch[["Grappes One Health"]] <- setNames(
        paste0("G:", seq_len(nrow(tg$grc))),
        paste0(tg$grc$fokontany, " — ",
               ifelse(nchar(tg$grc$suspicion) > 0, tg$grc$suspicion, "grappe inter-secteurs"),
               " (", format(tg$grc$debut, "%d/%m"), "–", format(tg$grc$fin, "%d/%m/%y"), ")"))
    if (nrow(tg$da) > 0)
      ch[["Alertes"]] <- setNames(
        paste0("A:", seq_len(nrow(tg$da))),
        paste0(tg$da$code, " — ", tg$da$fokontany,
               " (", format(tg$da$date_de_survenue, "%d/%m/%y"), ")"))
    updateSelectInput(session, "carte_nav", choices = ch, selected = isolate(input$carte_nav))
  })
  observeEvent(input$carte_nav, {
    v <- input$carte_nav; tg <- carte_targets()
    proxy <- leafletProxy("carte")
    if (is.null(v) || v == "") {
      d <- filtree() %>% filter(!is.na(lat), !is.na(lon))
      if (nrow(d) > 0)
        proxy %>% flyToBounds(min(d$lon), min(d$lat), max(d$lon), max(d$lat))
      return()
    }
    p <- strsplit(v, ":")[[1]]; idx <- suppressWarnings(as.integer(p[2]))
    if (p[1] == "G" && !is.null(tg$grc) && !is.na(idx) && idx <= nrow(tg$grc)) {
      g <- tg$grc[idx, ]; proxy %>% flyTo(g$lon, g$lat, zoom = 14)
    } else if (p[1] == "A" && !is.na(idx) && idx <= nrow(tg$da)) {
      a <- tg$da[idx, ]; proxy %>% flyTo(a$lon, a$lat, zoom = 15)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$carte_reset, {
    updateSelectInput(session, "carte_nav", selected = "")
    d <- filtree() %>% filter(!is.na(lat), !is.na(lon))
    if (nrow(d) > 0)
      leafletProxy("carte") %>%
        flyToBounds(min(d$lon), min(d$lat), max(d$lon), max(d$lat))
  })

  ## ---- Climat & environnement (district, données MDG) ----
  .need_climate <- function() shiny::need(
    !is.null(CLIMATE) && nrow(CLIMATE) > 0,
    "Données climatiques non chargées. Lancez extract_climate_mdg.R pour générer data_poc/climate_ifanadiana.csv.")
  output$clim_pluie <- renderEcharts4r({
    shiny::validate(.need_climate())
    lg <- current_lang()
    CLIMATE %>% e_charts(date) %>%
      e_bar(pluie_mm, name = i18n_lookup("Pluie (mm)", lg)) %>%
      e_line(pluie_normale, name = i18n_lookup("Normale", lg), symbol = "none") %>%
      e_color(c("#2B6CB0", "#9AA3AB")) %>%
      e_tooltip(trigger = "axis") %>% e_legend(top = 4) %>%
      e_toolbox(right = 8, top = 2) %>%
      e_toolbox_feature(feature = "saveAsImage", title = i18n_lookup("Exporter en PNG", lg),
                        name = "i-Tafaray_pluviometrie", backgroundColor = "#FFFFFF", pixelRatio = 2) %>%
      e_datazoom(type = "slider", startValue = "2023-01-01",
                 endValue = format(max(CLIMATE$date), "%Y-%m-%d"), bottom = 6, height = 18) %>%
      e_grid(left = 48, right = 16, top = 44, bottom = 64)
  })
  output$clim_temp <- renderEcharts4r({
    shiny::validate(.need_climate())
    lg <- current_lang()
    CLIMATE %>% e_charts(date) %>%
      e_line(temp_moy, name = i18n_lookup("Température (°C)", lg), symbol = "none") %>%
      e_line(temp_normale, name = i18n_lookup("Normale", lg), symbol = "none") %>%
      e_color(c("#9E2A2B", "#9AA3AB")) %>%
      e_tooltip(trigger = "axis") %>% e_legend(top = 4) %>%
      e_toolbox(right = 8, top = 2) %>%
      e_toolbox_feature(feature = "saveAsImage", title = i18n_lookup("Exporter en PNG", lg),
                        name = "i-Tafaray_temperature", backgroundColor = "#FFFFFF", pixelRatio = 2) %>%
      e_datazoom(type = "slider", startValue = "2023-01-01",
                 endValue = format(max(CLIMATE$date), "%Y-%m-%d"), bottom = 6, height = 18) %>%
      e_grid(left = 44, right = 16, top = 44, bottom = 64)
  })
  output$clim_fwi <- renderEcharts4r({
    shiny::validate(.need_climate())
    shiny::validate(shiny::need(any(!is.na(CLIMATE$fwi)), "FWI non disponible dans l'extraction."))
    lg <- current_lang()
    CLIMATE %>% e_charts(date) %>%
      e_line(fwi, name = "FWI", symbol = "none") %>%
      e_line(fwi_normale, name = i18n_lookup("Normale", lg), symbol = "none") %>%
      e_color(c("#C2703D", "#9AA3AB")) %>%
      e_tooltip(trigger = "axis") %>% e_legend(top = 4) %>%
      e_toolbox(right = 8, top = 2) %>%
      e_toolbox_feature(feature = "saveAsImage", title = i18n_lookup("Exporter en PNG", lg),
                        name = "i-Tafaray_fwi", backgroundColor = "#FFFFFF", pixelRatio = 2) %>%
      e_datazoom(type = "slider", startValue = "2023-01-01",
                 endValue = format(max(CLIMATE$date), "%Y-%m-%d"), bottom = 6, height = 18) %>%
      e_grid(left = 44, right = 16, top = 44, bottom = 64)
  })
  output$clim_overlay <- renderEcharts4r({
    shiny::validate(.need_climate())
    lg <- current_lang()
    env <- filtree() %>% filter(secteur == "Environnement", !is.na(date_de_survenue)) %>%
      mutate(m = lubridate::month(date_de_survenue)) %>% count(m, name = "n_env")
    clim <- CLIMATE %>% group_by(mois) %>%
      summarise(fwi_moy = mean(fwi, na.rm = TRUE), .groups = "drop")
    labs <- c("Jan","Fév","Mar","Avr","Mai","Jun","Jui","Aoû","Sep","Oct","Nov","Déc")
    df <- data.frame(mois = 1:12) %>%
      left_join(env, by = c("mois" = "m")) %>%
      left_join(clim, by = "mois") %>%
      mutate(n_env = ifelse(is.na(n_env), 0, n_env), lab = labs[mois])
    df %>% e_charts(lab) %>%
      e_bar(n_env, name = i18n_lookup("Signaux environnementaux", lg)) %>%
      e_line(fwi_moy, name = i18n_lookup("FWI moyen", lg), y_index = 1, symbol = "circle", symbolSize = 6) %>%
      e_color(c("#5E8B6A", "#C2703D")) %>%
      e_y_axis(index = 0, name = "Signaux", minInterval = 1) %>%
      e_y_axis(index = 1, name = "FWI") %>%
      e_tooltip(trigger = "axis") %>% e_legend(top = 4) %>%
      e_grid(left = 46, right = 46, top = 36, bottom = 36)
  })

  ## ============ Onglet Environnement (SMART) — couche autonome ============
  env_filtree <- reactive({
    if (is.null(SMART_ENV)) return(NULL)
    s <- SMART_ENV
    if (!is.null(input$env_secteur) && input$env_secteur != "Tous")
      s <- s[s$secteur == input$env_secteur, , drop = FALSE]
    if (!is.null(input$env_annee) && input$env_annee != "Toutes")
      s <- s[!is.na(s$annee) & s$annee == input$env_annee, , drop = FALSE]
    s
  })

  output$env_sub <- renderText({
    s <- env_filtree()
    if (is.null(s) || nrow(s) == 0) return("Données SMART non chargées.")
    dr <- suppressWarnings(range(s$date, na.rm = TRUE))
    paste0(nrow(s), " observations · ", format(dr[1], "%b %Y"), " – ", format(dr[2], "%b %Y"))
  })

  output$env_kpis <- renderUI({
    s <- env_filtree()
    if (is.null(s) || nrow(s) == 0)
      return(div(class = "synth-msg", "Aucune donnée environnementale SMART disponible."))
    npat <- length(unique(s$patrol[nzchar(s$patrol)]))
    nesp <- length(unique(s$especes[nzchar(s$especes)]))
    pmen <- round(100 * mean(s$est_menace, na.rm = TRUE))
    ncont <- sum(s$contact_dom, na.rm = TRUE)
    ncov <- if (!is.null(CORR_ENV))
      sum(grepl("directe|partielle|indirecte", CORR_ENV$type_correspondance, ignore.case = TRUE)) else 0
    kp <- function(v, lab, col) column(2,
      div(style = "background:#fff;border:1px solid #E3E7EB;border-radius:8px;padding:12px 8px;text-align:center;margin-bottom:8px;",
          div(style = paste0("font-size:23px;font-weight:700;color:", col, ";"), v),
          div(style = "font-size:11px;color:#5A6672;margin-top:3px;", lab)))
    fluidRow(
      kp(nrow(s), i18n$t("Observations"), "#1e3a5f"),
      kp(npat, i18n$t("Patrouilles"), "#35618E"),
      kp(paste0(pmen, "%"), i18n$t("Part de menaces"), "#9E2A2B"),
      kp(nesp, i18n$t("Espèces observées"), "#5E8B6A"),
      kp(ncont, i18n$t("Contacts domestiques"), "#C2703D"),
      kp(ncov, i18n$t("Signaux SBE couverts"), "#2E5A40"))
  })

  output$env_map <- renderLeaflet({
    s <- env_filtree()
    shiny::validate(shiny::need(!is.null(s) && nrow(s) > 0, "Aucune donnée."))
    dm <- s[!is.na(s$lat) & !is.na(s$lon), , drop = FALSE]
    shiny::validate(shiny::need(nrow(dm) > 0, "Aucune observation géolocalisée."))
    cats <- names(COL_ENVCAT)
    dm$catx <- ifelse(dm$categorie1 %in% cats, dm$categorie1, "Observation directe")
    pal <- colorFactor(unname(COL_ENVCAT[cats]), domain = cats)
    m <- leaflet(dm) %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(~lon, ~lat, radius = 4, stroke = FALSE, fillOpacity = 0.65,
        color = ~pal(catx), group = "Observations",
        popup = ~paste0("<b>", categorie1, "</b>",
          ifelse(nzchar(categorie2), paste0(" — ", categorie2), ""),
          "<br>Date : ", ifelse(is.na(date), "—", format(date, "%d/%m/%Y")),
          "<br>Secteur : ", secteur,
          ifelse(nzchar(especes), paste0("<br>Espèce : ", especes), ""),
          ifelse(nzchar(type_signe), paste0("<br>Signe : ", type_signe), "")))
    da <- dm[dm$est_menace, , drop = FALSE]
    if (nrow(da) > 0)
      m <- m %>% addCircleMarkers(data = da, lng = ~lon, lat = ~lat, radius = 8,
        stroke = TRUE, weight = 2, color = "#9E2A2B", fillOpacity = 0, opacity = 0.9, group = "Menaces")
    m %>% addLegend("bottomright", pal = pal, values = cats, title = "Catégorie") %>%
      addLayersControl(overlayGroups = c("Observations", "Menaces"),
        options = layersControlOptions(collapsed = FALSE))
  })

  output$env_cat <- renderEcharts4r({
    s <- env_filtree(); shiny::validate(shiny::need(!is.null(s) && nrow(s) > 0, "Aucune donnée."))
    d <- s[nzchar(s$categorie1), , drop = FALSE]
    dd <- as.data.frame(table(categorie = d$categorie1), stringsAsFactors = FALSE)
    dd <- dd[order(dd$Freq), ]
    dd %>% e_charts(categorie) %>% e_bar(Freq, legend = FALSE) %>%
      e_flip_coords() %>% e_color("#5E8B6A") %>% e_tooltip() %>% e_legend(show = FALSE) %>%
      e_grid(left = 135, right = 16, top = 10, bottom = 24)
  })

  output$env_trend <- renderEcharts4r({
    lg <- current_lang()
    s <- env_filtree(); shiny::validate(shiny::need(!is.null(s) && nrow(s) > 0, "Aucune donnée."))
    d <- s[!is.na(s$date), , drop = FALSE]
    shiny::validate(shiny::need(nrow(d) > 0, "Aucune observation datée."))
    d$mois   <- lubridate::floor_date(d$date, "month")
    d$menace <- ifelse(d$est_menace, "Menace", "Autre")
    # Format large : un mois = une ligne, tous les mois comblés à 0 (empilement aligné)
    allm <- seq(min(d$mois), max(d$mois), by = "month"); lv <- as.character(allm)
    cnt  <- function(g) as.integer(table(factor(as.character(d$mois[d$menace == g]), levels = lv)))
    wide <- data.frame(mois = allm, autre = cnt("Autre"), menace = cnt("Menace"))
    wide %>% e_charts(mois) %>%
      e_bar(autre,  stack = "g", name = i18n_lookup("Autre observation", lg)) %>%
      e_bar(menace, stack = "g", name = i18n_lookup("Menace", lg)) %>%
      e_color(c("#9AA3AB", "#9E2A2B")) %>%
      e_tooltip(trigger = "axis") %>% e_legend(top = 4) %>%
      e_datazoom(type = "slider", bottom = 6, height = 16) %>%
      e_y_axis(minInterval = 1) %>%
      e_grid(left = 44, right = 16, top = 40, bottom = 56)
  })

  output$env_menaces <- renderEcharts4r({
    s <- env_filtree(); shiny::validate(shiny::need(!is.null(s) && nrow(s) > 0, "Aucune donnée."))
    d <- s[!is.na(s$menace_type), , drop = FALSE]
    shiny::validate(shiny::need(nrow(d) > 0, "Aucune menace relevée sur ces filtres."))
    dd <- as.data.frame(table(type = d$menace_type), stringsAsFactors = FALSE)
    dd <- dd[order(dd$Freq), ]
    dd$color <- unname(COL_MENACE[dd$type]); dd$color[is.na(dd$color)] <- "#9AA3AB"
    dd %>% e_charts(type) %>% e_bar(Freq, legend = FALSE) %>%
      e_flip_coords() %>% e_add_nested("itemStyle", color) %>%
      e_tooltip() %>% e_legend(show = FALSE) %>%
      e_grid(left = 150, right = 16, top = 10, bottom = 24)
  })

  output$env_corr <- renderDT({
    shiny::validate(shiny::need(!is.null(CORR_ENV) && nrow(CORR_ENV) > 0, "Table de correspondance indisponible."))
    tab <- CORR_ENV[, c("categorie", "signal_sbe", "type_correspondance")]
    names(tab) <- c("Thème", "Signal SBE", "Couverture SMART")
    datatable(tab, rownames = FALSE, class = "stripe hover compact",
              options = list(pageLength = 8, dom = "tip", scrollX = TRUE))
  })

  output$env_especes <- renderEcharts4r({
    s <- env_filtree(); shiny::validate(shiny::need(!is.null(s) && nrow(s) > 0, "Aucune donnée."))
    d <- s[nzchar(s$especes), , drop = FALSE]
    shiny::validate(shiny::need(nrow(d) > 0, "Aucune espèce renseignée."))
    dd <- as.data.frame(table(espece = d$especes), stringsAsFactors = FALSE)
    dd <- utils::tail(dd[order(dd$Freq), ], 7)
    dd %>% e_charts(espece) %>% e_bar(Freq, legend = FALSE) %>%
      e_flip_coords() %>% e_color("#35618E") %>% e_tooltip() %>% e_legend(show = FALSE) %>%
      e_grid(left = 165, right = 12, top = 6, bottom = 18)
  })

  output$env_faune <- renderUI({
    s <- env_filtree(); if (is.null(s) || nrow(s) == 0) return(NULL)
    nb_sante <- sum(s$n_bonne_sante, na.rm = TRUE)
    nb_mal <- sum(s$n_malades, na.rm = TRUE); nb_mort <- sum(s$n_morts, na.rm = TRUE)
    ncont <- sum(s$contact_dom, na.rm = TRUE)
    nzebu <- sum(s$animaux_rencontres == "Zébu", na.rm = TRUE)
    HTML(paste0("<div style='font-size:12.5px;line-height:1.75;color:#26333F;margin-top:6px;'>",
      "<b>Contact faune / domestique :</b> ", ncont, " observation(s)",
      " (dont ", nzebu, " zébu(s) en aire protégée).<br>",
      "<b>Santé faune :</b> ", nb_sante, " en bonne santé · ", nb_mal, " malades · ", nb_mort, " morts.",
      "<br><span style='color:#7A8593;'>Données sanitaires faune rares ici : la valeur One Health porte surtout sur les menaces et les contacts inter-espèces.</span></div>"))
  })

  ## Par signal — vue d'ensemble (cartes de chaleur)
  .ord_signaux <- REF18 %>%
    mutate(sord = match(secteur, c("Humain", "Animal", "Environnement"))) %>%
    arrange(sord, code)
  heat_signal_levels <- function(labels, lg) {
    ref_levels <- rev(translate_signal(.ord_signaux$signal, lg))
    extra_levels <- sort(unique(labels[!(labels %in% ref_levels)]))
    if (length(extra_levels) == 0) ref_levels else c(ref_levels, extra_levels)
  }
  heat_visual_max <- function(values) {
    values <- values[is.finite(values) & !is.na(values)]
    if (!length(values)) return(1)
    vmax <- suppressWarnings(as.numeric(stats::quantile(values, 0.9, na.rm = TRUE, names = FALSE)))
    vmax <- max(2, ceiling(vmax))
    min(vmax, max(values, na.rm = TRUE))
  }
  output$p_heat_temps <- renderEcharts4r({
    lg <- current_lang()
    d <- filtree(); shiny::validate(shiny::need(nrow(d) > 0, "Aucun signal."))
    dd <- d %>% mutate(signal_label = translate_signal(signal, lg),
                       m = floor_date(date_de_survenue, "month")) %>%
      filter(!is.na(m), !is.na(signal_label), nzchar(signal_label)) %>%
      count(signal_label, m) %>% arrange(m) %>% mutate(mois = format(m, "%b %y"))
    shiny::validate(shiny::need(nrow(dd) > 0, "Aucune donnée datée pour ces filtres."))
    sig_order <- heat_signal_levels(dd$signal_label, lg)
    vmax <- heat_visual_max(dd$n)
    dd %>% e_charts(mois) %>%
      e_heatmap(signal_label, n) %>%
      e_visual_map(n, min = 1, max = vmax,
                   inRange = list(color = c("#DCEAF6", "#B7D0E4", "#7FA7C7", "#3E6C94", "#16324F"))) %>%
      e_y_axis(type = "category", data = sig_order) %>%
      e_x_axis(type = "category", axisLabel = list(rotate = 45)) %>%
      e_tooltip() %>%
      e_grid(left = 155, right = 18, top = 10, bottom = 55)
  })
  output$p_heat_geo <- renderEcharts4r({
    lg <- current_lang()
    d <- filtree(); shiny::validate(shiny::need(nrow(d) > 0, "Aucun signal."))
    dd <- d %>% mutate(signal_label = translate_signal(signal, lg),
                       fokontany = trimws(as.character(fokontany))) %>%
      filter(!is.na(signal_label), nzchar(signal_label), !is.na(fokontany), nzchar(fokontany)) %>%
      count(signal_label, fokontany)
    shiny::validate(shiny::need(nrow(dd) > 0, "Aucune localisation disponible pour ces filtres."))
    sig_order <- heat_signal_levels(dd$signal_label, lg)
    vmax <- heat_visual_max(dd$n)
    dd %>% e_charts(fokontany) %>%
      e_heatmap(signal_label, n) %>%
      e_visual_map(n, min = 1, max = vmax,
                   inRange = list(color = c("#DFEEE3", "#BFD8C4", "#8EB697", "#4E7D5E", "#2E5A40"))) %>%
      e_y_axis(type = "category", data = sig_order) %>%
      e_x_axis(type = "category", axisLabel = list(rotate = 45)) %>%
      e_tooltip() %>%
      e_grid(left = 155, right = 18, top = 10, bottom = 70)
  })
  
  ## Indicateurs — 18 signaux (+ drill-down popup)
  ind_ref <- reactive({
    d <- filtree()
    ind <- d %>% group_by(code) %>%
      summarise(Signalements = n(), Cas = sum(Nombre_cas), Deces = sum(Nombre_deces),
                Evalues = sum(a_ete_evalue), Alertes = sum(a_une_alerte),
                Dernier = suppressWarnings(max(date_de_survenue)), .groups = "drop")
    # Signalements impliqués dans une grappe One Health (cluster inter-secteurs), par code
    dd <- d %>% filter(!is.na(date_de_survenue))
    anchors <- dd %>% filter(a_une_alerte) %>%
      transmute(a_id = id_signal, a_fok = fokontany, a_date = date_de_survenue)
    oh_freq <- if (nrow(anchors) > 0)
      anchors %>%
        inner_join(dd, by = c("a_fok" = "fokontany")) %>%
        filter(date_de_survenue >= a_date - 14, date_de_survenue <= a_date + 3) %>%
        group_by(a_id) %>% mutate(nsec_grp = n_distinct(secteur)) %>% ungroup() %>%
        filter(nsec_grp >= 2) %>%
        distinct(id_signal, code) %>% count(code, name = "OH")
      else data.frame(code = character(), OH = integer(), stringsAsFactors = FALSE)
    REF18 %>% left_join(ind, by = "code") %>% left_join(oh_freq, by = "code") %>%
      mutate(across(c(Signalements, Cas, Deces, Evalues, Alertes, OH), ~ ifelse(is.na(.), 0, .))) %>%
      arrange(secteur, code)
  })
  output$t_indic <- renderDT({
    lg <- current_lang()
    tab <- ind_ref() %>%
      mutate(Dernier = ifelse(is.na(Dernier), "—",
                              format(as.Date(Dernier, origin = "1970-01-01"), "%d/%m/%Y")),
             Signal = translate_signal(signal, lg)) %>%
      transmute(Secteur = secteur, Code = code, Signal,
                Signalements, Cas, `Décès` = Deces, `Évalués` = Evalues,
                Alertes, `En grappe One Health` = OH, `Dernier signalement` = Dernier)
    datatable(tab, rownames = FALSE, selection = "single",
              options = list(pageLength = 18, dom = "t",
                             columnDefs = list(list(className = "dt-center", targets = 3:8)))) %>%
      formatStyle("Alertes", fontWeight = styleInterval(0, c("normal", "bold")),
                  color = styleInterval(0, c("inherit", "#9E2A2B"))) %>%
      formatStyle("En grappe One Health", fontWeight = styleInterval(0, c("normal", "bold")),
                  color = styleInterval(0, c("inherit", "#1e3a5f")))
  })
  ind_sel <- reactive({
    i <- input$t_indic_rows_selected
    shiny::validate(shiny::need(length(i) == 1, ""))
    ind_ref()[i, ]
  })
  ind_sig <- reactive(filtree() %>% filter(code == ind_sel()$code) %>% arrange(date_de_survenue))
  output$ind_summary <- renderUI({
    lg <- current_lang()
    g <- ind_sel()
    tags$div(style = "margin-bottom:10px;",
             tags$div(style = "font-size:15px; font-weight:600; color:#26333F;",
                      paste0(format_signal_label(g$code, g$signal, lg), "  ·  ", g$secteur)),
             tags$div(style = "color:#5A6672; margin-top:5px;",
                      paste0("Signalements : ", g$Signalements, "  ·  Cas : ", g$Cas,
                             "  ·  Décès : ", g$Deces, "  ·  Évalués : ", g$Evalues,
                             "  ·  Alertes : ", g$Alertes)))
  })
  output$ind_trend <- renderEcharts4r({
    d <- ind_sig(); shiny::validate(shiny::need(nrow(d) > 0, "Aucun signalement sur la période."))
    d %>% mutate(m = floor_date(date_de_survenue, "month")) %>% count(m) %>% arrange(m) %>%
      mutate(mois = format(m, "%b %y")) %>%
      e_charts(mois) %>%
      e_line(n, name = "Signalements / mois", areaStyle = list(opacity = 0.15)) %>%
      e_color("#35618E") %>% e_tooltip(trigger = "axis") %>%
      e_legend(show = FALSE) %>% e_y_axis(minInterval = 1)
  })
  output$ind_geo <- renderEcharts4r({
    d <- ind_sig(); shiny::validate(shiny::need(nrow(d) > 0, ""))
    d %>% count(fokontany, sort = TRUE) %>% head(10) %>% arrange(n) %>%
      e_charts(fokontany) %>%
      e_bar(n, name = "Signalements", legend = FALSE,
            barWidth = "62%",
            itemStyle = list(borderRadius = c(0, 4, 4, 0)),
            label = list(show = TRUE, position = "right",
                         color = "#26333F", fontSize = 11, fontWeight = "bold")) %>%
      e_flip_coords() %>%
      e_color("#35618E") %>%
      e_tooltip(trigger = "axis") %>% e_legend(show = FALSE) %>%
      e_x_axis(show = FALSE, minInterval = 1) %>%
      e_y_axis(axisTick = list(show = FALSE),
               axisLine = list(show = FALSE),
               axisLabel = list(color = "#26333F", fontSize = 12)) %>%
      e_grid(left = 8, right = 40, top = 8, bottom = 8, containLabel = TRUE)
  })
  output$ind_map <- renderLeaflet({
    d <- ind_sig(); shiny::validate(shiny::need(nrow(d) > 0, ""))
    agg <- d %>% group_by(fokontany) %>%
      summarise(n = n(),
                premier = suppressWarnings(min(date_de_survenue, na.rm = TRUE)),
                dernier = suppressWarnings(max(date_de_survenue, na.rm = TRUE)),
                .groups = "drop") %>%
      left_join(FOK_CENTROIDS, by = "fokontany") %>% filter(!is.na(lat))
    shiny::validate(shiny::need(nrow(agg) > 0, "Pas de localisation disponible."))
    pal <- colorNumeric(c("#EAF0F6", "#8FB3CE", "#2E5A82", "#16324F"),
                        domain = c(0, max(agg$n)))
    leaflet(agg) %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(~lon, ~lat, radius = ~6 + 3.2 * sqrt(n),
                       color = "#FFFFFF", weight = 1, fillColor = ~pal(n), fillOpacity = 0.85,
                       label = ~paste0(fokontany, " : ", n),
                       popup = ~paste0("<b>", fokontany, "</b><br>",
                                       n, " signalement(s)<br>",
                                       "Du ", format(premier, "%d/%m/%Y"),
                                       " au ", format(dernier, "%d/%m/%Y"))) %>%
      addLegend("bottomright", pal = pal, values = ~n, title = "Signalements", opacity = 0.9)
  })
  observeEvent(input$t_indic_rows_selected, {
    lg <- current_lang()
    if (length(input$t_indic_rows_selected) != 1) return()
    g <- ind_ref()[input$t_indic_rows_selected, ]
    showModal(modalDialog(
      title = format_signal_label(g$code, g$signal, lg),
      uiOutput("ind_summary"),
      echarts4rOutput("ind_trend", height = 220),
      fluidRow(
        column(6, echarts4rOutput("ind_geo", height = 300)),
        column(6, leafletOutput("ind_map", height = 300))
      ),
      size = "l", easyClose = TRUE, footer = modalButton("Fermer")
    ))
  }, ignoreInit = TRUE)
  
  ## One Health — grappes inter-secteurs ancrées sur une alerte (fenêtre 14 j amont)
  oh <- reactive({
    d <- filtree() %>% filter(!is.na(date_de_survenue))
    shiny::validate(shiny::need(nrow(d) > 0, "Aucun signal."))
    shiny::validate(shiny::need(any(d$a_une_alerte), "Aucune alerte sur la période / les filtres."))
    res <- oh_clusters(d)
    shiny::validate(shiny::need(nrow(res) > 0, "Aucune grappe inter-secteurs sur la période / les filtres."))
    res
  })
  output$oh_total <- renderValueBox(valueBox(nrow(oh()), "Grappes inter-secteurs",
                                             icon = icon("diagram-project"), color = "yellow"))
  output$oh_zoo <- renderValueBox(valueBox(sum(oh()$humain & oh()$animal), "Grappes zoonotiques (H + A)",
                                           icon = icon("shield-virus"), color = "red"))
  output$oh_env <- renderValueBox(valueBox(sum(oh()$humain & oh()$env), "Grappes humain + environnement",
                                           icon = icon("droplet"), color = "teal"))
  output$t_oh <- renderDT({
    tab <- oh() %>% transmute(
      Fokontany = fokontany,
      `Période` = paste0(format(debut, "%d/%m/%y"), " – ", format(fin, "%d/%m/%y")),
      Secteurs = secteurs, Signaux = nb, Cas = cas, `Décès` = deces,
      Risque = risque, Suspicion = ifelse(suspicion == "", "—", suspicion),
      `Avance détection (j)` = ifelse(is.na(avance), "—", as.character(round(avance))))
    datatable(tab, rownames = FALSE, selection = "single",
              options = list(pageLength = 10, dom = "tip"))
  })
  
  ## One Health — détail de la grappe sélectionnée (drill-down)
  oh_sel <- reactive({
    i <- input$t_oh_rows_selected
    shiny::validate(shiny::need(length(i) == 1,
                                "Cliquez sur une grappe dans le tableau ci-dessus pour afficher son détail."))
    oh()[i, ]
  })
  oh_sig <- reactive({
    g <- oh_sel()
    filtree() %>%
      filter(fokontany == g$fokontany,
             date_de_survenue >= g$debut, date_de_survenue <= g$fin) %>%
      arrange(date_de_survenue)
  })
  # Contenu de la fiche « grappe One Health » (partagé onglet One Health + Synthèse)
  oh_summary_ui <- function(g) {
    tags$div(style = "margin-bottom:12px;",
             tags$div(style = "font-size:15px; font-weight:600; color:#26333F;",
                      paste0("Fokontany ", g$fokontany, " — du ",
                             format(g$debut, "%d/%m/%Y"), " au ", format(g$fin, "%d/%m/%Y"))),
             tags$div(style = "color:#5A6672; margin-top:5px;",
                      paste0("Secteurs croisés : ", g$secteurs,
                             "  ·  Signaux : ", g$nb,
                             "  ·  Cas : ", g$cas, "  ·  Décès : ", g$deces,
                             "  ·  Risque max : ", g$risque,
                             if (!is.na(g$avance)) paste0("  ·  Avance de détection : ", round(g$avance), " jours") else "")),
             if (nchar(g$suspicion) > 0)
               tags$div(style = "color:#9E2A2B; margin-top:4px; font-weight:600;",
                        paste0("Suspicion : ", g$suspicion)) else NULL)
  }
  oh_timeline_plot <- function(d) {
    shiny::validate(shiny::need(nrow(d) > 0, ""))
    d <- d %>% mutate(secteur = factor(secteur,
                                       levels = c("Environnement", "Animal", "Humain")))
    ggplot(d, aes(date_de_survenue, secteur, color = secteur)) +
      geom_point(size = 4.2, alpha = 0.85) +
      geom_text(aes(label = code), vjust = -1.1, size = 3.4, show.legend = FALSE) +
      scale_color_manual(values = COL_SECTEUR) +
      labs(x = NULL, y = NULL,
           subtitle = "Chronologie des signaux de la grappe (le signal animal / environnemental précède l'humain)") +
      theme_it + theme(legend.position = "none",
                       plot.subtitle = element_text(color = "#5A6672", size = 11))
  }
  oh_signals_dt <- function(d) {
    lg <- current_lang()
    d <- d %>% transmute(
      Date = format(date_de_survenue, "%d/%m/%Y"), Secteur = secteur,
      Signal = format_signal_label(code, signal, lg), Cas = Nombre_cas, `Décès` = Nombre_deces,
      Risque = as.character(niveau_risque),
      Suspicion = ifelse(classification_event == "Non précisé", "—", classification_event),
      Source = ifelse(is.na(classe_source), "—", classe_source),
      `Vérifié` = ifelse(is.na(is_verifie), "—", is_verifie))
    datatable(d, rownames = FALSE, options = list(pageLength = 12, dom = "t"))
  }
  output$oh_summary  <- renderUI({ oh_summary_ui(oh_sel()) })
  output$oh_timeline <- renderPlot({ oh_timeline_plot(oh_sig()) })
  output$oh_signals  <- renderDT({ oh_signals_dt(oh_sig()) })
  observeEvent(input$t_oh_rows_selected, {
    if (length(input$t_oh_rows_selected) != 1) return()
    g <- oh()[input$t_oh_rows_selected, ]
    showModal(modalDialog(
      title = paste0("Grappe One Health — fokontany ", g$fokontany),
      uiOutput("oh_summary"),
      plotOutput("oh_timeline", height = 240),
      DTOutput("oh_signals"),
      size = "l", easyClose = TRUE, footer = modalButton("Fermer")
    ))
  }, ignoreInit = TRUE)
  
  ## Alertes (+ drill-down : chaîne de décision)
  al_data <- reactive({
    filtree() %>% filter(a_une_alerte) %>% arrange(desc(date_de_survenue))
  })
  output$t_alertes <- renderDT({
    lg <- current_lang()
    d <- al_data() %>%
      transmute(Date = format(date_de_survenue, "%d/%m/%Y"), Secteur = secteur,
                Signal = format_signal_label(code, signal, lg), Fokontany = fokontany,
                Suspicion = ifelse(classification_event == "Non précisé", "—", classification_event),
                Cas = Nombre_cas, Décès = Nombre_deces,
                Risque = as.character(niveau_risque), Alerte = translate_alert(alerte_label, lg))
    datatable(d, rownames = FALSE, selection = "single",
              options = list(pageLength = 10, dom = "tip"))
  })
  al_sel <- reactive({
    i <- input$t_alertes_rows_selected
    shiny::validate(shiny::need(length(i) == 1,
                                "Cliquez sur une alerte dans le tableau ci-dessus pour suivre sa chaîne de décision."))
    al_data()[i, ]
  })
  .vn <- function(x) ifelse(is.na(x) | x == "", "—", as.character(x))
  # Contenu du popup de détail d'une alerte (partagé Synthèse + onglet Alertes)
  alerte_summary_ui <- function(a) {
    tags$div(style = "margin-bottom:12px;",
             tags$div(style = "font-size:15px; font-weight:600; color:#26333F;",
                      paste0(format_signal_label(a$code, a$signal, current_lang()), "  ·  Fokontany ", a$fokontany)),
             tags$div(style = "color:#9E2A2B; font-weight:600; margin-top:5px;",
                      paste0("Alerte : ", translate_alert(a$alerte_label, current_lang()), "  (", as.character(a$niveau_risque), ")")),
             if (a$classification_event != "Non précisé")
               tags$div(style = "color:#5A6672; margin-top:3px;",
                        paste0("Suspicion : ", a$classification_event)) else NULL)
  }
  alerte_chain_dt <- function(a) {
    fmt <- function(d) if (length(d) == 0 || is.na(d)) "—" else format(as.Date(d, origin = "1970-01-01"), "%d/%m/%Y")
    chain <- data.frame(
      Étape = c("1. Signalement", "2. Triage", "3. Vérification",
                "4. Évaluation du risque", "5. Alerte"),
      Date = c(fmt(a$date_detection), fmt(a$date_triage), fmt(a$date_verification),
               fmt(a$date_evaluation), fmt(a$date_evaluation)),
      Détail = c(
        paste0("Secteur ", a$secteur, " · Cas : ", a$Nombre_cas, " · Décès : ", a$Nombre_deces,
               " · Source : ", .vn(a$classe_source)),
        paste0("Trié : ", .vn(a$is_trie), " · Pertinent : ", .vn(a$pertinence),
               " · Doublon : ", .vn(a$doublon)),
        paste0("Vérifié : ", .vn(a$is_verifie), " · Véracité : ", .vn(a$veracite)),
        paste0("Q1 sévérité : ", .vn(a$q1), " · Q2 propagation : ", .vn(a$q2),
               " · Q3 traitement/contrôle : ", .vn(a$q3), "  →  Niveau : ", as.character(a$niveau_risque)),
        translate_alert(a$alerte_label, current_lang())),
      check.names = FALSE, stringsAsFactors = FALSE)
    datatable(chain, rownames = FALSE, options = list(dom = "t", ordering = FALSE)) %>%
      formatStyle("Étape", fontWeight = "bold")
  }
  output$al_summary <- renderUI({ alerte_summary_ui(al_sel()) })
  output$al_chain   <- renderDT({ alerte_chain_dt(al_sel()) })
  observeEvent(input$t_alertes_rows_selected, {
    if (length(input$t_alertes_rows_selected) != 1) return()
    a <- al_data()[input$t_alertes_rows_selected, ]
    showModal(modalDialog(
      title = paste0("Chaîne de décision — ", a$code, " · ", a$fokontany),
      uiOutput("al_summary"),
      DTOutput("al_chain"),
      size = "l", easyClose = TRUE, footer = modalButton("Fermer")
    ))
  }, ignoreInit = TRUE)

  # --- Notification de l'autorité (E-Notification UGD) ------------------
  # Alerte sélectionnée + éventuelle grappe One Health (secteurs croisés, avance)
  en_info <- reactive({
    i <- input$t_alertes_rows_selected
    if (length(i) != 1) return(NULL)
    a  <- al_data()[i, ]
    g  <- tryCatch(grappe_par_alerte(exec_data()), error = function(e) NULL)
    gr <- if (!is.null(g)) g[g$a_id == a$id_signal, ] else NULL
    oh <- !is.null(gr) && nrow(gr) == 1 && !is.na(gr$nsec) && gr$nsec >= 2
    list(a = a, oh = oh,
         secteurs = if (oh) gr$secteurs else NA_character_,
         avance   = if (oh) gr$avance   else NA_real_)
  })

  en_titre <- function(info) {
    paste0(if (isTRUE(info$oh)) "ALERTE ONE HEALTH i-Tafaray" else "ALERTE i-Tafaray",
           " — ", info$a$code)
  }

  # Contenu adapté au canal (SMS bref / WhatsApp formaté / Email structuré)
  en_message_for <- function(canal, info) {
    a    <- info$a
    date <- format(a$date_de_survenue, "%d/%m/%Y")
    av   <- if (isTRUE(info$oh) && !is.na(info$avance) && info$avance > 0)
              paste0(" (+", round(info$avance), " j d'avance)") else ""
    if (canal == "SMS") {
      s <- paste0(if (isTRUE(info$oh)) "ALERTE ONE HEALTH i-Tafaray. " else "ALERTE i-Tafaray. ",
                  a$code, " ", a$fokontany, ". Risque ", as.character(a$niveau_risque),
                  ". Cas ", a$Nombre_cas, "/Deces ", a$Nombre_deces, ". ", date)
      if (isTRUE(info$oh)) s <- paste0(s, ". Grappe: ", info$secteurs, av)
      return(s)
    }
    if (canal == "WhatsApp") {
      l <- c(if (isTRUE(info$oh)) "\U0001F534 *ALERTE ONE HEALTH — i-Tafaray*"
             else "\U0001F514 *ALERTE i-Tafaray*",
             paste0("*Signal* : ", a$code, " — ", a$signal),
             paste0("*Secteur* : ", a$secteur),
             paste0("\U0001F4CD *Fokontany* : ", a$fokontany),
             paste0("⚠️ *Risque* : ", as.character(a$niveau_risque)),
             paste0("\U0001F465 *Cas* : ", a$Nombre_cas, "  ·  *Décès* : ", a$Nombre_deces),
             paste0("\U0001F5D3️ *Date* : ", date))
      if (isTRUE(info$oh))
        l <- c(l, paste0("\U0001F517 *Grappe One Health* : ", info$secteurs, av))
      return(paste(l, collapse = "\n"))
    }
    # Email — texte structuré (HTML possible si la passerelle l'interprète : à confirmer)
    l <- c(if (isTRUE(info$oh)) "ALERTE ONE HEALTH — i-Tafaray" else "ALERTE i-Tafaray", "",
           paste0("Signal        : ", a$code, " — ", a$signal),
           paste0("Secteur       : ", a$secteur),
           paste0("Fokontany     : ", a$fokontany),
           paste0("Niveau risque : ", as.character(a$niveau_risque)),
           paste0("Cas / Décès   : ", a$Nombre_cas, " / ", a$Nombre_deces),
           paste0("Date          : ", date))
    if (isTRUE(info$oh))
      l <- c(l, paste0("Grappe OH     : ", info$secteurs, av), "",
             "Cette alerte croise plusieurs secteurs (One Health).")
    paste(l, collapse = "\n")
  }

  output$en_apercu <- renderText({
    info <- en_info()
    if (is.null(info)) return("Sélectionnez une alerte dans le tableau ci-dessus.")
    cn <- input$en_canaux; if (length(cn) == 0) cn <- "SMS"
    paste(vapply(cn, function(x) paste0("[", x, "]\n", en_message_for(x, info)),
                 character(1)), collapse = "\n\n")
  })

  observeEvent(input$en_send, {
    info <- en_info()
    if (is.null(info)) { showNotification("Sélectionnez d'abord une alerte.", type = "warning"); return() }
    canaux <- input$en_canaux
    if (length(canaux) == 0) { showNotification("Choisissez au moins un canal.", type = "warning"); return() }
    dest <- trimws(if (is.null(input$en_dest)) "" else input$en_dest)
    prio <- if (isTRUE(info$oh)) "URGENT" else "HIGH"
    titre <- en_titre(info)
    res <- lapply(canaux, function(cn)
      envoyer_notification(title = titre, message = en_message_for(cn, info),
                           targets = dest, canaux = cn, priority = prio))
    ok  <- all(vapply(res, function(r) isTRUE(r$ok), logical(1)))
    dry <- any(vapply(res, function(r) isTRUE(r$dry), logical(1)))
    statuses <- paste0(canaux, " : ", vapply(res, function(r) r$status, character(1)))
    showNotification(paste0(if (ok) "OK" else "Erreur", " — ", paste(statuses, collapse = " | ")),
                     type = if (ok) "message" else "error", duration = 8)
    output$en_statut <- renderUI({
      col <- if (!ok) "#9E2A2B" else if (dry) "#8A6D00" else "#1E7E34"
      tags$div(style = paste0("margin-top:8px; font-weight:600; color:", col, ";"),
        HTML(paste(vapply(seq_along(canaux), function(k)
          paste0(canaux[k], " — ", res[[k]]$status, " : ", res[[k]]$message),
          character(1)), collapse = "<br>")))
    })
  })

  ## Qualité / pipeline
  output$q_verif <- renderValueBox({
    d <- base_filtree(); pct <- if (nrow(d)) round(100 * mean(d$is_verifie == "Oui", na.rm = TRUE)) else 0
    valueBox(paste0(pct, "%"), "Taux de vérification", icon = icon("check"), color = "green")
  })
  output$q_doublon <- renderValueBox({
    d <- base_filtree(); pct <- if (nrow(d)) round(100 * mean(d$doublon == "Oui", na.rm = TRUE)) else 0
    valueBox(paste0(pct, "%"), "Taux de doublons", icon = icon("clone"), color = "yellow")
  })
  output$q_delai <- renderValueBox({
    d <- base_filtree(); md <- if (any(!is.na(d$delai_verif))) round(median(d$delai_verif, na.rm = TRUE)) else 0
    valueBox(paste0(md, " j"), "Délai médian détection → vérif.", icon = icon("clock"), color = "aqua")
  })
  output$p_funnel <- renderPlot({
    d <- filtree(); shiny::validate(shiny::need(nrow(d) > 0, "Aucun signal."))
    fn <- data.frame(
      etape = factor(c("Collectés", "Vérifiés", "Évalués", "Alertes"),
                     levels = c("Collectés", "Vérifiés", "Évalués", "Alertes")),
      n = c(nrow(d), sum(d$is_verifie == "Oui", na.rm = TRUE),
            sum(d$a_ete_evalue), sum(d$a_une_alerte)))
    ggplot(fn, aes(etape, n)) + geom_col(fill = "#35618E", width = 0.65) +
      geom_text(aes(label = n), vjust = -0.3, size = 4.2) + labs(x = NULL, y = NULL) + theme_it
  })
  output$p_delai <- renderPlot({
    d <- base_filtree() %>% filter(!is.na(delai_verif))
    shiny::validate(shiny::need(nrow(d) > 0, "Aucun délai disponible."))
    ggplot(d, aes(delai_verif)) + geom_histogram(binwidth = 1, fill = "#35618E", color = "white") +
      labs(x = "Délai détection → vérification (jours)", y = "Nombre de signaux") + theme_it
  })
}

app_ui <- if (AUTH_ENABLED)
  secure_app(ui, enable_admin = TRUE, language = I18N_DEFAULT,
             tags_top = tags$div(style = "text-align:center; color:#26333F;",
                                 tags$h4("iTafaray — Tableau de bord One Health"),
                                 tags$p(style = "color:#5A6672;", "Accès réservé"))) else ui
shinyApp(ui = app_ui, server = server)
