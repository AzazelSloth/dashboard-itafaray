# =====================================================================
#  Extraction des variables climatiques (district d'Ifanadiana)
#  depuis le projet MDG (MDG_platform_SHINY_DASH) vers i-Tafaray.
#
#  À LANCER UNE FOIS dans R (RStudio). Produit :
#    Dashboard_POC_iTafaray/data_poc/climate_ifanadiana.csv
#  (série mensuelle : pluie, température, FWI — observé vs normale).
#
#  Adapter les deux chemins ci-dessous si besoin.
# =====================================================================

MDG_RDATA <- "/Users/florian/Documents/Madagascar/MDG_platform_SHINY_DASH/data/climate/climate_extremes_seasonal.RData"
OUT_CSV   <- "/Users/florian/Documents/Claude/Projects/CIRAD/Dashboard_POC_iTafaray/data_poc/climate_ifanadiana.csv"
DISTRICT  <- "ifanadiana"   # recherche insensible casse/accents

stopifnot(file.exists(MDG_RDATA))
env <- new.env(); load(MDG_RDATA, envir = env)
cat("Objets dans le .RData :", paste(ls(env), collapse = ", "), "\n")

ca <- env$climate_anomalies          # mensuel, par district (ADM2)
cat("Colonnes climate_anomalies :\n"); print(names(ca))

norm <- function(x) trimws(tolower(iconv(as.character(x), "UTF-8", "ASCII//TRANSLIT")))
d <- ca[grepl(DISTRICT, norm(ca$ADM2_NAME)), , drop = FALSE]
cat(sprintf("\nLignes trouvées pour « %s » : %d (ADM2_NAME : %s)\n",
            DISTRICT, nrow(d), paste(unique(d$ADM2_NAME), collapse = ", ")))
if (nrow(d) == 0) stop("District introuvable — vérifier l'orthographe dans ADM2_NAME.")

pick <- function(df, cands) { for (c in cands) if (c %in% names(df)) return(df[[c]]); rep(NA, nrow(df)) }

Year  <- pick(d, c("Year", "annee"))
Month <- pick(d, c("Month", "mois"))
Date  <- if ("Date" %in% names(d)) as.Date(d$Date) else as.Date(sprintf("%04d-%02d-01", Year, Month))

out <- data.frame(
  date               = format(Date, "%Y-%m-%d"),
  annee              = Year,
  mois               = Month,
  pluie_mm           = pick(d, c("Precipitation", "Rain_obs", "Rain_mean")),
  pluie_normale      = pick(d, c("Rain_normal", "Rain_normal_mean")),
  pluie_anomalie_pct = pick(d, c("Rain_anomaly_pct")),
  pluie_classe       = pick(d, c("Rain_class")),
  temp_moy           = pick(d, c("Temp_obs", "Temp_mean")),
  temp_normale       = pick(d, c("Temp_normal", "Temp_normal_mean")),
  temp_anomalie      = pick(d, c("Temp_anomaly", "Temp_anom_abs")),
  temp_classe        = pick(d, c("Temp_class")),
  fwi                = pick(d, c("FWI")),
  fwi_normale        = pick(d, c("FWI_normal")),
  fwi_classe         = pick(d, c("FWI_class_effis", "FWI_class")),
  stringsAsFactors   = FALSE
)
out <- out[order(out$date), ]

# Reconstituer l'observé depuis normale + anomalie si la colonne directe est vide
na_all <- function(x) all(is.na(x))
if (na_all(out$pluie_mm) && !na_all(out$pluie_normale) && !na_all(out$pluie_anomalie_pct))
  out$pluie_mm <- out$pluie_normale * (1 + out$pluie_anomalie_pct / 100)
if (na_all(out$temp_moy) && !na_all(out$temp_normale) && !na_all(out$temp_anomalie))
  out$temp_moy <- out$temp_normale + out$temp_anomalie

# Si FWI absent du mensuel, tenter une agrégation depuis le fichier journalier
if (all(is.na(out$fwi))) {
  fwi_daily_path <- file.path(dirname(MDG_RDATA), "fwi_daily_raw.rds")
  if (file.exists(fwi_daily_path)) {
    fd <- tryCatch(readRDS(fwi_daily_path), error = function(e) NULL)
    if (!is.null(fd) && all(c("ADM2_NAME") %in% names(fd))) {
      cat("Colonnes fwi_daily_raw :\n"); print(names(fd))
      message("FWI mensuel absent : agrège le journalier si possible (à adapter selon les colonnes ci-dessus).")
    }
  }
}

dir.create(dirname(OUT_CSV), showWarnings = FALSE, recursive = TRUE)
write.csv(out, OUT_CSV, row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("\n✔ Écrit : %s  (%d lignes, %s → %s)\n",
            OUT_CSV, nrow(out), min(out$date), max(out$date)))
print(utils::head(out))
