#!/usr/bin/env Rscript
# =====================================================================
#  i-Tafaray — Ingestion X-Road → cache local lu par le dashboard
# ---------------------------------------------------------------------
#  Récupère les données réelles via le Security Server (charger_xroad)
#  et les écrit dans un cache .rds que le dashboard relit automatiquement.
#
#  À PLANIFIER (cron / timer systemd) toutes les N minutes, SUR UNE MACHINE
#  QUI JOINT LE SECURITY SERVER (le serveur AWS s'il a la route réseau, ou
#  une machine interne au réseau autorisé qui synchronise ensuite le cache
#  vers AWS).
#
#  Exemple cron (toutes les 30 min, depuis le dossier de l'app) :
#     */30 * * * * cd /app && /usr/bin/Rscript ingest_xroad.R >> /var/log/itafaray_ingest.log 2>&1
#
#  Variables d'environnement (facultatives) :
#     XROAD_CACHE_PATH  chemin du cache .rds (défaut: data_poc/xroad_cache.rds)
#                       -> en production, pointer vers un VOLUME PERSISTANT
#                          monté dans le conteneur (et NON l'image Docker).
# =====================================================================

suppressWarnings(suppressMessages({
  library(dplyr)        # utilisé par charger_xroad()
}))

ts  <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
log <- function(...) cat(ts(), "-", ..., "\n")

# --- Localisation des fichiers ---
CACHE <- Sys.getenv("XROAD_CACHE_PATH", unset = "data_poc/xroad_cache.rds")
META  <- file.path(dirname(CACHE), "xroad_cache_meta.txt")
TMP   <- paste0(CACHE, ".tmp")
dir.create(dirname(CACHE), showWarnings = FALSE, recursive = TRUE)

# --- Pont X-Road ---
if (!file.exists("xroad_bridge.R")) {
  log("ERREUR : xroad_bridge.R introuvable. Lancer le script depuis le dossier de l'application.")
  quit(status = 2)
}
source("xroad_bridge.R")

log("Début ingestion X-Road…")
res <- tryCatch(charger_xroad(), error = function(e) e)

# --- Validation ---
if (inherits(res, "error") || is.null(res) || !is.data.frame(res) || nrow(res) == 0) {
  msg <- if (inherits(res, "error")) conditionMessage(res) else "réponse vide (0 ligne)"
  log("ÉCHEC :", msg, "— cache précédent conservé, pas d'écrasement.")
  quit(status = 1)
}

# --- Écriture atomique (tmp puis rename) pour éviter une lecture partielle ---
attr(res, "synced_at") <- Sys.time()
saveRDS(res, TMP)
file.rename(TMP, CACHE)
writeLines(format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), META)

log("OK :", nrow(res), "lignes écrites dans", CACHE)
quit(status = 0)
