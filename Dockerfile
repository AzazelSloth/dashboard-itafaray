# =====================================================================
#  i-Tafaray — Tableau de bord One Health (R / Shiny)
#  Image Docker autoportante : R + packages + application + données.
#  Build :  docker build -t itafaray-poc:1.1 .
#  Run   :  docker run --rm -p 3838:3838 itafaray-poc:1.1
#  (en production, voir docker-compose.yml : app + nginx + HTTPS)
# =====================================================================

# Base rocker : R + Shiny préinstallés, packages binaires rapides (Posit PM)
FROM rocker/shiny:4.4.1

# --- Dépendances système ---
#  * libs de compilation pour les packages R
#  * pandoc : requis par rmarkdown pour le rapport HTML interactif auto-contenu
RUN apt-get update && apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev libssl-dev libxml2-dev \
      libfontconfig1-dev libcairo2-dev libxt6 libsodium-dev \
      pandoc \
    && rm -rf /var/lib/apt/lists/*

# --- Packages R de l'application (shiny est déjà présent) ---
#  httr2     : pont X-Road (données réelles)
#  rmarkdown : génération du rapport HTML interactif
RUN install2.r --error --skipinstalled \
      shinydashboard dplyr ggplot2 DT leaflet scales lubridate \
      readxl stringr echarts4r shinymanager gridExtra jsonlite \
      httr2 rmarkdown

# --- Application + données ---
WORKDIR /app
COPY app.R prepare_data.R i18n_setup.R credentials.sqlite ./
COPY xroad_bridge.R ./
COPY data_poc/      ./data_poc/
COPY translations/  ./translations/
COPY report/        ./report/

# Port interne de l'application
EXPOSE 3838

# Vérification de bonne santé (le port 3838 accepte les connexions)
HEALTHCHECK --interval=30s --timeout=5s --start-period=45s --retries=3 \
  CMD R -q -e "tryCatch({c<-socketConnection('127.0.0.1',3838,open='r',timeout=4);close(c)},error=function(e)quit(status=1))" || exit 1

# Lancement de l'application (un seul processus Shiny)
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
