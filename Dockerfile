# =====================================================================
#  i-Tafaray - Tableau de bord One Health (R / Shiny)
#  Image Docker autoportante : R + packages + application + donnees.
#
#  Build :  docker build -t itafaray .
#  Run   :  docker run --rm -p 3838:3838 itafaray
#  Puis ouvrir :  http://localhost:3838
#
#  Base de deploiement alignee sur le dernier commit sain, avec
#  conservation des ressources statiques ajoutees depuis.
# =====================================================================

FROM rocker/shiny:4.4.1

RUN apt-get update \
    && apt-get upgrade -y \
    && apt-get install -y --no-install-recommends \
      libxt6 \
      pandoc \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      libfontconfig1-dev \
      libcairo2-dev \
      libsodium-dev \
    && install2.r --error --skipinstalled \
      shinydashboard \
      dplyr \
      ggplot2 \
      DT \
      leaflet \
      scales \
      lubridate \
      readxl \
      stringr \
      echarts4r \
      shinymanager \
      gridExtra \
      jsonlite \
      httr2 \
      rmarkdown \
    # Keep runtime packages only: headers and toolchains are useful to build R libs
    # but they unnecessarily increase the final image attack surface.
    && apt-get purge -y --auto-remove \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      libfontconfig1-dev \
      libcairo2-dev \
      libsodium-dev \
    && rm -rf /var/lib/apt/lists/* /tmp/downloaded_packages /tmp/*.rds

WORKDIR /app
COPY app.R prepare_data.R i18n_setup.R xroad_bridge.R credentials.sqlite ./
COPY data_poc/ ./data_poc/
COPY translations/ ./translations/
COPY report/ ./report/
COPY www/ ./www/

EXPOSE 3838

HEALTHCHECK --interval=30s --timeout=5s --start-period=45s --retries=3 \
  CMD R -q -e "tryCatch({c<-socketConnection('127.0.0.1',3838,open='r',timeout=4);close(c)},error=function(e)quit(status=1))" || exit 1

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
