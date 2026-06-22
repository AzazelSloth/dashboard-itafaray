# =====================================================================
#  i-Tafaray - Tableau de bord One Health (R / Shiny)
#  Image Docker durcie : build en deux etapes, runtime minimal, pas de
#  secrets applicatifs embarques, execution non root.
# =====================================================================

FROM rocker/shiny:4.4.1 AS build

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get upgrade -y && apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      libfontconfig1-dev \
      libcairo2-dev \
      libxt6 \
      libsodium-dev \
      pandoc \
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
      gridExtra \
      jsonlite \
      httr2 \
      rmarkdown \
    && rm -rf /var/lib/apt/lists/*

FROM rocker/shiny:4.4.1

ENV DEBIAN_FRONTEND=noninteractive \
    APP_HOME=/app \
    SHINY_HOST=0.0.0.0 \
    SHINY_PORT=3838

RUN apt-get update && apt-get upgrade -y && apt-get install -y --no-install-recommends \
      ca-certificates \
      tini \
      pandoc \
      libcairo2 \
      libcurl4 \
      libfontconfig1 \
      libsodium23 \
      libssl3 \
      libxml2 \
      libxt6 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/local/lib/R/site-library /usr/local/lib/R/site-library

WORKDIR ${APP_HOME}
COPY app.R prepare_data.R i18n_setup.R xroad_bridge.R ./
COPY data_poc/ ./data_poc/
COPY translations/ ./translations/
COPY report/ ./report/

RUN chown -R shiny:shiny ${APP_HOME}

USER shiny

# Port interne de l'application
EXPOSE 3838

HEALTHCHECK --interval=30s --timeout=5s --start-period=45s --retries=3 \
  CMD R -q -e "tryCatch({c<-socketConnection('127.0.0.1',3838,open='r',timeout=4);close(c)},error=function(e)quit(status=1))" || exit 1

ENTRYPOINT ["/usr/bin/tini", "--"]
CMD ["R", "-e", "shiny::runApp('/app', host=Sys.getenv('SHINY_HOST','0.0.0.0'), port=as.integer(Sys.getenv('SHINY_PORT','3838')))"]
