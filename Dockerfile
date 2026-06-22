# =====================================================================
#  i-Tafaray - Tableau de bord One Health (R / Shiny)
#  Image Docker durcie : build en deux etapes, runtime minimal, pas de
#  secrets applicatifs embarques, execution non root.
# =====================================================================

FROM rocker/r-ver:4.4.1 AS build

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get upgrade -y && apt-get install -y --no-install-recommends \
      g++ \
      make \
      libcurl4-openssl-dev \
      libgit2-dev \
      libharfbuzz-dev \
      libfribidi-dev \
      libfreetype6-dev \
      libpng-dev \
      libtiff5-dev \
      libjpeg-dev \
      libicu-dev \
      libssl-dev \
      libxml2-dev \
      libfontconfig1-dev \
      libcairo2-dev \
      libxt6 \
      libsodium-dev \
      pandoc \
      littler \
      r-cran-littler \
    && install2.r --error --skipinstalled \
      shiny \
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
    && rm -rf /var/lib/apt/lists/*

FROM rocker/r-ver:4.4.1

ENV DEBIAN_FRONTEND=noninteractive \
    APP_HOME=/app \
    SHINY_HOST=0.0.0.0 \
    SHINY_PORT=3838

RUN apt-get update && apt-get upgrade -y && apt-get install -y --no-install-recommends \
      ca-certificates \
      gosu \
      tini \
      pandoc \
      libcairo2 \
      libcurl4 \
      libfontconfig1 \
      libfreetype6 \
      libfribidi0 \
      libgit2-1.5 \
      libharfbuzz0b \
      libicu72 \
      libjpeg62-turbo \
      libpng16-16 \
      libsodium23 \
      libssl3 \
      libtiff6 \
      libxml2 \
      libxt6 \
    && useradd --create-home --shell /bin/bash shiny \
    && rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/local/lib/R/site-library /usr/local/lib/R/site-library

WORKDIR ${APP_HOME}
COPY app.R prepare_data.R i18n_setup.R xroad_bridge.R ./
COPY data_poc/ ./data_poc/
COPY translations/ ./translations/
COPY report/ ./report/
COPY www/ ./www/

RUN chown -R shiny:shiny ${APP_HOME}

USER shiny

# Port interne de l'application
EXPOSE 3838

HEALTHCHECK --interval=30s --timeout=5s --start-period=45s --retries=3 \
  CMD Rscript --vanilla -e "tryCatch({c<-socketConnection('127.0.0.1',3838,open='r',timeout=4);close(c)},error=function(e)quit(status=1))" || exit 1

ENTRYPOINT ["/usr/bin/tini", "--"]
CMD ["Rscript", "--vanilla", "-e", "shiny::runApp('/app', host=Sys.getenv('SHINY_HOST','0.0.0.0'), port=as.integer(Sys.getenv('SHINY_PORT','3838')))"]
