FROM rocker/r-ver:4.1.2
ENV RENV_VERSION 0.15.5
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*
COPY renv.lock renv.lock
RUN mkdir -p renv
COPY renv/activate.R renv/activate.R
COPY renv/settings.dcf renv/settings.dcf
COPY EdFi-Survey-Loader EdFi-Survey-Loader
RUN R -e "renv::restore()"
CMD ["R", "-e", "shiny::runApp('EdFi-Survey-Loader', port = 80, host = '0.0.0.0')"]