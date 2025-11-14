FROM rocker/r-ver:4.4.1

RUN apt-get update && \
    apt-get install -y \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      libpq-dev \
      libmariadb-dev \
      libsodium-dev \
      zlib1g-dev \
      pkg-config && \
    rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('plumber','DBI','RPostgres','RMariaDB','jsonlite','httr'), repos='https://cloud.r-project.org')"

WORKDIR /app
COPY plumber.R /app/plumber.R

EXPOSE 8888

CMD ["R", "-e", "source('plumber.R'); run_wslink()"]
