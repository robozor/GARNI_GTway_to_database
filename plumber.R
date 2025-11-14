# =============================================================================
#  WiFi meteostanice GARNI GTway Plus → R / Plumber → PostgreSQL / MySQL
#
#  KONFIGURACE DOCKER KONTEJNERU (ENV PROMĚNNÉ)
#  -------------------------------------------
#  - DB_URL
#      Connection string ve stylu:
#        Postgres: postgres://user:pass@host:5432/dbname
#        MySQL:   mysql://user:pass@host:3306/dbname
#
#      Příklady:
#        postgres://garni:secret@dwh-db-prod:5432/weather
#        mysql://garni:secret@mysql-server:3306/weather
#
#  - DB_SCHEMA
#      Jméno schématu, kam se bude zapisovat tabulka weather_observations
#      a weather_observations_wide.
#      Pro Postgres: např. "garni" (vytvoří se automaticky pokud neexistuje).
#      Pro MySQL: schéma se ignoruje a používá se přímo databáze z DB_URL.
#
#  - PLUMBER_HOST
#      IP / host, na kterém má plumber poslouchat:
#        "0.0.0.0"  = všechny IP (typicky v Dockeru)
#        "127.0.0.1" = jen localhost (např. pokud bude před tím reverzní proxy)
#
#  - PLUMBER_PORT
#      Port HTTP/HTTPS serveru plumberu (default 8888).
#
#
#  PŘÍKLAD docker run (HTTPS, certifikáty mountnuté z NAS)
#  -------------------------------------------------------
#  docker run -d \
#    --name wslink-plumber \
#    -e DB_URL='postgres://garni:secret@192.168.0.151:5432/weather' \
#    -e DB_SCHEMA='garni' \
#    -e PLUMBER_HOST='0.0.0.0' \
#    -e PLUMBER_PORT='8888' \
#    -p 8888:8888 \
#    -v /volume1/docker/wslink-plumber:/app \
#    wslink-plumber
#
#  V Dockerfile pak typicky:
#    CMD ["R", "-e", "source('plumber.R'); run_wslink()"]
#
#  NASTAVENÍ WSLink / GARNI GTway Plus
#  -----------------------------------
#  - Server / Host: IP adresu Synology (např. 192.168.0.151)
#  - Port:          8888
#  - Path / URL:    /data/upload.php
#
#  Dohromady tedy (POZOR: HTTPS!):
#    https://192.168.0.151:8888/data/upload.php
#
#  CO DĚLÁ TENTO ENDPOINT
#  ----------------------
#  - přijme HTTP GET / POST z meteostanice
#  - vezme všechny query parametry (všechna čidla)
#  - uloží je:
#
#    1) jako raw JSON do tabulky:
#         <DB_SCHEMA>.weather_observations      (Postgres)
#         weather.weather_observations          (MySQL)
#
#       Struktura:
#         id           BIGSERIAL / BIGINT AUTO_INCREMENT  PRIMARY KEY
#         received_at  TIMESTAMP / TIMESTAMPTZ (čas na serveru)
#         remote_ip    TEXT
#         method       TEXT  (GET / POST)
#         query_json   JSON / JSONB  (všechny parametry z meteostanice)
#
#    2) do široké tabulky s rozparsovanými sloupci:
#         <DB_SCHEMA>.weather_observations_wide (Postgres)
#         weather.weather_observations_wide     (MySQL)
#
#       - každý WSLink parametr z API.pdf má vlastní sloupec
#       - názvy sloupců jsou „lidské“ (snake_case, podle Description v PDF)
#       - datové typy:
#           string  → TEXT
#           float   → DOUBLE PRECISION (Postgres) / DOUBLE (MySQL)
#           integer → INTEGER (Postgres) / INT (MySQL)
#       - navíc:
#           received_at  (čas přijetí na serveru)
#           remote_ip    (IP stanice)
#           method       (GET/POST)
#           raw_json     (znovu uložený celý JSON s čidly)
#
#  Rozšiřitelnost:
#  ---------------
#  - Tabulka weather_observations_wide obsahuje i sloupce pro čidla,
#    která aktuálně nemusíš mít, takže při jejich
#    pozdějším připojení není třeba měnit strukturu tabulky.
#
# =============================================================================

library(plumber)
library(DBI)
library(RPostgres)
library(RMariaDB)
library(jsonlite)
library(httr)

# Jednoduchý "coalesce"
nz_or <- function(x, default) {
  if (is.null(x) || (is.character(x) && !nzchar(x))) default else x
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------------------------------------------------------------------
# Definice všech WSLink parametrů a mapování na „lidské“ názvy sloupců
# ---------------------------------------------------------------------------
# type:
#   - "string"  → TEXT
#   - "float"   → DOUBLE PRECISION (Postgres) / DOUBLE (MySQL)
#   - "integer" → INTEGER (Postgres) / INT (MySQL)
wslink_columns <- list(
  # Základní parametry
  list(param = "wsid",     col = "device_id",                    type = "string"),
  list(param = "wspw",     col = "device_password",              type = "string"),
  list(param = "datetime", col = "station_datetime",             type = "string"),
  list(param = "rbar",     col = "rel_air_pressure_hpa",         type = "float"),
  list(param = "abar",     col = "abs_air_pressure_hpa",         type = "float"),
  list(param = "intem",    col = "indoor_temperature_c",         type = "float"),
  list(param = "inhum",    col = "indoor_humidity_pct",          type = "integer"),
  list(param = "inbat",    col = "console_battery_level",        type = "integer"),

  # Type1 – hlavní venkovní senzor
  list(param = "t1tem",    col = "t1_outdoor_temperature_c",     type = "float"),
  list(param = "t1hum",    col = "t1_outdoor_humidity_pct",      type = "integer"),
  list(param = "t1feels",  col = "t1_feels_like_temperature_c",  type = "float"),
  list(param = "t1chill",  col = "t1_wind_chill_temperature_c",  type = "float"),
  list(param = "t1heat",   col = "t1_heat_index_temperature_c",  type = "float"),
  list(param = "t1dew",    col = "t1_dew_point_temperature_c",   type = "float"),
  list(param = "t1wdir",   col = "t1_wind_direction_deg",        type = "integer"),
  list(param = "t1ws",     col = "t1_wind_speed_ms",             type = "float"),
  list(param = "t1ws10mav",col = "t1_wind_speed_10min_avg_ms",   type = "float"),
  list(param = "t1wgust",  col = "t1_wind_gust_ms",              type = "float"),

  list(param = "t1rainra", col = "t1_rain_rate_mm_per_h",        type = "float"),
  list(param = "t1rainhr", col = "t1_rain_hourly_mm",            type = "float"),
  list(param = "t1raindy", col = "t1_rain_daily_mm",             type = "float"),
  list(param = "t1rainwy", col = "t1_rain_weekly_mm",            type = "float"),
  list(param = "t1rainmth",col = "t1_rain_monthly_mm",           type = "float"),
  list(param = "t1rainyr", col = "t1_rain_yearly_mm",            type = "float"),

  list(param = "t1uvi",    col = "t1_uvi_index",                 type = "float"),
  list(param = "t1solrad", col = "t1_solar_radiation_w_m2",      type = "float"),
  list(param = "t1wbgt",   col = "t1_wbgt_temperature_c",        type = "float"),

  list(param = "t1bat",    col = "t1_sensor_battery_level",      type = "integer"),
  list(param = "t1cn",     col = "t1_sensor_connection_status",  type = "integer"),

  # Type2/3/4 – kanálové teploměry / vlhkost / půda / bazén
  list(param = "t234c1tem", col = "t234_ch1_temperature_c",      type = "float"),
  list(param = "t234c1hum", col = "t234_ch1_humidity_pct",       type = "integer"),
  list(param = "t234c1bat", col = "t234_ch1_battery_level",      type = "integer"),
  list(param = "t234c1cn",  col = "t234_ch1_connection_status",  type = "integer"),
  list(param = "t234c1tp",  col = "t234_ch1_sensor_type",        type = "integer"),

  list(param = "t234c2tem", col = "t234_ch2_temperature_c",      type = "float"),
  list(param = "t234c2hum", col = "t234_ch2_humidity_pct",       type = "integer"),
  list(param = "t234c2bat", col = "t234_ch2_battery_level",      type = "integer"),
  list(param = "t234c2cn",  col = "t234_ch2_connection_status",  type = "integer"),
  list(param = "t234c2tp",  col = "t234_ch2_sensor_type",        type = "integer"),

  list(param = "t234c3tem", col = "t234_ch3_temperature_c",      type = "float"),
  list(param = "t234c3hum", col = "t234_ch3_humidity_pct",       type = "integer"),
  list(param = "t234c3bat", col = "t234_ch3_battery_level",      type = "integer"),
  list(param = "t234c3cn",  col = "t234_ch3_connection_status",  type = "integer"),
  list(param = "t234c3tp",  col = "t234_ch3_sensor_type",        type = "integer"),

  list(param = "t234c4tem", col = "t234_ch4_temperature_c",      type = "float"),
  list(param = "t234c4hum", col = "t234_ch4_humidity_pct",       type = "integer"),
  list(param = "t234c4bat", col = "t234_ch4_battery_level",      type = "integer"),
  list(param = "t234c4cn",  col = "t234_ch4_connection_status",  type = "integer"),
  list(param = "t234c4tp",  col = "t234_ch4_sensor_type",        type = "integer"),

  list(param = "t234c5tem", col = "t234_ch5_temperature_c",      type = "float"),
  list(param = "t234c5hum", col = "t234_ch5_humidity_pct",       type = "integer"),
  list(param = "t234c5bat", col = "t234_ch5_battery_level",      type = "integer"),
  list(param = "t234c5cn",  col = "t234_ch5_connection_status",  type = "integer"),
  list(param = "t234c5tp",  col = "t234_ch5_sensor_type",        type = "integer"),

  list(param = "t234c6tem", col = "t234_ch6_temperature_c",      type = "float"),
  list(param = "t234c6hum", col = "t234_ch6_humidity_pct",       type = "integer"),
  list(param = "t234c6bat", col = "t234_ch6_battery_level",      type = "integer"),
  list(param = "t234c6cn",  col = "t234_ch6_connection_status",  type = "integer"),
  list(param = "t234c6tp",  col = "t234_ch6_sensor_type",        type = "integer"),

  list(param = "t234c7tem", col = "t234_ch7_temperature_c",      type = "float"),
  list(param = "t234c7hum", col = "t234_ch7_humidity_pct",       type = "integer"),
  list(param = "t234c7bat", col = "t234_ch7_battery_level",      type = "integer"),
  list(param = "t234c7cn",  col = "t234_ch7_connection_status",  type = "integer"),
  list(param = "t234c7tp",  col = "t234_ch7_sensor_type",        type = "integer"),

  # Type5 – bleskový senzor
  list(param = "t5lst",    col = "t5_last_lightning_time",       type = "integer"),
  list(param = "t5lskm",   col = "t5_lightning_distance_km",     type = "float"),
  list(param = "t5lsf",    col = "t5_lightning_count_last_1h",   type = "integer"),
  list(param = "t5ls5mtc", col = "t5_lightning_count_5min",      type = "integer"),
  list(param = "t5ls30mtc",col = "t5_lightning_count_30min",     type = "integer"),
  list(param = "t5ls1htc", col = "t5_lightning_count_1h_total",  type = "integer"),
  list(param = "t5ls1dtc", col = "t5_lightning_count_1d_total",  type = "integer"),
  list(param = "t5lsbat",  col = "t5_sensor_battery_level",      type = "integer"),
  list(param = "t5lscn",   col = "t5_sensor_connection_status",  type = "integer"),

  # Type6 – detektory úniku vody
  list(param = "t6c1wls",  col = "t6_ch1_leak_status",           type = "integer"),
  list(param = "t6c1bat",  col = "t6_ch1_battery_level",         type = "integer"),
  list(param = "t6c1cn",   col = "t6_ch1_connection_status",     type = "integer"),

  list(param = "t6c2wls",  col = "t6_ch2_leak_status",           type = "integer"),
  list(param = "t6c2bat",  col = "t6_ch2_battery_level",         type = "integer"),
  list(param = "t6c2cn",   col = "t6_ch2_connection_status",     type = "integer"),

  list(param = "t6c3wls",  col = "t6_ch3_leak_status",           type = "integer"),
  list(param = "t6c3bat",  col = "t6_ch3_battery_level",         type = "integer"),
  list(param = "t6c3cn",   col = "t6_ch3_connection_status",     type = "integer"),

  list(param = "t6c4wls",  col = "t6_ch4_leak_status",           type = "integer"),
  list(param = "t6c4bat",  col = "t6_ch4_battery_level",         type = "integer"),
  list(param = "t6c4cn",   col = "t6_ch4_connection_status",     type = "integer"),

  list(param = "t6c5wls",  col = "t6_ch5_leak_status",           type = "integer"),
  list(param = "t6c5bat",  col = "t6_ch5_battery_level",         type = "integer"),
  list(param = "t6c5cn",   col = "t6_ch5_connection_status",     type = "integer"),

  list(param = "t6c6wls",  col = "t6_ch6_leak_status",           type = "integer"),
  list(param = "t6c6bat",  col = "t6_ch6_battery_level",         type = "integer"),
  list(param = "t6c6cn",   col = "t6_ch6_connection_status",     type = "integer"),

  list(param = "t6c7wls",  col = "t6_ch7_leak_status",           type = "integer"),
  list(param = "t6c7bat",  col = "t6_ch7_battery_level",         type = "integer"),
  list(param = "t6c7cn",   col = "t6_ch7_connection_status",     type = "integer"),

  # Type8 – prachové částice
  list(param = "t8pm25",   col = "t8_pm25_ug_m3",                type = "integer"),
  list(param = "t8pm10",   col = "t8_pm10_ug_m3",                type = "integer"),
  list(param = "t8pm25ai", col = "t8_pm25_aqi",                  type = "integer"),
  list(param = "t8pm10ai", col = "t8_pm10_aqi",                  type = "integer"),
  list(param = "t8bat",    col = "t8_pm_sensor_battery_level",   type = "integer"),
  list(param = "t8cn",     col = "t8_pm_sensor_connection_status", type = "integer"),

  # Type9 – HCHO / VOC
  list(param = "t9hcho",   col = "t9_hcho_ppb",                  type = "integer"),
  list(param = "t9voclv",  col = "t9_voc_level",                 type = "integer"),
  list(param = "t9bat",    col = "t9_sensor_battery_level",      type = "integer"),
  list(param = "t9cn",     col = "t9_sensor_connection_status",  type = "integer"),

  # Type10 – CO2
  list(param = "t10co2",   col = "t10_co2_ppm",                  type = "integer"),
  list(param = "t10bat",   col = "t10_sensor_battery_level",     type = "integer"),
  list(param = "t10cn",    col = "t10_sensor_connection_status", type = "integer"),

  # Type11 – CO
  list(param = "t11co",    col = "t11_co_ppm",                   type = "integer"),
  list(param = "t11bat",   col = "t11_sensor_battery_level",     type = "integer"),
  list(param = "t11cn",    col = "t11_sensor_connection_status", type = "integer"),

  # Verze API
  list(param = "apiver",   col = "api_version",                  type = "float")
)

# ---------------------------------------------------------------------------
# Parse DB_URL do seznamu
# ---------------------------------------------------------------------------
parse_db_url <- function(url) {
  if (!nzchar(url)) {
    stop("Environment variable DB_URL is not set.", call. = FALSE)
  }
  u <- httr::parse_url(url)

  driver <- switch(
    tolower(u$scheme),
    "postgres"   = "postgres",
    "postgresql" = "postgres",
    "mysql"      = "mysql",
    "mariadb"    = "mysql",
    stop(sprintf("Unsupported DB scheme '%s'. Use postgres:// or mysql://", u$scheme),
         call. = FALSE)
  )

  host <- u$hostname %||% "localhost"
  port <- if (!is.null(u$port)) u$port else if (driver == "postgres") 5432L else 3306L
  user <- u$username
  password <- u$password
  dbname <- sub("^/", "", u$path %||% "")

  if (!nzchar(dbname)) {
    stop("DB_URL must include database name in the path part, e.g. postgres://user:pass@host:5432/dbname",
         call. = FALSE)
  }

  list(
    driver   = driver,
    host     = host,
    port     = as.integer(port),
    user     = user,
    password = password,
    dbname   = dbname
  )
}

# ---------------------------------------------------------------------------
# Globální stav DB
# ---------------------------------------------------------------------------
.db_state <- new.env(parent = emptyenv())

# Helpery pro konverzi typů
to_str <- function(x) {
  if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[[1]])
}

to_float <- function(x) {
  if (is.null(x) || length(x) == 0) NA_real_ else suppressWarnings(as.numeric(x[[1]]))
}

to_int <- function(x) {
  if (is.null(x) || length(x) == 0) NA_integer_ else suppressWarnings(as.integer(x[[1]]))
}

# ---------------------------------------------------------------------------
# Vytvoření široké tabulky (PostgreSQL)
# ---------------------------------------------------------------------------
create_wide_table_postgres <- function(conn, schema) {
  cols <- c(
    "id BIGSERIAL PRIMARY KEY",
    "received_at TIMESTAMPTZ NOT NULL DEFAULT NOW()",
    "remote_ip TEXT",
    "method TEXT",
    "raw_json JSONB"
  )

  for (m in wslink_columns) {
    sql_type <- switch(
      m$type,
      "string"  = "TEXT",
      "float"   = "DOUBLE PRECISION",
      "integer" = "INTEGER",
      "TEXT"
    )
    cols <- c(cols, sprintf("%s %s", m$col, sql_type))
  }

  sql <- sprintf(
    "CREATE TABLE IF NOT EXISTS %s.weather_observations_wide (\n  %s\n);",
    schema,
    paste(cols, collapse = ",\n  ")
  )
  DBI::dbExecute(conn, sql)
}

# ---------------------------------------------------------------------------
# Vytvoření široké tabulky (MySQL / MariaDB)
# ---------------------------------------------------------------------------
create_wide_table_mysql <- function(conn) {
  cols <- c(
    "id BIGINT AUTO_INCREMENT PRIMARY KEY",
    "received_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP",
    "remote_ip TEXT",
    "method TEXT",
    "raw_json JSON"
  )

  for (m in wslink_columns) {
    sql_type <- switch(
      m$type,
      "string"  = "TEXT",
      "float"   = "DOUBLE",
      "integer" = "INT",
      "TEXT"
    )
    cols <- c(cols, sprintf("%s %s", m$col, sql_type))
  }

  sql <- sprintf(
    "CREATE TABLE IF NOT EXISTS weather_observations_wide (\n  %s\n) ENGINE=InnoDB;",
    paste(cols, collapse = ",\n  ")
  )
  DBI::dbExecute(conn, sql)
}

# ---------------------------------------------------------------------------
# Inicializace DB (připojení + vytvoření tabulek)
# ---------------------------------------------------------------------------
init_db <- function() {
  if (!is.null(.db_state$conn)) {
    # už inicializováno
    return(invisible(TRUE))
  }

  db_url <- Sys.getenv("DB_URL", "")
  cfg    <- parse_db_url(db_url)
  driver <- cfg$driver
  schema <- Sys.getenv("DB_SCHEMA", if (driver == "postgres") "public" else "")

  if (driver == "postgres") {
    # 1) Připojení do default DB (postgres) a pokus o vytvoření cílové DB
    admin_con <- dbConnect(
      RPostgres::Postgres(),
      dbname   = "postgres",
      host     = cfg$host,
      port     = cfg$port,
      user     = cfg$user,
      password = cfg$password
    )
    dbname_quoted <- DBI::dbQuoteIdentifier(admin_con, cfg$dbname)
    try({
      dbExecute(admin_con, sprintf("CREATE DATABASE %s", dbname_quoted))
    }, silent = TRUE)
    dbDisconnect(admin_con)

    # 2) Připojení do cílové DB
    conn <- dbConnect(
      RPostgres::Postgres(),
      dbname   = cfg$dbname,
      host     = cfg$host,
      port     = cfg$port,
      user     = cfg$user,
      password = cfg$password
    )

    # 3) Schéma
    if (nzchar(schema)) {
      dbExecute(conn, sprintf("CREATE SCHEMA IF NOT EXISTS %s", DBI::dbQuoteIdentifier(conn, schema)))
    } else {
      schema <- "public"
    }

    # 4) Tabulka raw JSON
    table_json_sql <- sprintf("
      CREATE TABLE IF NOT EXISTS %s.weather_observations (
        id           BIGSERIAL PRIMARY KEY,
        received_at  TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        remote_ip    TEXT,
        method       TEXT,
        query_json   JSONB
      );
    ", schema)
    dbExecute(conn, table_json_sql)

    # 5) Široká tabulka s rozparsovanými sloupci
    create_wide_table_postgres(conn, schema)

    table_id_json <- DBI::Id(schema = schema, table = "weather_observations")
    table_id_wide <- DBI::Id(schema = schema, table = "weather_observations_wide")

  } else if (driver == "mysql") {
    # MySQL / MariaDB
    # 1) Připojení bez DB, vytvoření DB
    admin_con <- dbConnect(
      RMariaDB::MariaDB(),
      host     = cfg$host,
      port     = cfg$port,
      user     = cfg$user,
      password = cfg$password
    )
    dbExecute(admin_con, sprintf("CREATE DATABASE IF NOT EXISTS `%s`", cfg$dbname))
    dbDisconnect(admin_con)

    # 2) Připojení do cílové DB
    conn <- dbConnect(
      RMariaDB::MariaDB(),
      dbname   = cfg$dbname,
      host     = cfg$host,
      port     = cfg$port,
      user     = cfg$user,
      password = cfg$password
    )

    # 3) Tabulka raw JSON
    table_json_sql <- "
      CREATE TABLE IF NOT EXISTS weather_observations (
        id           BIGINT AUTO_INCREMENT PRIMARY KEY,
        received_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
        remote_ip    TEXT,
        method       TEXT,
        query_json   JSON
      ) ENGINE=InnoDB;
    "
    dbExecute(conn, table_json_sql)

    # 4) Široká tabulka
    create_wide_table_mysql(conn)

    table_id_json <- "weather_observations"
    table_id_wide <- "weather_observations_wide"

    schema <- NULL

  } else {
    stop("Unsupported driver (should never happen).", call. = FALSE)
  }

  .db_state$conn          <- conn
  .db_state$driver        <- driver
  .db_state$schema        <- schema
  .db_state$table_id_json <- table_id_json
  .db_state$table_id_wide <- table_id_wide

  invisible(TRUE)
}

get_conn <- function() {
  init_db()
  conn <- .db_state$conn
  if (!DBI::dbIsValid(conn)) {
    .db_state$conn <- NULL
    init_db()
    conn <- .db_state$conn
  }
  conn
}

# =====================================================================
#  Plumber API
# =====================================================================

#* Healthcheck endpoint
#* @get /health
function() {
  list(
    status = "ok",
    time   = as.character(Sys.time()),
    db_ok  = tryCatch({
      conn <- get_conn()
      DBI::dbGetQuery(conn, "SELECT 1 AS ok")$ok[1] == 1
    }, error = function(e) FALSE)
  )
}

#* Příjem dat z meteostanice (GET / POST)
#* @get  /data/upload.php
#* @post /data/upload.php
function(req, res) {
  conn <- get_conn()
  table_id_json <- .db_state$table_id_json
  table_id_wide <- .db_state$table_id_wide

  # Všechna čidla a parametry jako list
  args_list <- req$args
  if (is.null(args_list)) args_list <- list()

  # JSON pro raw tabulku + wide raw_json
  query_json <- jsonlite::toJSON(args_list, auto_unbox = TRUE, null = "null")

  # ----------------------------------------------------------------------------
  # 1) Zápis do původní tabulky weather_observations (raw JSON)
  # ----------------------------------------------------------------------------
  row_raw <- data.frame(
    received_at = Sys.time(),
    remote_ip   = nz_or(req$REMOTE_ADDR, NA_character_),
    method      = nz_or(req$REQUEST_METHOD, NA_character_),
    query_json  = query_json,
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(
    conn,
    name       = table_id_json,
    value      = row_raw,
    append     = TRUE,
    row.names  = FALSE
  )

  # ----------------------------------------------------------------------------
  # 2) Zápis do široké tabulky weather_observations_wide
  # ----------------------------------------------------------------------------
  row_wide <- list(
    received_at = Sys.time(),
    remote_ip   = nz_or(req$REMOTE_ADDR, NA_character_),
    method      = nz_or(req$REQUEST_METHOD, NA_character_),
    raw_json    = query_json
  )

  for (m in wslink_columns) {
    val <- args_list[[m$param]]
    row_wide[[m$col]] <- switch(
      m$type,
      "string"  = to_str(val),
      "float"   = to_float(val),
      "integer" = to_int(val),
      to_str(val)
    )
  }

  row_wide_df <- as.data.frame(row_wide, stringsAsFactors = FALSE)

  DBI::dbWriteTable(
    conn,
    name       = table_id_wide,
    value      = row_wide_df,
    append     = TRUE,
    row.names  = FALSE
  )

  # Volitelně log do souboru
  log_file <- "wslink_raw.log"
  cat("---- PŘIJATÝ POŽADAVEK ----\n", file = log_file, append = TRUE)
  cat("Čas: ", as.character(Sys.time()), "\n", file = log_file, append = TRUE)
  cat("Remote IP: ", nz_or(req$REMOTE_ADDR, ""), "\n", file = log_file, append = TRUE)
  cat("Metoda: ", nz_or(req$REQUEST_METHOD, ""), "\n", file = log_file, append = TRUE)
  cat("Query/args:\n", file = log_file, append = TRUE)
  capture.output(str(args_list), file = log_file, append = TRUE)
  cat("----------------------------\n\n", file = log_file, append = TRUE)

  res$status <- 200
  res$body <- "success"
  res
}

# =====================================================================
#  SPUŠTĚNÍ PLUMBER API (HTTP uvnitř Dockeru)
# =====================================================================

# Voláno z Dockerfile:
#   R -e "source('plumber.R'); run_wslink()"
run_wslink <- function() {
  host <- Sys.getenv("PLUMBER_HOST", "0.0.0.0")
  port <- as.integer(Sys.getenv("PLUMBER_PORT", "8888"))

  message("==============================================")
  message("  Spouštím WSLink Plumber API (HTTP)")
  message("  Host: ", host)
  message("  Port: ", port)
  message("==============================================")

  pr <- plumber::plumb("plumber.R")
  pr$run(
    host = host,
    port = port
  )
}
