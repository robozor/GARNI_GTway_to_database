 =============================================================================
  WiFi meteostanice GARNI GTway Plus → R / Plumber → PostgreSQL / MySQL

  KONFIGURACE DOCKER KONTEJNERU (ENV PROMĚNNÉ)
  -------------------------------------------
  - DB_URL
      Connection string ve stylu:
        Postgres: postgres://user:pass@host:5432/dbname
        MySQL:   mysql://user:pass@host:3306/dbname

      Příklady:
        postgres://garni:secret@dwh-db-prod:5432/weather
        mysql://garni:secret@mysql-server:3306/weather

  - DB_SCHEMA
      Jméno schématu, kam se bude zapisovat tabulka weather_observations
      a weather_observations_wide.
      Pro Postgres: např. "garni" (vytvoří se automaticky pokud neexistuje).
      Pro MySQL: schéma se ignoruje a používá se přímo databáze z DB_URL.

  - PLUMBER_HOST
      IP / host, na kterém má plumber poslouchat:
        "0.0.0.0"  = všechny IP (typicky v Dockeru)
        "127.0.0.1" = jen localhost (např. pokud bude před tím reverzní proxy)

  - PLUMBER_PORT
      Port HTTP/HTTPS serveru plumberu (default 8888).


  PŘÍKLAD docker run (HTTPS, certifikáty mountnuté z NAS)
  -------------------------------------------------------
  docker run -d \
    --name wslink-plumber \
    -e DB_URL='postgres://garni:secret@192.168.0.151:5432/weather' \
    -e DB_SCHEMA='garni' \
    -e PLUMBER_HOST='0.0.0.0' \
    -e PLUMBER_PORT='8888' \
    -p 8888:8888 \
    -v /volume1/docker/wslink-plumber:/app \
    wslink-plumber

  V Dockerfile pak typicky:
    CMD ["R", "-e", "source('plumber.R'); run_wslink()"]

  NASTAVENÍ WSLink / GARNI GTway Plus
  -----------------------------------
  - Server / Host: IP adresu Synology (např. 192.168.0.151)
  - Port:          8888
  - Path / URL:    /data/upload.php

  Dohromady tedy (POZOR: HTTPS!):
    https://192.168.0.151:8888/data/upload.php

  CO DĚLÁ TENTO ENDPOINT
  ----------------------
  - přijme HTTP GET / POST z meteostanice
  - vezme všechny query parametry (všechna čidla)
  - uloží je:

    1) jako raw JSON do tabulky:
         <DB_SCHEMA>.weather_observations      (Postgres)
         weather.weather_observations          (MySQL)

       Struktura:
         id           BIGSERIAL / BIGINT AUTO_INCREMENT  PRIMARY KEY
         received_at  TIMESTAMP / TIMESTAMPTZ (čas na serveru)
         remote_ip    TEXT
         method       TEXT  (GET / POST)
         query_json   JSON / JSONB  (všechny parametry z meteostanice)

    2) do široké tabulky s rozparsovanými sloupci:
         <DB_SCHEMA>.weather_observations_wide (Postgres)
         weather.weather_observations_wide     (MySQL)

       - každý WSLink parametr z API.pdf má vlastní sloupec
       - názvy sloupců jsou „lidské“ (snake_case, podle Description v PDF)
       - datové typy:
           string  → TEXT
           float   → DOUBLE PRECISION (Postgres) / DOUBLE (MySQL)
           integer → INTEGER (Postgres) / INT (MySQL)
       - navíc:
           received_at  (čas přijetí na serveru)
           remote_ip    (IP stanice)
           method       (GET/POST)
           raw_json     (znovu uložený celý JSON s čidly)

  Rozšiřitelnost:
  ---------------
  - Tabulka weather_observations_wide obsahuje i sloupce pro čidla,
    která aktuálně nemusíš mít, takže při jejich
    pozdějším připojení není třeba měnit strukturu tabulky.

 =============================================================================
