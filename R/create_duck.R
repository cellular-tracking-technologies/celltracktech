create_duck <- function(conn) {
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS ctt_project
  (
    id	smallint PRIMARY KEY,
    name	TEXT NOT NULL UNIQUE
  )")
  #

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS nodes
  (
    node_id TEXT NOT NULL PRIMARY KEY
  )")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS data_file
  (
    path TEXT PRIMARY KEY
  )")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS ctt_project_station
  (
    db_id	smallint PRIMARY KEY,
    project_id smallint NOT NULL,
    station_id	TEXT NOT NULL,
    deploy_at	TIMESTAMP with time zone,
    end_at	TIMESTAMP with time zone,
    FOREIGN KEY (project_id)
      REFERENCES ctt_project (id)
        ON DELETE NO ACTION
        ON UPDATE NO ACTION
  )")

  DBI::dbExecute(conn, "
  CREATE SEQUENCE IF NOT EXISTS seq_id START 1;
  CREATE TABLE IF NOT EXISTS raw
  (
    id integer primary key default nextval('seq_id'),
    path  TEXT NOT NULL,
    radio_id smallint NOT NULL,
    tag_id TEXT,
    node_id TEXT,
    tag_rssi smallint,
    validated smallint,
    time TIMESTAMP with time zone NOT NULL,
    station_id TEXT
  )")

  DBI::dbExecute(conn, "
  CREATE SEQUENCE IF NOT EXISTS seq_idb START 1;
  CREATE TABLE IF NOT EXISTS blu
  (
    id integer primary key default nextval('seq_idb'),
    path  TEXT NOT NULL,
    radio_id smallint,
    usb_port smallint,
    blu_radio_id smallint,
    tag_id TEXT,
    node_id TEXT,
    tag_rssi smallint,
    sync integer,
    product smallint,
    revision smallint,
    payload text,
    time TIMESTAMP with time zone NOT NULL,
    station_id TEXT
  );
  ALTER TABLE blu
  ADD COLUMN IF NOT EXISTS battery_voltage_v DECIMAL(6, 3);
  ALTER TABLE blu
  ADD COLUMN IF NOT EXISTS temperature_celsius DECIMAL(6, 3)"
                 )

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS node_health
  (
    PRIMARY KEY (radio_id, node_id, time, station_id),
    time TIMESTAMP with time zone NOT NULL,
    radio_id smallint,
    node_id TEXT,
    node_rssi smallint,
    battery NUMERIC(3,2),
    celsius smallint,
    recorded_at TIMESTAMP with time zone,
    firmware TEXT,
    solar_volts NUMERIC(4,2),
    solar_current smallint,
    cumulative_solar_current integer,
    latitude NUMERIC(8,6),
    longitude NUMERIC(9,6),
    station_id TEXT,
    path  TEXT NOT NULL,
    FOREIGN KEY (node_id)
      REFERENCES nodes (node_id)
        ON DELETE NO ACTION
        ON UPDATE NO ACTION
  )")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS gps
  (
    path  TEXT NOT NULL,
    latitude NUMERIC(8,6),
    longitude NUMERIC(9,6),
    altitude NUMERIC(6,1),
    quality smallint,
    gps_at TIMESTAMP with time zone,
    recorded_at TIMESTAMP with time zone,
    station_id TEXT,
    mean_lat NUMERIC(8,6),
    mean_lng NUMERIC(9,6),
    n_fixes smallint,
    PRIMARY KEY (gps_at, station_id)
  )")

  DBI::dbExecute(conn, "
  CREATE SEQUENCE IF NOT EXISTS seq_id START 1;
  CREATE TABLE IF NOT EXISTS node_raw
  (
    id integer primary key default nextval('seq_id'),
    path  TEXT NOT NULL,
    radio_id TEXT,
    tag_id TEXT,
    node_id TEXT,
    tag_rssi smallint,
    time TIMESTAMP with time zone NOT NULL,
    validated smallint,
    station_id TEXT
  )")

  DBI::dbExecute(conn, "
  CREATE SEQUENCE IF NOT EXISTS seq_idb START 1;
  CREATE TABLE IF NOT EXISTS node_blu
  (
    id integer primary key default nextval('seq_idb'),
    path  TEXT NOT NULL,
    radio_id smallint,
    usb_port smallint,
    blu_radio_id smallint,
    tag_id TEXT,
    node_id TEXT,
    tag_rssi smallint,
    sync integer,
    product smallint,
    revision smallint,
    payload text,
    time TIMESTAMP with time zone NOT NULL,
    station_id TEXT
  );
  ALTER TABLE node_blu
  ADD COLUMN IF NOT EXISTS battery_voltage_v DECIMAL(6,3);
  ALTER TABLE node_blu
  ADD COLUMN IF NOT EXISTS temperature_celsius DECIMAL(6,3);")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS node_health_from_node
  (
    PRIMARY KEY (node_id, time, station_id),
    time TIMESTAMP with time zone NOT NULL,
    up_time BIGINT,
    power_ok smallint,
    batt_mv smallint,
    batt_temp_c smallint,
    charge_mv smallint,
    charge_ma smallint,
    charge_temp_c smallint,
    node_temp_c smallint,
    energy_used_mah smallint,
    sd_free smallint,
    sub_ghz_det smallint,
    ble_det smallint,
    errors TEXT,
    node_id TEXT,
    station_id TEXT,
    path  TEXT NOT NULL,
    FOREIGN KEY (node_id)
      REFERENCES nodes (node_id)
        ON DELETE NO ACTION
        ON UPDATE NO ACTION
  )")

  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS node_gps
  (
    path  TEXT NOT NULL,
    latitude NUMERIC(8,6),
    longitude NUMERIC(9,6),
    altitude NUMERIC(6,1),
    gps_at TIMESTAMP WITH TIME ZONE,
    hdop smallint,
    vdop smallint,
    pdop smallint,
    navigation_mode smallint,
    satellites NUMERIC(5,2),
    on_time NUMERIC(3,0),
    station_id TEXT,
    node_id TEXT,
    PRIMARY KEY (gps_at, node_id)
  )")
}
