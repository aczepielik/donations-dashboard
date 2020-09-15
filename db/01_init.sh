#!/bin/bash

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" -d "$POSTGRES_DB"  <<-EOSQL
    CREATE TABLE data_donations(
      id serial PRIMARY KEY,
      user_id int NOT NULL,
      date date NOT NULL,
      amount float8
    );
    
    CREATE TABLE model_latent_states(
      id serial PRIMARY KEY,
      user_id int NOT NULL,
      month date NOT NULL,
      state varchar(20),
      update_timestamp timestamp NOT NULL
    );
    
    CREATE VIEW view_last_fitted_states AS (
      SELECT user_id, "month", state
      FROM model_latent_states
      WHERE update_timestamp IN (
	            SELECT MAX(update_timestamp)
	            FROM model_latent_states)
	   );
    
    
    CREATE VIEW view_donations AS (
      WITH donations_appended AS (
        SELECT *, DATE_TRUNC('month', DATE)::DATE AS "month"
        FROM data_donations ),
      
      mask AS (
      SELECT FLOOR(RANDOM()* 100) AS additive)

    SELECT
	    ABS(FLOOR(SIN(da.user_id)* 100000000 + mask.additive)) AS user_id,
	    da."date",
	    da."month",
	    amount,
	    state
    FROM
	    mask, donations_appended AS da
    LEFT JOIN view_last_fitted_states AS lfs ON
    	da.user_id = lfs.user_id AND da."month" = lfs."month");
    
    CREATE ROLE admin WITH CREATEDB CREATEROLE LOGIN;
    CREATE ROLE dashboard_agent WITH LOGIN PASSWORD 'password1';
    CREATE ROLE data_provider WITH LOGIN PASSWORD 'password2';
    
    GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO admin;
    GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO admin;
    
    GRANT SELECT ON view_last_fitted_states TO dashboard_agent;
    GRANT SELECT ON view_donations TO dashboard_agent;
    
    GRANT SELECT, INSERT, UPDATE, DELETE ON data_donations TO data_provider;
    GRANT USAGE, SELECT, UPDATE ON sequence data_donations_id_seq TO data_provider;
    
    GRANT SELECT, INSERT, UPDATE, DELETE ON model_latent_states TO data_provider;
    GRANT USAGE, SELECT, UPDATE ON sequence model_latent_states_id_seq TO data_provider;
EOSQL