FROM postgres:12

COPY flat_data/synthetic2.csv /tmp/

COPY 01_init.sh /docker-entrypoint-initdb.d/
COPY 02_insert_data.sql /docker-entrypoint-initdb.d/