FROM rocker/r-ver:3.6.1 as builder

RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.1 \
    libpq-dev \
    libxml2-dev \
    locales \
    locales-all

RUN R -e "install.packages(c('shiny', 'shinydashboard', 'RPostgres', 'DBI', 'pool', 'ggplot2', 'plotly', 'knitr', 'kableExtra', 'dplyr', 'tidyr', 'TraMineR', 'seqHMM', 'config', 'glue', 'lubridate'), repos='https://mran.microsoft.com/snapshot/2019-12-08/')"  
 
FROM builder as builder2
RUN mkdir /root/app
COPY . /root/app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/app', port = 3838, host = '0.0.0.0')"]