library(pool)
library(dplyr)
library(ggplot2)
library(plotly)
library(knitr)
library(kableExtra)

Sys.setlocale(locale = "pl_PL.UTF-8")

theme_set(theme_minimal())
state_labels <-  c("Nowy", "Uśpiony", "Sporadyczny", "Regularny")
state_colours <- RColorBrewer::brewer.pal(4, "Set1")[c(3, 1, 4, 2)]
names(state_colours) <- state_labels

config <- config::get(config = "dashboard", file = "config/config.yml")

db <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = config$db_name,
  host = config$db_host,
  port = config$db_port,
  user = config$db_user,
  password = config$db_password)

onStop(function() {
  poolClose(db)
})

source("scripts/frontend/01_top_level.R")
source("scripts/frontend/02_constructors.R")
source("scripts/frontend/03_plots.R")
source("scripts/frontend/04_tables.R")
source("scripts/frontend/04_utils.R")