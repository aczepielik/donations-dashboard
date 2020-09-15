#!/usr/bin/env Rscript

# Load libraries ----------

library(dplyr) #for data transformation
library(tidyr)
library(TraMineR) #for seqHMM prep
library(seqHMM)
library(DBI)

# Setup -------
Sys.setenv(TZ = "UTC")
config <- config::get(file = "config/config.yml", config = "local")

con <- dbConnect(RPostgres::Postgres(),
                 dbname = config$db_name, 
                 host = config$db_host,
                 port = config$db_port,
                 user = config$db_user,
                 password = config$db_password)

# Read data from db --------
donations <- dbGetQuery(conn = con, "SELECT * FROM data_donations")

# Transform ------
donations_summary_long <- 
  donations %>% 
  rowwise() %>% 
  mutate(date = as.Date(trunc.POSIXt(as.POSIXct(date), units = "months"))) %>% 
  ungroup() %>% 
  #filter(date != as.Date('2019-07-01')) %>% 
  group_by(date, user_id) %>% 
  summarize(donation = sum(amount, na.rm = TRUE) > 0)

donations_summary <- 
  donations_summary_long %>% 
  spread(date, donation)

first_donations <- 
  donations_summary_long %>% 
  group_by(user_id) %>% 
  arrange(date) %>% 
  mutate(donation = ifelse(date == first(date), "New", as.character(donation))) %>%
  spread(date, donation)

len <- ncol(donations_summary)
donations_seq <- seqdef(donations_summary, var = 2:len, id = donations_summary[[1]],
                        alphabet = c(TRUE, FALSE), right =  FALSE, gaps = FALSE)

# Initial values --------
state_names <- c("dormant", "occasional", "regular")

# TO DO: read from databaes
init_transitions <- matrix(c(0.8, 0.1, 0.1, 0.1, 0.8, 0.1, 0.1, 0.1, 0.8), nrow = 3, ncol = 3)
init_emissions <- matrix(c(0.1, 0.5, 0.9, 0.9, 0.5, 0.1), nrow = 3, ncol = 2)
init_probs <- c(0.3, 0.1, 0.6)

colnames(init_transitions) <- state_names
rownames(init_transitions) <- state_names
rownames(init_emissions) <- state_names
names(init_probs) <- state_names

# Build and fit model --------
model <- build_hmm(donations_seq,
                   n_states = 3,
                   transition_probs = init_transitions,
                   emission_probs = init_emissions,
                   initial_probs = init_probs)

fit <- fit_model(model)

# Extract hidden states -------
states <- hidden_paths(fit$model) %>% as.data.frame()
states <- sapply(states, as.character) %>% as.data.frame(stringsAsFactors = FALSE)

states[donations_seq == "*"] <- NA
states[first_donations[-1] == "New"] <- "new"

colnames(states) <- colnames(donations_seq)

states$id <- rownames(donations_seq)

# Transform to long format ----
states_long <- 
  states %>% 
  tidyr::gather(month, state, -id) %>% 
  na.omit()

states_long <- 
  states_long %>% 
  rename(user_id = id) %>% 
  mutate(user_id = as.integer(user_id),
         month = as.Date(month)) %>% 
  tibble::add_column(update_timestamp = Sys.time())



# write to db
dbWriteTable(con, name = "model_latent_states", value = states_long, append = TRUE)
dbDisconnect(con)
