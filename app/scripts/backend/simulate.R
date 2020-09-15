library(seqHMM)
library(dplyr)
library(tidyr)

simulate_cohort <- function(cohort_ord, size) {

tmp <- simulate_hmm(n_sequences = size,
             initial_probs = c(0.2, 0.6, 0.2),
             transition_probs = matrix(c(0.8, 0.1, 0.1, 0.2, 0.4, 0.4, 0, 0.2, 0.8),
                                       nrow = 3, byrow = TRUE),
             emission_probs = matrix(c(0.9, 0.1, 0.5, 0.5, 0.1, 0.9), ncol = 2, byrow = TRUE),
             sequence_length = 21 - cohort_ord)

tmp$observations %>% 
  as_tibble() %>% 
  `colnames<-`(seq.Date(as.Date("2019-01-01"), by = "month", length.out = 20)[cohort_ord:20]) %>% 
  mutate(id = runif(length(`2020-08-01`))) %>% 
  gather(date, emission, -id) %>% 
  filter(emission == "1") %>% 
  mutate(amount = rlnorm(length(emission), 4, 1),
         day = sample(1:28, length(emission), replace = TRUE),
         date = as.Date(date) + lubridate::days(day)) %>% 
  select(id, date, amount)
}

purrr::map2_df(1:19, c(20, sample.int(20, size = 18, replace = TRUE)), simulate_cohort) %>% 
  mutate(id = as.integer(factor(id))) %>% 
  arrange(date) %>% 
  write.csv("../db/flat_data/synthetic2.csv")
  
