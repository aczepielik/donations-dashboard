monthMenuSubItemFactory <- function(year, month){
  months <- c("Styczeń", "Luty", "Marzec",
              "Kwiecień", "Maj", "Czerwiec",
              "Lipiec", "Sierpień", "Wrzesień",
              "Październik", "Listopad", "Grudzień")
  monthString <- ifelse(month < 10, paste0("0", month), as.character(month))
  
  menuSubItem(months[month], tabName = paste(year, monthString, sep = "-"))
}

yearMenuItemFactory <- function(year, months){
  menuItem(year, do.call(tagList, lapply(months, function(m) monthMenuSubItemFactory(year, m))))
}

timeMenuItemsFactory <- function(dates){
  datesFrame <- data.frame(year = lubridate::year(dates), month = lubridate::month(dates))
  years <- unique(datesFrame$year)
  
  lapply(years, function(y) { 
    yearMenuItemFactory(y, unique(datesFrame[datesFrame$year == y, ]$month))
  }) 
}

mainPageData <- function(data){
  
  months <- data %>% select(month) %>% distinct() %>% arrange(month) %>% pull(month)
  last_month <- nth(months, -1)
  prev_month <- nth(months, -2)
  
  donors_data_summary <-
    data %>% 
    filter(month >= prev_month) %>% 
    group_by(month) %>% 
    summarise(money = sum(amount, na.rm = TRUE),
              donors = n_distinct(user_id)) %>% 
    arrange(month) %>% 
    collect()
  
  states_data_summary <- 
    data %>% 
    filter(month >= prev_month & state == "new") %>% 
    group_by(month) %>% 
    summarize(new_donors = n()) %>% 
    arrange(month) %>% 
    collect() %>% 
    mutate(new_donors = as.integer(new_donors))
  
  list(values = list(donations = donors_data_summary$money[2],
                     donationsChange = perc_change(donors_data_summary$money[2], donors_data_summary$money[1]),
                     donors = donors_data_summary$donors[2],
                     donorsChange = perc_change(donors_data_summary$donors[2], donors_data_summary$donors[1]),
                     newDonors = states_data_summary$new_donors[2],
                     newDonorsChange = states_data_summary$new_donors[2] - states_data_summary$new_donors[1]))
}

monthPageData <- function(data, curr_month){
  prev_month <- as.Date(trunc.POSIXt(curr_month - 1, units = "months"))
  
  current_month_data <- data %>% filter(month == curr_month) %>% collect()
  prev_month_data <- data %>% filter(month == prev_month) %>% collect()
  
  curr_donations <-  sum(current_month_data$amount, na.rm = TRUE)
  curr_donors <-  n_distinct(current_month_data$user_id, na.rm = TRUE)
  
  prev_donations <-  sum(prev_month_data$amount, na.rm = TRUE)
  prev_donors <-  n_distinct(prev_month_data$user_id, na.rm = TRUE)
  
  list(donations = curr_donations,
       donations_increment = curr_donations - prev_donations,
       donations_percent_increment = (curr_donations - prev_donations)/prev_donations,
       donors = curr_donors,
       donors_increment = curr_donors - prev_donors,
       donors_percent_increment = (curr_donors - prev_donors)/prev_donors)
}
