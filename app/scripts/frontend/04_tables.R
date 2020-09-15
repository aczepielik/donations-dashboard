rowsPrevMonthSingleState <- function(prev_month_data, curr_month_data, state){
  #just to keep function argument simple and avoid name conflict at the same time
  selected_state <- state
  
  prev_month_filtered <- prev_month_data %>% filter(state == selected_state)
  
  prev_month_filtered_stayed <- 
    prev_month_filtered %>% 
    inner_join(curr_month_data, by = "user_id", suffix = c(".prev", ".curr"))
  
  prev_month_filtered_churned <- prev_month_filtered %>% anti_join(curr_month_data, by = "user_id")
  
  all_prev_donors <- nrow(prev_month_filtered)
  stayed_donors <- nrow(prev_month_filtered_stayed)
  churned_donors <- nrow(prev_month_filtered_churned)
  
  all_prev_sum <- sum(prev_month_filtered$amount, na.rm = TRUE) %>% cell_spec(bold = TRUE)
  
  stayed_change <- 
    sum(prev_month_filtered_stayed$amount.curr, na.rm = TRUE) - 
    sum(prev_month_filtered_stayed$amount.prev, na.rm = TRUE)
  
  stayed_change <- cell_spec(stayed_change, color = case_when(stayed_change > 0 ~ "green",
                                                              stayed_change < 0 ~ "red",
                                                              TRUE ~ "black"))
  
  churned_change <- -sum(prev_month_filtered_churned$amount, na.rm = TRUE)
  
  churned_change <- cell_spec(churned_change, color = case_when(churned_change > 0 ~ "green",
                                                                churned_change < 0 ~ "red",
                                                                TRUE ~ "black"))
  
  tibble(label = c("Było", "Odeszło", "Zostało"),
         donors = c(all_prev_donors, churned_donors, stayed_donors),
         change = c(all_prev_sum, churned_change, stayed_change))
}

rowsCurrMonthSingleState <- function(prev_month_data, curr_month_data, state){
  #just to keep function argument simple and avoid name conflict at the same time
  selected_state <- state
  
  curr_month_filtered <- curr_month_data %>% filter(state == selected_state)
  
  curr_month_filtered_new <- 
    curr_month_filtered %>% 
    anti_join(prev_month_data, by = "user_id", suffix = c(".curr", ".prev"))
  
  n_donors <- n_distinct(curr_month_filtered_new$user_id)
  amount <- sum(curr_month_filtered_new$amount, na.rm = TRUE)
  
  tibble(label = case_when(selected_state == "new" ~ "Nowy",
                           selected_state == "regular" ~ "Aktywny",
                           selected_state == "occasional" ~ "Sporadyczny",
                           selected_state == "dormant" ~ "Uśpiony"),
         donors = n_donors,
         change = cell_spec(amount, color = ifelse(amount > 0, "green", "black")))
  
  
}

tableMonthlyChanges <- function(data, curr_month){
  # curr_month is the first day of the month. We subtract one day,
  # so we get the last day of the previous month. Then we trunc it to the full month
  prev_month <- as.Date(trunc.POSIXt(curr_month - 1, units = "months")) 
  
  curr_month_data <-
    data %>% 
    filter(month == curr_month) %>% 
    group_by(user_id, state) %>% #user can have only one state in month but we want to preserve it
    summarize(amount = sum(amount, na.rm = TRUE)) %>% 
    collect()
  
  prev_month_data <- 
    data %>% 
    filter(month == prev_month) %>% 
    group_by(user_id, state) %>% 
    summarize(amount = sum(amount, na.rm = TRUE)) %>% 
    collect()
  
  from_prev_month <- 
    lapply(c("regular", "occasional", "new", "dormant"),
           rowsPrevMonthSingleState, 
           prev_month_data = prev_month_data,
           curr_month_data = curr_month_data)
  
  prev_month_summary_row <- tibble(label = "Poprzedni miesiąc: razem",
                                   donors = n_distinct(prev_month_data$user_id, na.rm = TRUE),
                                   change = as.character(sum(prev_month_data$amount, na.rm = TRUE)))
  
  from_curr_month <- 
    lapply(c("regular", "occasional", "new", "dormant"),
           rowsCurrMonthSingleState, 
           prev_month_data = prev_month_data,
           curr_month_data = curr_month_data)
  
  curr_month_summary_row <- tibble(label = "Obency miesiąc: razem",
                                   donors = n_distinct(curr_month_data$user_id, na.rm = TRUE),
                                   change = as.character(sum(curr_month_data$amount, na.rm = TRUE)))
  
  bind_rows(prev_month_summary_row,
            from_prev_month,
            from_curr_month,
            curr_month_summary_row) %>% 
    kable(format = "html",
          col.names = c("", "Liczba darczyńców", "Kwota"),
          escape = FALSE,
          align = c("l", "r", "r")) %>% 
    kable_styling(c("striped", "hover", "condensed")) %>% 
    row_spec(c(1, 18), bold = TRUE, background = "#888888", color = "white", hline_after = TRUE) %>% 
    pack_rows("Regularni (poprzedni miesiąc)", 2, 4) %>% 
    pack_rows("Sporadyczni (poprzedni miesiąc)", 5, 7) %>% 
    pack_rows("Nowi (poprzedni miesiąc)", 8, 10) %>% 
    pack_rows("Uśpieni (poprzedni miesiąc)", 11, 13) %>% 
    pack_rows("Wpłacili w tym miesiącu, a w poprzednim nie:", 14, 17)
}
