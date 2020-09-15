makeDynamic <- function(static_plot, session, name, height = 240){
  width_def_name <- paste("output", name, "width", sep = "_")
  width <- session$clientData[[width_def_name]]
  
  ggplotly(static_plot, tooltip = "text", height = height, width = width) %>% 
    config(displaylogo = FALSE, displayModeBar = FALSE) %>% 
    layout(
      margin = list(l = 0, r = 0, b = 0, t = 0, pad = 5),
      legend = list(orientation = "h", x = 0.4, y = -0.2),
      font = list(size = 13, family = "Source Sans Pro"))
}

makeDynamic2 <- function(static_plot, session, name, height = 270){
  width_def_name <- paste("output", name, "width", sep = "_")
  width <- session$clientData[[width_def_name]]
  
  ggplotly(tooltip = "text", height = height, width = width) %>% 
    config(displaylogo = FALSE, displayModeBar = FALSE) %>% 
    layout(
      margin = list(l = 0, r = 0, b = 0, pad = 0),
      font = list(size = 12, family = "Source Sans Pro"))
}



plotDonationsGlobal <- function(data, session, name){
  static_plot <- 
    data %>% 
    #tbl("view_donations") %>% 
    group_by(month) %>% 
    summarize(donations = sum(amount, na.rm = TRUE)) %>% 
    collect() %>% 
    mutate(text = as.character(glue::glue("{formatBY(month)} \n Suma wpłat: {donations} zł"))) %>% 
    ggplot(aes(month, donations, text = text, group = 1)) + 
    geom_line(size = 0.9) + geom_point(size = 1.7) + 
    scale_y_continuous(limits = c(0, NA), breaks = function(ylim) pretty(ylim)) +
    theme(axis.title = element_blank())
  
  makeDynamic(static_plot, session, name)
}

plotDonorsGlobal <- function(data, session, name){
  static_plot <- 
    data %>% 
    #tbl("view_donations") %>% 
    group_by(month) %>% 
    summarize(donors = n_distinct(user_id)) %>% 
    collect() %>% 
    mutate(donors = as.integer(donors)) %>% 
    mutate(text = as.character(glue::glue("{formatBY(month)} \n Liczba darczyńców: {donors}"))) %>%
    ggplot(aes(month, donors, text = text, group = 1)) + 
    geom_line(size = 0.9) + geom_point(size = 1.7) + 
    scale_y_continuous(limits = c(0, NA), breaks = function(ylim) pretty(ylim)) +
    theme(axis.title = element_blank())
  
  makeDynamic(static_plot, session, name)
}

plotDonationsGlobalByState <- function(data, session, name){
  static_plot <- 
    data %>% 
    filter(state != "dormant") %>% 
    group_by(month, state) %>% 
    summarize(donations = sum(amount, na.rm = TRUE)) %>% 
    collect() %>%
    ungroup() %>% 
    mutate(state = factor(state,
                          levels = c("new", "dormant", "occasional", "regular"),
                          labels = state_labels)) %>% 
    mutate(text = as.character(glue::glue("{formatBY(month)} \n Suma wpłat: {donations} zł"))) %>%
    ggplot(aes(month, donations, color = state, text = text, group = 1)) + 
    geom_line(size = 0.9) + geom_point(size = 1.7) + 
    scale_y_continuous(limits = c(0, NA), breaks = function(ylim) pretty(ylim)) +
    scale_colour_manual(values = state_colours, name = "Profil") +
    theme(axis.title = element_blank(), legend.position = "bottom")
  
  makeDynamic(static_plot, session, name)
}

plotDonorsGlobalByState <- function(data, session, name){
  static_plot <- 
    data %>% 
    filter(state != "dormant") %>% 
    group_by(month, state) %>% 
    summarize(donors = n_distinct(user_id)) %>% 
    collect() %>% 
    mutate(donors = as.integer(donors)) %>% 
    mutate(state = factor(state,
                          levels = c("new", "dormant", "occasional", "regular"),
                          labels = state_labels)) %>% 
    mutate(text = as.character(glue::glue("{formatBY(month)} \n Liczba darczyńców: {donors}"))) %>%
    ggplot(aes(month, donors, color = state, text = text, group = 1)) + 
    geom_line(size = 0.9) + geom_point(size = 1.7) + 
    scale_y_continuous(limits = c(0, NA), breaks = function(ylim) pretty(ylim)) +
    scale_colour_manual(values = state_colours) +
    theme(axis.title = element_blank(), legend.position = "none")
  
  makeDynamic(static_plot, session, name)
}

plotStateTimeline <- function(data, session, name, height = 250) {
  
  plot_data <- 
    data %>%
    group_by(date) %>% 
    summarize(donations = n()) %>% 
    ungroup() %>% 
    collect()
  
  if (nrow(plot_data) == 0) {
    static_plot <- ggplot() + labs(title = "Brak darowizn w tej grupie, w tym miesiącu")
  } else {
    
    static_plot <- 
      plot_data %>% 
      mutate(donations = as.integer(donations)) %>% 
      mutate(text = as.character(glue::glue("{date} \n Liczba darowizn: {donations}"))) %>% 
      ggplot(aes(date, donations, text = text)) + 
      geom_col() +
      labs(title = "Liczba darowizn") +
      theme(axis.title = element_blank())
  }
  
  static_plot %>% makeDynamic2(session, name, height)
}

plotAmountHistograms <- function(lazy_data, threshold = 100, session, name, height = 250){
  
  plot_data <- 
    lazy_data %>%
    collect() %>% 
    mutate(above_threshold = (.data$amount > threshold))
  
  if (nrow(plot_data) == 0) {
    static_plot <- ggplot()
  } else {
    
    if(nrow(plot_data %>% filter(!above_threshold)) > 0){
      histogram_smaller_amounts <- 
        plot_data %>% 
        filter(!above_threshold) %>% 
        pull(amount) %>% 
        hist(plot = FALSE)
      
      histogram_smaller_amounts_tibble <- 
        tibble(x = histogram_smaller_amounts$mids,
               y = histogram_smaller_amounts$counts,
               left = head(histogram_smaller_amounts$breaks, -1),
               right = tail(histogram_smaller_amounts$breaks, -1),
               text = as.character(glue::glue("Przedział: ({left} zł, {right} zł] \n Liczba darowizn: {y}")),
               width = (right - left)*0.9) %>% 
        tibble::add_column(bin = as.character(glue::glue("Darowizny poniżej {threshold} zł")))
    } else {
      histogram_smaller_amounts_tibble <- 
        tibble(x = 0,
               y = 0,
               left = 0,
               right = 0,
               text = "",
               width = 0) %>% 
        tibble::add_column(bin = as.character(glue::glue("Darowizny poniżej {threshold} zł")))
    }
    
    if(nrow(plot_data %>% filter(above_threshold)) > 0){
      histogram_bigger_amounts <- 
        plot_data %>% 
        filter(above_threshold) %>% 
        pull(amount) %>% 
        hist(plot = FALSE)
      
      histogram_bigger_amounts_tibble <- 
        tibble(x = histogram_bigger_amounts$mids,
               y = histogram_bigger_amounts$counts,
               left = head(histogram_bigger_amounts$breaks, -1),
               right = tail(histogram_bigger_amounts$breaks, -1),
               text = as.character(glue::glue("Przedział: ({left} zł, {right} zł] \n Liczba darowizn: {y}")),
               width = (right - left)*0.9) %>% 
        tibble::add_column(bin = as.character(glue::glue("Darowizny powyżej {threshold} zł")))
    } else {
      histogram_bigger_amounts_tibble <- 
        tibble(x = 0,
               y = 0,
               left = 0,
               right = 0,
               text = "",
               width = 0) %>% 
        tibble::add_column(bin = as.character(glue::glue("Darowizny powyżej {threshold} zł")))
    }
    
    histogram_tibble <- bind_rows(histogram_smaller_amounts_tibble, histogram_bigger_amounts_tibble)
    
    static_plot <- 
      ggplot(histogram_tibble, aes(x, y, text = text, width = width)) + 
      geom_col() + 
      facet_wrap(~bin, scales = "free_x") +
      labs(x = "Kwota", title = "Wysokość Darowizn") +
      scale_y_continuous(labels = scales::comma) +
      theme(strip.background = element_rect(fill = "grey30"),
            strip.text = element_text(family = "Source Sans Pro", face = "bold", size = 9, color = "white"),
            axis.title.y = element_blank())
  }
  
  makeDynamic2(static_plot, session, name, height)
}

plotGroupChanges <- function(lazy_states_data, curr_month){
  state_labels <- c("Regularny" = "regular",
                    "Sporadyczny" = "occasional",
                    "Uśpiony" = "dormant",
                    "Nowy" = "new")
  
  state_colours <- RColorBrewer::brewer.pal(4, "Set1")[c(2, 4, 1, 3)]
  
  # curr_month is the first day of the month. We subtract one day,
  # so we get the last day of the previous month. Then we trunc it to the full month
  prev_month <- as.Date(trunc.POSIXt(curr_month - 1, units = "months")) 
  
  curr_month_data <-
    lazy_states_data %>% 
    filter(month == curr_month) %>% 
    select(user_id, state) %>% 
    collect()
  
  prev_month_data <- 
    lazy_states_data %>% 
    filter(month == prev_month) %>% 
    select(user_id, state) %>% 
    collect()
  
  plot_data <- 
    left_join(prev_month_data, curr_month_data, by = "user_id", suffix = c(".prev", ".curr")) %>% 
    filter(!(state.prev == "dormant" & state.curr == "dormant")) %>% 
    group_by(state.prev, state.curr) %>% 
    count()
  
  plot_ly(type = "sankey", orientation = "h",
          node = list(
            label = rep(names(state_labels), 2),
            color = rep(state_colours, 2),
            pad = 10,
            thickness = 40),
          link = list(
            source = match(plot_data$state.prev, state_labels) -1,
            target = match(plot_data$state.curr, state_labels) -1 + length(state_labels),
            value = plot_data$n
          )) %>% 
    config(displaylogo = FALSE, displayModeBar = FALSE) %>% 
    layout(
      margin = list(
        l = 0,
        r = 0,
        b = 5,
        t = 5,
        pad = 30
      ),
      font = list(size = 14, family = "Source Sans Pro"))
}


