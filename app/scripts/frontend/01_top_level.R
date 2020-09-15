mainPage <- function(lazy_donations_data, output, session){
  
  output$plotMainTopLeft <- 
    renderPlotly(plotDonationsGlobal(lazy_donations_data, session, name = "plotMainTopLeft"))
  
  output$plotMainBottomLeft <-
    renderPlotly(plotDonorsGlobal(lazy_donations_data, session, name = "plotMainBottomLeft"))
  
  output$plotMainTopRight <- 
    renderPlotly(plotDonationsGlobalByState(lazy_donations_data, session, name = "plotMainTopRight"))
  
  output$plotMainBottomRight <-
    renderPlotly(plotDonorsGlobalByState(lazy_donations_data, session, name = "plotMainBottomRight"))
  
  plotHeight <- "240px"
  
  info_data <- mainPageData(lazy_donations_data)
  
  list(
    fluidRow(
      column(width = 2,
             valueBox(paste(format(info_data$values$donations, big.mark = " "), "zł"),
                      "Darowizny w ostatnim miesiącu",
                      color = "navy",
                      width = NULL),
             valueBox(scales::percent(info_data$values$donationsChange),
                      "Zmiana od ostatniego miesiąca",
                      color = ifelse(info_data$values$donationsChange >= 0,
                                     "green", "red"),
                      width = NULL),
             valueBox(info_data$values$donors,
                      "Darczyńcy w ostatnim miesiącu",
                      #icon = "donate",
                      color = "navy",
                      width = NULL),
             valueBox(scales::percent(info_data$values$donorsChange),
                      "Zmiana od ostatniego miesiąca",
                      #icon = "balance-scale",
                      color = ifelse(info_data$values$donorsChange >= 0,
                                     "green", "red"),
                      width = NULL),
             valueBox(info_data$values$newDonors,
                      "Nowi Darczyńcy w ostatnim miesiącu",
                      #icon = "donate",
                      color = "navy",
                      width = NULL),
             valueBox(info_data$values$newDonorsChange,
                      "Zmiana od ostatniego miesiąca",
                      #icon = "balance-scale",
                      color = ifelse(info_data$values$newDonorsChange >= 0,
                                     "green", "red"),
                      width = NULL)
      ),
      column(width = 5,
             box(title = "Wysokość darowizn", width = NULL,
                 plotlyOutput('plotMainTopLeft', height = plotHeight)),
             box(title = "Liczba darczyńców", width = NULL,
                 plotlyOutput('plotMainBottomLeft', height = plotHeight))
      ),
      column(width = 5,
             box(title = "Wysokość darowizn według grup", width = NULL,
                 plotlyOutput('plotMainTopRight', height = plotHeight)),
             box(title = "Liczba darczyńców według grup", width = NULL,
                 plotlyOutput('plotMainBottomRight', height = plotHeight))
             #renderPlotly(plotDonorsGlobalByState(lazy_donations_data, session, name = "plotMainBottomRight")))
      )
    )
  )
}

monthPage <- function(lazy_donations_data, lazy_states_data, month, output, session){
  month_ <- month <- as.Date(paste(month, "01", sep = "-"))
  month_data <- monthPageData(lazy_donations_data, month)
  
  # left column output elements ----
  output$monthlyChanges <- function() {tableMonthlyChanges(lazy_donations_data, month)}
  
  # central column output elemnts -----
  output$monthlyStateTransitions <-
    renderPlotly(plotGroupChanges(lazy_states_data, month))
  
  # right column output elements ----
  
  ## Tab: All ----
  output$allTimeline <- 
    renderPlotly(lazy_donations_data %>% filter(month == month_) %>% 
                   plotStateTimeline(session, "allTimeline", height = 275))
  output$allHistogram <- 
    renderPlotly(lazy_donations_data %>% filter(month == month_) %>% 
                   plotAmountHistograms(threshold = 100, session, "allHistogram", height = 300))
  
  ## Tab: New ----
  output$newTimeline <- 
    renderPlotly(lazy_donations_data %>% filter(month == month_ & state == "new") %>% 
                   plotStateTimeline(session, "newTimeline", height = 275))
  output$newHistogram <- 
    renderPlotly(lazy_donations_data %>% filter(month == month_ & state == "new") %>% 
                   plotAmountHistograms(threshold = 100, session, "newHistogram", height = 300))
  
  ## Tab: Regular ----
  output$regularTimeline <- 
    renderPlotly(lazy_donations_data %>% filter(month == month_ & state == "regular") %>% 
                   plotStateTimeline(session, "regularTimeline", height = 275))
  output$regularHistogram <- 
    renderPlotly(lazy_donations_data %>% filter(month == month_ & state == "regular") %>% 
                   plotAmountHistograms(threshold = 100, session, "regularHistogram", height = 300))
  
  ## Tab: occasional ----
  output$occasionalTimeline <- 
    renderPlotly(lazy_donations_data %>% filter(month == month_ & state == "occasional") %>% 
                   plotStateTimeline(session, "occasionalTimeline", height = 275))
  output$occasionalHistogram <- 
    renderPlotly(lazy_donations_data %>% filter(month == month_ & state == "occasional") %>% 
                   plotAmountHistograms(threshold = 100, session, "occasionalHistogram", height = 300))
  
  # UI building ----
  
  list(
    fluidRow(
      column(width = 4,
             box(title = "Różnice w wysokości wpłat",
                 tableOutput("monthlyChanges"),
                 width = NULL)
      ),
      column(width = 4,
             div(
               class = "center-column-box-container",
               valueBox(formatBY(month),
                        "",
                        color = "navy",
                        width = NULL),
               valueBox(glue::glue(
                 "Suma darowizn: {format(month_data$donations, big.mark = ' ', scientific = FALSE)} zł"),
                 glue::glue("Zmiana od poprzedniego miesiąca: \
                        {format(month_data$donations_increment, big.mark = ' ', scientific = FALSE)} \
                        ({scales::percent(month_data$donations_percent_increment)})"),
                 color = ifelse(month_data$donations_increment >= 0, "green", "red"),
                 width = NULL),
               valueBox(glue::glue(
                 "Liczba darczyńców: {format(month_data$donors, big.mark = ' ', scientific = FALSE)}"),
                 glue::glue("Zmiana od poprzedniego miesiąca: \
                        {month_data$donors_increment} \
                        ({scales::percent(month_data$donors_percent_increment)})"),
                 color = ifelse(month_data$donors_increment >= 0, "green", "red"),
                 width = NULL))
             ,
             box(title = "Zmiany grup od poprzedniego miesiąca", 
                 plotlyOutput("monthlyStateTransitions", height = "355px"),
                 width = NULL)
      ),
      column(width = 4,
             tabBox(
               id = "timeline-and-histogram", width = NULL,
               tabPanel("Razem",
                        plotlyOutput("allTimeline", height = "275px"),
                        plotlyOutput("allHistogram", height = "300px")),
               tabPanel("Nowi",
                        plotlyOutput("newTimeline", height = "275px"),
                        plotlyOutput("newHistogram", height = "300px")),
               tabPanel("Sporadyczni",
                        plotlyOutput("occasionalTimeline", height = "275px"),
                        plotlyOutput("occasionalHistogram", height = "300px")),
               tabPanel("Regularni",
                        plotlyOutput("regularTimeline", height = "275px"),
                        plotlyOutput("regularHistogram", height = "300px"))
             )
      )
    )
  )
}

