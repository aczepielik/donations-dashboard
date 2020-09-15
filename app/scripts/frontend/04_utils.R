emphasize <- function(text){
  tags$h2(text)
}

perc_change <- function(new, last){
  (new - last)/last
}

formatBY <- function(date){
  stringr::str_replace_all(format(date, "%B %Y"), 
                           c("stycznia" = "Styczeń",
                             "lutego" = "Luty",
                             "marca" = "Marzec",
                             "kwietnia" = "Kwiecień",
                             "maja" = "Maj",
                             "czerwca" = "Czerwiec",
                             "lipca" = "Lipiec",
                             "sierpnia" = "Sierpień",
                             "września" = "Wrzesień",
                             "października" = "Październik",
                             "listopada" = "Listopad",
                             "grudnia" = "Grudzień",
                             "styczeń" = "Styczeń",
                             "luty" = "Luty",
                             "marzec" = "Marzec",
                             "kwiecień" = "Kwiecień",
                             "maj" = "Maj",
                             "czerwiec" = "Czerwiec",
                             "lipiec" = "Lipiec",
                             "sierpień" = "Sierpień",
                             "wrzesień" = "Wrzesień",
                             "październik" = "Październik",
                             "listopad" = "Listopad",
                             "grudzień" = "Grudzień"))
}
