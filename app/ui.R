library(shinydashboard)

dashboardPage(
  title = "Darowizny",
  skin = "black",
  dashboardHeader(title = "Darowizny"),
  dashboardSidebar(sidebarMenuOutput("menu")),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    uiOutput("body"))
)
