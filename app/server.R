library(shinydashboard)

function(input, output, session) {
    dates <- 
        db %>% 
        tbl("view_donations") %>% 
        select(month) %>% 
        distinct() %>% 
        arrange(month) %>% 
        pull(month)
    
    output$menu <- renderMenu({
        sidebarMenu(id = "leftMenu",
                    menuItem("Panel Główny", tabName = "main", selected = TRUE, icon = icon("home")),
                    .list = timeMenuItemsFactory(dates)
        )
    })
    
    reactive_session <- reactive({session})
    
    output$body <- renderUI({
        ifelse(input$leftMenu == "main",
               tagList(
                   list(
                       mainPage(lazy_donations_data = tbl(db, "view_donations"),
                                output = output,
                                session = reactive_session())
                   )
               ),
               tagList(
                   list(
                       #emphasize(input$leftMenu)
                       # mainPage(lazy_data = tbl(db, "view_donations"),
                       #          output = output,
                       #          session = reactive_session())
                       do.call(tagList, monthPage(lazy_donations_data = tbl(db, "view_donations"),
                                                  lazy_states_data = tbl(db, "view_last_fitted_states"),
                                                  month = input$leftMenu,
                                                  output = output,
                                                  session = session))
                   )
               )
        )
    })
}
