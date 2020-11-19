
library(shiny)
library(shiny.semantic)
library(leaflet)
library(tidyverse)
library(lubridate)
library(semantic.dashboard)
library(reactable)
library(geosphere)
library(htmltools)
library(rsconnect)


# load module functions
source("Module.R")

ui <- dashboardPage(
    dashboardHeader(color = "blue",title = "SHIPS APPSILON", inverted = TRUE),
    dashboardSidebar(
        size = "thin", color = "teal",
        sidebarMenu(
            menuItem(tabName = "leaflet", "Leaflet", icon = icon("flag")),
            menuItem(tabName = "ships", "Ships", icon = icon("database")),
            tags$div(class="header", checked=NA,
                     tags$p("Github code"),
                     tags$a(href="https://github.com/dalerodr", "Click Here!")
            )
        )
    ),
    dashboardBody(
        tabItems(
            selected = 1,
            tabItem(
                tabName = "leaflet",
                fluidRow(
                    box(
                        title = "SELECT A SHIP TYPE", width = 8, collapse_icon = "minus",
                        dropdown_fieldUI(id = "id_1", label = "dropdown_ship_type", choices = unique(ships$ship_type))
                    ),
                    box(
                        title = "SELECT A SHIP NAME", width = 8, collapse_icon = "minus",
                        dropdown_fieldUI(id = "id_2", label = "dropdown_SHIP_NAME", choices = unique(ships$SHIPNAME))
                    )
                ),
                fluidRow(
                    valueBoxOutput("value_box1"),
                    valueBoxOutput("value_box2"),
                    valueBoxOutput("value_box3")
                ),
                br(),
                leafletOutput("map"),
                br(),
                fluidRow(
                    reactableOutput("data")
                )
            ),
            tabItem(
                tabName = "ships",
                fluidRow(
                    box(
                        title = "SELECT SHIP TYPE AND SHIP NAME TO SEE INFORMATION", width = 11,
                        title_side = "top left", collapsible = FALSE, color = "blue"
                        )
                ),
                fluidRow(
                    reactableOutput("filter_data")
                )
            )
        )
    ), theme = "cyborg"
)


server <- function(input, output, session) {
    
    oceanIcons <- iconList(
        ship = makeIcon("anchor-solid.svg", "anchor-solid.svg", 25, 25)
    )
    
    ship_type <- reactive({
        filter(ships, ship_type == input$dropdown_ship_type)
    })
    observeEvent(ship_type(), {
        choices <- sort(unique(ship_type()$SHIPNAME))
        update_dropdown_input(session, "dropdown_SHIP_NAME", choices = choices) 
    })
    
    output$map <- renderLeaflet({
        req(input$dropdown_SHIP_NAME)

        plot <- ship_type() %>%
                    filter(SHIPNAME == input$dropdown_SHIP_NAME)
        
        plot <- plot[order(plot$DATETIME, decreasing = TRUE),]

        distance_matrix <- plot %>% 
                                select(LON, LAT) %>%
                                as.data.frame()
        
        ### DISTANCE BETWEEN TWO POINTS ###
        unique_coordinates <- distance_matrix %>% unique()
        distance_matrix_2 <- distm(unique_coordinates)
        max_distance_no_consecutive <- round(max(distance_matrix_2), 2)
        
        
        ### DISTANCE BETWEEN TWO CONSECUTIVE POINTS ###
        distance <- vector()

        for (x in 1:(nrow(distance_matrix)-1)) {
            distance[x] <- distm(c(distance_matrix[x,1],distance_matrix[x,2]),c(distance_matrix[x+1,1],distance_matrix[x+1,2]))
        }

        meters <- round(max(distance), 2)
        total_distance <- round((sum(distance)/1000), 2)

        output$value_box1 <- renderValueBox({
            valueBox(
                value = paste(meters, "m"),
                subtitle = "Longest distance between two consecutive observations",
                icon = icon("ship"),
                color = "olive",
                size = "tiny",
                width = 10)
        })

        output$value_box2 <- renderValueBox({
            valueBox(
                value = paste(total_distance, "km"),
                subtitle = "Total distance traveled",
                icon = icon("bookmark"),
                color = "green",
                size = "tiny",
                width = 10)
        })
        
        output$value_box3 <- renderValueBox({
            valueBox(
                value = paste(max_distance_no_consecutive, "m"),
                subtitle = "Max distance between no consecutive observations",
                icon = icon("globe"),
                color = "green",
                size = "tiny",
                width = 10)
        })

        coordinates <- plot[c(which.max(distance),(which.max(distance))+1),] %>% 
                            mutate(type = factor("ship"))
        
        output$data <- renderReactable({
            reactable(coordinates, outlined = TRUE, striped = TRUE, height = 200, width = 1000,
                      columns = list(DESTINATION = colDef(minWidth = 150),
                                     SHIPNAME = colDef(minWidth = 150),
                                     DATETIME = colDef(minWidth = 160),
                                     FLAG = colDef(maxWidth = 60),
                                     SPEED = colDef(maxWidth = 70),
                                     COURSE = colDef(maxWidth = 70)
                                     ),
                      resizable = TRUE, bordered = TRUE,
                      fullWidth = FALSE,
                      selection = "multiple",
                      onClick = "select",
                      rowStyle = list(cursor = "pointer"),
                      theme =  reactableTheme ( 
                          borderColor =  "# dfe2e5" ,
                          stripedColor =  "# f6f8fa" ,
                          highlightColor =  "# f0f5f9" ,
                          cellPadding =  "8px 12px" ,
                          style =  list ( fontFamily =  "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif" ) ,
                          searchInputStyle = list( ancho =  "100%" ))
            )
        })

        map2 <- leaflet(coordinates, width = 100, height = 500, padding = 10,
                        options = leafletOptions(minZoom = 0, maxZoom = 18)) %>%
                    addTiles() %>%  # Add default OpenStreetMap map tiles
                    addPolylines(lng = ~LON, lat = ~LAT) %>% 
                    addMarkers(icon = ~oceanIcons[type],
                               lng=coordinates$LON, lat=coordinates$LAT,
                               label=~htmlEscape(paste("LAT: ",coordinates$LAT, ",","LON: ",coordinates$LON, "TIME: ", coordinates$DATETIME)),
                               labelOptions = labelOptions(noHide = FALSE, direction = "bottom", textsize = "15px",
                                                           style = list(
                                                               "color" = "black",
                                                               "font-family" = "serif",
                                                               "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                               "font-size" = "12px",
                                                               "border-color" = "rgba(0,0,0,0.5)"
                                                           )))
    })
    
    output$filter_data <- renderReactable({
        req(input$dropdown_SHIP_NAME)

        information <- ship_type() %>%
            filter(SHIPNAME == input$dropdown_SHIP_NAME)
        
        pre_data <- unique(information[, c("date", "port")]) %>% arrange("date")
        reactable(pre_data, columns = list(port = colDef(minWidth = 150)), 
            details = function(index) {
                data <- filter(information, information$date == pre_data$date[index])
                htmltools::div(style = "padding: 16px",
                reactable(arrange(data, "DATETIME"), outlined = TRUE, striped = TRUE, height = 500, width = 1000,
                          columns = list(DESTINATION = colDef(minWidth = 150),
                                         SHIPNAME = colDef(minWidth = 150),
                                         DATETIME = colDef(minWidth = 160),
                                         FLAG = colDef(maxWidth = 60),
                                         SPEED = colDef(maxWidth = 60),
                                         COURSE = colDef(maxWidth = 60)),
                          resizable = TRUE, bordered = TRUE,
                          fullWidth = FALSE, defaultPageSize = 15,
                          searchable = TRUE
                )
                )
        })
    })
    
    
}

shinyApp(ui = ui, server = server)

