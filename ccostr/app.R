# PURPOSE

# LIBRARIES

library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(leaflet)
#library(geosphere)

# source
source('utils.R')

# UI Elements


ui <- page_navbar(
  fillable_mobile = FALSE,
  title = 'Find out your carboncost!',
  sidebar = sidebar(sidebar_acc),
  #create 3 paels
  nav_panel(title = 'Estimations',
    # within estimation panel
    layout_columns(title = "test",value = 'hello',
      leafletOutput('map')
      ),
    layout_columns(
      value_box(class = 'bg-warning',showcase= icon('a'), title = 'Your Estimated carbon cost:',
        value = verbatimTextOutput('departure$lat')
      ),
      value_box(
        title = "test2",value = 'hello2',
        #need a submit button
        actionButton('send','Submit')
      )
    )
    
    ),
  nav_panel(title = 'User Guide & Assumptions',
    page_fillable(
      card(height = '200px',
          card_header(h6('User Guide')),
          card_body(usgg)
      ),
    card(height = '200px',
        card_header(h6('Assumptions')),
        card_body()
      
    )
    )
    ),
  nav_panel(title = 'About & Feedback',
    page_fillable(
      card(height = '200px',
          card_header(h6('Dev Team')),
          card_body('Adrian Wong, Teddy Hla')
    ),
      card(
        card_header(h6('Feedback')),
        card_body('We are most grateful for your feedback.')
      )
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #create a reactive object to store variables
  values <- reactiveValues(departure = NULL)
  
  #render the map
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = 0, lat = 51, zoom = 2)#center location
    })
    
  # observe for click event and update 
    observe({
      click <- input$map_click
      if(!is.null(click)) {
        values$departure <- click
        leafletProxy('map') %>%
          clearMarkers() %>%
          addMarkers(lng = click$lng, lat = click$lat)
        
      }
    })
    
    # calculate the distance
    output$dist_out <- renderText({
      departure <- values$departure
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
