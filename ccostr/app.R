# PURPOSE

# LIBRARIES

library(shiny)
library(bslib)
library(leaflet)
library(geosphere)
library(dplyr)

# source
source('utils.R')

# UI Elements


ui <- page_navbar(
  fillable_mobile = FALSE,
  tags$script(HTML("
    function updateUserTime() {
      var currentTime = new Date();
      var year = currentTime.getFullYear();
      var month = currentTime.getMonth() + 1;
      var day = currentTime.getDate();
      var hours = currentTime.getHours();
      var minutes = currentTime.getMinutes();
      var seconds = currentTime.getSeconds();
      var timeZone = Intl.DateTimeFormat().resolvedOptions().timeZone;
      
      var formattedTime = year + '-' + month + '-' + day + ' ' + hours + ':' + minutes + ':' + seconds + ' ' + timeZone;
      Shiny.setInputValue('user_time', formattedTime);
    }

    // Update the time every second
    setInterval(updateUserTime, 1000);
    
    // Initial update
    updateUserTime();
  ")),
  title = 'Find out your carboncost!',
  sidebar = sidebar(sidebar_acc),
  #create 3 paels
  nav_panel(title = 'Estimations',
    # within estimation panel
    layout_columns(title = "test",value = 'hello',
      leafletOutput('map')
      ),
    tableOutput('t1'),
    layout_columns(
      value_box(title = 'Your Estimated carbon cost:',
        value = textOutput('ans')
      ),
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
        setView(lng = 9.1 , lat = 45.5, zoom = 6)#center location
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
    
    b <- reactive({input$return})
    c <- reactive({input$travel_mode})
    d <- reactive({input$accom})
    f <- reactive({input$event})
  
    df <- reactive({
      departure <- values$departure
      data.frame(
        t = input$user_time,
        return = b(),
        mode = c(),
        accom = d(),
        event = f(),
        lat = departure$lat,
        lng = departure$lng,
        destlat = 45.481,
        destlng = 9.155
      )
    })
    
    dfproc <- reactive({
      #here we will call mod function
      mod(df())
    })

    output$ans <- renderText({
      val1 <- round(dfproc()$dist,2)
      val2 <- round(dfproc()$total,2)
      paste0("Distance travelled(km):", val1, "\n\n Est. Total Carbon Cost(co2 equiv):", val2)
    })
    
    output$t1 <- renderTable(df())
}

# Run the application 
shinyApp(ui = ui, server = server)
