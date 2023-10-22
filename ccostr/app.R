# PURPOSE

# LIBRARIES

library(shiny)
library(leaflet)
library(geosphere)
library(dplyr)
library(googledrive)
library(googlesheets4)
library(ggplot2)

# source
source('utils.R')

#
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)

# UI Elements

sheet_id <- googledrive::drive_get('ccostr')$id

ui <- fluidPage(
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
  titlePanel('Find out your carboncost.'),
  sidebarLayout(
    sidebarPanel(
      h4('1. Click on the map to select your departure destination(default set to London).'),
      selectInput('return','2. Is your journey return?', c('Yes'=TRUE,'No'=FALSE)),
      selectInput('travel_mode','3. How did you travel?', c('Train'='train','Car'='car', 'Flight'='flight')),
      numericInput(
        'accom',
        '4. How many nights are you staying in hotel? (Select 0 if not staying in hotel.)',
        min= 0, max = 14, step = 1, value = 5
      ),
      selectInput('event','6. Select the event you are attending?', c('ESICM Lives @ Milan'='esicm23'))
    ),
    mainPanel(
      tabsetPanel(type = 'tabs',
        tabPanel('Estimations',
          
          leafletOutput('map'),
          br(),
            h3('Based on your input'),
            h4('Your estimated carbon cost is: ',textOutput('ans1',inline=T),'kilograms of CO2 equivalent.'),
            br(),
            h4('Your estimated travel distance is:',textOutput('ans2',inline=T),'kilometres.'),
            br(),
          actionButton("submit", label= "Submit"),
          #tableOutput('t1')
          ),
        tabPanel('Analysis',
          
          h3('Responses:'),
          br(),
          h4(textOutput('resp1',inline = T), 'has responded.'),
          h4('Mean distance',textOutput('resp2',inline= T), ' kilometres.'),
          h4('Mean carboncost',textOutput('resp3',inline=T),'kilograms of CO2 equivalent.'),
          plotOutput('resp4')
          ),
        tabPanel('About & Feedback')
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
        lat = ifelse(is.null(departure$lat),0,departure$lat),
        lng = ifelse(is.null(departure$lng),0,departure$lng),
        destlat = 45.481,
        destlng = 9.155
      )
    })
    
    dfproc <- reactive({
      #here we will call mod function
      mod(df())
    })

    output$ans1 <- renderText({
      val2 <- round(dfproc()$total,2)
    })
    
    output$ans2 <- renderText({
      val1 <- round(dfproc()$dist,2)
    })
    
    output$t1 <- renderTable(dfproc())
    
    output$resp1 <- renderText({
      i1 <- read_sheet(ss=sheet_id,sheet='main')
      nrow(i1)
    })
    output$resp2 <- renderText({
      i1 <- read_sheet(ss=sheet_id,sheet='main')
      mean(i1$dist)
    })
    
    output$resp3 <- renderText({
      i1 <- read_sheet(ss=sheet_id,sheet='main')
      mean(i1$total)
    })
    
    output$resp4 <- renderPlot({
      i1 <- read_sheet(ss=sheet_id,sheet='main')
      p <- hist(i1$dist,main='Histogram of distances in km',xlab = 'Distances(km)')
      p
    })
    
    
    observeEvent(input$submit,{
      wrt <- read_sheet(ss=sheet_id,sheet ='main')
      response_data <- dfproc()
    
      if (nrow(wrt)==0){
        sheet_write(data = response_data,
          ss = sheet_id, sheet = 'main')
      } else {
        sheet_append(data = response_data,
          ss = sheet_id,
          sheet = 'main')
      }
      showModal(modalDialog(
        title = "Thank you!"
        
      ))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
