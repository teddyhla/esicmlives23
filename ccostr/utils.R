### UTILS.R
### FOR APP.R to pull 
### SEVERAL OBJECTS AND FUNCTIONS

#10.4 kg co2 per room per night
CONST_HOTEL <- 10.4
#0.03549 kg co2 per unit kilometer per person for rail
CONST_RAIL <- 0.03549
#0.140625 kg co2 per unit kilometres per person for flight
CONST_AIR <- 0.140625
#0.168 kg co2 per unit km for car
CONST_CAR <- 0.168

## SIDEBAR ACC object
#sidebar_acc <- sidebarLayout(
# accordion_panel(
#   'Click on the map to select your departure destination', icon = icon("map"),
#   checkboxInput('return','Is your journey return?', value = TRUE),
#   selectInput('travel_mode',
#     'How did you get here:',
#     c('Train' = 'train','Car' = 'car','Flight' = 'flight')),
#   numericInput('accom',
#     'How many nights are you staying in hotel? Select 0 if not staying in hotel.',
#     min = 0,
#     max = 10,
#     step = 1,
#     value = 5),
#   selectInput('event',
#     'Select the event you are attending:',
#     c('ESICM LIVES Milan' = 'esicm23')
#     ),
#   'Scan this QR code to try it on your device.',
#   img(src = "qrcode_ccostr.png", height = 200, width = 200)
# )
#

mod <- function(df){
  
  df <- df %>%
    mutate(
      conv = case_when(
        mode == 'car' ~ CONST_CAR ,
        mode == 'flight' ~ CONST_AIR ,
        mode == 'train' ~ CONST_RAIL, 
      )
      )
  
  #calculate dist
  dept_coords <- c(df$lng,df$lat)
  dest_coords <- c(df$destlng, df$destlat)
  df$dist <- geosphere::distVincentySphere(dept_coords,dest_coords)/1000
  
  df$totdist <- ifelse(df$return == TRUE,df$dist*2,df$dist*1)
  
  df$ctravel <- df$totdist * df$conv
  
  df$convaccomo <- CONST_HOTEL
  df$caccomo <- df$accom * df$convaccomo
  
  df$total <- df$caccomo + df$ctravel
  
  df
  
}



usgg <- 'Select on the map to input your departure destination.'

