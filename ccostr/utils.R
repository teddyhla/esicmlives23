### UTILS.R
### FOR APP.R to pull 
### SEVERAL OBJECTS AND FUNCTIONS


## SIDEBAR ACC object
sidebar_acc <- accordion(
  open = c('Select your departure location:'),
  accordion_panel(
    'Select your departure location:', icon = icon("globe"),
    numericInput('loc',
      'Departure Location:',
      min = 1,
      max = 1000,
      step = 1,
      value = 30),
    numericInput('accom',
      'Do you stay in hotel?',
      min = 1,
      max = 1000,
      step = 2,
      value = 50)
  )
)

usgg <- 'Select on the map to input your departure destination.'

