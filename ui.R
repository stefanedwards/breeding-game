
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(title='Breeding cattle with AlphaSimR',

  titlePanel('Breeding cattle'),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel('Select...', 
                 sliderInput('sliderBreedAnimals', 
                 "How many animals do you want to use for breeding?",
                 min=2, max=1000,
                 value=100),
          radioButtons('radioSelectionMethod', 'Use random or truncation selection?',
                       c('Random'='random', 'Trucation selection'='trunc')),
          actionButton('btnGO', 'Calculate', icon=icon('arrow-circle-right'))
        ),
        tabPanel('Settings', 
          helpText('Scalars for making the cows look nice'),
          numericInput('cexWidth','Width divider', 10),
          numericInput('cexHeight', 'Height divider', 10),
          hr()
        )
      )
    ),
  
    # Show result
    mainPanel(
      tabsetPanel(
        tabPanel('Cows!', 
                 plotOutput('plotCows', height='800px')  ## modify height of plot windows here! height="auto" or height="100%" does not work; kills everything
                 ),        
        tabPanel('Diagnostics', plotOutput('plotMain'))   ##
      )
    )
  )
))
