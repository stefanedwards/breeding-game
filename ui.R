
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(title='Breeding cattle with AlphaSimR',
  useShinyjs(),
  titlePanel('Breeding cattle'),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel('Selection', 
                 sliderInput('sliderSires', 
                   "How many sires do you want to use for breeding?",
                   min=2, max=400, 
                   value=20
                 ),
                 sliderInput('sliderDams', 
                   "How many dams do you want to use for breeding?",
                   min=2, max=400, 
                   value=100
                 ),
          radioButtons('radioSelectionMethod', 'How do you want to select who to breed?',
                       c("By random ('Random selection')"='random', 
                         "Best performance ('Truncation selection')"='trunc',
                         "Genomic selection"="gs",
                         "Optimal contribution selection"="ocs")),
          numericInput('numOCS', 'Degree of optimal contribution',
                       min = 0, max=90, value=45),
          actionButton('btnGO', 'Calculate', icon=icon('arrow-circle-right'))
        ),
        tabPanel('Settings', 
          helpText('Scalars for making the cows look nice'),
          numericInput('cexWidth','Width divider', 15),
          numericInput('cexHeight', 'Height divider', 6),
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
        tabPanel('£££',
                 plotOutput('plotMonies', height='800px')
                 ),
        tabPanel('Diagnostics', plotOutput('plotMain'))   ##
      )
    )
  )
))
