
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(title='Breeding cattle with AlphaSimR',
  useShinyjs(),
  titlePanel('Breeding cattle, select per generation'),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel('Selection', 
                 sliderInput('sliderSires', 
                   "How many sires do you want to use for breeding?",
                   min=2, max=100, 
                   value=10
                 ),
                 sliderInput('sliderDams', 
                   "How many dams do you want to use for breeding?",
                   min=2, max=100, 
                   value=50
                 ),
          radioButtons('radioSelectionMethod', 'How do you want to select who to breed?',
                       c("By random ('Random selection')"='random', 
                         "Parent average"='pa',
                         "Pedigree based selection"="pebv",
                         "Genomic selection"="gs")),
          selectInput('numSNPs', 'Genotyping density', 
                      choices = structure(names(costs$snp.cost), .Names=sprintf('%s SNPS (%.1f£ per animal)', names(costs$snp.cost), costs$snp.cost))),
          numericInput('numGenerations', 'How many generations do you want to do this?', value=1, min=1, max=15, step=1),
          actionButton('btnGO', 'GO!', icon=icon('arrow-circle-right')),
          hr(),
          DT::dataTableOutput('tblHistory')
        ),
        tabPanel('Settings', 
          helpText('Scalars for making the cows look nice'),
          numericInput('cexWidth','Width factor', 0.01, step=0.03, min=0),
          numericInput('cexHeight', 'Height factor', 0.02, step=0.03, min=0),
          radioButtons('viewCows', 'View cow plot or just scatter plot:',
                       c('Cows!'='cows',
                        'Scatter plot'='scatter')),
          hr()
        )
      )
    ),
  
    # Show result
    mainPanel(
      tabsetPanel(id='maintab',
        tabPanel('Cows!', value='cows',
                 plotOutput('plotCows', height='700px')  ## modify height of plot windows here! height="auto" or height="100%" does not work; kills everything
                 ),    
        tabPanel('£££', value='monies',
                 plotOutput('plotMonies', height='700px')
                 ),
        tabPanel('Diagnostics',  value='diagnostics',
                 plotOutput('plotMain')
                 )   ##
      )
    )
  )
))
