
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
          #  numericInput('numOCS', 'Degree of optimal contribution',
          #             min = 0, max=90, value=45),
          selectInput('numSNPs', 'Genotyping density', 
                      choices = structure(names(costs$snp.cost), .Names=sprintf('%s SNPS (%.1f£ per animal)', names(costs$snp.cost), costs$snp.cost))),
          actionButton('btnGO', 'GO!', icon=icon('arrow-circle-right')),
          hr(),
          #verbatimTextOutput('tmptxt'),
          #h5('History of settings'),
          DT::dataTableOutput('tblHistory')
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
      tabsetPanel(id='maintab',
        tabPanel('Cows!', value='cows',
                 plotOutput('plotCows', height='700px')  ## modify height of plot windows here! height="auto" or height="100%" does not work; kills everything
                 ),    
        tabPanel('£££', value='monies',
                 plotOutput('plotMonies', height='700px')
                 ),
        tabPanel('Compare settings', value='summaries',
                 plotOutput('plotSummaries', height='700px')
                 ),
        tabPanel('Diagnostics',  value='diagnostics',
                 plotOutput('plotMain')
                 )   ##
      )
    )
  )
))
