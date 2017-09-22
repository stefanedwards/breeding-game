
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source('global.R')

shinyServer(function(input, output, session) {

  breed <- eventReactive(input$btnGO, {
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(message='Breeding, 15 generations.',
                 detail='Come back in 15-20 years, this stuff takes time!')
    
    load(.data('small_population.Rdata'))
    
    if (input$radioSelectionMethod == 'random') {
      method <- function(pop) {
        
        parents <- sample.int(pop@nInd, size=input$sliderBreedAnimals, replace=FALSE)
        randCross(pop=pop, nCrosses = pop@nInd, balance=TRUE, parents=parents, ignoreGender = FALSE, simParam = SIMPARAM)
      }
    } else {
      method <- function(pop) {
        pop <- setPheno(pop, varE = varG(pop)*2)
        nInd <- ceiling(input$sliderBreedAnimals / 2)
        bulls = selectMale(pop, nInd, use="pheno",simParam=SIMPARAM)
        cows = selectFemale(pop, nInd, use="pheno",simParam=SIMPARAM)
        randCross2(cows, bulls, nCrosses = pop@nInd, balance = TRUE, ignoreGender = FALSE, simParam=SIMPARAM)
      }
    }
    new.pop <- setPheno(seed.pop, varE = varG(seed.pop)*2)
    stat.G <- data.frame(Generation=1:15, meanG=integer(15), varG=integer(15), meanY=integer(15), h2=numeric(15))
    for (i in 1:15) {
      progress$set(value = i)
      new.pop <- method(new.pop)
      new.pop <- setPheno(new.pop, varE = varG(new.pop)*2)
      stat.G[i,2:5] <- c(meanG(new.pop), varG(new.pop), meanP(new.pop), varG(new.pop) / var(new.pop@pheno))
    }
    stat.G
  })
  
  output$plotMain <- renderPlot({
    data <- breed()
    
    data %>% gather(stat, val, -Generation) %>%
      ggplot(aes(x=Generation, y=val, colour=stat)) +
        geom_point() + geom_line() +
        coord_capped_cart(bottom='none', left='none') +
        facet_rep_grid(stat ~ ., switch='y', scales='free_y') + 
        theme_classic() + theme(strip.placement='outside')

  })

})
