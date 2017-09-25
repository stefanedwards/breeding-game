
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
    
    results <- list(animals=data.frame())
    new.pop <- setPheno(seed.pop, varE = varG(seed.pop)*2)
    stat.G <- data.frame(Generation=0:15) %>% mutate(meanG=0, varG=NA, meanY=0, h2=NA)
    stat.G[1, 2:5] <- c(meanG(new.pop), varG(new.pop), meanP(new.pop), varG(new.pop) / var(new.pop@pheno))
    for (i in 1:15) {
      progress$set(value = i)
      new.pop <- method(new.pop)
      new.pop <- setPheno(new.pop, varE = varG(new.pop)*2)
      results$animals <- bind_rows(results$animals, data.frame(Generation=i, id=new.pop@id, y=new.pop@pheno, stringsAsFactors = FALSE))
      stat.G[i+1,2:5] <- c(meanG(new.pop), varG(new.pop), meanP(new.pop), varG(new.pop) / var(new.pop@pheno))
      
      if (varG(new.pop) < 0.1) {
        showModal(modalDialog('Your heard, it ded!', footer = modalButton("Dismiss"), fade=TRUE))
        break
      }
    }
    results$stat.G <- stat.G
    results
  })
  
  output$plotMain <- renderPlot({
    data <- breed()
    
    data$stat.G %>% gather(stat, val, -Generation) %>%
      ggplot(aes(x=Generation, y=val, colour=stat)) +
        geom_point() + geom_line() +
        coord_capped_cart(bottom='none', left='none') +
        facet_rep_grid(stat ~ ., switch='y', scales='free_y') + 
        theme_classic() + theme(strip.placement='outside')

  })
  
  output$plotCows <- renderPlot({
    data <- breed()
    
    r <- range(data$animals$y)
    scale.w <- input$cexWidth *  (r[2]-r[1]) / 15
    scale.h <- input$cexHeight * 15 / (r[2]-r[1])
    
    p <- data$animals %>% group_by(Generation) %>% do(shake_and_sample(.$y, .$Generation)) %>%
      ggplot(aes(x=x, y=y, width=y/scale.w, height=y/scale.h)) + geom_tile() +
        coord_cartesian(xlim=c(0,15)) +
        labs(x='Generation', title='Growing cows')
    
    p1 <- replace_rect_cows(p)
    p2 <-     data$stat.G %>% ggplot(aes(x=Generation, y=varG * 100, fill=varG)) +
      geom_bar(stat='identity') +
      scale_fill_continuous(low='#F7FBFF', high='#08306B')  +            # brewer.pal(9, 'Blues')  [1] "#F7FBFF" "#DEEBF7" "#C6DBEF" "#9ECAE1" "#6BAED6" "#4292C6" "#2171B5" "#08519C" "#08306B"
      coord_cartesian(xlim=c(0,15)) +
      guides(fill=FALSE) +
      labs(y='Genes in herd', title='How many genes are there in the herd?')
    p2 <- ggplotGrob(p2)
    
    grid.draw(rbind(p1, p2, size='first'))
  })  

})
