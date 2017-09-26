
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source('global.R')

shinyServer(function(input, output, session) {

  #blarg <- data.frame()
  
  ## Main selection generator -----------------
  
  breed <- eventReactive(input$btnGO, {
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    #my.blarg <- data.frame(row=nrow(blarg)+1, animals=input$sliderBreedAnimals, method=input$radioSelectionMethod)
    #blarg <<- bind_rows(blarg, my.blarg)
    
    #str(blarg)
    
    progress$set(message='Breeding, 15 generations.',
                 detail='Come back in 15-20 years, this stuff takes time!')
    
    load(.data('small_population.Rdata'))
    
    if (input$radioSelectionMethod == 'random') {
      method <- function(pop) {
        gender <- split(pop@id, pop@gender)
        randCross2(pop[sample(gender$F, input$sliderDams)], pop[sample(gender$M, input$sliderDams)], 
                   nCrosses = pop@nInd, balance = TRUE, ignoreGender = FALSE, simParam=SIMPARAM)
      }
    } else {
      method <- function(pop) {
        pop <- setPheno(pop, varE = varG(pop)*2)

        bulls = selectMale(pop, input$sliderSires, use="pheno",simParam=SIMPARAM)
        cows = selectFemale(pop, input$sliderDams, use="pheno",simParam=SIMPARAM)
        randCross2(cows, bulls, nCrosses = pop@nInd, balance = TRUE, ignoreGender = FALSE, simParam=SIMPARAM)
      }
    }
    
    results <- list(animals=data.frame(), monies=data.frame())
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
      
      males <- new.pop[new.pop@gender == 'M', ]
      females <- new.pop[new.pop@gender == 'F', ]
      
      monies <- tribble(
        ~Generation, ~item, ~value,
        i, 'keep', males@nInd * costs$bull.cost + females@nInd * costs$cow.cost,
        i, 'selling', sum(males@pheno * costs$bull.sell) + sum(males@pheno * costs$cow.sell)
      )
      
      results$monies <- bind_rows(results$monies, monies)
    }
    results$stat.G <- stat.G
    results
  })
  
  # Visual changes from inputs ----------
  
  observeEvent(input$radioSelectionMethod, {
    #cat('On button!', input$radioSelectionMethod, '\n')
    if (input$radioSelectionMethod == 'ocs') {
      shinyjs::enable('numOCS')
    } else {
      shinyjs::disable('numOCS')
    }
  })
  
  
  ## Update outputs with result from breeding -------------
  
  output$plotMain <- renderPlot({
    data <- breed()
    
    data$stat.G %>% gather(stat, val, -Generation) %>%
      ggplot(aes(x=Generation, y=val, colour=stat)) +
        geom_point() + geom_line() +
        coord_capped_cart(bottom='none', left='none') +
        facet_rep_grid(stat ~ ., switch='y', scales='free_y') + 
        theme_classic() + theme(strip.placement='outside', 
                                axis.title.y=element_blank(),
                                strip.background = element_blank())

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
  
  output$plotMonies <- renderPlot({
    data <- breed()
    saveRDS(data, file='data/breed.rds')
    
    monies <- data$monies
    cummulative <- monies %>% group_by(Generation) %>% summarise(total=sum(value, na.rm=TRUE)) %>%
      arrange(Generation) %>% mutate(cummulative=cumsum(total))
    
    ggplot(monies, aes(x=Generation, y=value)) + 
      geom_col(aes(fill=as.factor(sign(value)))) + 
      scale_fill_manual(values=c('-1'='red', '1'='#4ea815'), guide=FALSE) +
      scale_y_continuous('Profit (£)', labels=scales::comma) +
      geom_line(data=cummulative, aes(y=cummulative), size=1) + 
      geom_point(data=cummulative, aes(y=cummulative), size=3, pch=21, fill='white') +
      labs(y='Profit (£)')
  })

})
