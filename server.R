
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source('global.R')

shinyServer(function(input, output, session) {

  
  ## Private variables, specific to the session (apparently) -----
  .saved_results <- list()
  .saved_settings <- data.frame()
  .saved_summaries_monies <- data.frame()
  .saved_summaries_animals <- data.frame()
  
  ## Main selection generator -----------------
  simulate <- function() {
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(message='Breeding, 15 generations.',
                 detail='Come back in 15-20 years, this stuff takes time!')
    
    load(.data('small_population.Rdata'))
    
    if (input$radioSelectionMethod == 'random') {
      method <- function(pop) {
        gender <- split(pop@id, pop@gender)
        randCross2(pop[sample(gender$F, input$sliderDams)], pop[sample(gender$M, input$sliderDams)], 
                   nCrosses = pop@nInd, balance = TRUE, ignoreGender = FALSE, simParam=SIMPARAM)
      }
    } else if (input$radioSelectionMethod == 'trunc') {
      method <- function(pop) {
        #pop <- setPheno(pop, varE = varG(pop)*2,simParam=SIMPARAM)
  
        bulls = selectMale(pop, input$sliderSires, use="pheno",simParam=SIMPARAM)
        cows = selectFemale(pop, input$sliderDams, use="pheno",simParam=SIMPARAM)
        randCross2(cows, bulls, nCrosses = pop@nInd, balance = TRUE, ignoreGender = FALSE, simParam=SIMPARAM)
      }
    } else if (input$radioSelectionMethod == 'gs') {
      y <- matrix(ncol=1, nrow=0)
      snps <- matrix(nrow=0, ncol=as.integer(input$numSNPs))
      
      method <- function(pop) {
        
        snpi <- which(input$numSNPs == names(costs$snp.cost))
        
        snps <- rbind(snps, pullSnpGeno(pop, snpChip = snpi, simParam=SIMPARAM))
        if (nrow(snps) > 3000)
          snps <- snps[nrow(snps) + (-3000:-1) + 1,]
        y <- rbind(y, pop@pheno)
        if (nrow(y) > 3000)
          y <- y[nrow(snps) + (-3000:-1) + 1,]
        Z <- diag(1, nrow(y))
        X <- matrix(1, ncol=1, nrow=nrow(y))
        
        sol <- solveUVM(y, X, Z, calcG(snps))
        pop@ebv <- sol$u[nrow(sol$u) + (-1000:-1) + 1,,drop=FALSE]

        acc <- cor(pop@pheno[,1], pop@ebv[,1])
        
        bulls = selectMale(pop, input$sliderSires, use="ebv",simParam=SIMPARAM)
        cows = selectFemale(pop, input$sliderDams, use="ebv",simParam=SIMPARAM)        
        res <- randCross2(cows, bulls, nCrosses = pop@nInd, balance = TRUE, ignoreGender = FALSE, simParam=SIMPARAM)
        attr(res, 'acc') <- acc
        res
      }
    } else {
      stop('An unimplemented selection method, `', input$radioSelectionMethod, '` was selected.')
    }
    
    results <- list(animals=data.frame(), monies=data.frame())
    new.pop <- setPheno(seed.pop, varE = varG(seed.pop)*2,simParam=SIMPARAM)
    stat.G <- data.frame(Generation=0:15) %>% mutate(meanG=0, varG=NA, meanY=0, h2=NA, acc=NA)
    stat.G[1, 2:5] <- c(meanG(new.pop), varG(new.pop), meanP(new.pop), varG(new.pop) / var(new.pop@pheno))
    for (i in 1:15) {
      
      progress$set(value = i)
      new.pop <- method(new.pop)
      acc <- ggplot2:::`%||%`(attr(new.pop, 'acc'), NA)
      new.pop <- setPheno(new.pop, varE = varG(new.pop)*2,simParam=SIMPARAM)
      
      results$animals <- bind_rows(results$animals, data.frame(Generation=i, id=new.pop@id, y=new.pop@pheno, stringsAsFactors = FALSE))
      stat.G[i+1,2:6] <- c(meanG(new.pop), varG(new.pop), meanP(new.pop), varG(new.pop) / var(new.pop@pheno), acc=acc)
      
      
      males <- new.pop[new.pop@gender == 'M', ]
      females <- new.pop[new.pop@gender == 'F', ]
      
      monies <- tribble(
        ~Generation, ~item, ~value,
        i, 'keep', males@nInd * costs$bull.cost + females@nInd * costs$cow.cost,
        i, 'selling', sum(males@pheno * costs$bull.sell) + sum(males@pheno * costs$cow.sell)
      )
      
      results$monies <- bind_rows(results$monies, monies)
      
      
      if (varG(new.pop) < 0.1) {
        showModal(modalDialog('Your heard, it ded!', footer = modalButton("Dismiss"), fade=TRUE))
        break
      }
    }
    results$stat.G <- stat.G
    results
  }
  
  # Visual changes from inputs ----------
  
  observeEvent(input$radioSelectionMethod, {
    if (input$radioSelectionMethod == 'gs') {
      shinyjs::show('numSNPs')
    } else {
      shinyjs::hide('numSNPs')
    }
  })
  
  
  ## Update outputs with result from breeding -------------
  
  output$plotMain <- renderPlot({
    data <- breed()
    if (is.null(data)) return(NULL)
    
    data$stat.G %>% gather(stat, val, -Generation) %>%
      drop_na(val) %>%
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
    if (is.null(data)) return(NULL)
    
    yranges <- sapply(.saved_results, function(x) c(range(x$animals$y, na.rm=TRUE), max(x$stat.G$varG, na.rm=TRUE)))
    varGmax <- max(yranges[3,])
    yranges <- c(min(yranges[1,])*1.05-0.3, max(yranges[2,])*1.05+0.3)
    
    r <- yranges
    scale.w <- input$cexWidth *  (r[2]-r[1]) / 15
    scale.h <- input$cexHeight * 15 / (r[2]-r[1])
    
    p <- data$animals %>% group_by(Generation) %>% do(shake_and_sample(.$y, .$Generation)) %>%
      ggplot(aes(x=x, y=y, width=y/scale.w, height=y/scale.h)) + geom_tile() +
        coord_cartesian(xlim=c(0,15), ylim=yranges) +
        labs(x='Generation', title='Growing cows')
    #p <- data$animals %>% ggplot(aes(x=Generation, y=y, size=y)) + geom_point() +
    #p1 <- ggplotGrob(p)
      
    p1 <- replace_rect_cows(p)
    i <- which(p1$layout$name == 'panel')
    t <- p1$layout$t[i]
    p1$heights[[t]] <- unit(2, 'null')
    
    p2 <-     data$stat.G %>% ggplot(aes(x=Generation, y=varG * 100, fill=varG)) +
      geom_bar(stat='identity') +
      scale_fill_continuous(low='#F7FBFF', high='#08306B')  +            # brewer.pal(9, 'Blues')  [1] "#F7FBFF" "#DEEBF7" "#C6DBEF" "#9ECAE1" "#6BAED6" "#4292C6" "#2171B5" "#08519C" "#08306B"
      coord_cartesian(xlim=c(0,15), ylim=c(0, varGmax*100)) +
      guides(fill=FALSE) +
      labs(y='Genes in herd', title='How many genes are there in the herd?')
    p2 <- ggplotGrob(p2)
    
    grid.draw(rbind(p1, p2, size='first'))
    #grid.arrange(p1, p2)
  })  
  
  output$plotMonies <- renderPlot({
    data <- breed()
    if (is.null(data)) return(NULL)
    
    yranges <- sapply(.saved_results, function(x) sum(x$monies$value)) %>% range
    yranges <- yranges * 1.05 + c(-500, 500)
    yranges[1] <- min(yranges[1], 0)
    yranges[2] <- max(yranges[2], 0)
    
    saveRDS(data, file='data/breed.rds')
    
    monies <- data$monies
    cummulative <- monies %>% group_by(Generation) %>% summarise(total=sum(value, na.rm=TRUE)) %>%
      arrange(Generation) %>% mutate(cummulative=cumsum(total))
    
    ggplot(monies, aes(x=Generation, y=value)) + 
      geom_col(aes(fill=as.factor(sign(value)))) + 
      scale_fill_manual(values=c('-1'='red', '1'='#4ea815'), guide=FALSE) +
      scale_y_continuous('Profit (£)', labels=scales::comma) +
      coord_cartesian(xlim=c(1,15), ylim=yranges) +
      geom_line(data=cummulative, aes(y=cummulative), size=1) + 
      geom_point(data=cummulative, aes(y=cummulative), size=3, pch=21, fill='white') +
      labs(y='Profit (£)')
  })
  
  output$plotSummaries <- renderPlot({
    data <- saved_summaries()
    
    if (nrow(data$animals) == 0) return(NULL)
    
    p1 <- data$animals %>% 
      ggplot(aes(x=as.factor(id), y=y, fill=method)) + 
      geom_boxplot() +
      labs(y='Phenotypic value of animals', x='Setting #')
    p2 <- data$monies %>%
      ggplot(aes(x=as.factor(id), y=monies, fill=method)) +
      geom_col() +
      labs(y='Total profit (£)', x='Setting #')
    
    grid.draw(gtable_rbind(ggplotGrob(p1), ggplotGrob(p2)))
    
  })
  

  old.btn.go <- 0
  breed <- reactive({
    b <- input$btnGO
    h <- input$tblHistory_rows_selected
    if (b > old.btn.go) {
      old.btn.go <<- b
  
      res <- simulate()
      
      .saved_results <<- append(.saved_results, list(res))
      
      
      ## summarise monies:
      monies <- sum(res$monies$value)
      last.y <- res$animals[res$animals$Generation == max(res$animals$Generation),'y',drop=FALSE]
      last.y$id <- as.integer(b)
      last.y$method <- input$radioSelectionMethod
      
      .saved_summaries_monies <<- bind_rows(.saved_summaries_monies,
                                     data.frame(id=as.integer(b), method=input$radioSelectionMethod, monies=monies))
      .saved_summaries_animals <<- bind_rows(.saved_summaries_animals, last.y)
      
      return(res)
    } else if (length(h) > 0) {
      settings <- .saved_settings[h,]
      updateSliderInput(session, 'sliderSires', value=settings$sires)
      updateSliderInput(session, 'sliderDams', value=settings$dams)
      updateRadioButtons(session, 'radioSelectionMethod', selected = settings$method)
      updateNumericInput(session, 'numOCS', value = settings$ocs)
      
      return(.saved_results[[h]])
    } else if (length(.saved_results) > 0) {
      return(.saved_results[[length(.saved_results)]])
    }
  })
  
  output$tblHistory <- DT::renderDataTable({
    datatable(saved_settings(),
              options = list(paging=FALSE),
              caption='History of settings',
              autoHideNavigation = TRUE,
              filter = 'none',
              rownames = FALSE,
              colnames = c('Setting #', '# Sires', '# Dams', 'Selection method', '# SNPs for GS'),
              selection=list(mode='single', target='row')
    )
  })

  
  saved_settings = eventReactive(input$btnGO, {
    .saved_settings <<- bind_rows(.saved_settings, 
                                  data.frame(row=as.integer(input$btnGO), sires=input$sliderSires, dams=input$sliderDams, method=input$radioSelectionMethod, snps=input$numSNPs))   
    .saved_settings
    
  })
  
  saved_summaries <- eventReactive(input$maintab, {
    list(animals=.saved_summaries_animals, monies=.saved_summaries_monies)
  })
})

