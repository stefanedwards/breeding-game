
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source('global.R')

shinyServer(function(input, output, session) {

  # Have the user decide generation by generation what to do.
  load(.data('small_population.Rdata'))  # loads `parent.pop`
  
  ## Private variables, specific to the session (apparently) -----
  # new.pop and parent.pop loaded from small.simulation.Rdata
  .new.pop <- new.pop
  .parent.pop <- setPheno(parent.pop, varE = varG(parent.pop)*2,simParam=SIMPARAM)
  .grand.parent.pop <- NULL
  rm(new.pop, parent.pop)
  
  .stat.G <- data.frame(Generation=integer(0), meanG=numeric(0), varG=numeric(0), h2=numeric(0), acc=numeric(0))
  .animals <- data.frame()
  .monies <- data.frame()
  .settings <- data.frame()
  .settings.full <- data.frame()
  .current.gen <- 0
  
  ## Main selection generator -----------------
  simulate <- eventReactive(input$btnGO, {
  
    ebvmethod <- getebvmethod(input$radioSelectionMethod)
    
    progress <- NULL
    if (input$numGenerations > 2) {
      progress <- Progress$new(session, min=1, max=input$numGenerations)
      on.exit(progress$close())
      
      progress$set(message='Breeding many generations.',
                   detail='Come back in 15-20 years, this stuff takes time!')
    }
    
    these.generations <- (.current.gen+1):(.current.gen + as.integer(input$numGenerations))
    new.pop <- .new.pop
    parent.pop <- .parent.pop
    grand.parent.pop <- .grand.parent.pop
    cont <- TRUE
    
    for (i in these.generations) {
      if (!is.null(progress)) progress$set(value = i - .current.gen)
      
      new.pop@ebv <- ebvmethod(new.pop, parent.pop, grand.parent.pop, input, SIMPARAM)
      grand.parent.pop <- parent.pop
      parent.pop <- new.pop
      males <- selectMale(parent.pop, input$sliderSires, use='ebv', simParam=SIMPARAM)
      females <- selectFemale(parent.pop, input$sliderDams, use='ebv', simParam=SIMPARAM)
      new.pop <- randCross2(females, males, parent.pop@nInd, simParam=SIMPARAM)
      
      parent.pop <- setPheno(parent.pop, varE = varG(parent.pop)*2,simParam=SIMPARAM)
      .animals <<- bind_rows(.animals, data.frame(Generation=i, id=parent.pop@id, y=parent.pop@pheno, stringsAsFactors = FALSE))
      .stat.G <<- bind_rows(.stat.G, data.frame(Generation=i, 
                                                meanG=meanG(parent.pop), 
                                                varG=varG(parent.pop), 
                                                h2=varG(parent.pop)/var(parent.pop@pheno), 
                                                acc=cor(parent.pop@ebv, parent.pop@pheno)))
      
      males <- parent.pop[parent.pop@gender == 'M', ]
      females <- parent.pop[parent.pop@gender == 'F', ]
      
      monies <<- tribble(
        ~Generation, ~item, ~value,
        i-1, 'upkeep', males@nInd * costs$bull.cost + females@nInd * costs$cow.cost,
        i-1, 'selling', sum(males@pheno * costs$bull.sell) + sum(males@pheno * costs$cow.sell)
      )
      if (input$radioSelectionMethod == 'gs') 
        monies <- bind_rows(monies, data.frame(Generation=i, item='genotyping', value=-1 * parent.pop@nInd * costs$snp.cost[input$numSNPs]))
      .monies <<- bind_rows(.monies, monies)
      
      .settings.full <<- bind_rows(.settings.full, 
                                   data.frame(Generation=i, Sires=input$sliderSires, Dams=input$sliderDams, method=input$radioSelectionMethod))
      
      if (varG(new.pop) < 0.1) {
        showModal(modalDialog('Your heard, it ded!', footer = modalButton("Dismiss"), fade=TRUE))
        cont <- FALSE
        break
      }
    }
    generations <- ifelse(as.integer(input$numGenerations) == 1, i,sprintf('%d:%d', .current.gen+1, i)) 
    .settings <<- bind_rows(.settings,
                            data.frame(Generations=as.character(generations),
                                       Sires=input$sliderSires,
                                       Dams=input$sliderDams,
                                       method=input$radioSelectionMethod, snps=input$numSNPs)
                            )
    
    .current.gen <<- i
    
    .new.pop <<- new.pop
    .parent.pop <<- parent.pop
    .grand.parent.pop <<- grand.parent.pop
    
    
    list(animals=.animals, monies=.monies, stat.G=.stat.G, settings=.settings, full=.settings.full)
  })
  
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
    data <- simulate()
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
    data <- simulate()
    if (is.null(data)) return(NULL)
    
    r <- range(data$animals$y)
    r1 <- min(0, r[1])
    r2 <- max(15, r[2])
    x <- range(data$animals$Generation)
    xr <- max(x[2]-x[1], 1)
    scale.w <- input$cexWidth * xr #*  (r[2]-r[1]) / xr
    scale.h <- input$cexHeight * r2 #* xr / (r[2]-r[1])
    
    if (input$viewCows == 'cows') {
      
      p <- data$animals %>% group_by(Generation) %>% do(shake_and_sample(.$y, .$Generation, frac = 5/250)) %>%
        ggplot(aes(x=x, y=y, width=y*scale.w, height=y*scale.h)) + geom_tile() +
          coord_cartesian(ylim=c(r[1], r[2]*1.15), xlim=c(0.5, xr*1.05+1)) + 
          labs(x='Generation', title='Growing cows', y='Size of cows') + 
          theme_minimal(12) + theme(axis.title.x = element_blank(), axis.text.x=element_blank())
        
      p1 <- replace_rect_cows(p)
      i <- which(p1$layout$name == 'panel')
      t <- p1$layout$t[i]
      p1$heights[[t]] <- unit(4, 'null')
    
      p3 <- ggplot(data$full, aes(x=Generation-0.5, y=1, fill=method)) + geom_tile(height=1, width=1) + method.fill.scale +
        theme_minimal(12) + coord_cartesian(xlim=c(0.5, xr*1.05 + 1)) +
        theme(axis.title.y=element_blank(), axis.ticks.x = element_blank(), axis.text.y=element_blank(),
              legend.position='bottom') +
        labs(x='Generation')
      p3 <- ggplotGrob(p3)
      
    } else {
      p1 <- inner_join(data$animals, data$full, by='Generation') %>%
        ggplot(aes(x=Generation, y=y)) + 
        geom_point(aes(colour=method), position=position_jitter(width=0.3, height=NULL)) +
        stat_summary(geom='line', fun.y=mean, size=1) +
        stat_summary(geom='point', fun.y=mean, size=3) +
        method.col.scale +
        coord_cartesian(xlim=c(0.5, xr*1.05 + 1)) +
        theme_minimal(12) + 
        theme(axis.ticks.x = element_blank(), legend.position='bottom') +
        labs(x='Generation', y='Size of cows')
      
      p1 <- ggplotGrob(p1)
      i <- which(p1$layout$name == 'panel')
      t <- p1$layout$t[i]
      p1$heights[[t]] <- unit(4, 'null')
      
      p3 <- zeroGrob()
    }
    
    p4 <- data$full %>% select(Generation, Sires, Dams) %>%
      gather(stat, val, Sires, Dams) %>%
      mutate(Generation=ifelse(stat=='Sires', Generation-0.1, Generation+0.1)) %>%
      ggplot(aes(x=Generation, y=val, colour=stat)) +
        geom_point() +
        geom_segment(aes(xend=Generation), yend=0) +
       theme_minimal(12) + coord_cartesian(xlim=c(0.5, xr*1.05 + 1)) +
       theme(axis.title.x=element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(),
             legend.position='hidden') +
      labs(y='Sires / dams selected\nfor breeding')
    p4 <- ggplotGrob(p4)   

                 

    
    p2 <-     data$stat.G %>% ggplot(aes(x=Generation, y=varG * 100, fill=varG*100)) +
      geom_bar(stat='identity') +
      #scale_fill_continuous(low='#F7FBFF', high='#08306B')  +            # brewer.pal(9, 'Blues')  [1] "#F7FBFF" "#DEEBF7" "#C6DBEF" "#9ECAE1" "#6BAED6" "#4292C6" "#2171B5" "#08519C" "#08306B"4
      scale_fill_gradient2(midpoint=20, low='#FC8D59', mid='#FFFFBF', high='#91CF60') + #scale_fill_brewer(palette = 'RdYlGn', limits=c(0, varGmax*100)) +  # brewer.pal(3, 'RdYlGn') [1] "#FC8D59" "#FFFFBF" "#91CF60"
      guides(fill=FALSE) +
      labs(y='Genes in herd', title='How many genes are there in the herd?') + 
      theme_minimal(12) + theme(panel.grid.major.x = element_blank()) +
      coord_cartesian(xlim=c(0.5, xr*1.05+1))
    p2 <- ggplotGrob(p2)
    
    i <- which(p2$layout$name == 'panel')
    t <- p2$layout$t[i]
    p2$heights[[t]] <- unit(2, 'null')
    
    
    if (input$viewCows == 'cows') {
      combined <- rbind(p1, p4, p3, p2, size='first')
    } else {
      combined <- rbind(p1, p4, p2, size='first')
    }
    
    i <- which(p4$layout$name == 'ylab-l')
    w <- p4$widths[p4$layout$l[i]]
    i <- which(combined$layout$name == 'ylab-l')
    combined$widths[[combined$layout$l[i[1]]]] <- w
    
    grid.draw(combined)
    #grid.arrange(p1, p2)
  })  
  
  output$plotMonies <- renderPlot({
    data <- simulate()
    if (is.null(data)) return(NULL)
    
    monies <- data$monies
    cummulative <- monies %>% group_by(Generation) %>% summarise(total=sum(value, na.rm=TRUE)) %>%
      arrange(Generation) %>% mutate(cummulative=cumsum(total))
    
    ggplot(monies, aes(x=Generation, y=value)) + 
      geom_col(aes(fill=item)) + 
      #scale_fill_manual(values=c('-1'='red', '1'='#4ea815'), guide=FALSE) +
      scale_fill_manual(values=c(genotyping='#619CFF', selling='#00BA38', upkeep='#F8766D')) +
      scale_y_continuous('Profit (£)', labels=scales::comma) +
      geom_line(data=cummulative, aes(y=cummulative), size=1) + 
      geom_point(data=cummulative, aes(y=cummulative), size=3, pch=21, fill='white') +
      labs(y='Profit (£)', title='How much money are you making?') + 
      theme_minimal(12) + theme(panel.grid.major.x = element_blank())
  })
  
  
  output$tblHistory <- DT::renderDataTable({
    data <- simulate()
    
    datatable(data$settings,
              options = list(paging=FALSE),
              caption='Breeding strategy',
              autoHideNavigation = TRUE,
              filter = 'none',
              rownames = FALSE,
              colnames = c('Generations', '# Sires', '# Dams', 'Selection method', '# SNPs for GS'),
              selection=list(mode='single', target='row')
    )
  })

})

