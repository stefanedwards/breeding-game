# Global modules and variables for shiny app

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(lemon)
library(AlphaSimR)
library(shinyjs)
library(DT)
library(nadiv)
library(ggbeeswarm)


#.cache <- .dot('cache')
.data <- .dot('data')

options(stringsAsFactors = FALSE)

# Get the cow emoji -----
if (file.exists(.data('cow.emoji.rds'))) {
  cow.emoji <- readRDS(.data('cow.emoji.rds'))
} else {
  require(emoGG)
  cow.emoji <- emoGG::emoji_get('1f404')[[1]]
  saveRDS(cow.emoji, .data('cow.emoji.rds'))
}

# Monetary options -----

costs <- list(
  bull.cost=-7,
  cow.cost=-7,
  bull.sell=1.5, # multiplier for selling a bull
  cow.sell=1.5,  # multiplier for selling a cow
  eh=NA,
  snp.cost=c('50'=0.1, '250'=.14, '1000'=0.2)  # Cost for genotyping per animal
)

# Get EBV method ---------

getebvmethod <- function(method) {
  ### Setup a function for estimating breeding value -----
  if (method == 'random') {
    ebvmethod <- function(pop, parent.pop, grand.parent.pop, ...) {
      matrix(runif(pop@nInd), ncol=1)
    }
  } else if (method == 'pa') {
    ebvmethod <- function(pop, parent.pop, grand.parent.pop, ...) {
      parents <- structure(parent.pop@pheno, .Names=parent.pop@id)
      ebvs <- (parents[pop@mother] + parents[pop@father]) / 2 
      # make parent averages a bit worse by adding more noise.
      ebvs <- ebvs * rnorm(length(ebvs), mean=1, sd=0.2)
      matrix(ebvs, ncol=1)
    }
  } else if (method == 'pebv') {
    ebvmethod <- function(pop, parent.pop, grand.parent.pop, ...) {
      ped <- data.frame(ind=character(0), s=character(0), d=character(0))
      if (!is.null(grand.parent.pop)) 
        ped <- bind_rows(ped, data.frame(ind=grand.parent.pop@id, s=grand.parent.pop@father, d=grand.parent.pop@mother))
      ped <- bind_rows(ped, 
                       data.frame(ind=parent.pop@id, s=parent.pop@father, d=parent.pop@mother),
                       data.frame(ind=pop@id, s=pop@father, d=pop@mother))
      ped$s[!ped$s %in% ped$ind] <- 0
      ped$d[!ped$d %in% ped$ind] <- 0
      
      y <- matrix(ncol=1, nrow=0)
      if (!is.null(grand.parent.pop)) 
        y <- rbind(y, grand.parent.pop@pheno)
      y <- rbind(y, parent.pop@pheno)
      
      X <- matrix(1, ncol=1, nrow=length(y))
      
      Z <- diag(1, nrow=length(y), ncol=pop@nInd + parent.pop@nInd + ifelse(!is.null(grand.parent.pop), grand.parent.pop@nInd, 0))
      suppressWarnings(A <- as.matrix(makeA(ped)))
      sol <- solveUVM(y, X, Z, A)
      sol$u[nrow(sol$u) + (-pop@nInd + 1):0,,drop=FALSE]
    } 
  } else if (method == 'gs') {
    ebvmethod <- function(pop, parent.pop, grand.parent.pop, input, SIMPARAM) {
      snpi <- which(input$numSNPs == names(costs$snp.cost))
      snps <- matrix(nrow=0, ncol=as.integer(input$numSNPs))
      
      if (!is.null(grand.parent.pop)) 
        snps <- rbind(snps, pullSnpGeno(grand.parent.pop, snpChip = snpi, simParam = SIMPARAM))
      snps <- rbind(snps,
                    pullSnpGeno(parent.pop, snpChip = snpi, simParam = SIMPARAM),
                    pullSnpGeno(pop, snpChip = snpi, simParam = SIMPARAM)
      )
      
      
      y <- matrix(ncol=1, nrow=0)
      if (!is.null(grand.parent.pop)) 
        y <- rbind(y, grand.parent.pop@pheno)
      y <- rbind(y, parent.pop@pheno)
      
      X <- matrix(1, ncol=1, nrow=length(y))
      
      Z <- diag(1, nrow=length(y), ncol=pop@nInd + parent.pop@nInd + ifelse(!is.null(grand.parent.pop), grand.parent.pop@nInd, 0))
      sol <- solveUVM(y, X, Z, calcG(snps))
      sol$u[nrow(sol$u) + (-pop@nInd + 1):0,,drop=FALSE]
    }
  } else {
    stop('An unimplemented selection method, `', input$radioSelectionMethod, '` was selected.')
  }
  ebvmethod
}

# Plotting functions --------------

hues <- scales::hue_pal()(4)
names(hues) <- c('random','pa','pebv','gs')
method.fill.scale <- scale_fill_manual('Selection method:', values=hues, labels=c('random'='Random','pa'='Parent average', 'pebv'='Pedigree based\nbreeding value', 'gs'='Genomic based\nbreeding value'))
method.col.scale <- scale_colour_manual('Selection method:', values=hues, labels=c('random'='Random','pa'='Parent average', 'pebv'='Pedigree based\nbreeding value', 'gs'='Genomic based\nbreeding value'))

#' Reduces the size of animals to plot and jitters the x-values.
#' Selects top and bottom 5, and samples among the remaing with `frac`.
#' Usage:
#' df %>% group_by(Generation) %>% do(shake_and_sample(.$y, .$Generation))
shake_and_sample <- function(y, x, frac=0.01, width_jitter=0.4) {
  y <- sort(y)
  res <- c(y[1:5], y[length(y) - (1:5)])
  res <- c(res, sample(y[6:((length(y)-6))], frac*(length(y)-10)))
  x <- x[1] + runif(length(res), -width_jitter, width_jitter)
  data.frame(y=res, x=x)
}


#' Replaces rect Grobs in the main panel with raster Grobs (emojiGrob from emoGG)
#' Returns a gtable object with an updated grob (corresponding to the rect-grobs).
#' 
#' Usage:
#' p <- ggplot(aes(x=x, y=y, width=y/20, height=y/20)) + geom_tile()
#' g <- replace_rect_cows(p)
#' grid.draw(g)
replace_rect_cows <- function(g, panel='panel') {
  if (is.ggplot(g)) g <- ggplotGrob(g)
  
  i <- which(g$layout$name == 'panel')
  j <- which(sapply(g$grobs[[i]]$children, function(x) 'rect' %in% class(x)))

  cow.tiles <- with(g$grobs[[i]]$children[[j]], 
                    rasterGrob(image=cow.emoji, x=x, y=y, width=width, height=height))
  g$grobs[[i]]$children[[j]] <- cow.tiles
  
  g
}

