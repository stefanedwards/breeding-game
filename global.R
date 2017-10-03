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

# Plotting functions --------------

hues <- scales::hue_pal()(4)
names(hues) <- c('random','pa','pebv','gs')
method.fill.scale <- scale_fill_manual('Selection method:', values=hues, labels=c('random'='Random','pa'='Parent average', 'pebv'='Pedigree based\nbreeding value', 'gs'='Genomic based\nbreeding value'))

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

