# Run only once to generate data files and install packages.

install.packages(c('shiny','dplyr','tidyr','ggplot2','grid','lemon','shinyjs','DT','devtools'))
devtools::install_bitbucket('hickeyjohnteam/AlphaSimr')
devtools::install_github("dill/emoGG")

require(shiny)
require(dplyr)
require(tidyr)
require(ggplot2)
require(grid)
require(lemon)
require(AlphaSimR)
require(emoGG)
require(shinyjs)
require(DT)


source('init_small_population.R')  # to generate the initial breeding population.

