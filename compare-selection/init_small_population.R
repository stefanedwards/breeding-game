# Creates small, simple AlphaSimR population with single chromosome
# for quick preview of chosen options.

source('global.R')

FOUNDER <- AlphaSimR::runMacs(500, 1, 2000, species='CATTLE')

SIMPARAM <- createSimulation(FOUNDER, maxQtl = 500, maxSnp = 1000, gender='yes_rand') %>%
  addTraitA(FOUNDER,nQtlPerChr=500,meanG=5,varG=1, simParam=.) %>%
  addSnpChip(50, simParam=.) %>%
  addSnpChip(250, simParam=.) %>%
  addSnpChip(1000, simParam=.)


base.pop <- newPop(FOUNDER, simParam = SIMPARAM)


# 10 generations of random mating
new.pop <- base.pop
stat.G <- matrix(ncol=2,nrow=10, dimnames = list(NULL, c('meanG','varG')))
for (i in 1:10) {
  cat('Spawning random mating generation', i, '\n')
  parent.pop <- new.pop
  new.pop <- randCross(parent.pop, 125, 2, balance=TRUE)
  stat.G[i,] <- c(meanG(new.pop), varG(new.pop))
}

save(FOUNDER, SIMPARAM, base.pop, parent.pop, new.pop, stat.G, file=.data('small_population.Rdata'))
