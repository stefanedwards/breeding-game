# Creates small, simple AlphaSimR population with single chromosome
# for quick preview of chosen options.

source('global.R')

FOUNDER <- AlphaSimR::runMacs(500, 1, 2000, species='CATTLE')

SIMPARAM <- createSimulation(FOUNDER, maxQtl = 500, maxSnp = 500, gender='yes_rand')
SIMPARAM <- addTraitA(FOUNDER,nQtlPerChr=500,meanG=0,varG=1,
                     simParam=SIMPARAM)

base.pop <- newPop(FOUNDER, simParam = SIMPARAM)


# 10 generations of random mating
new.pop <- base.pop
stat.G <- matrix(ncol=2,nrow=10, dimnames = list(NULL, c('meanG','varG')))
for (i in 1:10) {
  cat('Spawning random mating generation', i, '\n')
  
  new.pop <- randCross(new.pop, 500, 2, balance=TRUE)
  stat.G[i,] <- c(meanG(new.pop), varG(new.pop))
}

seed.pop <- new.pop
save(FOUNDER, SIMPARAM, base.pop, seed.pop, stat.G, file=.data('small_population.Rdata'))
