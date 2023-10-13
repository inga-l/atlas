
### SUB Rate
## No sub. Sc 28, 30
RD <- load("/Users/Ent/Downloads/ATLAS CEA/CoteIvoireScens20220201.RData")
NoHIVST <- ScenCI[[1]]
ATLASonly.sub.CI <- ScenCI[[28]]
ATLASSU.sub.CI <- ScenCI[[30]]
country <- "CI"

### MA
RD <- load("/Users/Ent/Downloads/ATLAS CEA/MaliScens20220131.RData")
NoHIVST <- ScenMA[[1]]
ATLASonly.sub.MA <- ScenMA[[28]]
ATLASSU.sub.MA <- ScenMA[[30]]
country <- "MA"

### SE
RD <- load("/Users/Ent/Downloads/ATLAS CEA/SenegalScens20220203.RData")
NoHIVST <- ScenSE[[1]]
ATLASonly.sub.SE <- ScenSE[[28]]
ATLASSU.sub.SE <- ScenSE[[30]]
country <- "SE"


### DALY.diff
DALY.base <- DALY.Calc(NoHIVST, country = country)
DALY.base.dis <- DALY.Calc(NoHIVST, country = country)

DALY.ATLAS <- DALY.Calc(ATLASonly.sub.MA, country = country)
DALY.ATLAS.dis <- DALY.Calc(ATLASonly.sub.MA, country = country)

DALY.SU <- DALY.Calc(ATLASSU.sub.MA, country = country)
DALY.SU.dis <- DALY.Calc(ATLASSU.sub.MA, country = country)

DALY.diff <- compare(DALY.base, DALY.ATLAS, DALY.SU, averted = TRUE)
DALY.total.diff <- list(rowSums(DALY.diff[[1]][1:21]), rowSums(DALY.diff[[2]][1:21])) 
names(DALY.total.diff) <- c("ATLAS vs Base", "Scale up vs Base")
DALY.diff <- list(DALY.diff[[1]][1:21], DALY.diff[[2]][1:21])
names(DALY.diff) <- c("ATLAS vs Base", "Scale up vs Base")

## AIDS Death

death <- AIDS.deaths(NoHIVST, timeframe = 20)
death.base <- death[[1]][[1]]
death.base.dis <- death[[1]][[2]]

death <- AIDS.deaths(ATLASonly.sub.MA, timeframe = 20)
death.ATLAS <- death[[1]][[1]]
death.ATLAS.dis <- death[[1]][[2]]

death <- AIDS.deaths(ATLASSU.sub.MA, timeframe = 20)
death.SU <- death[[1]][[1]]
death.SU.dis <- death[[1]][[2]]

death.diff <- compare(death.base, death.ATLAS, death.SU, averted = TRUE)
death.diff <- list(death.diff[[1]][1:21], death.diff[[2]][1:21])
names(death.diff) <- c("ATLAS vs Base", "Scale up vs Base")
#death.diff

## AIDS Inf

inf <- AIDS.inf(NoHIVST, timeframe = 20)
inf.base <- inf[[1]][[1]]
inf.base.dis <- inf[[1]][[2]]

inf <- AIDS.inf(ATLASonly.sub.MA, timeframe = 20)
inf.ATLAS <- inf[[1]][[1]]
inf.ATLAS.dis <- inf[[1]][[2]]

inf <- AIDS.inf(ATLASSU.sub.MA, timeframe = 20)
inf.SU <- inf[[1]][[1]]
inf.SU.dis <- inf[[1]][[2]]

inf.diff <- compare(inf.base, inf.ATLAS, inf.SU, averted = TRUE)
names(inf.diff) <- c("ATLAS vs Base", "Scale up vs Base")

daly.ci.sub <- monte.carlo.icer.ci(num = 100, rate = 0.04, outcome = DALY.diff) 
inf.ci.sub <- monte.carlo.icer.ci(num = 100, rate = 0.04, outcome = inf.diff)
death.ci.sub <- monte.carlo.icer.ci(num = 100, rate = 0.04, outcome = death.diff)

Unc.int(daly.ci.sub[[1]][,1])
Unc.int(daly.ci.sub[[2]][,1])

daly.ma.sub <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = DALY.diff)
inf.ma.sub <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = inf.diff)
death.ma.sub <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = death.diff)

Unc.int(daly.ma.sub[[1]][,1])
Unc.int(daly.ma.sub[[2]][,1])

  daly.se.sub <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = DALY.diff)
inf.se.sub <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = inf.diff)
death.se.sub <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = death.diff)

Unc.int(daly.se.sub[[1]][,1])
Unc.int(daly.se.sub[[2]][,1])
