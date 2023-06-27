## Sens. Analysis

### Higher linkage
## 80% linkage, Sc 10, 12
### CI
RD <- load("/Users/Ent/Downloads/ATLAS CEA/CoteIvoireScens20220201.RData")
NoHIVST <- ScenCI[[1]]
ATLASonly.linkage.CI <- ScenCI[[10]]
ATLASSU.linkage.CI <- ScenCI[[12]]
country <- "CI"

### MA
RD <- load("/Users/Ent/Downloads/ATLAS CEA/MaliScens20220131.RData")
NoHIVST <- ScenMA[[1]]
ATLASonly.linkage.MA <- ScenMA[[10]]
ATLASSU.linkage.MA <- ScenMA[[12]]
country <- "MA"

### SE
RD <- load("/Users/Ent/Downloads/ATLAS CEA/SenegalScens20220203.RData")
NoHIVST <- ScenSE[[1]]
ATLASonly.linkage.SE <- ScenSE[[10]]
ATLASSU.linkage.SE <- ScenSE[[12]]
country <- "SE"


### DALY.diff
DALY.base <- DALY.Calc(NoHIVST, country = country)
DALY.base.dis <- DALY.Calc(NoHIVST, country = country)

DALY.ATLAS <- DALY.Calc(ATLASonly.linkage.SE, country = country)
DALY.ATLAS.dis <- DALY.Calc(ATLASonly.linkage.SE, country = country)

DALY.SU <- DALY.Calc(ATLASSU.linkage.SE, country = country)
DALY.SU.dis <- DALY.Calc(ATLASSU.linkage.SE, country = country)

DALY.diff <- compare(DALY.base, DALY.ATLAS, DALY.SU, averted = TRUE)
DALY.total.diff <- list(rowSums(DALY.diff[[1]][1:21]), rowSums(DALY.diff[[2]][1:21])) 
names(DALY.total.diff) <- c("ATLAS vs Base", "Scale up vs Base")
DALY.diff <- list(DALY.diff[[1]][1:21], DALY.diff[[2]][1:21])
names(DALY.diff) <- c("ATLAS vs Base", "Scale up vs Base")

## AIDS Death

death <- AIDS.deaths(NoHIVST, timeframe = 20)
death.base <- death[[1]][[1]]
death.base.dis <- death[[1]][[2]]

death <- AIDS.deaths(ATLASonly.linkage.SE, timeframe = 20)
death.ATLAS <- death[[1]][[1]]
death.ATLAS.dis <- death[[1]][[2]]

death <- AIDS.deaths(ATLASSU.linkage.SE, timeframe = 20)
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

inf <- AIDS.inf(ATLASonly.linkage.SE, timeframe = 20)
inf.ATLAS <- inf[[1]][[1]]
inf.ATLAS.dis <- inf[[1]][[2]]

inf <- AIDS.inf(ATLASSU.linkage.SE, timeframe = 20)
inf.SU <- inf[[1]][[1]]
inf.SU.dis <- inf[[1]][[2]]

inf.diff <- compare(inf.base, inf.ATLAS, inf.SU, averted = TRUE)
names(inf.diff) <- c("ATLAS vs Base", "Scale up vs Base")

daly.ci.linkage <- monte.carlo.icer.ci(num = 100, rate = 0.04, outcome = DALY.diff)
inf.ci.linkage <- monte.carlo.icer.ci(num = 100, rate = 0.04, outcome = inf.diff)
death.ci.linkage <- monte.carlo.icer.ci(num = 100, rate = 0.04, outcome = death.diff)

daly.ma.linkage <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = DALY.diff)
inf.ma.linkage <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = inf.diff)
death.ma.linkage <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = death.diff)

daly.se.linkage <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = DALY.diff)
inf.se.linkage <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = inf.diff)
death.se.linkage <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = death.diff)

Unc.int(daly.ci.linkage[[1]][,1])
Unc.int(inf.ci.linkage[[1]][,1])
Unc.int(death.ci.linkage[[1]][,1])
Unc.int(daly.ci.linkage[[2]][,1])
Unc.int(inf.ci.linkage[[2]][,1])
Unc.int(death.ci.linkage[[2]][,1])

Unc.int(daly.ma.linkage[[1]][,1])
Unc.int(inf.ma.linkage[[1]][,1])
Unc.int(death.ma.linkage[[1]][,1])
Unc.int(daly.ma.linkage[[2]][,1])
Unc.int(inf.ma.linkage[[2]][,1])
Unc.int(death.ma.linkage[[2]][,1])

Unc.int(daly.se.linkage[[1]][,1])
Unc.int(inf.se.linkage[[1]][,1])
Unc.int(death.se.linkage[[1]][,1])
Unc.int(daly.se.linkage[[2]][,1])
Unc.int(inf.se.linkage[[2]][,1])
Unc.int(death.se.linkage[[2]][,1])



