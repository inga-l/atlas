### HIVST usage
## All kits used, Scn 40, 42
RD <- load("/Users/Ent/Downloads/ATLAS CEA/CoteIvoireScens20220201.RData")
NoHIVST <- ScenCI[[1]]
ATLASonly.usage.CI <- ScenCI[[40]]
ATLASSU.usage.CI <- ScenCI[[42]]
country <- "CI"

### MA
RD <- load("/Users/Ent/Downloads/ATLAS CEA/MaliScens20220131.RData")
NoHIVST <- ScenMA[[1]]
ATLASonly.usage.MA <- ScenMA[[40]]
ATLASSU.usage.MA <- ScenMA[[42]]
country <- "MA"

### SE
RD <- load("/Users/Ent/Downloads/ATLAS CEA/SenegalScens20220203.RData")
NoHIVST <- ScenSE[[1]]
ATLASonly.usage.SE <- ScenSE[[40]]
ATLASSU.usage.SE <- ScenSE[[42]]
country <- "SE"

### DALY.diff
DALY.base <- DALY.Calc(NoHIVST, country = country)
DALY.base.dis <- DALY.Calc(NoHIVST, country = country)

DALY.ATLAS <- DALY.Calc(ATLASonly.usage.MA, country = country)
DALY.ATLAS.dis <- DALY.Calc(ATLASonly.usage.MA, country = country)

DALY.SU <- DALY.Calc(ATLASSU.usage.MA, country = country)
DALY.SU.dis <- DALY.Calc(ATLASSU.usage.MA, country = country)

DALY.diff <- compare(DALY.base, DALY.ATLAS, DALY.SU, averted = TRUE)
DALY.total.diff <- list(rowSums(DALY.diff[[1]][1:21]), rowSums(DALY.diff[[2]][1:21])) 
names(DALY.total.diff) <- c("ATLAS vs Base", "Scale up vs Base")
DALY.diff <- list(DALY.diff[[1]][1:21], DALY.diff[[2]][1:21])
names(DALY.diff) <- c("ATLAS vs Base", "Scale up vs Base")

## AIDS Death

death <- AIDS.deaths(NoHIVST, timeframe = 20)
death.base <- death[[1]][[1]]
death.base.dis <- death[[1]][[2]]

death <- AIDS.deaths(ATLASonly.usage.MA, timeframe = 20)
death.ATLAS <- death[[1]][[1]]
death.ATLAS.dis <- death[[1]][[2]]

death <- AIDS.deaths(ATLASSU.usage.MA, timeframe = 20)
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

inf <- AIDS.inf(ATLASonly.usage.MA, timeframe = 20)
inf.ATLAS <- inf[[1]][[1]]
inf.ATLAS.dis <- inf[[1]][[2]]

inf <- AIDS.inf(ATLASSU.usage.MA, timeframe = 20)
inf.SU <- inf[[1]][[1]]
inf.SU.dis <- inf[[1]][[2]]

inf.diff <- compare(inf.base, inf.ATLAS, inf.SU, averted = TRUE)
names(inf.diff) <- c("ATLAS vs Base", "Scale up vs Base")

daly.ci.usage <- monte.carlo.icer.ci(num = 100, rate = 0.04, outcome = DALY.diff)
inf.ci.usage <- monte.carlo.icer.ci(num = 100, rate = 0.04, outcome = inf.diff)
death.ci.usage <- monte.carlo.icer.ci(num = 100, rate = 0.04, outcome = death.diff)

Unc.int(daly.ci.usage[[1]][,1])
Unc.int(daly.ci.usage[[2]][,1])

daly.ma.usage <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = DALY.diff)
inf.ma.usage <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = inf.diff)
death.ma.usage <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = death.diff)

Unc.int(daly.ma.usage[[1]][,1])
Unc.int(daly.ma.usage[[2]][,1])

daly.se.usage <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = DALY.diff)
inf.se.usage <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = inf.diff)
death.se.usage <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = death.diff)

Unc.int(daly.se.usage[[1]][,1])
Unc.int(daly.se.usage[[2]][,1])
