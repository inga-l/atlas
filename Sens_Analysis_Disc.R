### Discount rate

RD <- load("/Users/Ent/Downloads/ATLAS CEA/CoteIvoireScens20220201.RData")
NoHIVST <- ScenCI[[1]]
ATLASonly <- ScenCI[[2]]
ATLASSU <- ScenCI[[4]]
ATLASSU.CI <- ScenCI[[4]]
country <- "CI"

### MA
RD <- load("/Users/Ent/Downloads/ATLAS CEA/MaliScens20220131.RData")
NoHIVST <- ScenMA[[1]]
ATLASonly <- ScenMA[[2]]
ATLASSU <- ScenMA[[4]]
ATLASSU.MA <- ScenMA[[4]]
country <- "MA"

### SE
RD <- load("/Users/Ent/Downloads/ATLAS CEA/SenegalScens20220203.RData")
NoHIVST <- ScenSE[[1]]
ATLASonly <- ScenSE[[2]]
ATLASSU <- ScenSE[[4]]
ATLASSU.SE <- ScenSE[[4]]
country <- "SE"


###### Outcomes ######
### NEED TO MANUALLY CHANGE TIME HORIZON FOR DIFFERENCE OUTPUT
## DALY

DALY.base <- DALY.Calc(NoHIVST, country = country)
DALY.base.dis <- DALY.Calc(NoHIVST, country = country)

DALY.ATLAS <- DALY.Calc(ATLASonly, country = country)
DALY.ATLAS.dis <- DALY.Calc(ATLASonly, country = country)

DALY.SU <- DALY.Calc(ATLASSU, country = country)
DALY.SU.dis <- DALY.Calc(ATLASSU, country = country)

DALY.diff <- compare(DALY.base, DALY.ATLAS, DALY.SU, averted = TRUE)
DALY.total.diff <- list(rowSums(DALY.diff[[1]][1:21]), rowSums(DALY.diff[[2]][1:21])) 
names(DALY.total.diff) <- c("ATLAS vs Base", "Scale up vs Base")
DALY.diff <- list(DALY.diff[[1]][1:21], DALY.diff[[2]][1:21])
names(DALY.diff) <- c("ATLAS vs Base", "Scale up vs Base")

## AIDS Death

death <- AIDS.deaths(NoHIVST, timeframe = 20)
death.base <- death[[1]][[1]]
death.base.dis <- death[[1]][[2]]

death <- AIDS.deaths(ATLASonly, timeframe = 20)
death.ATLAS <- death[[1]][[1]]
death.ATLAS.dis <- death[[1]][[2]]

death <- AIDS.deaths(ATLASSU, timeframe = 20)
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

inf <- AIDS.inf(ATLASonly, timeframe = 20)
inf.ATLAS <- inf[[1]][[1]]
inf.ATLAS.dis <- inf[[1]][[2]]

inf <- AIDS.inf(ATLASSU, timeframe = 20)
inf.SU <- inf[[1]][[1]]
inf.SU.dis <- inf[[1]][[2]]

inf.diff <- compare(inf.base, inf.ATLAS, inf.SU, averted = TRUE)
names(inf.diff) <- c("ATLAS vs Base", "Scale up vs Base")

## 0%

daly.ci.no.disc <- monte.carlo.icer.ci(num = 100, rate = 0.00, outcome = DALY.diff)
inf.ci.no.disc <- monte.carlo.icer.ci(num = 100, rate = 0.00, outcome = inf.diff)
death.ci.no.disc <- monte.carlo.icer.ci(num = 100, rate = 0.00, outcome = death.diff)

Unc.int(daly.ci.no.disc[[1]][,1])
Unc.int(daly.ci.no.disc[[2]][,1])


daly.ma.no.disc <- monte.carlo.icer.ma(num = 100, rate = 0.00, outcome = DALY.diff)
inf.ma.no.disc <- monte.carlo.icer.ma(num = 100, rate = 0.00, outcome = inf.diff)
death.ma.no.disc <- monte.carlo.icer.ma(num = 100, rate = 0.00, outcome = death.diff)

Unc.int(daly.ma.no.disc[[1]][,1])
Unc.int(daly.ma.no.disc[[2]][,1])


daly.se.no.disc <- monte.carlo.icer.se(num = 100, rate = 0.00, outcome = DALY.diff)
inf.se.no.disc <- monte.carlo.icer.se(num = 100, rate = 0.00, outcome = inf.diff)
death.se.no.disc <- monte.carlo.icer.se(num = 100, rate = 0.00, outcome = death.diff)
  
Unc.int(daly.se.no.disc[[1]][,1])
Unc.int(daly.se.no.disc[[2]][,1])
