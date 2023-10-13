#MAIN SE

## TIDIED UP WHOLE SCRIPT
library(pacman)
p_load(tidyverse, readr, ggplot2, ggrepel, ggfortify, ggthemes, RColorBrewer, ggpubr, cowplot, "scales", EnvStats,hesim, ggbreak, wesanderson)
theme_set(theme_few())

### SE
RD <- load("/Users/Ent/Downloads/ATLAS CEA/SenegalScens20220203.RData")
NoHIVST <- ScenSE[[1]]
ATLASonly <- ScenSE[[2]]
ATLASSU <- ScenSE[[4]]
ATLASSU.SE <- ScenSE[[4]]
country <- "SE"

##SE

scale.fsw.se <- Self.Test(ATLASSU.SE, "FSW")[,6]

fc_se <- 32639+41676+988+39852+70514+43129

SE.fsw.cf <- avg_st_unit_cost(fc_se, 3.08, scale.fsw.se)

st.fsw.dis.se <- mmm(SE.fsw.cf)


scale.msm.se <- Self.Test(ATLASSU.SE, "MSM")[,6]
fc_se_msm <- 23681 + 32696 + 7612 + 85122 + 154677 + 94607
SE.msm.cf <- avg_st_unit_cost(fc_se_msm, 3.08, scale.msm.se)
st.msm.dis.se <- mmm(SE.msm.cf)


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

daly.se <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = DALY.diff)
inf.se <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = inf.diff)
death.se <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = death.diff)

Unc.int(daly.se[[1]][,1])
Unc.int(daly.se[[2]][,1])

