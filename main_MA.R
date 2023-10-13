#MAIN MA

## TIDIED UP WHOLE SCRIPT
library(pacman)
p_load(tidyverse, readr, ggplot2, ggrepel, ggfortify, ggthemes, RColorBrewer, ggpubr, cowplot, "scales", EnvStats,hesim, ggbreak, wesanderson)
theme_set(theme_few())

### MA
RD <- load("/Users/Ent/Downloads/ATLAS CEA/MaliScens20220131.RData")
NoHIVST <- ScenMA[[1]]
ATLASonly <- ScenMA[[2]]
ATLASSU <- ScenMA[[4]]
ATLASSU.MA <- ScenMA[[4]]
country <- "MA"

## MA
scale.fsw.ma <- Self.Test(ATLASSU.MA, "FSW")[,6]

fc_ma <- 116572+126200+5622+308143+153731+125814

MA.fsw.cf <- avg_st_unit_cost(fc_ma, 3.08, scale.fsw.ma)

st.fsw.dis.ma <- mmm(MA.fsw.cf)

### MA

scale.msm.ma <- Self.Test(ATLASSU.MA, "MSM")[,6]
fc_ma_msm <- 52548 + 58366 + 2554 + 138943 + 71049 + 85964
MA.msm.cf <- avg_st_unit_cost(fc_ma_msm, 3.08, scale.msm.ma)
st.msm.dis.ma <- mmm(MA.msm.cf)


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


daly.ma <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = DALY.diff)

inf.ma <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = inf.diff)
death.ma <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = death.diff)

Unc.int(daly.ma[[1]][,1])
Unc.int(daly.ma[[2]][,1])