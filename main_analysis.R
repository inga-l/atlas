## TIDIED UP WHOLE SCRIPT
library(pacman)
p_load(tidyverse, readr, ggplot2, ggrepel, ggfortify, ggthemes, RColorBrewer, ggpubr, cowplot, "scales", EnvStats,hesim, ggbreak, wesanderson, writexl, openxlsx, lzma)
theme_set(theme_few())

#LOADING DB----------

### CI
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


## Cost Function

## FSW

## CI

scale.fsw.ci <- Self.Test(ATLASSU.CI, "FSW")[,6]

fc_ci <- 62455+88073+100175+37611+653937+50155

CI.fsw.cf <- avg_st_unit_cost(fc_ci, 2.63, scale.fsw.ci)


st.fsw.dis.ci <- mmm(CI.fsw.cf) # obtained the average unit cost of ST after scale up, integrating the cost function

## MA
scale.fsw.ma <- Self.Test(ATLASSU.MA, "FSW")[,6]

fc_ma <- 116572+126200+5622+308143+153731+125814

MA.fsw.cf <- avg_st_unit_cost(fc_ma, 3.08, scale.fsw.ma)

st.fsw.dis.ma <- mmm(MA.fsw.cf)

##SE

scale.fsw.se <- Self.Test(ATLASSU.SE, "FSW")[,6]

fc_se <- 32639+41676+988+39852+70514+43129

SE.fsw.cf <- avg_st_unit_cost(fc_se, 3.08, scale.fsw.se)

st.fsw.dis.se <- mmm(SE.fsw.cf)

## MSM

### CI
scale.msm.ci <- Self.Test(ATLASSU.CI, "MSM")[,6]
fc_ci_msm <- 27766 + 39347 + 52436 + 21845 + 324112 + 27262
CI.msm.cf <- avg_st_unit_cost(fc_ci_msm, 2.63, scale.msm.ci)
st.msm.dis.ci <- mmm(CI.msm.cf)

### MA

scale.msm.ma <- Self.Test(ATLASSU.MA, "MSM")[,6]
fc_ma_msm <- 52548 + 58366 + 2554 + 138943 + 71049 + 85964
MA.msm.cf <- avg_st_unit_cost(fc_ma_msm, 3.08, scale.msm.ma)
st.msm.dis.ma <- mmm(MA.msm.cf)


### SE

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


daly.ci <- monte.carlo.icer.ci(num = 100, rate = 0.04, outcome = DALY.diff)
inf.ci <- monte.carlo.icer.ci(num = 100, rate = 0.04, outcome = inf.diff)
death.ci <- monte.carlo.icer.ci(num = 100, rate = 0.04, outcome = death.diff)

mmm(death.ci[[1]][,3])

Unc.int(daly.ci[[2]][,1])
Unc.int(death.ma[[1]][,1])
Unc.int(d.se[[1]][,1])

Unc.int(inf.ci[[2]][,1])
Unc.int(daly.ma[[2]][,1])
Unc.int(inf.se[[2]][,1])

Unc.int(death.ci[[2]][,1])
Unc.int(death.ma[[2]][,1])
Unc.int(death.se[[2]][,1])

Unc.int(daly.ci[[1]][,2])
Unc.int(daly.ma[[1]][,2])
Unc.int(daly.se[[1]][,2])

  daly.ma <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = DALY.diff)
  inf.ma <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = inf.diff)
  death.ma <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = death.diff)

mmm(inf.ma[[1]][,1])
mmm(inf.ma[[2]][,1])
mmm(death.ma[[1]][,1])
mmm(death.ma[[2]][,1])

daly.se <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = DALY.diff)
inf.se <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = inf.diff)
death.se <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = death.diff)

mmm(inf.se[[1]][,1])
mmm(inf.se[[2]][,1])
mmm(death.se[[1]][,1])
mmm(death.se[[2]][,1])

# COST
cost.ci <- monte.carlo.cost.ci(num = 100, rate = 0.04)
median.cost.ci.nohivst <- apply(cost.ci[[1]],2, median) #median cost by year
median.cost.ci.atlas <- apply(cost.ci[[2]],2, median)
median.cost.ci.su <- apply(cost.ci[[3]],2, median)

cost.ma <- monte.carlo.cost.ma(num = 100, rate = 0.04)

median.cost.ma.nohivst <- apply(cost.ma[[1]],2,median) #median cost by year
median.cost.ma.atlas <- apply(cost.ma[[2]],2,median)
median.cost.ma.su <- apply(cost.ma[[3]],2,median)

cost.se <- monte.carlo.cost.se(num = 100, rate = 0.04)

median.cost.se.nohivst <- apply(cost.se[[1]],2,median) #median cost by year
median.cost.se.atlas <- apply(cost.se[[2]],2,median)
median.cost.se.su <- apply(cost.se[[3]],2,median)

total.ui(cost.ci[[1]]) # total cost by simulation, then median
total.ui(cost.ci[[2]])
total.ui(cost.ci[[3]])

total.ui(cost.ma[[1]]) # total cost by simulation, then median
total.ui(cost.ma[[2]])
total.ui(cost.ma[[3]])

total.ui(cost.se[[1]]) # total cost by simulation, then median
total.ui(cost.se[[2]])
total.ui(cost.se[[3]])

total.ui(DALY.diff[[1]]) 
total.ui(DALY.diff[[2]]) 

