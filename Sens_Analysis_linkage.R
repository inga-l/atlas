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

monte.carlo.icer.ci <- function(num, rate = 0.04, outcome, timeframe = 20){ # time frame = 2019-2039
  set.seed(34242)
  # 5 costs
  OTFSW <- rtri(num, min = 13.10, max = 43.40, mode = 17.54)
  OTMSM <- rtri(num, min = 15.16, max = 28.26, mode = 22.68)
  OTGEN <- rtri(num, min = 7.67, max = 40.22, mode = 8.31)
  STFSW <- rtri(num, min = 11.9, max = 16.7, mode = 13) # OBSERVED INTERVENTION COST for 2019, 2020
  STMSM <- rtri(num, min = 13.9, max = 19.2, mode = 15)
  STFSW.22 <- rtri(num, min = 9.53, max = 10.80, mode = 10.17)
  STFSW.23 <- rtri(num, min = 8.22, max = 9.40, mode = 8.81)
  STFSW.24 <- rtri(num, min = 7.8, max = 8.98, mode = 8.39)
  STMSM.22 <- rtri(num, min = 9.22, max = 10.97, mode = 10.09)
  STMSM.23 <- rtri(num, min = 8.00, max = 9.69, mode = 8.85)
  STMSM.24 <- rtri(num, min = 7.64, max = 9.34, mode = 8.49)
  ART <- rtri(num, min = 102.63, max = 232.60, mode = 198.4) # (65*0.9+227*0.1) /0.8 = 81.25 #1L 2L (165.34*0.9+332.1*0.1)*1.09 # max: (188.66*0.9+436*0.1)*1.09
  STFSWSU <- rtri(num, min = st.fsw.dis.ci[1], max = st.fsw.dis.ci[2], mode = st.fsw.dis.ci[3])
  STMSMSU <- rtri(num, min = st.msm.dis.ci[1], max = st.msm.dis.ci[2], mode = st.msm.dis.ci[3])
  for(i in 1:num){
    cost.list <- c(OTFSW[i], OTMSM[i],OTGEN[i], STFSW[i], STMSM[i],
                   STFSW.22[i], STFSW.23[i], STFSW.24[i],
                   STMSM.22[i], STMSM.23[i], STMSM.24[i],
                   ART[i], STFSWSU[i], STMSMSU[i])
    list.by.scenario <- total.cost.calc.updated(cost.list, rate, timeframe = 20)
    compared.cost <- compare.costs(list.by.scenario)
    temp.atlas <- icer(compared.cost[[1]], outcome[[1]][,1:(timeframe+1)]) %>% as.data.frame()
    temp.su <- icer(compared.cost[[2]], outcome[[2]][ ,1:(timeframe+1)]) %>% as.data.frame()
    if(i == 1){
      icer.atlas <- temp.atlas
      icer.su <- temp.su
    }else{
      icer.atlas <- rbind(icer.atlas, temp.atlas)
      icer.su <- rbind(icer.su, temp.su)
    }
  }
  icer.list <- list(icer.atlas, icer.su)
  names(icer.list) <- c("atlas icer", "su icer")
  return(icer.list)
}

monte.carlo.icer.ma <- function(num, rate = 0.04, outcome, timeframe = 20){ # time frame = 2019-2039
  set.seed(34242)
  # 5 costs
  OTFSW <- rtri(num, min = 13.10, max = 43.40, mode = 17.54)
  OTMSM <- rtri(num, min = 15.16, max = 28.26, mode = 22.68)
  OTGEN <- rtri(num, min = 7.67, max = 40.22, mode = 8.31)
  STFSW.22 <- rtri(num, min = 9.73, max = 11.50, mode = 10.61) # MA
  STFSW.23 <- rtri(num, min = 9.00, max = 10.66, mode = 9.83)
  STFSW.24 <- rtri(num, min = 8.77, max = 10.41, mode = 9.59)
  STMSM.22 <- rtri(num, min = 16.83, max = 19.20, mode = 18.02)
  STMSM.23 <- rtri(num, min = 15.43, max = 17.71, mode = 16.57)
  STMSM.24 <- rtri(num, min = 14.99, max = 17.25, mode = 16.12)

  STFSW <- rtri(num, min = 14.4, max = 16.8, mode = 16) #MA
  STMSM <- rtri(num, min = 25.5, max = 29.3, mode = 28) #MA

  ART <- rtri(num, min = 102.63, max = 232.60, mode = 198.4) # (65*0.9+227*0.1) /0.8 = 81.25 #1L 2L (165.34*0.9+332.1*0.1)*1.09 # max: (188.66*0.9+436*0.1)*1.09

  STFSWSU <- rtri(num, min = st.fsw.dis.ma[1], max = st.fsw.dis.ma[2], mode = st.fsw.dis.ma[3]) # MA
  STMSMSU <- rtri(num, min = st.msm.dis.ma[1], max = st.msm.dis.ma[2], mode = st.msm.dis.ma[3]) # MA
  for(i in 1:num){
    cost.list <- c(OTFSW[i], OTMSM[i],OTGEN[i], STFSW[i], STMSM[i],
                   STFSW.22[i], STFSW.23[i], STFSW.24[i],
                   STMSM.22[i], STMSM.23[i], STMSM.24[i],
                   ART[i], STFSWSU[i], STMSMSU[i])
    list.by.scenario <- total.cost.calc.updated(cost.list, rate, timeframe = timeframe)
    compared.cost <- compare.costs(list.by.scenario)
    temp.atlas <- icer(compared.cost[[1]], outcome[[1]][,1:(timeframe+1)]) %>% as.data.frame()
    temp.su <- icer(compared.cost[[2]], outcome[[2]][ ,1:(timeframe+1)]) %>% as.data.frame()
    if(i == 1){
      icer.atlas <- temp.atlas
      icer.su <- temp.su
    }else{
      icer.atlas <- rbind(icer.atlas, temp.atlas)
      icer.su <- rbind(icer.su, temp.su)
    }
  }
  icer.list <- list(icer.atlas, icer.su)
  names(icer.list) <- c("atlas icer", "su icer")
  return(icer.list)
}

monte.carlo.icer.se <- function(num, rate = 0.04, outcome, timeframe = 20){ # time frame = 2019-2039
  set.seed(34242)
  # 5 costs
  OTFSW <- rtri(num, min = 13.10, max = 43.40, mode = 17.54)
  OTMSM <- rtri(num, min = 15.16, max = 28.26, mode = 22.68)
  OTGEN <- rtri(num, min = 7.67, max = 40.22, mode = 8.31)
  STFSW.22 <- rtri(num, min = 12.42, max = 14.19, mode = 13.30) # SE
  STFSW.23 <- rtri(num, min = 11.87, max = 13.52, mode = 12.69)
  STFSW.24 <- rtri(num, min = 11.67, max = 13.32, mode = 12.50)
  STMSM.22 <- rtri(num, min = 22.53, max = 25.99, mode = 24.26)
  STMSM.23 <- rtri(num, min = 21.21, max = 24.00, mode = 22.60)
  STMSM.24 <- rtri(num, min = 20.83, max = 23.75, mode = 22.29)
  STFSW <- rtri(num, min = 16.7, max = 19, mode = 17.1) #SE
  STMSM <- rtri(num, min = 26.1, max = 30.2, mode = 27) #SE
  ART <- rtri(num, min = 102.63, max = 232.60, mode = 198.4) # (65*0.9+227*0.1) /0.8 = 81.25 #1L 2L (165.34*0.9+332.1*0.1)*1.09 # max: (188.66*0.9+436*0.1)*1.09
  STFSWSU <- rtri(num, min = st.fsw.dis.se[1], max = st.fsw.dis.se[2], mode = st.fsw.dis.se[3])# SE
  STMSMSU <- rtri(num, min = st.msm.dis.se[1], max = st.msm.dis.se[2], mode = st.msm.dis.se[3])  #SE
  for(i in 1:num){
    cost.list <- c(OTFSW[i], OTMSM[i],OTGEN[i], STFSW[i], STMSM[i],
                   STFSW.22[i], STFSW.23[i], STFSW.24[i],
                   STMSM.22[i], STMSM.23[i], STMSM.24[i],
                   ART[i], STFSWSU[i], STMSMSU[i])
    list.by.scenario <- total.cost.calc.updated(cost.list, rate, timeframe = timeframe)
    compared.cost <- compare.costs(list.by.scenario)
    temp.atlas <- icer(compared.cost[[1]], outcome[[1]][,1:(timeframe+1)]) %>% as.data.frame()
    temp.su <- icer(compared.cost[[2]], outcome[[2]][ ,1:(timeframe+1)]) %>% as.data.frame()
    if(i == 1){
      icer.atlas <- temp.atlas
      icer.su <- temp.su
    }else{
      icer.atlas <- rbind(icer.atlas, temp.atlas)
      icer.su <- rbind(icer.su, temp.su)
    }
  }
  icer.list <- list(icer.atlas, icer.su)
  names(icer.list) <- c("atlas icer", "su icer")
  return(icer.list)
}

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



