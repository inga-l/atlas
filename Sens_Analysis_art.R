## ART Price
monte.carlo.icer.ci.art <- function(num, rate = 0.04, outcome, timeframe = 20){ # time frame = 2019-2039
  set.seed(34242)
  # 5 costs
  OTFSW <- rtri(num, min = 13.10, max = 43.40, mode = 17.54)
  OTMSM <- rtri(num, min = 15.16, max = 28.26, mode = 22.68)
  OTGEN <- rtri(num, min = 7.67, max = 40.22, mode = 8.31)
  STFSW <- rep(17, num) # OBSERVED INTERVENTION COST for 2019, 2020
  STMSM <- rep(27, num) #
  STFSW.22 <- rtri(num, min = 12.47, max = 17.74, mode = 13.30)
  STFSW.23 <- rtri(num, min = 11.07, max = 16.93, mode = 12.69)
  STFSW.24 <- rtri(num, min = 9.90, max = 17.85, mode = 12.50)
  STMSM.22 <- rtri(num, min = 22.77, max = 32.35, mode = 24.26)
  STMSM.23 <- rtri(num, min = 20.93, max = 30.14, mode = 22.60)
  STMSM.24 <- rtri(num, min = 19.98, max = 31.85, mode = 22.29)
  ART <- rtri(num, min = 102.63, max = 609.22, mode = 232.60) # (65*0.9+227*0.1) /0.8 = 81.25 #1L 2L (165.34*0.9+332.1*0.1)*1.09 # max: (188.66*0.9+436*0.1)*1.09
  STFSWSU <- rtri(num, min = st.fsw.dis.ci[1], max = st.fsw.dis.ci[2], mode = st.fsw.dis.ci[3])
  STMSMSU <- rtri(num, min = st.msm.dis.ci[1], max = st.msm.dis.ci[2], mode = st.msm.dis.ci[3])
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

monte.carlo.icer.ma.art <- function(num, rate = 0.04, outcome, timeframe = 20){ # time frame = 2019-2039
  set.seed(34242)
  # 5 costs
  OTFSW <- rtri(num, min = 13.10, max = 43.40, mode = 17.54)
  OTMSM <- rtri(num, min = 15.16, max = 28.26, mode = 22.68)
  OTGEN <- rtri(num, min = 7.67, max = 40.22, mode = 8.31)
  STFSW.22 <- rtri(num, min = 9.93, max = 13.26, mode = 10.61) # MA
  STFSW.23 <- rtri(num, min = 8.79, max = 12.29, mode = 9.83)
  STFSW.24 <- rtri(num, min = 7.99, max = 12.79, mode = 9.59)
  STMSM.22 <- rtri(num, min = 17.34, max = 22.52, mode = 18.02)
  STMSM.23 <- rtri(num, min = 14.62, max = 20.71, mode = 16.57)
  STMSM.24 <- rtri(num, min = 13.12, max = 21.49, mode = 16.12)
  
  STFSW <- rtri(num, min = 15, max = 27, mode = 16) #MA
  STMSM <- rtri(num, min = 17, max = 59, mode = 28) #MA
  
  ART <- rtri(num, min = 102.63, max = 609.22, mode = 232.60) # (65*0.9+227*0.1) /0.8 = 81.25 #1L 2L (165.34*0.9+332.1*0.1)*1.09 # max: (188.66*0.9+436*0.1)*1.09
  
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

monte.carlo.icer.se.art <- function(num, rate = 0.04, outcome, timeframe = 20){ # time frame = 2019-2039
  set.seed(34242)
  # 5 costs
  OTFSW <- rtri(num, min = 13.10, max = 43.40, mode = 17.54)
  OTMSM <- rtri(num, min = 15.16, max = 28.26, mode = 22.68)
  OTGEN <- rtri(num, min = 7.67, max = 40.22, mode = 8.31)
  STFSW.22 <- rtri(num, min = 12.47, max = 17.74, mode = 13.30) # SE
  STFSW.23 <- rtri(num, min = 11.07, max = 16.93, mode = 12.69)
  STFSW.24 <- rtri(num, min = 9.90, max = 17.85, mode = 12.50)
  STMSM.22 <- rtri(num, min = 22.77, max = 32.35, mode = 24.26)
  STMSM.23 <- rtri(num, min = 20.93, max = 30.14, mode = 22.60)
  STMSM.24 <- rtri(num, min = 19.98, max = 31.85, mode = 22.29)
  STFSW <- rtri(num, min = 13, max = 32, mode = 17) #SE
  STMSM <- rtri(num, min = 25, max = 28, mode = 27) #SE
  ART <- rtri(num, min = 102.63, max = 609.22, mode = 232.60) # (65*0.9+227*0.1) /0.8 = 81.25 #1L 2L (165.34*0.9+332.1*0.1)*1.09 # max: (188.66*0.9+436*0.1)*1.09
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


### HIVST usage
## All kits used, Scn 40, 42
RD <- load("/Users/Ent/Downloads/ATLAS CEA/CoteIvoireScens20220201.RData")
NoHIVST <- ScenCI[[1]]
ATLASonly.art.CI <- ScenCI[[2]]
ATLASSU.art.CI <- ScenCI[[4]]
country <- "CI"

### MA
RD <- load("/Users/Ent/Downloads/ATLAS CEA/MaliScens20220131.RData")
NoHIVST <- ScenMA[[1]]
ATLASonly.art.MA <- ScenMA[[2]]
ATLASSU.art.MA <- ScenMA[[4]]
country <- "MA"
NoHIVST <- ScenMA[[1]]
ATLASonly <- ScenMA[[2]]
ATLASSU <- ScenMA[[4]]
country <- "MA"

### SE
RD <- load("/Users/Ent/Downloads/ATLAS CEA/SenegalScens20220203.RData")
NoHIVST <- ScenSE[[1]]
ATLASonly.art.SE <- ScenSE[[2]]
ATLASSU.art.SE <- ScenSE[[4]]
country <- "SE"

### DALY.diff
DALY.base <- DALY.Calc(NoHIVST, country = country)
DALY.base.dis <- DALY.Calc(NoHIVST, country = country)

DALY.ATLAS <- DALY.Calc(ATLASonly.art.MA, country = country)
DALY.ATLAS.dis <- DALY.Calc(ATLASonly.art.MA, country = country)

DALY.SU <- DALY.Calc(ATLASSU.art.MA, country = country)
DALY.SU.dis <- DALY.Calc(ATLASSU.art.MA, country = country)

DALY.diff <- compare(DALY.base, DALY.ATLAS, DALY.SU, averted = TRUE)
DALY.total.diff <- list(rowSums(DALY.diff[[1]][1:21]), rowSums(DALY.diff[[2]][1:21])) 
names(DALY.total.diff) <- c("ATLAS vs Base", "Scale up vs Base")
DALY.diff <- list(DALY.diff[[1]][1:21], DALY.diff[[2]][1:21])
names(DALY.diff) <- c("ATLAS vs Base", "Scale up vs Base")

## AIDS Death

death <- AIDS.deaths(NoHIVST, timeframe = 20)
death.base <- death[[1]][[1]]
death.base.dis <- death[[1]][[2]]

death <- AIDS.deaths(ATLASonly.art.MA, timeframe = 20)
death.ATLAS <- death[[1]][[1]]
death.ATLAS.dis <- death[[1]][[2]]

death <- AIDS.deaths(ATLASSU.art.MA, timeframe = 20)
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

inf <- AIDS.inf(ATLASonly.art.MA, timeframe = 20)
inf.ATLAS <- inf[[1]][[1]]
inf.ATLAS.dis <- inf[[1]][[2]]

inf <- AIDS.inf(ATLASSU.art.MA, timeframe = 20)
inf.SU <- inf[[1]][[1]]
inf.SU.dis <- inf[[1]][[2]]

inf.diff <- compare(inf.base, inf.ATLAS, inf.SU, averted = TRUE)
names(inf.diff) <- c("ATLAS vs Base", "Scale up vs Base")

daly.ci.art <- monte.carlo.icer.ci.art(num = 100, rate = 0.04, outcome = DALY.diff)
inf.ci.art <- monte.carlo.icer.ci.art(num = 100, rate = 0.04, outcome = inf.diff)
death.ci.art <- monte.carlo.icer.ci.art(num = 100, rate = 0.04, outcome = death.diff)

Unc.int(daly.ci.art[[1]][,1])
Unc.int(daly.ci.art[[2]][,1])

daly.ma.art <- monte.carlo.icer.ma.art(num = 100, rate = 0.04, outcome = DALY.diff)
inf.ma.art <- monte.carlo.icer.ma.art(num = 100, rate = 0.04, outcome = inf.diff)
death.ma.art <- monte.carlo.icer.ma.art(num = 100, rate = 0.04, outcome = death.diff)

Unc.int(daly.ma.art[[1]][,1])
Unc.int(daly.ma.art[[2]][,1])

daly.se.art <- monte.carlo.icer.se.art(num = 100, rate = 0.04, outcome = DALY.diff)
inf.se.art <- monte.carlo.icer.se.art(num = 100, rate = 0.04, outcome = inf.diff)
death.se.art <- monte.carlo.icer.se.art(num = 100, rate = 0.04, outcome = death.diff)

Unc.int(daly.se.art[[1]][,1])
Unc.int(daly.se.art[[2]][,1])

monte.carlo.cost.ma <- function(num, rate = 0.04, timeframe = 20){ # time frame = 2020-2040
  set.seed(34242)
  # 5 costs
  OTFSW <- rtri(num, min = 13.10, max = 43.40, mode = 17.54)
  OTMSM <- rtri(num, min = 15.16, max = 28.26, mode = 22.68)
  OTGEN <- rtri(num, min = 7.67, max = 40.22, mode = 8.31)
  STFSW.22 <- rtri(num, min = 9.93, max = 13.26, mode = 10.61) # MA
  STFSW.23 <- rtri(num, min = 8.79, max = 12.29, mode = 9.83)
  STFSW.24 <- rtri(num, min = 7.99, max = 12.79, mode = 9.59)
  STMSM.22 <- rtri(num, min = 17.34, max = 22.52, mode = 18.02)
  STMSM.23 <- rtri(num, min = 14.62, max = 20.71, mode = 16.57)
  STMSM.24 <- rtri(num, min = 13.12, max = 21.49, mode = 16.12)

  STFSW <- rtri(num, min = 15, max = 27, mode = 16) #MA
  STMSM <- rtri(num, min = 17, max = 59, mode = 28) #MA
  ART <- rtri(num, min = 102.63, max = 232.60, mode = 198.4) # (65*0.9+227*0.1) /0.8 = 81.25 #1L 2L (165.34*0.9+332.1*0.1)*1.09 # max: (188.66*0.9+436*0.1)*1.09
  
  STFSWSU <- rtri(num, min = st.fsw.dis.ma[1], max = st.fsw.dis.ma[2], mode = st.fsw.dis.ma[3])
  STMSMSU <- rtri(num, min = st.msm.dis.ma[1], max = st.msm.dis.ma[2], mode = st.msm.dis.ma[3])

  for(i in 1:num){
    cost.list <- c(OTFSW[i], OTMSM[i],OTGEN[i], STFSW[i], STMSM[i],
                   STFSW.22[i], STFSW.23[i], STFSW.24[i],
                   STMSM.22[i], STMSM.23[i], STMSM.24[i],
                   ART[i], STFSWSU[i], STMSMSU[i])
    list.by.scenario <- total.cost.calc.updated(cost.list, rate,timeframe = timeframe)
    
    
    temp.base <- list.by.scenario[[1]] %>% as.data.frame()
    temp.atlas <- list.by.scenario[[2]] %>% as.data.frame()
    temp.su <- list.by.scenario[[3]] %>% as.data.frame()
    
    if(i == 1){
      cost.base <- temp.base
      cost.atlas <- temp.atlas
      cost.su <- temp.su
    }else{
      cost.base <- rbind(cost.base, temp.base)
      cost.atlas <- rbind(cost.atlas, temp.atlas)
      cost.su <- rbind(cost.su, temp.su)
    }
  }
  cost.list <- list(cost.base, cost.atlas, cost.su)
  names(cost.list) <- c("base", "atlas", "su icer")
  return(cost.list)
}
cost.ma.sens.art <- monte.carlo.cost.ma(100, rate = 0.04, timeframe = 20)
cost.ma.sens.art[[3]] - cost.ma.sens.art[[1]]
