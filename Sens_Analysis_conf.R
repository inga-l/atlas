
Conf.Test.sens <- function(df, pop,timeframe = 20){
  thend <- 51+timeframe
  if(pop == "FSW"){
    PosST <- df[[133]][,1,51:thend] + df[[136]][,1,51:thend]
  }else if(pop == "MSM"){
    PosST <- df[[137]][,1,51:thend] + df[[138]][,1,51:thend]
  }else if(pop == "BOTH"){
    PosST <- df[[133]][,1,51:thend] + df[[136]][,1,51:thend] + df[[137]][,1,51:thend] + df[[138]][,1,51:thend]
  }
  else{
    warning("invalid population input")
  }
  PosST <- as.data.frame(PosST*0.8)
  return(PosST)
}

total.cost.calc.sens <- function(cost.list, rate = 0.04, timeframe){
  
  ## Conv Test
  NbOTFSW_Base <- Conv.Test(NoHIVST, pop = "FSW", timeframe = timeframe)
  CostConvFSW_Base <- discount(NbOTFSW_Base, cost = cost.list[1], rate)
  NbOTMSM_Base <- Conv.Test(NoHIVST, pop = "MSM", timeframe = timeframe)
  CostConvMSM_Base <- discount(NbOTMSM_Base, cost = cost.list[2], rate)
  NbOTGEN_Base <- Conv.Test(NoHIVST, pop = "GEN", timeframe = timeframe)
  CostConvGEN_Base <- discount(NbOTGEN_Base, cost = cost.list[3], rate)
  CostConv_Base <- CostConvFSW_Base + CostConvMSM_Base + CostConvGEN_Base
  
  NbOTFSW_ATLAS <- Conv.Test(ATLASonly, pop = "FSW", timeframe = timeframe)
  CostConvFSW_ATLAS <- discount(NbOTFSW_ATLAS, cost = cost.list[1], rate)
  NbOTMSM_ATLAS <- Conv.Test(ATLASonly, pop = "MSM", timeframe = timeframe)
  CostConvMSM_ATLAS <- discount(NbOTMSM_ATLAS, cost = cost.list[2], rate)
  NbOTGEN_ATLAS <- Conv.Test(ATLASonly, pop = "GEN", timeframe = timeframe)
  CostConvGEN_ATLAS <- discount(NbOTGEN_ATLAS, cost = cost.list[3], rate)
  CostConv_ATLAS <- CostConvFSW_ATLAS + CostConvMSM_ATLAS + CostConvGEN_ATLAS
  
  NbOTFSW_SU <- Conv.Test(ATLASSU, pop = "FSW", timeframe = timeframe)
  CostConvFSW_SU <- discount(NbOTFSW_SU, cost = cost.list[1], rate)
  NbOTMSM_SU <- Conv.Test(ATLASSU, pop = "MSM", timeframe = timeframe)
  CostConvMSM_SU <- discount(NbOTMSM_SU, cost = cost.list[2], rate)
  NbOTGEN_SU <- Conv.Test(ATLASSU, pop = "GEN", timeframe = timeframe)
  CostConvGEN_SU <- discount(NbOTGEN_SU, cost = cost.list[3], rate)
  CostConv_SU <- CostConvFSW_SU + CostConvMSM_SU + CostConvGEN_SU
  
  CostConv_Base <- as.data.frame(CostConv_Base)
  CostConv_ATLAS <- as.data.frame(CostConv_ATLAS)
  CostConv_SU <- as.data.frame(CostConv_SU)
  # names(CostByYear) <- c("Base Scenario", "ATLAS Only", "Scale Up Scenario")
  
  ## Self Test
  NbSTFSW_ATLAS <- Self.Test(df = ATLASonly, pop = "FSW", timeframe = timeframe)
  STFSW_ATLAS <- discount(NbSTFSW_ATLAS, cost = cost.list[4], rate)
  NbSTMSM_ATLAS <- Self.Test(df = ATLASonly, pop = "MSM", timeframe = timeframe)
  STMSM_ATLAS <- discount(NbSTMSM_ATLAS, cost = cost.list[5], rate)
  ST_ATLAS <- STFSW_ATLAS + STMSM_ATLAS
  
  NbSTFSW_SU <- Self.Test(df = ATLASSU, pop = "FSW", timeframe = timeframe)
  STFSW_SU <- discount.su(NbSTFSW_SU, cost = cost.list[4], 
                          cost.22 = cost.list[6], cost.23 = cost.list[7], cost.24 = cost.list[8], 
                          cost.su = cost.list[13], rate)
  NbSTMSM_SU <- Self.Test(df = ATLASSU, pop = "MSM", timeframe = timeframe)
  STMSM_SU <- discount.su(NbSTMSM_SU, cost = cost.list[5],
                          cost.22 = cost.list[9], cost.23 = cost.list[10], cost.24 = cost.list[11], 
                          cost.su = cost.list[14], rate)
  ST_SU <- STFSW_SU + STMSM_SU
  
  ST_ATLAS <- as.data.frame(ST_ATLAS)
  ST_SU <- as.data.frame(ST_SU)
  
  #  names(CostByYear) <- c("ATLAS Only", "Scale Up Scenario")
  
  ## Treatment
  NbTreated_Base <- ART(NoHIVST, timeframe = timeframe)
  NbTreated_ATLAS <- ART(ATLASonly, timeframe = timeframe)
  NbTreated_SU <- ART(ATLASSU, timeframe = timeframe)
  Treated_Base <- discount(df = NbTreated_Base, cost = cost.list[12], rate)
  Treated_ATLAS <- discount(df = NbTreated_ATLAS, cost = cost.list[12], rate)
  Treated_SU <-  discount(df = NbTreated_SU, cost = cost.list[12], rate)
  
  Treated_Base <- as.data.frame(Treated_Base)
  Treated_ATLAS <- as.data.frame(Treated_ATLAS)
  Treated_SU <- as.data.frame(Treated_SU)
  
  #  CostByYear <- list(Treated_Base, Treated_ATLAS, Treated_SU)
  #  names(CostByYear) <- c("Base Scenario", "ATLAS Only", "Scale Up Scenario")
  
  
  ## Conf Test
  NbConf_FSWATLAS <- Conf.Test.sens(ATLASonly, pop = "FSW", timeframe = timeframe)
  Conf_FSWATLAS <- discount(NbConf_FSWATLAS, cost = cost.list[1], rate)
  NbConf_MSMATLAS <- Conf.Test.sens(ATLASonly, pop = "MSM", timeframe = timeframe)
  Conf_MSMATLAS <- discount(NbConf_MSMATLAS, cost = cost.list[2], rate)
  Conf_ATLAS <- Conf_FSWATLAS + Conf_MSMATLAS
  
  NbConf_FSWSU <- Conf.Test.sens(ATLASSU, pop = "FSW", timeframe = timeframe)
  Conf_FSWSU <- discount(NbConf_FSWSU, cost = cost.list[1], rate)
  NbConf_MSMSU <- Conf.Test.sens(ATLASSU, pop = "MSM", timeframe = timeframe)
  Conf_MSMSU <- discount(NbConf_MSMSU, cost = cost.list[2], rate)
  Conf_SU <- Conf_FSWSU + Conf_MSMSU
  
  Conf_ATLAS <- as.data.frame(Conf_ATLAS)
  Conf_SU <- as.data.frame(Conf_SU)
  
  # Conf <- list(Conf_ATLAS, Conf_SU)
  # names(Conf) <- c("ATLAS Only", "Scale Up Scenario")
  
  ## total
  total.by.year.base <- CostConv_Base + Treated_Base
  total.by.year.ATLAS <- CostConv_ATLAS + ST_ATLAS + Treated_ATLAS + Conf_ATLAS
  total.by.year.SU <- CostConv_SU + ST_SU + Treated_SU + Conf_SU
  
  total.list <- list(total.by.year.base, total.by.year.ATLAS, total.by.year.SU)
  names(total.list) <- c("Base Scenario", "ATLAS Only", "Scale Up Scenario")
  return(total.list)
}

### Higher Conf Testing

monte.carlo.icer.ci.sens <- function(num, rate = 0.04, outcome, timeframe = 20){ # time frame = 2019-2039
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
  ART <- rtri(num, min = 102.63, max = 232.60, mode = 198.4) # (65*0.9+227*0.1) /0.8 = 81.25 #1L 2L (165.34*0.9+332.1*0.1)*1.09 # max: (188.66*0.9+436*0.1)*1.09
  STFSWSU <- rtri(num, min = st.fsw.dis.ci[1], max = st.fsw.dis.ci[2], mode = st.fsw.dis.ci[3])
  STMSMSU <- rtri(num, min = st.msm.dis.ci[1], max = st.msm.dis.ci[2], mode = st.msm.dis.ci[3])
  for(i in 1:num){
    cost.list <- c(OTFSW[i], OTMSM[i],OTGEN[i], STFSW[i], STMSM[i],
                   STFSW.22[i], STFSW.23[i], STFSW.24[i],
                   STMSM.22[i], STMSM.23[i], STMSM.24[i],
                   ART[i], STFSWSU[i], STMSMSU[i])
    list.by.scenario <- total.cost.calc.sens(cost.list, rate, timeframe = timeframe)
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

monte.carlo.icer.ma.sens <- function(num, rate = 0.04, outcome, timeframe = 20){ # time frame = 2019-2039
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
  
  STFSWSU <- rtri(num, min = st.fsw.dis.ma[1], max = st.fsw.dis.ma[2], mode = st.fsw.dis.ma[3]) # MA
  STMSMSU <- rtri(num, min = st.msm.dis.ma[1], max = st.msm.dis.ma[2], mode = st.msm.dis.ma[3]) # MA
  for(i in 1:num){
    cost.list <- c(OTFSW[i], OTMSM[i],OTGEN[i], STFSW[i], STMSM[i],
                   STFSW.22[i], STFSW.23[i], STFSW.24[i],
                   STMSM.22[i], STMSM.23[i], STMSM.24[i],
                   ART[i], STFSWSU[i], STMSMSU[i])
    list.by.scenario <- total.cost.calc.sens(cost.list, rate, timeframe = timeframe)
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

monte.carlo.icer.se.sens <- function(num, rate = 0.04, outcome, timeframe = 20){ # time frame = 2019-2039
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
  ART <- rtri(num, min = 102.63, max = 232.60, mode = 198.4) # (65*0.9+227*0.1) /0.8 = 81.25 #1L 2L (165.34*0.9+332.1*0.1)*1.09 # max: (188.66*0.9+436*0.1)*1.09
  STFSWSU <- rtri(num, min = st.fsw.dis.se[1], max = st.fsw.dis.se[2], mode = st.fsw.dis.se[3])# SE
  STMSMSU <- rtri(num, min = st.msm.dis.se[1], max = st.msm.dis.se[2], mode = st.msm.dis.se[3])  #SE
  for(i in 1:num){
    cost.list <- c(OTFSW[i], OTMSM[i],OTGEN[i], STFSW[i], STMSM[i],
                   STFSW.22[i], STFSW.23[i], STFSW.24[i],
                   STMSM.22[i], STMSM.23[i], STMSM.24[i],
                   ART[i], STFSWSU[i], STMSMSU[i])
    list.by.scenario <- total.cost.calc.sens(cost.list, rate, timeframe = timeframe)
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

daly.ci.conf <- monte.carlo.icer.ci.sens(num = 100, rate = 0.04, outcome = DALY.diff)
inf.ci.conf <- monte.carlo.icer.ci.sens(num = 100, rate = 0.04, outcome = inf.diff)
death.ci.conf <- monte.carlo.icer.ci.sens(num = 100, rate = 0.04, outcome = death.diff)
Unc.int(daly.ci.conf[[1]][,1])
Unc.int(daly.ci.conf[[2]][,1])

daly.ma.conf <- monte.carlo.icer.ma.sens(num = 100, rate = 0.04, outcome = DALY.diff)
inf.ma.conf <- monte.carlo.icer.ma.sens(num = 100, rate = 0.04, outcome = inf.diff)
death.ma.conf <- monte.carlo.icer.ma.sens(num = 100, rate = 0.04, outcome = death.diff)

Unc.int(daly.ma.conf[[1]][,1])
Unc.int(daly.ma.conf[[2]][,1])

daly.se.conf <- monte.carlo.icer.se.sens(num = 100, rate = 0.04, outcome = DALY.diff)
inf.se.conf <- monte.carlo.icer.se.sens(num = 100, rate = 0.04, outcome = inf.diff)
death.se.conf <- monte.carlo.icer.se.sens(num = 100, rate = 0.04, outcome = death.diff)

Unc.int(daly.se.conf[[1]][,1])
Unc.int(daly.se.conf[[2]][,1])
