## FUNCTIONS

### DISCOUNTING FUNCTION

discount <- function(df, rate = 0.04, cost = 1){
  DISCOUNTED <- matrix(nrow = nrow(df), ncol = ncol(df))
  for(i in 1:nrow(df)){
    DISCOUNTED[,1] <- df[ ,1]*cost
    for(j in 2:ncol(df)){
      DISCOUNTED[i,j] <- (df[i,j] * cost)/(1+rate)^(j-1)
    }
  }
  return(DISCOUNTED)
}

## Cost function

avg_st_unit_cost<- function(fc, uc, s){
  total <- fc+uc*s
  output <- total/s
  return(output)
}

mmm <- function(df){
  min <- min(df)
  max <- max(df)
  median <- median(df)
  output <- c(min, max, median)
  return(output)
} ## numeric output of triangular distributio

## DISCOUNT SCALED UP
discount.su <- function(df, cost, cost.22, cost.23, cost.24, cost.su, rate = 0.04){
  DISCOUNTED <- matrix(nrow = nrow(df), ncol = ncol(df))
  for(i in 1:nrow(df)){ # row = each simulation
    DISCOUNTED[,1] <- df[ ,1]*cost # initial cost at 2019
    for(j in 2:ncol(df)){
      if(j <=5){
        DISCOUNTED[i,2] <- (df[i,2] * cost)/(1+rate)^(1)
        DISCOUNTED[i,3] <- (df[i,3] * cost.22)/(1+rate)^(2)
        DISCOUNTED[i,4] <- (df[i,4] * cost.23)/(1+rate)^(3)
        DISCOUNTED[i,5] <- (df[i,5] * cost.24)/(1+rate)^(4) # j = year
      }
      else{
        DISCOUNTED[i,j] <- (df[i,j] * cost.su)/(1+rate)^(j-1)
      }
    }
  }
  return(DISCOUNTED)
}


### DIFF

compare <- function(base, atlas, su, discount = FALSE, cost = 1, rate = 0.03, averted){
  base <- as.data.frame(base)
  atlas <- as.data.frame(atlas)
  su <- as.data.frame(su)
  if(discount == FALSE){
    atlas <- atlas*cost
    base <- base*cost
    su <- su*cost
    ATLASDIFF <- atlas - base
    SUDIFF <- su - base
  }
  else{
    ATLASDIFF <- discount(atlas, rate, cost) - discount(base, rate, cost)
    SUDIFF <- discount(su, rate, cost) - discount(base, rate, cost)
  }
  
  if(averted == TRUE){
    output <- list(ATLASDIFF*-1, SUDIFF*-1)}
  else{
    output <- list(ATLASDIFF, SUDIFF)
  }
  
  names(output) <- c("ATLAS vs Base", "Scale up vs Base")
  return(output)
}

compare.costs <- function(list){
  base <- as.data.frame(list[[1]])
  atlas <- as.data.frame(list[[2]])
  su <- as.data.frame(list[[3]])
  
  ATLASDIFF <- atlas - base
  SUDIFF <- su - base
  
  output <- list(ATLASDIFF, SUDIFF)
  
  names(output) <- c("ATLAS vs Base", "Scale up vs Base")
  return(output)
}

#### MEDIAN and CUMULATIVE ###

med.cum <- function(double, rate = 0.03, cost = 1){
  df <- as.data.frame(double)
  med <- apply(df, 2, median)
  cum <- cumsum(med)
  dis <- discount(df, rate, cost)
  dis.med <- apply(dis, 2, median)
  dis.cum <- cumsum(dis.med)
  
  list.by.year <- list(df, dis)
  names(list.by.year) <- c("All Sims By Year", "All Sims By Year Discounted")
  list.med <- list(med, dis.med)
  names(list.med) <- c("Median", "Median Discounted")
  list.cum <- list(cum, dis.cum)
  names(list.cum) <- c("Cumulative", "Cumulative Discounted")
  list <- list(list.by.year, list.med, list.cum)
  return(list)
}

## OUTCOMES

##### OUTCOMES #####

## DALY Calc
LE_CI <- c(46.65, 42.41, 30.60, 18.04) 
LE_MA <- c(49.94, 45.75, 33.40, 19.22)
LE_SE <- c(53.92, 35.80, 30.60, 20.83)
DisStage <- c(0.012,0.274,0.582, 0.078)

# NbDiseaseStage <- ATLASonly[[143]][, 1, 50:71] %>% as.data.frame()
# write_csv(NbDiseaseStage, "/Users/Ent/Desktop/BYDISEASESTAGE.csv")

DALY.Calc <- function(list, country, discount = FALSE, rate = 0.04){
  if(country == "CI"){
    LE <- LE_CI
  }
  else if(country == "MA"){
    LE <- LE_MA
  }
  else if(country == "SE"){
    LE <- LE_SE
  }
  else{
    warning("invalid population input")
  }
  YLL_ByAge <- list()   
  YLD_ByStage <- list()
  for(i in 2:5){
    j <- i-1
    YLL_temp <- list[[142]][, i, 51:71] * LE[j]
    YLL_ByAge[[j]] <- YLL_temp
  }
  YLL <- YLL_ByAge[[1]]+YLL_ByAge[[2]]+YLL_ByAge[[3]]+YLL_ByAge[[4]]
  
  for(i in 2:5){
    j <- i-1
    YLD_temp <- list[[143]][, i, 51:71] * DisStage[j]
    YLD_ByStage[[j]] <- YLD_temp
  }
  YLD <- YLD_ByStage[[1]]+YLD_ByStage[[2]]+YLD_ByStage[[3]]+YLD_ByStage[[4]]
  DALY.by.year <- YLL + YLD
  if(discount == FALSE){
    DALY.total <- rowSums(DALY.by.year)
  }
  else{
    DALY.by.year <- discount(DALY.by.year, rate)
    DALY.total <- rowSums(DALY.by.year)
  }
  DALY <- list(DALY.by.year, DALY.total)
  names(DALY) <- c("DALY by Year", "Total DALY")
  return(DALY)
}


##### AIDS Deaths averted 

AIDS.deaths <- function(list, rate = 0.04, timeframe = 20){
  thend <- 51+timeframe
  death <- list[[142]][,1,51:thend]
  list <- med.cum(death, rate)
  return(list)
}

LYS <- function(year){
  age1 <- rowSums(NoHIVST[[142]][,2, year]) - rowSums(ATLASonly[[142]][,2, year])
  age2 <- rowSums(NoHIVST[[142]][,3, year]) - rowSums(ATLASonly[[142]][,3, year])
  age3 <- rowSums(NoHIVST[[142]][,4, year]) - rowSums(ATLASonly[[142]][,4, year])
  age4 <- rowSums(NoHIVST[[142]][,5, year]) - rowSums(ATLASonly[[142]][,5, year])
  total <- age1*46.65+age2*42.41+age3*30.6+age4*18.04
  output <- quantile(total, c(0.025, 0.5, 0.975))
  return(output)
}

LYS(51:53)

### INFECTIONS AVERTED

AIDS.inf <- function(list, rate = 0.04, timeframe = 20){
  thend <- 51+timeframe
  inf <- list[[27]][,1,51:thend]
  for(i in 28:34){
    inf + list[[i]][,1,51:thend]
  }
  list <- med.cum(inf, rate)
  return(list)
}

##### COSTS #####

### CONV TEST ###

Conv.Test <- function(df, pop, timeframe = 20){
  thend <- 51+timeframe
  if(pop == "FSW"){
    NbOT <- df[[93]][,1,51:thend] + df[[96]][,1,51:thend]
  }else if(pop == "MSM"){
    NbOT <- df[[97]][,1,51:thend] + df[[98]][,1,51:thend]
  }else if(pop == "GEN"){
    NbOT <- df[[91]][,1,51:thend] + df[[92]][,1,51:thend] + df[[94]][,1,51:thend] + df[[95]][,1,51:thend]
  }else{
    warning("wrong pop input")
  }
  NbOT <- as.data.frame(NbOT)
  #NbSTmed <- median(rowSums(NbST))
  return(NbOT)
}

# Conv.Test.all <- function(df){
#   NbOT <- df[[91]][,1,50:71]
#   for(i in 92:98){
#     NbOT + df[[i]][,1,50:71]
#   }
#   NbOT <- as.data.frame(NbST)
#   #NbOTmed <- median(rowSums(NbST))
#   return(NbOT)
# }


FSW <- function(df, timeframe = 20){
  51:thend <- 51+timeframe
  FSWdf <- as.data.frame(df[[93]][,1,51:thend] + df[[96]][,1,51:thend])
  return(FSWdf)
}

MSM <- function(df){
  MSMdf <- as.data.frame(df[[97]][,1,51:thend] + df[[98]][,1,51:thend])
}


# ## FSW
# NbOTNoSTFSW <- FSW(NoHIVST)
# CostConvFSW <- discount(NbOTNoSTFSW, cost = 17.54)
# 
# NbOTATLASFSW <- FSW(ATLASonly)
# 
# NbOTATLASSUFSW <- FSW(ATLASSU)
# 
# ## MSM
# NbOTNoSTMSM <- MSM(NoHIVST)
# 
# NbOTATLASMSM <- MSM(ATLASonly)
# 
# NbOTATLASSUMSM <- MSM(ATLASSU)


### SELF TEST ###
Self.Test <- function(df, pop, timeframe = 20){
  thend <- 51+timeframe
  if(pop == "FSW"){
    NbST <- df[[12]][,51:thend]
  }else if(pop == "MSM"){
    NbST <- df[[13]][,51:thend]
  }else if (pop == "BOTH"){
    NbST <- df[[12]][,51:thend] + df[[13]][,51:thend]
  }else{
    warning("invalid population input")
  }
  NbST <- as.data.frame(NbST)
  #NbSTmed <- median(rowSums(NbST))
  return(NbST)
  #print(NbSTmed)
}

### CONFIRMATORY TESTING ###
Conf.Test <- function(df, pop,timeframe = 20){
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
  PosST <- as.data.frame(PosST*0.5)
  return(PosST)
}

### TREATMENT ###

ART <- function(df, timeframe = 20){
  thend <- 51+timeframe
  NbTreated <- df[[83]][,1,51:thend]
  for(i in 84:90){
    NbTreated + df[[i]][,1,51:thend]
  }
  NbTreated <- as.data.frame(NbTreated)
  return(NbTreated)
}

###### Resources ######

total.med <- function(df){
  total <- rowSums(df)
  output <- median(total)
  return(output)
}




### COST CALC FUNCTION ###

## Input Cost category, population, and discount rate
## Need to define the List of Data first

### MONT CARLO ICER ###
monte.carlo.icer <- function(num, rate = 0.04, outcome, timeframe = 20){ # time frame = 2019-2039
  set.seed(34242)
  # 5 costs
  OTFSW <- rtri(num, min = 13.10, max = 43.40, mode = 17.54)
  OTMSM <- rtri(num, min = 15.16, max = 28.26, mode = 22.68)
  OTGEN <- rtri(num, min = 7.67, max = 40.22, mode = 8.31)
  # STFSW <- rep(17, num) # OBSERVED INTERVENTION COST for 2019, 2020
  # STMSM <- rep(27, num) #
  # STFSW.22 <- rtri(num, min = 12.47, max = 17.74, mode = 13.30)
  # STFSW.23 <- rtri(num, min = 11.07, max = 16.93, mode = 12.69)
  # STFSW.24 <- rtri(num, min = 9.90, max = 17.85, mode = 12.50)
  # STMSM.22 <- rtri(num, min = 22.77, max = 32.35, mode = 24.26)
  # STMSM.23 <- rtri(num, min = 20.93, max = 30.14, mode = 22.60)
  # STMSM.24 <- rtri(num, min = 19.98, max = 31.85, mode = 22.29)
  # STFSW.22 <- rtri(num, min = 9.93, max = 13.26, mode = 10.61) # MA
  # STFSW.23 <- rtri(num, min = 8.79, max = 12.29, mode = 9.83)
  # STFSW.24 <- rtri(num, min = 7.99, max = 12.79, mode = 9.59)
  # STMSM.22 <- rtri(num, min = 17.34, max = 22.52, mode = 18.02)
  # STMSM.23 <- rtri(num, min = 14.62, max = 20.71, mode = 16.57)
  # STMSM.24 <- rtri(num, min = 13.12, max = 21.49, mode = 16.12)
  STFSW.22 <- rtri(num, min = 12.47, max = 17.74, mode = 13.30) # SE
  STFSW.23 <- rtri(num, min = 11.07, max = 16.93, mode = 12.69)
  STFSW.24 <- rtri(num, min = 9.90, max = 17.85, mode = 12.50)
  STMSM.22 <- rtri(num, min = 22.77, max = 32.35, mode = 24.26)
  STMSM.23 <- rtri(num, min = 20.93, max = 30.14, mode = 22.60)
  STMSM.24 <- rtri(num, min = 19.98, max = 31.85, mode = 22.29)
  # STFSW <- rtri(num, min = 15, max = 27, mode = 16) #MA
  # STMSM <- rtri(num, min = 17, max = 59, mode = 28) #MA
  STFSW <- rtri(num, min = 13, max = 32, mode = 17) #SE
  STMSM <- rtri(num, min = 25, max = 28, mode = 27) #SE
  ART <- rtri(num, min = 102.63, max = 232.60, mode = 198.4) # (65*0.9+227*0.1) /0.8 = 81.25 #1L 2L (165.34*0.9+332.1*0.1)*1.09 # max: (188.66*0.9+436*0.1)*1.09
  # STFSWSU <- rtri(num, min = st.fsw.dis.ci[1], max = st.fsw.dis.ci[2], mode = st.fsw.dis.ci[3])
  # STMSMSU <- rtri(num, min = st.msm.dis.ci[1], max = st.msm.dis.ci[2], mode = st.msm.dis.ci[3])
  # STFSWSU <- rtri(num, min = st.fsw.dis.ma[1], max = st.fsw.dis.ma[2], mode = st.fsw.dis.ma[3]) # MA
  # STMSMSU <- rtri(num, min = st.msm.dis.ma[1], max = st.msm.dis.ma[2], mode = st.msm.dis.ma[3]) # MA
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

icer <- function(cost, outcome, time_horizon = 20){
  cost.by.year <- cost[,1:time_horizon]
  total.cost <- rowSums(cost.by.year)
  outcome.by.year <- outcome[,1:time_horizon]
  total.outcome <- rowSums(outcome.by.year)
  icer <- total.cost/total.outcome
  output <- data.frame(icer, total.cost, total.outcome)
  return(output)
}


compare.costs.3Y <- function(list){
  base <- as.data.frame(list[[1]])
  atlas <- as.data.frame(list[[2]])
  #  su <- as.data.frame(list[[3]])
  
  ATLASDIFF <- atlas - base
  #  SUDIFF <- su - base
  
  output <- list(ATLASDIFF)
  
  names(output) <- c("ATLAS vs Base")
  return(output)
}

total.cost.calc.updated <- function(cost.list, rate = 0.04, timeframe){
  
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
  NbConf_FSWATLAS <- Conf.Test(ATLASonly, pop = "FSW", timeframe = timeframe)
  Conf_FSWATLAS <- discount(NbConf_FSWATLAS, cost = cost.list[1], rate)
  NbConf_MSMATLAS <- Conf.Test(ATLASonly, pop = "MSM", timeframe = timeframe)
  Conf_MSMATLAS <- discount(NbConf_MSMATLAS, cost = cost.list[2], rate)
  Conf_ATLAS <- Conf_FSWATLAS + Conf_MSMATLAS
  
  NbConf_FSWSU <- Conf.Test(ATLASSU, pop = "FSW", timeframe = timeframe)
  Conf_FSWSU <- discount(NbConf_FSWSU, cost = cost.list[1], rate)
  NbConf_MSMSU <- Conf.Test(ATLASSU, pop = "MSM", timeframe = timeframe)
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

total.cost.calc.noart <- function(cost.list, rate = 0.0, timeframe){
  
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

  
  ## Conf Test
  NbConf_FSWATLAS <- Conf.Test(ATLASonly, pop = "FSW", timeframe = timeframe)
  Conf_FSWATLAS <- discount(NbConf_FSWATLAS, cost = cost.list[1], rate)
  NbConf_MSMATLAS <- Conf.Test(ATLASonly, pop = "MSM", timeframe = timeframe)
  Conf_MSMATLAS <- discount(NbConf_MSMATLAS, cost = cost.list[2], rate)
  Conf_ATLAS <- Conf_FSWATLAS + Conf_MSMATLAS
  
  NbConf_FSWSU <- Conf.Test(ATLASSU, pop = "FSW", timeframe = timeframe)
  Conf_FSWSU <- discount(NbConf_FSWSU, cost = cost.list[1], rate)
  NbConf_MSMSU <- Conf.Test(ATLASSU, pop = "MSM", timeframe = timeframe)
  Conf_MSMSU <- discount(NbConf_MSMSU, cost = cost.list[2], rate)
  Conf_SU <- Conf_FSWSU + Conf_MSMSU
  
  Conf_ATLAS <- as.data.frame(Conf_ATLAS)
  Conf_SU <- as.data.frame(Conf_SU)
  
  ## total
  total.by.year.base <- CostConv_Base
  total.by.year.ATLAS <- CostConv_ATLAS + ST_ATLAS + Conf_ATLAS
  total.by.year.SU <- CostConv_SU + ST_SU + Conf_SU
  
  total.list <- list(total.by.year.base, total.by.year.ATLAS, total.by.year.SU)
  names(total.list) <- c("Base Scenario", "ATLAS Only", "Scale Up Scenario")
  return(total.list)
}

total.cost.calc.3Y <- function(cost.list, rate = 0.04, timeframe = 3){
  
  ## Conv Test
  NbOTFSW_Base <- Conv.Test(NoHIVST, pop = "FSW")
  CostConvFSW_Base <- discount(NbOTFSW_Base, cost = cost.list[1], rate)
  NbOTMSM_Base <- Conv.Test(NoHIVST, pop = "MSM")
  CostConvMSM_Base <- discount(NbOTMSM_Base, cost = cost.list[2], rate)
  NbOTGEN_Base <- Conv.Test(NoHIVST, pop = "GEN")
  CostConvGEN_Base <- discount(NbOTGEN_Base, cost = cost.list[3], rate)
  CostConv_Base <- CostConvFSW_Base + CostConvMSM_Base + CostConvGEN_Base
  
  NbOTFSW_ATLAS <- Conv.Test(ATLASonly, pop = "FSW")
  CostConvFSW_ATLAS <- discount(NbOTFSW_ATLAS, cost = cost.list[1], rate)
  NbOTMSM_ATLAS <- Conv.Test(ATLASonly, pop = "MSM")
  CostConvMSM_ATLAS <- discount(NbOTMSM_ATLAS, cost = cost.list[2], rate)
  NbOTGEN_ATLAS <- Conv.Test(ATLASonly, pop = "GEN")
  CostConvGEN_ATLAS <- discount(NbOTGEN_ATLAS, cost = cost.list[3], rate)
  CostConv_ATLAS <- CostConvFSW_ATLAS + CostConvMSM_ATLAS + CostConvGEN_ATLAS
  
  CostConv_Base <- as.data.frame(CostConv_Base)
  CostConv_ATLAS <- as.data.frame(CostConv_ATLAS)
  #  CostConv_SU <- as.data.frame(CostConv_SU)
  # names(CostByYear) <- c("Base Scenario", "ATLAS Only", "Scale Up Scenario")
  
  ## Self Test
  NbSTFSW_ATLAS <- Self.Test(df = ATLASonly, pop = "FSW")
  STFSW_ATLAS <- discount(NbSTFSW_ATLAS, cost = cost.list[4], rate)
  NbSTMSM_ATLAS <- Self.Test(df = ATLASonly, pop = "MSM")
  STMSM_ATLAS <- discount(NbSTMSM_ATLAS, cost = cost.list[5], rate)
  ST_ATLAS <- STFSW_ATLAS + STMSM_ATLAS
  
  ST_ATLAS <- as.data.frame(ST_ATLAS)
  
  
  ## Treatment
  NbTreated_Base <- ART(NoHIVST)
  NbTreated_ATLAS <- ART(ATLASonly)
  Treated_Base <- discount(df = NbTreated_Base, cost = cost.list[6], rate)
  Treated_ATLAS <- discount(df = NbTreated_ATLAS, cost = cost.list[6], rate)
  
  
  Treated_Base <- as.data.frame(Treated_Base)
  Treated_ATLAS <- as.data.frame(Treated_ATLAS)
  
  
  ## Conf Test
  NbConf_FSWATLAS <- Conf.Test(ATLASonly, pop = "FSW")
  Conf_FSWATLAS <- discount(NbConf_FSWATLAS, cost = cost.list[1], rate)
  NbConf_MSMATLAS <- Conf.Test(ATLASonly, pop = "MSM")
  Conf_MSMATLAS <- discount(NbConf_MSMATLAS, cost = cost.list[2], rate)
  Conf_ATLAS <- Conf_FSWATLAS + Conf_MSMATLAS
  
  
  Conf_ATLAS <- as.data.frame(Conf_ATLAS)
  
  
  ## total
  total.by.year.base <- CostConv_Base + Treated_Base
  total.by.year.ATLAS <- CostConv_ATLAS + ST_ATLAS + Treated_ATLAS + Conf_ATLAS
  
  total.list <- list(total.by.year.base, total.by.year.ATLAS)
  names(total.list) <- c("Base Scenario", "ATLAS Only")
  return(total.list)
}

monte.carlo.icer.3 <- function(num, rate = 0.04, outcome, timeframe = 20){ # time frame = 2019-2039
  set.seed(34242)
  # 5 costs
  OTFSW <- rtri(num, min = 13.10, max = 43.40, mode = 17.54)
  OTMSM <- rtri(num, min = 15.16, max = 28.26, mode = 22.68)
  OTGEN <- rtri(num, min = 7.67, max = 40.22, mode = 8.31)
  STFSW <- rep(16, num) # OBSERVED INTERVENTION COST for 2019, 2020
  STMSM <- rep(28, num) #
  ART.1 <- rtri(num, min = 102.63, max = 232.60, mode = 198.4) # (65*0.9+227*0.1) /0.8 = 81.25 #1L 2L (165.34*0.9+332.1*0.1)*1.09 # max: (188.66*0.9+436*0.1)*1.09
  for(i in 1:num){
    cost.list <- c(OTFSW[i], OTMSM[i],OTGEN[i], STFSW[i], STMSM[i],
                   ART.1[i])#, STFSWSU[i], STMSMSU[i])
    list.by.scenario <- total.cost.calc.3Y(cost.list, rate)
    compared.cost <- compare.costs.3Y(list.by.scenario)
    temp.atlas <- icer(compared.cost[[1]], outcome[[1]][,1:(timeframe+1)]) %>% as.data.frame()
    #  temp.su <- icer(compared.cost[[2]], outcome[[2]][ ,1:(timeframe+1)]) %>% as.data.frame()
    if(i == 1){
      icer.atlas <- temp.atlas
      #   icer.su <- temp.su
    }else{
      icer.atlas <- rbind(icer.atlas, temp.atlas)
      #   icer.su <- rbind(icer.su, temp.su)
    }
  }
  icer.list <- list(icer.atlas)
  names(icer.list) <- c("atlas icer")
  return(icer.list)
}

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

monte.carlo.cost.ci <- function(num, rate = 0.04, timeframe = 20){ # time frame = 2020-2040
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

monte.carlo.cost.ma <- function(num, rate = 0.04, timeframe = 20){ # time frame = 2020-2040
  set.seed(34242)
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

monte.carlo.cost.se <- function(num, rate = 0.04, timeframe = 20){ # time frame = 2020-2040
  set.seed(34242)
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



## Uncertainty interval

total.ui <- function(df,tf = 21){
  cumulative <- rowSums(df[,1:tf])
  ui <- quantile(cumulative, c(0.5, 0.05,0.95))
  return(ui)
}

Unc.int <- function(df){
  ui <- quantile(df, c(0.5, 0.05,0.95))
  return(ui)
}




