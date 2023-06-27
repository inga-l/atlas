monte.carlo.cost.total <- function(num, rate = 0.04, timeframe = 20){ # time frame = 2020-2040
  set.seed(34242)
  # 5 costs
  OTFSW <- rtri(num, min = 13.10, max = 43.40, mode = 17.54)
  OTMSM <- rtri(num, min = 15.16, max = 28.26, mode = 22.68)
  OTGEN <- rtri(num, min = 7.67, max = 40.22, mode = 8.31)
  STFSW <- rep(13, num) # OBSERVED INTERVENTION COST for 2019, 2020
  STMSM <- rep(15, num) #
  STFSW.22 <- rtri(num, min = 9.4, max = 13.6, mode = 10.2) #CI
  STFSW.23 <- rtri(num, min = 8.0, max = 11.7, mode = 8.8)
  STFSW.24 <- rtri(num, min = 7.6, max = 12, mode = 8.4)
  STMSM.22 <- rtri(num, min = 9.3, max = 13.5, mode = 10.1)
  STMSM.23 <- rtri(num, min = 8.1, max = 11.8, mode = 8.8)
  STMSM.24 <- rtri(num, min = 7.7, max = 12.1, mode = 8.5)
  # STFSW.22 <- rtri(num, min = 9.93, max = 13.26, mode = 10.61) # MA
  # STFSW.23 <- rtri(num, min = 8.79, max = 12.29, mode = 9.83)
  # STFSW.24 <- rtri(num, min = 7.99, max = 12.79, mode = 9.59)
  # STMSM.22 <- rtri(num, min = 17.34, max = 22.52, mode = 18.02)
  # STMSM.23 <- rtri(num, min = 14.62, max = 20.71, mode = 16.57)
  # STMSM.24 <- rtri(num, min = 13.12, max = 21.49, mode = 16.12)
  # STFSW.22 <- rtri(num, min = 12.47, max = 17.74, mode = 13.30) # SE
  # STFSW.23 <- rtri(num, min = 11.07, max = 16.93, mode = 12.69)
  # STFSW.24 <- rtri(num, min = 9.90, max = 17.85, mode = 12.50)
  # STMSM.22 <- rtri(num, min = 22.77, max = 32.35, mode = 24.26)
  # STMSM.23 <- rtri(num, min = 20.93, max = 30.14, mode = 22.60)
  # STMSM.24 <- rtri(num, min = 19.98, max = 31.85, mode = 22.29)
  # STFSW <- rtri(num, min = 15, max = 27, mode = 16) #MA
  # STMSM <- rtri(num, min = 17, max = 59, mode = 28) #MA
  # STFSW <- rtri(num, min = 13, max = 32, mode = 17) #SE
  # STMSM <- rtri(num, min = 25, max = 28, mode = 27) #SE
  ART <- rtri(num, min = 102.63, max = 232.60, mode = 198.4) # (65*0.9+227*0.1) /0.8 = 81.25 #1L 2L (165.34*0.9+332.1*0.1)*1.09 # max: (188.66*0.9+436*0.1)*1.09
  STFSWSU <- rtri(num, min = st.fsw.dis.ci[1], max = st.fsw.dis.ci[2], mode = st.fsw.dis.ci[3])
  STMSMSU <- rtri(num, min = st.msm.dis.ci[1], max = st.msm.dis.ci[2], mode = st.msm.dis.ci[3])
  # STFSWSU <- rtri(num, min = st.fsw.dis.ma[1], max = st.fsw.dis.ma[2], mode = st.fsw.dis.ma[3])
  # STMSMSU <- rtri(num, min = st.msm.dis.ma[1], max = st.msm.dis.ma[2], mode = st.msm.dis.ma[3])
  # STFSWSU <- rtri(num, min = st.fsw.dis.se[1], max = st.fsw.dis.se[2], mode = st.fsw.dis.se[3])# SE
  # STMSMSU <- rtri(num, min = st.msm.dis.se[1], max = st.msm.dis.se[2], mode = st.msm.dis.se[3])  #SE
  for(i in 1:num){
    cost.list <- c(OTFSW[i], OTMSM[i],OTGEN[i], STFSW[i], STMSM[i],
                   STFSW.22[i], STFSW.23[i], STFSW.24[i],
                   STMSM.22[i], STMSM.23[i], STMSM.24[i],
                   ART[i], STFSWSU[i], STMSMSU[i])
    list.by.scenario <- total.cost.calc.updated(cost.list, rate,timeframe = timeframe)
    
    
    temp.base <- list.by.scenario[[1]] %>% as.data.frame()
    temp.atlas <- list.by.scenario[[2]] %>% as.data.frame()
    temp.su <- list.by.scenario[[3]] %>% as.data.frame()
    temp.conv.base <- total.cost.calc.component(cost.list, rate, type = "Conv", timeframe = timeframe)[[1]] %>% as.data.frame()
    temp.self.base <- total.cost.calc.component(cost.list, rate, type = "Self", timeframe = timeframe)[[1]] %>% as.data.frame()
    temp.art.base <- total.cost.calc.component(cost.list, rate, type = "ART", timeframe = timeframe)[[1]] %>% as.data.frame()
    temp.conf.base <- total.cost.calc.component(cost.list, rate, type = "Conf", timeframe = timeframe)[[1]] %>% as.data.frame()
    
    if(i == 1){
      conv.base <- temp.conv.base
      self.base <- temp.self.base
      art.base <- temp.art.base
      conf.base <- temp.conf.base
      cost.base <- temp.base
      cost.atlas <- temp.atlas
      cost.su <- temp.su
    }else{
      conv.base <- rbind(conv.base, temp.conv.base)
      self.base <- rbind(self.base, temp.self.base)
      art.base <- rbind(art.base, temp.art.base)
      conf.base <- rbind(conf.base, temp.conf.base)
      cost.base <- rbind(cost.base, temp.base)
      cost.atlas <- rbind(cost.atlas, temp.atlas)
      cost.su <- rbind(cost.su, temp.su)
    }
  }
  conv.base <- rowSums(conv.base)
  self.base <- rowSums(self.base)
  art.base <- rowSums(art.base)
  conf.base <- rowSums(conf.base)
  cost.base <- rowSums(cost.base)
#  cost.base <- rbind(cost.base, conv.base)
  cost.atlas <- rowSums(cost.atlas)
  cost.su <- rowSums(cost.su)
  
  cost.list <- list(conv.base, self.base, art.base, conf.base, cost.base
                    #, cost.atlas, cost.su)
  )
  names(cost.list) <- c("conv","self","art","conf", "base")
  # names(cost.list) <- c("conv", "base", "atlas", "su icer")
  return(cost.list)
}

cost.ci.component <- monte.carlo.cost.total(num = 100, rate = 0.04)

mmm(cost.ci.component[[1]]/cost.ci.component[[5]])
mmm(cost.ci.component[[3]]/cost.ci.component[[5]])
mmm(cost.ci.component[[4]]/cost.ci.component[[5]])


monte.carlo.cost.total.atlas <- function(num, rate = 0.04, timeframe = 20){ # time frame = 2020-2040
  set.seed(34242)
  # 5 costs
  OTFSW <- rtri(num, min = 13.10, max = 43.40, mode = 17.54)
  OTMSM <- rtri(num, min = 15.16, max = 28.26, mode = 22.68)
  OTGEN <- rtri(num, min = 7.67, max = 40.22, mode = 8.31)
  STFSW <- rep(13, num) # OBSERVED INTERVENTION COST for 2019, 2020
  STMSM <- rep(15, num) #
  STFSW.22 <- rtri(num, min = 9.4, max = 13.6, mode = 10.2) #CI
  STFSW.23 <- rtri(num, min = 8.0, max = 11.7, mode = 8.8)
  STFSW.24 <- rtri(num, min = 7.6, max = 12, mode = 8.4)
  STMSM.22 <- rtri(num, min = 9.3, max = 13.5, mode = 10.1)
  STMSM.23 <- rtri(num, min = 8.1, max = 11.8, mode = 8.8)
  STMSM.24 <- rtri(num, min = 7.7, max = 12.1, mode = 8.5)
  # STFSW.22 <- rtri(num, min = 9.93, max = 13.26, mode = 10.61) # MA
  # STFSW.23 <- rtri(num, min = 8.79, max = 12.29, mode = 9.83)
  # STFSW.24 <- rtri(num, min = 7.99, max = 12.79, mode = 9.59)
  # STMSM.22 <- rtri(num, min = 17.34, max = 22.52, mode = 18.02)
  # STMSM.23 <- rtri(num, min = 14.62, max = 20.71, mode = 16.57)
  # STMSM.24 <- rtri(num, min = 13.12, max = 21.49, mode = 16.12)
  # STFSW.22 <- rtri(num, min = 12.47, max = 17.74, mode = 13.30) # SE
  # STFSW.23 <- rtri(num, min = 11.07, max = 16.93, mode = 12.69)
  # STFSW.24 <- rtri(num, min = 9.90, max = 17.85, mode = 12.50)
  # STMSM.22 <- rtri(num, min = 22.77, max = 32.35, mode = 24.26)
  # STMSM.23 <- rtri(num, min = 20.93, max = 30.14, mode = 22.60)
  # STMSM.24 <- rtri(num, min = 19.98, max = 31.85, mode = 22.29)
  # STFSW <- rtri(num, min = 15, max = 27, mode = 16) #MA
  # STMSM <- rtri(num, min = 17, max = 59, mode = 28) #MA
  # STFSW <- rtri(num, min = 13, max = 32, mode = 17) #SE
  # STMSM <- rtri(num, min = 25, max = 28, mode = 27) #SE
  ART <- rtri(num, min = 102.63, max = 232.60, mode = 198.4) # (65*0.9+227*0.1) /0.8 = 81.25 #1L 2L (165.34*0.9+332.1*0.1)*1.09 # max: (188.66*0.9+436*0.1)*1.09
  STFSWSU <- rtri(num, min = st.fsw.dis.ci[1], max = st.fsw.dis.ci[2], mode = st.fsw.dis.ci[3])
  STMSMSU <- rtri(num, min = st.msm.dis.ci[1], max = st.msm.dis.ci[2], mode = st.msm.dis.ci[3])
  # STFSWSU <- rtri(num, min = st.fsw.dis.ma[1], max = st.fsw.dis.ma[2], mode = st.fsw.dis.ma[3])
  # STMSMSU <- rtri(num, min = st.msm.dis.ma[1], max = st.msm.dis.ma[2], mode = st.msm.dis.ma[3])
  # STFSWSU <- rtri(num, min = st.fsw.dis.se[1], max = st.fsw.dis.se[2], mode = st.fsw.dis.se[3])# SE
  # STMSMSU <- rtri(num, min = st.msm.dis.se[1], max = st.msm.dis.se[2], mode = st.msm.dis.se[3])  #SE
  for(i in 1:num){
    cost.list <- c(OTFSW[i], OTMSM[i],OTGEN[i], STFSW[i], STMSM[i],
                   STFSW.22[i], STFSW.23[i], STFSW.24[i],
                   STMSM.22[i], STMSM.23[i], STMSM.24[i],
                   ART[i], STFSWSU[i], STMSMSU[i])
    list.by.scenario <- total.cost.calc.updated(cost.list, rate,timeframe = timeframe)
    
    
    temp.base <- list.by.scenario[[1]] %>% as.data.frame()
    temp.atlas <- list.by.scenario[[2]] %>% as.data.frame()
    temp.su <- list.by.scenario[[3]] %>% as.data.frame()
    temp.conv.base <- total.cost.calc.component(cost.list, rate, type = "Conv", timeframe = timeframe)[[2]] %>% as.data.frame()
    temp.self.base <- total.cost.calc.component(cost.list, rate, type = "Self", timeframe = timeframe)[[2]] %>% as.data.frame()
    temp.art.base <- total.cost.calc.component(cost.list, rate, type = "ART", timeframe = timeframe)[[2]] %>% as.data.frame()
    temp.conf.base <- total.cost.calc.component(cost.list, rate, type = "Conf", timeframe = timeframe)[[2]] %>% as.data.frame()
    
    if(i == 1){
      conv.base <- temp.conv.base
      self.base <- temp.self.base
      art.base <- temp.art.base
      conf.base <- temp.conf.base
      cost.base <- temp.base
      cost.atlas <- temp.atlas
      cost.su <- temp.su
    }else{
      conv.base <- rbind(conv.base, temp.conv.base)
      self.base <- rbind(self.base, temp.self.base)
      art.base <- rbind(art.base, temp.art.base)
      conf.base <- rbind(conf.base, temp.conf.base)
      cost.base <- rbind(cost.base, temp.base)
      cost.atlas <- rbind(cost.atlas, temp.atlas)
      cost.su <- rbind(cost.su, temp.su)
    }
  }
  conv.base <- rowSums(conv.base)
  self.base <- rowSums(self.base)
  art.base <- rowSums(art.base)
  conf.base <- rowSums(conf.base)
  cost.base <- rowSums(cost.atlas)
  #  cost.base <- rbind(cost.base, conv.base)
  cost.atlas <- rowSums(cost.atlas)
  cost.su <- rowSums(cost.su)
  
  cost.list <- list(conv.base, self.base, art.base, conf.base, cost.atlas
                    #, cost.atlas, cost.su)
  )
  names(cost.list) <- c("conv","self","art","conf", "atlas")
  # names(cost.list) <- c("conv", "base", "atlas", "su icer")
  return(cost.list)
}

cost.ci.component.atlas <- monte.carlo.cost.total.atlas(num = 100, rate = 0.04)

mmm(cost.ci.component.atlas[[1]]/cost.ci.component.atlas[[5]])
mmm(cost.ci.component.atlas[[2]]/cost.ci.component.atlas[[5]])
mmm(cost.ci.component.atlas[[3]]/cost.ci.component.atlas[[5]])
mmm(cost.ci.component.atlas[[4]]/cost.ci.component.atlas[[5]])

monte.carlo.cost.total.su <- function(num, rate = 0.04, timeframe = 20){ # time frame = 2020-2040
  set.seed(34242)
  # 5 costs
  OTFSW <- rtri(num, min = 13.10, max = 43.40, mode = 17.54)
  OTMSM <- rtri(num, min = 15.16, max = 28.26, mode = 22.68)
  OTGEN <- rtri(num, min = 7.67, max = 40.22, mode = 8.31)
  STFSW <- rep(13, num) # OBSERVED INTERVENTION COST for 2019, 2020
  STMSM <- rep(15, num) #
  STFSW.22 <- rtri(num, min = 9.4, max = 13.6, mode = 10.2) #CI
  STFSW.23 <- rtri(num, min = 8.0, max = 11.7, mode = 8.8)
  STFSW.24 <- rtri(num, min = 7.6, max = 12, mode = 8.4)
  STMSM.22 <- rtri(num, min = 9.3, max = 13.5, mode = 10.1)
  STMSM.23 <- rtri(num, min = 8.1, max = 11.8, mode = 8.8)
  STMSM.24 <- rtri(num, min = 7.7, max = 12.1, mode = 8.5)
  # STFSW.22 <- rtri(num, min = 9.93, max = 13.26, mode = 10.61) # MA
  # STFSW.23 <- rtri(num, min = 8.79, max = 12.29, mode = 9.83)
  # STFSW.24 <- rtri(num, min = 7.99, max = 12.79, mode = 9.59)
  # STMSM.22 <- rtri(num, min = 17.34, max = 22.52, mode = 18.02)
  # STMSM.23 <- rtri(num, min = 14.62, max = 20.71, mode = 16.57)
  # STMSM.24 <- rtri(num, min = 13.12, max = 21.49, mode = 16.12)
  # STFSW.22 <- rtri(num, min = 12.47, max = 17.74, mode = 13.30) # SE
  # STFSW.23 <- rtri(num, min = 11.07, max = 16.93, mode = 12.69)
  # STFSW.24 <- rtri(num, min = 9.90, max = 17.85, mode = 12.50)
  # STMSM.22 <- rtri(num, min = 22.77, max = 32.35, mode = 24.26)
  # STMSM.23 <- rtri(num, min = 20.93, max = 30.14, mode = 22.60)
  # STMSM.24 <- rtri(num, min = 19.98, max = 31.85, mode = 22.29)
  # STFSW <- rtri(num, min = 15, max = 27, mode = 16) #MA
  # STMSM <- rtri(num, min = 17, max = 59, mode = 28) #MA
  # STFSW <- rtri(num, min = 13, max = 32, mode = 17) #SE
  # STMSM <- rtri(num, min = 25, max = 28, mode = 27) #SE
  ART <- rtri(num, min = 102.63, max = 232.60, mode = 198.4) # (65*0.9+227*0.1) /0.8 = 81.25 #1L 2L (165.34*0.9+332.1*0.1)*1.09 # max: (188.66*0.9+436*0.1)*1.09
  STFSWSU <- rtri(num, min = st.fsw.dis.ci[1], max = st.fsw.dis.ci[2], mode = st.fsw.dis.ci[3])
  STMSMSU <- rtri(num, min = st.msm.dis.ci[1], max = st.msm.dis.ci[2], mode = st.msm.dis.ci[3])
  # STFSWSU <- rtri(num, min = st.fsw.dis.ma[1], max = st.fsw.dis.ma[2], mode = st.fsw.dis.ma[3])
  # STMSMSU <- rtri(num, min = st.msm.dis.ma[1], max = st.msm.dis.ma[2], mode = st.msm.dis.ma[3])
  # STFSWSU <- rtri(num, min = st.fsw.dis.se[1], max = st.fsw.dis.se[2], mode = st.fsw.dis.se[3])# SE
  # STMSMSU <- rtri(num, min = st.msm.dis.se[1], max = st.msm.dis.se[2], mode = st.msm.dis.se[3])  #SE
  for(i in 1:num){
    cost.list <- c(OTFSW[i], OTMSM[i],OTGEN[i], STFSW[i], STMSM[i],
                   STFSW.22[i], STFSW.23[i], STFSW.24[i],
                   STMSM.22[i], STMSM.23[i], STMSM.24[i],
                   ART[i], STFSWSU[i], STMSMSU[i])
    list.by.scenario <- total.cost.calc.updated(cost.list, rate,timeframe = timeframe)
    
    
    temp.base <- list.by.scenario[[1]] %>% as.data.frame()
    temp.atlas <- list.by.scenario[[2]] %>% as.data.frame()
    temp.su <- list.by.scenario[[3]] %>% as.data.frame()
    temp.conv.base <- total.cost.calc.component(cost.list, rate, type = "Conv", timeframe = timeframe)[[3]] %>% as.data.frame()
    temp.self.base <- total.cost.calc.component(cost.list, rate, type = "Self", timeframe = timeframe)[[3]] %>% as.data.frame()
    temp.art.base <- total.cost.calc.component(cost.list, rate, type = "ART", timeframe = timeframe)[[3]] %>% as.data.frame()
    temp.conf.base <- total.cost.calc.component(cost.list, rate, type = "Conf", timeframe = timeframe)[[3]] %>% as.data.frame()
    
    if(i == 1){
      conv.base <- temp.conv.base
      self.base <- temp.self.base
      art.base <- temp.art.base
      conf.base <- temp.conf.base
      cost.base <- temp.base
      cost.atlas <- temp.atlas
      cost.su <- temp.su
    }else{
      conv.base <- rbind(conv.base, temp.conv.base)
      self.base <- rbind(self.base, temp.self.base)
      art.base <- rbind(art.base, temp.art.base)
      conf.base <- rbind(conf.base, temp.conf.base)
      cost.base <- rbind(cost.base, temp.base)
      cost.atlas <- rbind(cost.atlas, temp.atlas)
      cost.su <- rbind(cost.su, temp.su)
    }
  }
  conv.base <- rowSums(conv.base)
  self.base <- rowSums(self.base)
  art.base <- rowSums(art.base)
  conf.base <- rowSums(conf.base)
  cost.base <- rowSums(cost.atlas)
  #  cost.base <- rbind(cost.base, conv.base)
  cost.atlas <- rowSums(cost.atlas)
  cost.su <- rowSums(cost.su)
  
  cost.list <- list(conv.base, self.base, art.base, conf.base, cost.su
                    #, cost.atlas, cost.su)
  )
  names(cost.list) <- c("conv","self","art","conf", "atlas")
  # names(cost.list) <- c("conv", "base", "atlas", "su icer")
  return(cost.list)
}

cost.ci.component.su <- monte.carlo.cost.total.su(num = 100, rate = 0.04)

mmm(cost.ci.component.su[[1]]/cost.ci.component.su[[5]])
mmm(cost.ci.component.su[[2]]/cost.ci.component.su[[5]])
mmm(cost.ci.component.su[[3]]/cost.ci.component.su[[5]])
mmm(cost.ci.component.su[[4]]/cost.ci.component.su[[5]])


Self.Test <- function(df, pop, timeframe = 20){
  thend <- 51+timeframe
  if(pop == "FSW"){
    NbST <- df[[101]][,1,51:thend] + df[[104]][,1,51:thend]
  }else if(pop == "MSM"){
    NbST <- df[[105]][,1,51:thend] + df[[106]][,1,51:thend]
  }else if (pop == "BOTH"){
    NbST <- df[[101]][,1,51:thend] + df[[104]][,1,51:thend] + df[[105]][,1,51:thend] + df[[106]][,1,51:thend]
  }else{
    warning("invalid population input")
  }
  NbST <- as.data.frame(NbST)
  #NbSTmed <- median(rowSums(NbST))
  return(NbST)
  #print(NbSTmed)
}
Self.se <- Self.Test(ATLASSU.SE,"BOTH")
mmm(rowSums(Self.se))


inf.atlas.ci <- mmm(rowSums(inf.diff[[1]]))
inf.su.ci <- mmm(rowSums(inf.diff[[2]]))
death.atlas.ci <- mmm(rowSums(death.diff[[1]]))
death.su.ci <- mmm(rowSums(death.diff[[2]]))

inf.atlas.ma <- mmm(rowSums(inf.diff[[1]]))
inf.su.ma <- mmm(rowSums(inf.diff[[2]]))
death.atlas.ma <- mmm(rowSums(death.diff[[1]]))
death.su.ma <- mmm(rowSums(death.diff[[2]]))

inf.atlas.se <- mmm(rowSums(inf.diff[[1]]))
inf.su.se <- mmm(rowSums(inf.diff[[2]]))
death.atlas.se <- mmm(rowSums(death.diff[[1]]))
death.su.se <- mmm(rowSums(death.diff[[2]]))


