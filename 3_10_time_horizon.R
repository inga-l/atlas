#3 year
daly.ci.3 <- monte.carlo.icer.ci.3(num = 100, rate = 0.04, outcome = DALY.diff, timeframe = 3)
daly.ma.3 <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = DALY.diff, timeframe = 3)
daly.se.3 <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = DALY.diff, timeframe = 3)

daly.ci.10 <- monte.carlo.icer.ci.3(num = 100, rate = 0.04, outcome = DALY.diff, timeframe = 10)
daly.ma.10 <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = DALY.diff, timeframe = 10)
daly.se.10 <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = DALY.diff, timeframe = 10)

Unc.int(daly.ci.3[[1]][,1])
Unc.int(daly.ma.3[[1]][,1])
Unc.int(daly.se.3[[1]][,1])

Unc.int(daly.ci.10[[1]][,1])
Unc.int(daly.ma.10[[1]][,1])
Unc.int(daly.se.10[[1]][,1])

Unc.int(daly.ci.10[[2]][,1])
Unc.int(daly.ma.10[[2]][,1])
Unc.int(daly.se.10[[2]][,1])

inf.ci.3 <- monte.carlo.icer.ci.3(num = 100, rate = 0.04, outcome = inf.diff, timeframe = 3)
inf.ma.3 <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = inf.diff, timeframe = 3)
inf.se.3 <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = inf.diff, timeframe = 3)
Unc.int(inf.ci.3[[1]][,1])
Unc.int(inf.ci.10[[1]][,1])
Unc.int(inf.ci.10[[2]][,1])

inf.ci.10 <- monte.carlo.icer.ci.3(num = 100, rate = 0.04, outcome = inf.diff, timeframe = 10)
inf.ma.10 <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = inf.diff, timeframe = 10)
inf.se.10 <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = inf.diff, timeframe = 10)

death.ci.3 <- monte.carlo.icer.ci.3(num = 100, rate = 0.04, outcome = death.diff, timeframe = 3)
death.ci.10 <- monte.carlo.icer.ci.3(num = 100, rate = 0.04, outcome = death.diff, timeframe = 10)

death.ma.3 <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = death.diff, timeframe = 3)
death.ma.10 <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = death.diff, timeframe = 10)

death.se.3 <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = death.diff, timeframe = 3)
death.se.10 <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = death.diff, timeframe = 10)

Unc.int(death.ci.3[[1]][,1])
Unc.int(death.ci.10[[1]][,1])
Unc.int(death.ci.10[[2]][,1])

Unc.int(inf.ma.3[[1]][,1])
Unc.int(inf.ma.10[[1]][,1])
Unc.int(inf.ma.10[[2]][,1])
Unc.int(death.ma.3[[1]][,1])
Unc.int(death.ma.10[[1]][,1])
Unc.int(death.ma.10[[2]][,1])

Unc.int(inf.se.3[[1]][,1])
Unc.int(inf.se.10[[1]][,1])
Unc.int(inf.se.10[[2]][,1])
Unc.int(death.se.3[[1]][,1])
Unc.int(death.se.10[[1]][,1])
Unc.int(death.se.10[[2]][,1])

monte.carlo.icer.ci.3 <- function(num, rate = 0.04, outcome, timeframe){ # time frame = 2019-2039
  set.seed(34242)
  # 5 costs
  OTFSW <- rtri(num, min = 13.10, max = 43.40, mode = 17.54)
  OTMSM <- rtri(num, min = 15.16, max = 28.26, mode = 22.68)
  OTGEN <- rtri(num, min = 7.67, max = 40.22, mode = 8.31)
  STFSW <- rtri(num, min = 13.10, max = 43.40, mode = 13) # OBSERVED INTERVENTION COST for 2019, 2020
  STMSM <- rtri(num, min = 13.10, max = 43.40, mode = 15) #
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
    list.by.scenario <- total.cost.calc.updated(cost.list, rate, timeframe = 20)
    compared.cost <- compare.costs(list.by.scenario)
    atlas <- outcome[[1]][,1:(timeframe+1)]
    su <- outcome[[2]][,1:(timeframe+1)]
    temp.atlas <- icer(compared.cost[[1]], atlas,time_horizon = timeframe) %>% as.data.frame()
    temp.su <- icer(compared.cost[[2]], su,time_horizon = timeframe) %>% as.data.frame()
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
#10 year