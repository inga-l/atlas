#Main CI
## TIDIED UP WHOLE SCRIPT
library(pacman)
p_load(tidyverse, readr, ggplot2, ggrepel, ggfortify, ggthemes, RColorBrewer, ggpubr, cowplot, "scales", EnvStats,hesim, ggbreak, wesanderson)
theme_set(theme_few())

#LOADING DB----------

### CI
RD <- load("/Users/Ent/Downloads/ATLAS CEA/CoteIvoireScens20220201.RData")
NoHIVST <- ScenCI[[1]]
ATLASonly <- ScenCI[[2]]
ATLASSU <- ScenCI[[4]]
ATLASSU.CI <- ScenCI[[4]]
country <- "CI"


scale.fsw.ci <- Self.Test(ATLASSU.CI, "FSW")[,6]

fc_ci <- 62455+88073+100175+37611+653937+50155

CI.fsw.cf <- avg_st_unit_cost(fc_ci, 2.63, scale.fsw.ci)


st.fsw.dis.ci <- mmm(CI.fsw.cf) # obtained the average unit cost of ST after scale up, integrating the cost function

scale.msm.ci <- Self.Test(ATLASSU.CI, "MSM")[,6]
fc_ci_msm <- 27766 + 39347 + 52436 + 21845 + 324112 + 27262
CI.msm.cf <- avg_st_unit_cost(fc_ci_msm, 2.63, scale.msm.ci)
st.msm.dis.ci <- mmm(CI.msm.cf)

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


Unc.int(daly.ci[[1]][,1])
Unc.int(daly.ci[[2]][,1])

# COST
cost.ci <- monte.carlo.cost.ci(num = 100, rate = 0.04)
median.cost.ci.nohivst <- apply(cost.ci[[1]],2, median) #median cost by year
median.cost.ci.atlas <- apply(cost.ci[[2]],2, median)
median.cost.ci.su <- apply(cost.ci[[3]],2, median)

median.cost.df.ci <- rbind(median.cost.ci.nohivst,median.cost.ci.atlas,median.cost.ci.su) %>% 
  as.data.frame()
colnames(median.cost.df.ci) <- c(2019:2039)
rownames(median.cost.df.ci) <- c("No HIVST", "ATLAS Only", "ATLAS Scale-up")

Cost_20y_CI <- rowSums(median.cost.df.ci)
median.cost.df <- median.cost.df %>% 
  rownames_to_column(var = "Scenario") %>% 
  pivot_longer(cols = !Scenario,
               names_to = c("Year"),
               values_to = "Cost")
median.cost.df$Year <- as.numeric(median.cost.df$Year)
median.cost.df$Cost <- median.cost.df$Cost/1000000

cost.by.year <- ggplot(median.cost.df,aes(x = Year, y = Cost, colour = Scenario))+
  geom_line(position=position_dodge(width=0.3), size = 0.5)+
  scale_colour_manual(values = wes_palette("FantasticFox1")) +
  ylab("Cost (mllion USD, not discounted)") +
  scale_y_continuous(labels = comma,  expand = c(0,0))+
  labs(title = "Median Annual Cost of Program")
cost.by.year