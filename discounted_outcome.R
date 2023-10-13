## discounted outcome

DALY.base <- DALY.Calc(NoHIVST, country = country)
DALY.base.dis <- DALY.Calc(NoHIVST, country = country, discount = 0.04)

DALY.ATLAS <- DALY.Calc(ATLASonly, country = country)
DALY.ATLAS.dis <- DALY.Calc(ATLASonly, country = country, discount = 0.04)

DALY.SU <- DALY.Calc(ATLASSU, country = country)
DALY.SU.dis <- DALY.Calc(ATLASSU, country = country, discount = 0.04)

DALY.diff <- compare(DALY.base, DALY.ATLAS, DALY.SU, averted = TRUE)
DALY.total.diff <- list(rowSums(DALY.diff[[1]][1:21]), rowSums(DALY.diff[[2]][1:21])) 
names(DALY.total.diff) <- c("ATLAS vs Base", "Scale up vs Base")
DALY.diff <- list(DALY.diff[[1]][1:21], DALY.diff[[2]][1:21])
names(DALY.diff) <- c("ATLAS vs Base", "Scale up vs Base")

DALY.diff.dis <- compare(DALY.base.dis, DALY.ATLAS.dis, DALY.SU.dis, averted = TRUE)
DALY.total.diff.dis <- list(rowSums(DALY.diff.dis[[1]][1:21]), rowSums(DALY.diff.dis[[2]][1:21])) 
names(DALY.total.diff.dis) <- c("ATLAS vs Base", "Scale up vs Base")
DALY.diff.dis <- list(DALY.diff.dis[[1]][1:21], DALY.diff.dis[[2]][1:21])
names(DALY.diff.dis) <- c("ATLAS vs Base", "Scale up vs Base")

daly.ci <- monte.carlo.icer.ci(num = 100, rate = 0.04, outcome = DALY.diff.dis)
daly.ma <- monte.carlo.icer.ma(num = 100, rate = 0.04, outcome = DALY.diff.dis)
daly.se <- monte.carlo.icer.se(num = 100, rate = 0.04, outcome = DALY.diff.dis)


Unc.int(daly.ci[[1]][,1])
Unc.int(daly.ma[[1]][,1])
Unc.int(daly.se[[1]][,1])

Unc.int(daly.ci[[2]][,1])
Unc.int(daly.ma[[2]][,1])
Unc.int(daly.se[[2]][,1])