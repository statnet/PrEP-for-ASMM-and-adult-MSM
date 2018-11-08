


## Life cycle scenario setup file
setwd("/homes/dth2/Campcl")
rm(list = ls())
library(EpiModelHIV)


# Time unit for simulation, relative to 1 day
time.unit <- 7

# Population size by race
num.B <- 13500/2
num.W <- 13500/2

num.B.msm <- 10500/2
num.W.msm <- 10500/2

num.B.asmm <- 3000/2
num.W.asmm <- 3000/2




deg.mp.B <- deg.mp.W <-
  (matrix(c(0.506, 0.151, 0.053, 0.207, 0.061, 0.022), byrow = TRUE, nrow = 2) +
   matrix(c(0.435, 0.184, 0.095, 0.233, 0.033, 0.020), byrow = TRUE, nrow = 2))/2

#Mean degree for ASMM Calculated conditional on age of enrty and debut.
deg.asmm <- 0.162

#The deg.mp.B and deg.mp.W distributions must be adjusted to account for 6.67% of ties from 19:25 go to ASMM.
# 1-deg.mp.B[1,1] = .5295 of MSM have a relationship.
#length(19:25)/length(19:40) = .318 of MSM are 19:25
#the fraction of all ties belonging to MSM 19:25 .318*.5295 = .168381
##The fraction of ties to be moved from the MSM network to the ASMM network is .0667*.168381=0.01123101

deg.mp.B[1,1]<-deg.mp.B[1,1]+0.01123101
deg.mp.B[1,2]<-deg.mp.B[1,2]-0.01123101
deg.mp.W <- deg.mp.B

#Mean degree for ASMM Calculated conditional on age of enrty and debut.
deg.asmm <- 0.162
#The fraction of MSM ties moving to the ASMM network.
cross.frac<-0.01123101
  
# Adult Instant rates
mdeg.inst.B <- mdeg.inst.W <-
  (matrix(c(0.010402, 0.012954, 0.011485, 0.007912, 0.007424, 0.007424),
          byrow = TRUE, nrow = 2) +
   matrix(c(0.008186, 0.012017, 0.013024, 0.008151, 0.008341, 0.008341),
          byrow = TRUE, nrow = 2))/2



# ADULT Quintile distribution of overall AI rates
qnts.W <- qnts.B <-
  c(0, 0.00100413270621469, 0.00536670889830508,
    0.0101956474689266, 0.0315210354777778)

# Proportion in same-race partnerships (main, casl, inst)
prop.hom.mpi.B <- prop.hom.mpi.W <-
  (c(0.9484, 0.9019, 0.9085) + c(0.9154, 0.8509, 0.8944))/2

#ADULT Mean age diffs (main, casl, inst)
sqrt.adiff.BB <- c(0.417, 0.498, 0.456)
sqrt.adiff.BW <- c(0.454, 0.629, 0.585)
sqrt.adiff.WW <- c(0.520, 0.632, 0.590)

# ADULT Mean durations
rates.main <- mean(c(0.00287612937991679, 0.00269183371091241, 0.00180272348650181))
rates.pers <- mean(c(0.00761700198417522, 0.00350074333616134, 0.00693147180559945))

durs.main <- 1/rates.main
durs.pers <- 1/rates.pers

##ASMM durations.
rates.asmm <- .049505
durs.asmm <- 1/rates.asmm



#  Minimum age at entry .
#
#birth.age<-13
#debut.prob<-c(((0.44/52)*((.5062467131/52)+1-(1-.0004864819)^7)),
#((0.13/52)*((.5062467131/52)+1-(1-.0004864819)^7)),
#((0.12/52)*((.5062467131/52)+1-(1-.0004864819)^7)),
#((0.11/52)*((.5062467131/52)+1-(1-.0004864819)^7)),
#((0.10/52)*((.5062467131/52)+1-(1-.0004864819)^7)),
#((0.10/52)*((.5062467131/52)+1-(1-.0004864819)^7)))

#  Minimum age at entry .

birth.age<-13
out.age.prob<-c(0.44, 0.13, 0.12, 0.11, 0.10, 0.10)

debut.entry.prob<-.5062467131
debut.prob<- 1-(1-.0004864819)^7



# Quintile distribution weights for risk groups for partnership formation 
riskg.asmm <- c(.039682539, 0.079365079, 0.119047619 , 0.182539682, 0.579365079)




ages<-13:39
ages.asmm<-13:18
ages.yamsm<-19:25
ages.oamsm<-26:39
ages.msm<-19:39

# Age-sex-specific mortality rates

asmr.B <- c(rep(0, 12),
            1-(1-c(rep(0.00159, 6),
                   rep(0.00159, 7),
                   rep(0.00225, 10),
                   rep(0.00348, 4)))^(1/(365/time.unit)), 1)

asmr.W <- c(rep(0, 12),
            1-(1-c(rep(0.00103, 6),
                   rep(0.00103, 7),
                   rep(0.00133, 10),
                   rep(0.00214, 4)))^(1/(365/time.unit)), 1)


# I, R, V role frequencies
role.B.prob.asmm <- c(0.145, 0.282, 0.573)
role.W.prob.asmm <- c(0.145, 0.282, 0.573)

# I, R, V role frequencies
role.B.prob.msm <- role.W.prob.msm <-
  (c(0.242, 0.321, 0.437) + c(0.228, 0.228, 0.544))/2


r.to.i <- (0.282 - ((0.321 + 0.228) /2 )) / 0.282 
v.to.i <- (0.573 - ((0.437 + 0.544) /2 )) / 0.573 
role.shift <- c(r.to.i, v.to.i)

# Create meanstats
st <- calc_nwstats_msm(
  method = 1,
  time.unit = time.unit,
  num.B = num.B,
  num.W = num.W,
  num.B.msm = num.B.msm,
  num.W.msm = num.W.msm,
  num.B.asmm = num.B.asmm,
  num.W.asmm = num.W.asmm,
  deg.mp.B = deg.mp.B,
  deg.mp.W = deg.mp.W,
  mdeg.inst.B = mdeg.inst.B,
  mdeg.inst.W = mdeg.inst.W,
  deg.asmm = deg.asmm,
  cross.frac = cross.frac,
  qnts.B = qnts.B,
  qnts.W = qnts.W,
  prop.hom.mpi.B = prop.hom.mpi.B,
  prop.hom.mpi.W = prop.hom.mpi.W,
  balance = "mean",
  sqrt.adiff.BB = sqrt.adiff.BB,
  sqrt.adiff.WW = sqrt.adiff.WW,
  sqrt.adiff.BW = sqrt.adiff.BW,
  diss.main = ~offset(edges),
  diss.pers = ~offset(edges),
  diss.asmm = ~offset(edges),
  durs.main = durs.main,
  durs.pers = durs.pers,
  rates.asmm = rates.asmm,
  durs.asmm = durs.asmm,
  ages = ages,
  ages.asmm = ages.asmm,
  ages.yamsm = ages.yamsm,
  ages.oamsm = ages.oamsm,
  ages.msm = ages.msm,
  birth.age = birth.age,
  out.age.prob = out.age.prob,
  debut.entry.prob = debut.entry.prob,
  debut.prob = debut.prob,
  asmr.B = asmr.B,
  asmr.W = asmr.W,
  role.B.prob.msm = role.B.prob.msm,
  role.W.prob.msm = role.W.prob.msm,
  role.B.prob.asmm = role.B.prob.asmm,
  role.W.prob.asmm = role.W.prob.asmm,
  role.shift = role.shift)


save(st, file = "~/Campcl/scenarios/est/nwstats.rda")

