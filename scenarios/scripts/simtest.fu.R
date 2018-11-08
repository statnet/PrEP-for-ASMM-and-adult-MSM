
## Packages
library("methods")
suppressMessages(library("EpiModelHIV"))
library("EpiModelHPC")



load("~/Campcl/scenarios/est/fit.rda")
load("~/Campcl/scenarios/est/nwstats.rda")

param <- param_cl(nwstats = st,
                  FU = FALSE,
                  testing.pattern = "interval",
                  
                  prep.start = Inf,
                  prep.start.asmm = Inf,

                  ai.pers.scale = 1.2316111,
                  ai.asmm.scale = 6.2866832,
                  cond.asmm.BB.prob = 0.2338672,
                  cond.asmm.BW.prob = 0.2338672,
                  cond.asmm.WW.prob = 0.2338672)

init <- init_cl(st,
                prev.B = 0.18,
                prev.W = 0.18,
                prev.asmm = 0.045)

control <- control_cl(nsteps =2000)

## Simulation
s_lo<-netsim(est, param, init, control)


t<-1:2000
plot(t,tail(s_lo$epi$i.prev[,1],2000),type="l",col = "black")
lines(tail(s_lo$epi$i.prev.msm[,1],2000), col="red")
lines(tail(s_lo$epi$i.prev.asmm[,1],2000), col="blue")