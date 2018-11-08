
## Packages
library("methods")
suppressMessages(library("EpiModelHIV"))
library("EpiModelHPC")



#load("~/Campcl/scenarios/est/fit.rda")
load("~/Campcl/scenarios/est/nwstats.rda")
load("~/Campcl/scenarios/est/p2.burnin.rda")

param <- param_cl(nwstats = st,
                  
                  testing.pattern = "interval",
                  
                  FU = TRUE,
                  prep.start = 8002,
                  prep.elig.model = "cdc3",
                  prep.coverage.adol.naive = .4,
                  prep.coverage.adol.exp = .4,
                  
                  prep.start.asmm = Inf,
                  prep.elig.model.asmm = "adol.AI.older.time",
                  prep.coverage.asmm = .0,
                  
                  ai.pers.scale = 1.2316111,
                  ai.asmm.scale = 6.2866832,
                  cond.asmm.BB.prob = 0.2338672,
                  cond.asmm.BW.prob = 0.2338672,
                  cond.asmm.WW.prob = 0.2338672)

init <- init_cl(st,
                prev.B = 0.18,
                prev.W = 0.18,
                prev.asmm = 0.045)

control <- control_cl(nsteps =9000,
                      start = 8001,
                      initialize.FUN = reinit_msm)


## Simulation
sim.a<-netsim(sim, param, init, control)

save(sim.a, file = "~/Campcl/scenarios/out/sim.a.rda")


param <- param_cl(nwstats = st,
                  
                  testing.pattern = "interval",
                  
                  FU = TRUE,
                  prep.start = 8002,
                  prep.elig.model = "cdc3",
                  prep.coverage.adol.naive = .4,
                  prep.coverage.adol.exp = .4,
                  
                  prep.start.asmm = 8002,
                  prep.elig.model.asmm = "adol.AI.older.time",
                  prep.coverage.asmm = .3,
                  
                  ai.pers.scale = 1.2316111,
                  ai.asmm.scale = 6.2866832,
                  cond.asmm.BB.prob = 0.2338672,
                  cond.asmm.BW.prob = 0.2338672,
                  cond.asmm.WW.prob = 0.2338672)

init <- init_cl(st,
                prev.B = 0.18,
                prev.W = 0.18,
                prev.asmm = 0.045)

control <- control_cl(nsteps =9000,
                      start = 8001,
                      initialize.FUN = reinit_msm)

## Simulation
sim.b<-netsim(sim, param, init, control)

save(sim.b, file = "~/Campcl/scenarios/out/sim.b.rda")


param <- param_cl(nwstats = st,
                  
                  testing.pattern = "interval",
                  
                  FU = TRUE,
                  prep.start = 8002,
                  prep.elig.model = "cdc3",
                  prep.coverage.adol.naive = .4,
                  prep.coverage.adol.exp = .4,
                  
                  prep.start.asmm = 8002,
                  prep.elig.model.asmm = "adol.AI.older.time",
                  prep.coverage.asmm = .5,
                  
                  ai.pers.scale = 1.2316111,
                  ai.asmm.scale = 6.2866832,
                  cond.asmm.BB.prob = 0.2338672,
                  cond.asmm.BW.prob = 0.2338672,
                  cond.asmm.WW.prob = 0.2338672)

init <- init_cl(st,
                prev.B = 0.18,
                prev.W = 0.18,
                prev.asmm = 0.045)

control <- control_cl(nsteps =9000,
                      start = 8001,
                      initialize.FUN = reinit_msm)

## Simulation
sim.c<-netsim(sim, param, init, control)

save(sim.c, file = "~/Campcl/scenarios/out/sim.c.rda")



t<-1:1000
plot(t,tail(sim.a$epi$i.prev[,1],1000),type="l",col = "black")
lines(tail(sim.b$epi$i.prev[,1],1000), col="red")
lines(tail(sim.c$epi$i.prev[,1],1000), col="blue")

t<-1:1000
plot(t,tail(sim.a$epi$i.prev.asmm[,1],1000),type="l",col = "black")
lines(tail(sim.b$epi$i.prev.asmm[,1],1000), col="red")
lines(tail(sim.c$epi$i.prev.asmm[,1],1000), col="blue")

t<-1:1000
plot(t,tail(sim.a$epi$prepCurr.msm[,1],1000),type="l",col = "black")
lines(tail(sim.b$epi$prepCurr.msm[,1],1000), col="red")
lines(tail(sim.c$epi$prepCurr.msm[,1],1000), col="blue")

t<-1:1000
plot(t,tail(sim.a$epi$prepCov.adol.naive[,1],1000),type="l",col = "black")
lines(tail(sim.b$epi$prepCov.adol.naive[,1],1000), col="red")
lines(tail(sim.c$epi$prepCov.adol.naive[,1],1000), col="blue")

t<-1:1000
plot(t,tail(sim.a$epi$prepCurr.asmm[,1],1000),type="l",col = "black", ylim=c(0,4000))
lines(tail(sim.b$epi$prepCurr.asmm[,1],1000), col="red")
lines(tail(sim.c$epi$prepCurr.asmm[,1],1000), col="blue")

t<-1:1000
plot(t,tail(sim.a$epi$prepCov.asmm[,1],1000),type="l",col = "black", ylim=c(0,.5))
lines(tail(sim.b$epi$prepCov.asmm[,1],1000), col="red")
lines(tail(sim.c$epi$prepCov.asmm[,1],1000), col="blue")

t<-1:1000
plot(t,tail(sim.a$epi$prepCov.msm[,1],1000),type="l",col = "black", ylim=c(0,.5))
lines(tail(sim.b$epi$prepCov.msm[,1],1000), col="red")
lines(tail(sim.c$epi$prepCov.msm[,1],1000), col="blue")

