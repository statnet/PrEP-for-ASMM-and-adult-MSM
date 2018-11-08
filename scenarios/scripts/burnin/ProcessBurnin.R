
## Process burn-in
library("EpiModelHPC")
library("EpiModelHIV")



##PREV
sim <- merge_simfiles(1, indir = "~/Campcl/scenarios/data/", ftype = "max")
simtest <- get_sims2(sim, tar = c(.28,.07), var = c("i.prev.msm","i.prev.age18"))
tail(as.data.frame(simtest)$i.prev.msm)

plot(simtest, y = "i.prev.msm", ylim = c(0.22, 0.32), qnts = 0.5)
abline(h = 0.28)
plot(simtest, y = "i.prev.age18", ylim = c(0.03, 0.12), qnts = 0.5)
abline(h = 0.07)

df <- as.data.frame(simtest)
round(mean(tail(df$i.prev, 100)), 3)

round(mean(tail(df$i.prev.msm,100)),3)
round(mean(tail(df$i.prev.age18,100)),3)



save(sim, file = "~/Campcl/scenarios/est/p2.burnin.rda")

