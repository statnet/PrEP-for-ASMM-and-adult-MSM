
## abc model analysis

library("EpiModelHIV")
library("EasyABC")


## averaged fits
load("~/Campcl/scenarios/data/abc/clR.10pct.10sim.rda")


p <- as.data.frame(a$param)
s <- as.data.frame(a$stats)
w <- a$weights
tar <- c(.283, 0.07)

names(p) <- c("ai.pers.scale", "ai.asmm.scale", "cond.asmm.prob")

names(s) <- c("prev.msm", "prev.age18")

mean.s <- apply(s, 2, function(x) sum(x * w))
mean.p <- apply(p, 2, function(x) sum(x * w))



mean.p
mean.s

par(mar = c(3,3,1,1), mgp = c(2,1,0), mfrow = c(3,2))
for (i in 1:ncol(s)) {
  hist(s[, i], col = "bisque2", border = "white", main = names(s)[i])
  abline(v = tar[i], lwd = 2, col = "red")
}

par(mar = c(3,3,1,1), mgp = c(2,1,0), mfrow = c(4,4))
for (i in 1:ncol(p)) {
  hist(p[, i], col = "bisque2", border = "white", main = names(p)[i])
}

save(mean.p, file = "~/Campcl/scenarios/data/abc/abc.avg.parms.10pct.rda")
save(mean.p, file = "~/Campcl/scenarios/est/meta.parms.rda")



