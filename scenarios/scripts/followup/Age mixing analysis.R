
## whamp analysis file

library(EpiModelHIV)
library(EpiModelHPC)

load("~/Campcl/scenarios/out/sim.agemixing.rda")

x<-as.data.frame(sim.agemixing$el.age)

x$diff<-abs(x$age1-x$age2)
x$age1.r<-floor(x$age1)
x$age2.r<-floor(x$age2)

y.y <-which(x$age1.r < 19 & x$age2.r < 19)
o.o <-which(x$age1.r >= 19 & x$age2.r >= 19)
mix.1 <-length(x$age1.r)-(length(y.y)+length(o.o))
mix2 <- which((x$age1.r < 19 & x$age2.r >= 19) | (x$age1.r >= 19 & x$age2.r < 19))

l1<-x$age1.r[y.y]
l2<-x$age2.r[y.y]
l3<-x$age1.r[mix2]
l4<-x$age2.r[mix2]
ages<-c(l1,l2,l3,l4)