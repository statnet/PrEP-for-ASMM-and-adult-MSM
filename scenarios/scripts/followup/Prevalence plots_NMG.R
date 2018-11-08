
## CAMP analysis file

library(EpiModelHIV)
library(EpiModelHPC)


s1 <- merge_simfiles(1, indir = "~/Campcl/scenarios/data/")
s2 <- merge_simfiles(2, indir = "~/Campcl/scenarios/data/")
s3 <- merge_simfiles(3, indir = "~/Campcl/scenarios/data/")
s4 <- merge_simfiles(4, indir = "~/Campcl/scenarios/data/")
s5 <- merge_simfiles(5, indir = "~/Campcl/scenarios/data/")
s6 <- merge_simfiles(6, indir = "~/Campcl/scenarios/data/")

# Figure 1 ----------------------------------------------------------------

# Line plots of cumulative PIA

steps <- 1:780

base.prev.all<-rep(NA,780)
base.prev.all_hi<-rep(NA,780)
base.prev.all_low<-rep(NA,780)

base.prev.msm<-rep(NA,780)
base.prev.msm_hi<-rep(NA,780)
base.prev.msm_low<-rep(NA,780)

base.prev.asmm<-rep(NA,780)
base.prev.asmm_hi<-rep(NA,780)
base.prev.asmm_low<-rep(NA,780)



##Intervention 1 adult only.
inter.1.prev.all<-rep(NA,780)
inter.1.prev.all_hi<-rep(NA,780)
inter.1.prev.all_low<-rep(NA,780)

inter.1.prev.msm<-rep(NA,780)
inter.1.prev.msm_hi<-rep(NA,780)
inter.1.prev.msm_low<-rep(NA,780)

inter.1.prev.asmm<-rep(NA,780)
inter.1.prev.asmm_hi<-rep(NA,780)
inter.1.prev.asmm_low<-rep(NA,780)


##Intervention 2 Adults plus ASMM @ 10%

inter.2.prev.all<-rep(NA,780)
inter.2.prev.all_hi<-rep(NA,780)
inter.2.prev.all_low<-rep(NA,780)

inter.2.prev.msm<-rep(NA,780)
inter.2.prev.msm_hi<-rep(NA,780)
inter.2.prev.msm_low<-rep(NA,780)

inter.2.prev.asmm<-rep(NA,780)
inter.2.prev.asmm_hi<-rep(NA,780)
inter.2.prev.asmm_low<-rep(NA,780)


##Intervention 3 Adults plus ASMM @ 20%

inter.3.prev.all<-rep(NA,780)
inter.3.prev.all_hi<-rep(NA,780)
inter.3.prev.all_low<-rep(NA,780)

inter.3.prev.msm<-rep(NA,780)
inter.3.prev.msm_hi<-rep(NA,780)
inter.3.prev.msm_low<-rep(NA,780)

inter.3.prev.asmm<-rep(NA,780)
inter.3.prev.asmm_hi<-rep(NA,780)
inter.3.prev.asmm_low<-rep(NA,780)


##Intervention 4 Adults plus ASMM @ 30%

inter.4.prev.all<-rep(NA,780)
inter.4.prev.all_hi<-rep(NA,780)
inter.4.prev.all_low<-rep(NA,780)

inter.4.prev.msm<-rep(NA,780)
inter.4.prev.msm_hi<-rep(NA,780)
inter.4.prev.msm_low<-rep(NA,780)

inter.4.prev.asmm<-rep(NA,780)
inter.4.prev.asmm_hi<-rep(NA,780)
inter.4.prev.asmm_low<-rep(NA,780)

##Intervention 5 Adults plus ASMM @ 40%

inter.5.prev.all<-rep(NA,780)
inter.5.prev.all_hi<-rep(NA,780)
inter.5.prev.all_low<-rep(NA,780)

inter.5.prev.msm<-rep(NA,780)
inter.5.prev.msm_hi<-rep(NA,780)
inter.5.prev.msm_low<-rep(NA,780)

inter.5.prev.asmm<-rep(NA,780)
inter.5.prev.asmm_hi<-rep(NA,780)
inter.5.prev.asmm_low<-rep(NA,780)


x.all <- x.1.all <- x.2.all <- x.3.all <- x.4.all <- x.5.all <- NULL
x.msm <- x.1.msm <- x.2.msm <- x.3.msm <- x.4.msm <- x.5.msm <- NULL 
x.asmm <- x.1.asmm <- x.2.asmm <- x.3.asmm <- x.4.asmm <- x.5.asmm  <- NULL

for (i in seq_along(steps)) {
  sim.base <- truncate_sim(s1, at = 8001)
  
  x.all<-sort(as.numeric(sim.base$epi$i.prev[i,1:25]))
  base.prev.all[i]<-mean(x.all)
  base.prev.all_hi[i]<-mean(x.all[24],x.all[25])
  base.prev.all_low[i]<-mean(x.all[1],x.all[2])
  
  x.msm<-sort(as.numeric(sim.base$epi$i.prev.msm[i,1:25]))
  base.prev.msm[i]<-mean(x.msm)
  base.prev.msm_hi[i]<-mean(x.msm[24],x.msm[25])
  base.prev.msm_low[i]<-mean(x.msm[1],x.msm[2])
  
  x.asmm<-sort(as.numeric(sim.base$epi$i.prev.age18[i,1:25]))
  base.prev.asmm[i]<-mean(x.asmm)
  base.prev.asmm_hi[i]<-mean(x.asmm[24],x.asmm[25])
  base.prev.asmm_low[i]<-mean(x.asmm[1],x.asmm[2])
  
##################.
  sim.int1 <- truncate_sim(s2, at = 8001)
  mn <- head(as.data.frame(sim.int1), steps[i])
  
  x.1.all<-sort(as.numeric(sim.int1$epi$i.prev[i,1:25]))
  inter.1.prev.all[i]<-mean(x.1.all)
  inter.1.prev.all_hi[i]<-mean(x.1.all[24],x.1.all[25])
  inter.1.prev.all_low[i]<-mean(x.1.all[1],x.1.all[2])
  
  x.1.msm<-sort(as.numeric(sim.int1$epi$i.prev.msm[i,1:25]))
  inter.1.prev.msm[i]<-mean(x.1.msm)
  inter.1.prev.msm_hi[i]<-mean(x.1.msm[24],x.1.msm[25])
  inter.1.prev.msm_low[i]<-mean(x.1.msm[1],x.1.msm[2])
  
  x.1.asmm<-sort(as.numeric(sim.int1$epi$i.prev.age18[i,1:25]))
  inter.1.prev.asmm[i]<-mean(x.1.asmm)
  inter.1.prev.asmm_hi[i]<-mean(x.1.asmm[24],x.1.asmm[25])
  inter.1.prev.asmm_low[i]<-mean(x.1.asmm[1],x.1.asmm[2])


  ##############.
#  sim.int2 <- truncate_sim(s3, at = 8001)
#  mn <- head(as.data.frame(sim.int2), steps[i])
#  
#  x.2.all<-sort(as.numeric(sim.int2$epi$i.prev[i,1:50]))
#  inter.2.prev.all[i]<-mean(x.2.all)
#  inter.2.prev.all_hi[i]<-mean(x.2.all[48],x.2.all[49])
#  inter.2.prev.all_low[i]<-mean(x.2.all[3],x.2.all[2])
#  
#  x.2.msm<-sort(as.numeric(sim.int2$epi$i.prev.msm[i,1:50]))
#  inter.2.prev.msm[i]<-mean(x.2.msm)
#  inter.2.prev.msm_hi[i]<-mean(x.2.msm[48],x.2.msm[49])
#  inter.2.prev.msm_low[i]<-mean(x.2.msm[3],x.2.msm[2])
#  
#  x.2.asmm<-sort(as.numeric(sim.int2$epi$i.prev.age18[i,1:50]))
#  inter.2.prev.asmm[i]<-mean(x.2.asmm)
#  inter.2.prev.asmm_hi[i]<-mean(x.2.asmm[48],x.2.asmm[49])
#  inter.2.prev.asmm_low[i]<-mean(x.2.asmm[3],x.2.asmm[2])
  

  ##############.
#  sim.int3 <- truncate_sim(s4, at = 8001)
#  mn <- head(as.data.frame(sim.int3), steps[i])
#  
#  x.3.all<-sort(as.numeric(sim.int3$epi$i.prev[i,1:50]))
#  inter.3.prev.all[i]<-mean(x.3.all)
#  inter.3.prev.all_hi[i]<-mean(x.3.all[48],x.3.all[49])
#  inter.3.prev.all_low[i]<-mean(x.3.all[3],x.3.all[2])
#  
#  x.3.msm<-sort(as.numeric(sim.int3$epi$i.prev.msm[i,1:50]))
#  inter.3.prev.msm[i]<-mean(x.3.msm)
#  inter.3.prev.msm_hi[i]<-mean(x.3.msm[48],x.3.msm[49])
#  inter.3.prev.msm_low[i]<-mean(x.3.msm[3],x.3.msm[2])
#  
#  x.3.asmm<-sort(as.numeric(sim.int3$epi$i.prev.age18[i,1:50]))
#  inter.3.prev.asmm[i]<-mean(x.3.asmm)
#  inter.3.prev.asmm_hi[i]<-mean(x.3.asmm[48],x.3.asmm[49])
#  inter.3.prev.asmm_low[i]<-mean(x.3.asmm[3],x.3.asmm[2])
  

  
##############.
  sim.int4 <- truncate_sim(s5, at = 8001)
  mn <- head(as.data.frame(sim.int4), steps[i])
  
  x.4.all<-sort(as.numeric(sim.int4$epi$i.prev[i,1:25]))
  inter.4.prev.all[i]<-mean(x.4.all)
  inter.4.prev.all_hi[i]<-mean(x.4.all[24],x.4.all[25])
  inter.4.prev.all_low[i]<-mean(x.4.all[1],x.4.all[2])
  
  x.4.msm<-sort(as.numeric(sim.int4$epi$i.prev.msm[i,1:25]))
  inter.4.prev.msm[i]<-mean(x.4.msm)
  inter.4.prev.msm_hi[i]<-mean(x.4.msm[24],x.4.msm[25])
  inter.4.prev.msm_low[i]<-mean(x.4.msm[1],x.4.msm[2])
  
  x.4.asmm<-sort(as.numeric(sim.int4$epi$i.prev.age18[i,1:25]))
  inter.4.prev.asmm[i]<-mean(x.4.asmm)
  inter.4.prev.asmm_hi[i]<-mean(x.4.asmm[24],x.4.asmm[25])
  inter.4.prev.asmm_low[i]<-mean(x.4.asmm[1],x.4.asmm[2])
  

  ##############.
#  sim.int5 <- truncate_sim(s6, at = 8001)
#  mn <- head(as.data.frame(sim.int5), steps[i])
#  
#  x.5.all<-sort(as.numeric(sim.int5$epi$i.prev[i,1:50]))
#  inter.5.prev.all[i]<-mean(x.5.all)
#  inter.5.prev.all_hi[i]<-mean(x.5.all[48],x.5.all[49])
#  inter.5.prev.all_low[i]<-mean(x.5.all[3],x.5.all[2])
#  
#  x.5.msm<-sort(as.numeric(sim.int5$epi$i.prev.msm[i,1:50]))
#  inter.5.prev.msm[i]<-mean(x.5.msm)
#  inter.5.prev.msm_hi[i]<-mean(x.5.msm[48],x.5.msm[49])
#  inter.5.prev.msm_low[i]<-mean(x.5.msm[3],x.5.msm[2])
  
#  x.5.asmm<-sort(as.numeric(sim.int5$epi$i.prev.age18[i,1:50]))
#  inter.5.prev.asmm[i]<-mean(x.5.asmm)
#  inter.5.prev.asmm_hi[i]<-mean(x.5.asmm[48],x.5.asmm[49])
#  inter.5.prev.asmm_low[i]<-mean(x.5.asmm[3],x.5.asmm[2])
  
  

}

base.prev.all <- base.prev.all * 100
base.prev.all_hi <- base.prev.all_hi * 100
base.prev.all_low <- base.prev.all_low * 100

base.prev.msm <- base.prev.msm * 100
base.prev.msm_hi <- base.prev.msm_hi * 100
base.prev.msm_low <- base.prev.msm_low * 100

base.prev.asmm <- base.prev.asmm * 100
base.prev.asmm_hi <- base.prev.asmm_hi * 100
base.prev.asmm_low <- base.prev.asmm_low * 100

############# 1. 
inter.1.prev.all <- inter.1.prev.all * 100
inter.1.prev.all_hi <- inter.1.prev.all_hi * 100
inter.1.prev.all_low <- inter.1.prev.all_low * 100

inter.1.prev.msm <- inter.1.prev.msm * 100
inter.1.prev.msm_hi <- inter.1.prev.msm_hi * 100
inter.1.prev.msm_low <- inter.1.prev.msm_low * 100

inter.1.prev.asmm <- inter.1.prev.asmm * 100
inter.1.prev.asmm_hi <- inter.1.prev.asmm_hi * 100
inter.1.prev.asmm_low <- inter.1.prev.asmm_low * 100

################# 2.
#inter.2.prev.all <- inter.2.prev.all * 100
#inter.2.prev.all_hi <- inter.2.prev.all_hi * 100
#inter.2.prev.all_low <- inter.2.prev.all_low * 100

#inter.2.prev.msm <- inter.2.prev.msm * 100
#inter.2.prev.msm_hi <- inter.2.prev.msm_hi * 100
#inter.2.prev.msm_low <- inter.2.prev.msm_low * 100

#inter.2.prev.asmm <- inter.2.prev.asmm * 100
#inter.2.prev.asmm_hi <- inter.2.prev.asmm_hi * 100
#inter.2.prev.asmm_low <- inter.2.prev.asmm_low * 100

################# 3.
#inter.3.prev.all <- inter.3.prev.all * 100
#inter.3.prev.all_hi <- inter.3.prev.all_hi * 100
#inter.3.prev.all_low <- inter.3.prev.all_low * 100

#inter.3.prev.msm <- inter.3.prev.msm * 100
#inter.3.prev.msm_hi <- inter.3.prev.msm_hi * 100
#inter.3.prev.msm_low <- inter.3.prev.msm_low * 100

#inter.3.prev.asmm <- inter.3.prev.asmm * 100
#inter.3.prev.asmm_hi <- inter.3.prev.asmm_hi * 100
#inter.3.prev.asmm_low <- inter.3.prev.asmm_low * 100

################# 4.
inter.4.prev.all <- inter.4.prev.all * 100
inter.4.prev.all_hi <- inter.4.prev.all_hi * 100
inter.4.prev.all_low <- inter.4.prev.all_low * 100

inter.4.prev.msm <- inter.4.prev.msm * 100
inter.4.prev.msm_hi <- inter.4.prev.msm_hi * 100
inter.4.prev.msm_low <- inter.4.prev.msm_low * 100

inter.4.prev.asmm <- inter.4.prev.asmm * 100
inter.4.prev.asmm_hi <- inter.4.prev.asmm_hi * 100
inter.4.prev.asmm_low <- inter.4.prev.asmm_low * 100

################# 5.
#inter.5.prev.all <- inter.5.prev.all * 100
#inter.5.prev.all_hi <- inter.5.prev.all_hi * 100
#inter.5.prev.all_low <- inter.5.prev.all_low * 100

#inter.5.prev.msm <- inter.5.prev.msm * 100
#inter.5.prev.msm_hi <- inter.5.prev.msm_hi * 100
#inter.5.prev.msm_low <- inter.5.prev.msm_low * 100

#inter.5.prev.asmm <- inter.5.prev.asmm * 100
#inter.5.prev.asmm_hi <- inter.5.prev.asmm_hi * 100
#inter.5.prev.asmm_low <- inter.5.prev.asmm_low * 100

library(wesanderson)
pal <- wes_palette("Zissou")[5]
pal2<- wes_palette("Zissou")[2]
  
## For Paper
#pdf(file = "Fig1.pdf", height = 6, width = 12, pointsize = 16)
tiff(filename = "~/Campcl/scenarios/out/Fig_PrEP 40_0_NMG.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(3,3,2.5,5), mgp = c(2,1,0))
xticks <- seq(0, 780, 52)
yticks <- seq(0,40,5)
plot(base.prev.all, type = "n", ylim = c(0, 35), lwd = 3, col="black", axes=FALSE,
     main = "HIV Prevalence: Adult MSM PrEP - 40%, ASMM PrEP - 0%", xlab = "Years", ylab = "HIV Prevalence")
axis(1, at = xticks, labels = c("-5","-4","-3","-2","-1","0","1","2","3","4","5","6","7","8","9","10"), col.axis="black", las=1, cex.axis=.6)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.6)

##ALL
xx <- c(1:(length(base.prev.all)), (length(base.prev.all)):1)
yy <- c(base.prev.all_low, rev(base.prev.all_hi))
polygon(xx, yy, col = EpiModel::transco("light blue", alpha = 0.5), border = NA)
lines(base.prev.all, lwd = 1, col = "light blue")

aa <- c(1:(length(inter.1.prev.all)), (length(inter.1.prev.all)):1)
bb <- c(inter.1.prev.all_low, rev(inter.1.prev.all_hi))
polygon(aa, bb, col = EpiModel::transco("blue", alpha = 0.3), border = NA)
lines(inter.1.prev.all, lwd = 2, col = "blue")

##MSM
xx <- c(1:(length(base.prev.msm)), (length(base.prev.msm)):1)
yy <- c(base.prev.msm_low, rev(base.prev.msm_hi))
polygon(xx, yy, col = EpiModel::transco("red", alpha = 0.3), border = NA)
lines(base.prev.msm, lwd = 1, col = "red")

aa <- c(1:(length(inter.1.prev.msm)), (length(inter.1.prev.msm)):1)
bb <- c(inter.1.prev.msm_low, rev(inter.1.prev.msm_hi))
polygon(aa, bb, col = EpiModel::transco("red", alpha = 0.3), border = NA)
lines(inter.1.prev.msm, lwd = 1, col = "red")

##ASMM
xx <- c(1:(length(base.prev.asmm)), (length(base.prev.asmm)):1)
yy <- c(base.prev.asmm_low, rev(base.prev.asmm_hi))
polygon(xx, yy, col = EpiModel::transco("light green", alpha = 0.3), border = NA)
lines(base.prev.asmm, lwd = 1, col = "light green")

aa <- c(1:(length(inter.1.prev.asmm)), (length(inter.1.prev.asmm)):1)
bb <- c(inter.1.prev.asmm_low, rev(inter.1.prev.asmm_hi))
polygon(aa, bb, col = EpiModel::transco("green", alpha = 0.3), border = NA)
lines(inter.1.prev.asmm, lwd = 2, col = "green")

legend("topright",c("All","Adult MSM", "ASMM"),text.col=c("blue","red", "green") )
dev.off()

###############################################################################################
#pdf(file = "Fig1.pdf", height = 6, width = 12, pointsize = 16)
#tiff(filename = "Fig_PrEP 40/10.tiff", height = 7, width = 7, units = "in", res = 250)

#par(mfrow = c(1,1), mar = c(3,3,2.5,5), mgp = c(2,1,0))
#xticks <- seq(0, 780, 52)
#yticks <- seq(0,40,5)
#plot(base.prev.all, type = "n", ylim = c(0, 35), lwd = 3, col="black", axes=FALSE,
#     main = "HIV Prevalence: Adult MSM PrEP - 40%, ASMM PrEP - 10%", xlab = "Years", ylab = "HIV Prevalence")
#axis(1, at = xticks, labels = c("-5","-4","-3","-2","-1","0","1","2","3","4","5","6","7","8","9","10"), col.axis="black", las=1, cex.axis=.6)
#axis(2, at = yticks, col.axis="black", las=1, cex.axis=.6)

##ALL
#xx <- c(1:(length(base.prev.all)), (length(base.prev.all)):1)
#yy <- c(base.prev.all_low, rev(base.prev.all_hi))
#polygon(xx, yy, col = EpiModel::transco("red", alpha = 0.3), border = NA)
#lines(base.prev.all, lwd = 1, col = "red")

#aa <- c(1:(length(inter.2.prev.all)), (length(inter.2.prev.all)):1)
#bb <- c(inter.2.prev.all_low, rev(inter.2.prev.all_hi))
#polygon(aa, bb, col = EpiModel::transco("red", alpha = 0.3), border = NA)
#lines(inter.2.prev.all, lwd = 2, col = "red")

##MSM
#xx <- c(1:(length(base.prev.msm)), (length(base.prev.msm)):1)
#yy <- c(base.prev.msm_low, rev(base.prev.msm_hi))
#polygon(xx, yy, col = EpiModel::transco("light blue", alpha = 0.3), border = NA)
#lines(base.prev.msm, lwd = 2, col = "light blue")

#aa <- c(1:(length(inter.2.prev.msm)), (length(inter.2.prev.msm)):1)
#bb <- c(inter.2.prev.msm_low, rev(inter.2.prev.msm_hi))
#polygon(aa, bb, col = EpiModel::transco("blue", alpha = 0.3), border = NA)
#lines(inter.2.prev.msm, lwd = 2, col = "blue")

##ASMM
#xx <- c(1:(length(base.prev.asmm)), (length(base.prev.asmm)):1)
#yy <- c(base.prev.asmm_low, rev(base.prev.asmm_hi))
#polygon(xx, yy, col = EpiModel::transco("light green", alpha = 0.3), border = NA)
#lines(base.prev.asmm, lwd = 2, col = "light green")

#aa <- c(1:(length(inter.2.prev.asmm)), (length(inter.2.prev.asmm)):1)
#bb <- c(inter.2.prev.asmm_low, rev(inter.2.prev.asmm_hi))
#polygon(aa, bb, col = EpiModel::transco("green", alpha = 0.3), border = NA)
#lines(inter.2.prev.asmm, lwd = 2, col = "green")

#dev.off()

###############################################################################################
#pdf(file = "Fig1.pdf", height = 6, width = 12, pointsize = 16)
#tiff(filename = "Fig_PrEP 40/20.tiff", height = 7, width = 7, units = "in", res = 250)

#par(mfrow = c(1,1), mar = c(3,3,2.5,5), mgp = c(2,1,0))
#xticks <- seq(0, 780, 52)
#yticks <- seq(0,40,5)
#plot(base.prev.all, type = "n", ylim = c(0, 16), lwd = 3, col="black", axes=FALSE,
#     main = "HIV Prevalence: Adult MSM PrEP - 40%, ASMM PrEP - 20%", xlab = "Years", ylab = "HIV Prevalence")
#axis(1, at = xticks, labels = c("-5","-4","-3","-2","-1","0","1","2","3","4","5","6","7","8","9","10"), col.axis="black", las=1, cex.axis=.6)
#axis(2, at = yticks, col.axis="black", las=1, cex.axis=.6)

##ALL
#xx <- c(1:(length(base.prev.all)), (length(base.prev.all)):1)
#yy <- c(base.prev.all_low, rev(base.prev.all_hi))
#polygon(xx, yy, col = EpiModel::transco("purple", alpha = 0.3), border = NA)
#lines(base.prev.w, lwd = 2, col = "purple")

#aa <- c(1:(length(inter.prev.all)), (length(inter.prev.all)):1)
#bb <- c(inter.prev.all_low, rev(inter.prev.all_hi))
#polygon(aa, bb, col = EpiModel::transco("blue", alpha = 0.3), border = NA)
#lines(inter.prev.all, lwd = 2, col = "blue")

##MSM
#xx <- c(1:(length(base.prev.msm)), (length(base.prev.msm)):1)
#yy <- c(base.prev.msm_low, rev(base.prev.msm_hi))
#polygon(xx, yy, col = EpiModel::transco("pink", alpha = 0.3), border = NA)
#lines(inter.4.prev.msm, lwd = 2, col = "pink")

#aa <- c(1:(length(inter.3.prev.msm)), (length(inter.3.prev.msm)):1)
#bb <- c(inter.3.prev.msm_low, rev(inter.3.prev.msm_hi))
#polygon(aa, bb, col = EpiModel::transco("red", alpha = 0.3), border = NA)
#lines(inter.3.prev.msm, lwd = 2, col = "red")

##ASMM
#xx <- c(1:(length(base.prev.asmm)), (length(base.prev.asmm)):1)
#yy <- c(base.asmm_low, rev(base.prev.asmm_hi))
#polygon(xx, yy, col = EpiModel::transco("pink", alpha = 0.3), border = NA)
#lines(inter.4.prev.asmm, lwd = 2, col = "pink")

#aa <- c(1:(length(inter.3.prev.asmm)), (length(inter.3.prev.asmm)):1)
#bb <- c(inter.3.prev.asmm_low, rev(inter.3.prev.asmm_hi))
#polygon(aa, bb, col = EpiModel::transco("red", alpha = 0.3), border = NA)
#lines(inter.3.prev.asmm, lwd = 2, col = "red")

#dev.off()

###############################################################################################
#pdf(file = "Fig1.pdf", height = 6, width = 12, pointsize = 16)
tiff(filename = "~/Campcl/scenarios/out/Fig_PrEP 40_30_NMG.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(3,3,2.5,5), mgp = c(2,1,0))
xticks <- seq(0, 780, 52)
yticks <- seq(0,40,5)
plot(base.prev.all, type = "n", ylim = c(0, 40), lwd = 3, col="black", axes=FALSE,
     main = "HIV Prevalence: Adult MSM PrEP - 40%, ASMM PrEP - 30%", xlab = "Years", ylab = "HIV Prevalence")
axis(1, at = xticks, labels = c("-5","-4","-3","-2","-1","0","1","2","3","4","5","6","7","8","9","10"), col.axis="black", las=1, cex.axis=.6)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.6)

##ALL
xx <- c(1:(length(base.prev.all)), (length(base.prev.all)):1)
yy <- c(base.prev.all_low, rev(base.prev.all_hi))
polygon(xx, yy, col = EpiModel::transco("light blue", alpha = 0.3), border = NA)
lines(base.prev.all, lwd = 2, col = "light blue")

aa <- c(1:(length(inter.4.prev.all)), (length(inter.4.prev.all)):1)
bb <- c(inter.4.prev.all_low, rev(inter.4.prev.all_hi))
polygon(aa, bb, col = EpiModel::transco("blue", alpha = 0.3), border = NA)
lines(inter.4.prev.all, lwd = 2, col = "blue")

##MSM
xx <- c(1:(length(base.prev.msm)), (length(base.prev.msm)):1)
yy <- c(base.prev.msm_low, rev(base.prev.msm_hi))
polygon(xx, yy, col = EpiModel::transco("red", alpha = 0.3), border = NA)
lines(base.prev.msm, lwd = 2, col = "red")

aa <- c(1:(length(inter.4.prev.msm)), (length(inter.4.prev.msm)):1)
bb <- c(inter.4.prev.msm_low, rev(inter.4.prev.msm_hi))
polygon(aa, bb, col = EpiModel::transco("red", alpha = 0.3), border = NA)
lines(inter.4.prev.msm, lwd = 2, col = "red")

##ASMM
xx <- c(1:(length(base.prev.asmm)), (length(base.prev.asmm)):1)
yy <- c(base.prev.asmm_low, rev(base.prev.asmm_hi))
polygon(xx, yy, col = EpiModel::transco("light green", alpha = 0.3), border = NA)
lines(base.prev.asmm, lwd = 2, col = "light green")

aa <- c(1:(length(inter.4.prev.asmm)), (length(inter.4.prev.asmm)):1)
bb <- c(inter.4.prev.asmm_low, rev(inter.4.prev.asmm_hi))
polygon(aa, bb, col = EpiModel::transco("green", alpha = 0.3), border = NA)
lines(inter.4.prev.asmm, lwd = 2, col = "green")

legend("topright",c("All","Adult MSM", "ASMM"),text.col=c("blue","red", "green") )

dev.off()

###############################################################################################
#pdf(file = "Fig1.pdf", height = 6, width = 12, pointsize = 16)
#tiff(filename = "Fig_PrEP 40/40.tiff", height = 7, width = 7, units = "in", res = 250)

#par(mfrow = c(1,1), mar = c(3,3,2.5,5), mgp = c(2,1,0))
#xticks <- seq(0, 780, 52)
#yticks <- seq(0,40,5)
#plot(base.prev.all, type = "n", ylim = c(0, 16), lwd = 3, col="black", axes=FALSE,
#     main = "HIV Prevalence: Adult MSM PrEP - 40%, ASMM PrEP - 40%", xlab = "Years", ylab = "HIV Prevalence")
#axis(1, at = xticks, labels = c("-5","-4","-3","-2","-1","0","1","2","3","4","5","6","7","8","9","10"), col.axis="black", las=1, cex.axis=.6)
#axis(2, at = yticks, col.axis="black", las=1, cex.axis=.6)

##ALL
#xx <- c(1:(length(base.prev.all)), (length(base.prev.all)):1)
#yy <- c(base.prev.all_low, rev(base.prev.all_hi))
#polygon(xx, yy, col = EpiModel::transco("purple", alpha = 0.3), border = NA)
#lines(base.prev.w, lwd = 2, col = "purple")

#aa <- c(1:(length(inter.prev.all)), (length(inter.prev.all)):1)
#bb <- c(inter.prev.all_low, rev(inter.prev.all_hi))
#polygon(aa, bb, col = EpiModel::transco("blue", alpha = 0.3), border = NA)
#lines(inter.prev.all, lwd = 2, col = "blue")

##MSM
#xx <- c(1:(length(base.prev.msm)), (length(base.prev.msm)):1)
#yy <- c(base.prev.msm_low, rev(base.prev.msm_hi))
#polygon(xx, yy, col = EpiModel::transco("pink", alpha = 0.3), border = NA)
#lines(inter.4.prev.msm, lwd = 2, col = "pink")

#aa <- c(1:(length(inter.5.prev.msm)), (length(inter.5.prev.msm)):1)
#bb <- c(inter.5.prev.msm_low, rev(inter.5.prev.msm_hi))
#polygon(aa, bb, col = EpiModel::transco("red", alpha = 0.3), border = NA)
#lines(inter.5.prev.msm, lwd = 2, col = "red")

##ASMM
#xx <- c(1:(length(base.prev.asmm)), (length(base.prev.asmm)):1)
#yy <- c(base.asmm_low, rev(base.prev.asmm_hi))
#polygon(xx, yy, col = EpiModel::transco("pink", alpha = 0.3), border = NA)
#lines(inter.4.prev.asmm, lwd = 2, col = "pink")

#aa <- c(1:(length(inter.5.prev.asmm)), (length(inter.5.prev.asmm)):1)
#bb <- c(inter.5.prev.asmm_low, rev(inter.5.prev.asmm_hi))
#polygon(aa, bb, col = EpiModel::transco("red", alpha = 0.3), border = NA)
#lines(inter.5.prev.asmm, lwd = 2, col = "red")

#dev.off()


###############################################################################################
#pdf(file = "Fig1.pdf", height = 6, width = 12, pointsize = 16)
tiff(filename = "~/Campcl/scenarios/out/Fig_Prev_all_NMG.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(3,3,2.5,5), mgp = c(2,1,0))
xticks <- seq(0, 780, 52)
yticks <- seq(0,40,5)
plot(base.prev.all, type = "n", ylim = c(0, 30), lwd = 3, col="black", axes=FALSE,
     main = "Overall HIV Prevalence under PrEP interventions", xlab = "Years", ylab = "HIV Prevalence")
axis(1, at = xticks, labels = c("-5","-4","-3","-2","-1","0","1","2","3","4","5","6","7","8","9","10"), col.axis="black", las=1, cex.axis=.6)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.6)

##Baseline
xx <- c(1:(length(base.prev.all)), (length(base.prev.all)):1)
yy <- c(base.prev.all_low, rev(base.prev.all_hi))
polygon(xx, yy, col = EpiModel::transco("grey", alpha = 0.3), border = NA)
lines(base.prev.all, lwd = 2, col = "black")

#INT1.
aa <- c(1:(length(inter.1.prev.all)), (length(inter.1.prev.all)):1)
bb <- c(inter.1.prev.all_low, rev(inter.1.prev.all_hi))
polygon(aa, bb, col = EpiModel::transco("red", alpha = 0.3), border = NA)
lines(inter.1.prev.all, lwd = 2, col = "red")

#INT2.
#aa <- c(1:(length(inter.2.prev.all)), (length(inter.2.prev.all)):1)
#bb <- c(inter.2.prev.all_low, rev(inter.2.prev.all_hi))
#polygon(aa, bb, col = EpiModel::transco("orange", alpha = 0.3), border = NA)
#lines(inter.2.prev.all, lwd = 2, col = "orange")

#INT3.
#aa <- c(1:(length(inter.3.prev.all)), (length(inter.3.prev.all)):1)
#bb <- c(inter.3.prev.all_low, rev(inter.3.prev.all_hi))
#polygon(aa, bb, col = EpiModel::transco("green", alpha = 0.3), border = NA)
#lines(inter.3.prev.all, lwd = 2, col = "green")

#INT4.
aa <- c(1:(length(inter.4.prev.all)), (length(inter.4.prev.all)):1)
bb <- c(inter.4.prev.all_low, rev(inter.4.prev.all_hi))
polygon(aa, bb, col = EpiModel::transco("blue", alpha = 0.3), border = NA)
lines(inter.4.prev.all, lwd = 2, col = "blue")

#INT5.
#aa <- c(1:(length(inter.5.prev.all)), (length(inter.5.prev.all)):1)
#bb <- c(inter.5.prev.all_low, rev(inter.5.prev.all_hi))
#polygon(aa, bb, col = EpiModel::transco("violet", alpha = 0.3), border = NA)
#lines(inter.5.prev.all, lwd = 2, col = "violet")


legend("topright",c("No PreP","PreP for Adult MSM", "+ ASMM PrEP"),text.col=c("black","red", "blue") )

dev.off()


###############################################################################################
#pdf(file = "Fig1.pdf", height = 6, width = 12, pointsize = 16)
tiff(filename = "~/Campcl/scenarios/out/Fig_Prev_MSM.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(3,3,2.5,5), mgp = c(2,1,0))
xticks <- seq(0, 780, 52)
yticks <- seq(0,40,5)
plot(base.prev.all, type = "n", ylim = c(0, 30), lwd = 3, col="black", axes=FALSE,
     main = "HIV Prevalence among adult MSM under PrEP interventions", xlab = "Years", ylab = "HIV Prevalence")
axis(1, at = xticks, labels = c("-5","-4","-3","-2","-1","0","1","2","3","4","5","6","7","8","9","10"), col.axis="black", las=1, cex.axis=.6)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.6)

##Baseline
xx <- c(1:(length(base.prev.msm)), (length(base.prev.msm)):1)
yy <- c(base.prev.msm_low, rev(base.prev.msm_hi))
polygon(xx, yy, col = EpiModel::transco("grey", alpha = 0.3), border = NA)
lines(base.prev.msm, lwd = 2, col = "black")

#INT1.
aa <- c(1:(length(inter.1.prev.msm)), (length(inter.1.prev.msm)):1)
bb <- c(inter.1.prev.msm_low, rev(inter.1.prev.msm_hi))
polygon(aa, bb, col = EpiModel::transco("red", alpha = 0.3), border = NA)
lines(inter.1.prev.msm, lwd = 2, col = "red")

#INT2.
#aa <- c(1:(length(inter.2.prev.msm)), (length(inter.2.prev.msm)):1)
#bb <- c(inter.2.prev.msm_low, rev(inter.2.prev.msm_hi))
#polygon(aa, bb, col = EpiModel::transco("orange", alpha = 0.3), border = NA)
#lines(inter.2.prev.msm, lwd = 2, col = "orange")

#INT3.
#aa <- c(1:(length(inter.3.prev.msm)), (length(inter.3.prev.msm)):1)
#bb <- c(inter.3.prev.msm_low, rev(inter.3.prev.msm_hi))
#polygon(aa, bb, col = EpiModel::transco("green", alpha = 0.3), border = NA)
#lines(inter.3.prev.msm, lwd = 2, col = "green")

#INT4.
aa <- c(1:(length(inter.4.prev.msm)), (length(inter.4.prev.msm)):1)
bb <- c(inter.4.prev.msm_low, rev(inter.4.prev.msm_hi))
polygon(aa, bb, col = EpiModel::transco("blue", alpha = 0.3), border = NA)
lines(inter.4.prev.msm, lwd = 2, col = "blue")

#INT5.
#aa <- c(1:(length(inter.5.prev.msm)), (length(inter.5.prev.msm)):1)
#bb <- c(inter.5.prev.msm_low, rev(inter.5.prev.msm_hi))
#polygon(aa, bb, col = EpiModel::transco("violet", alpha = 0.3), border = NA)
#lines(inter.5.prev.msm, lwd = 2, col = "violet")

legend("topright",c("No PreP","PreP for Adult MSM", "+ ASMM PrEP"),text.col=c("black","red", "blue") )

dev.off()


###############################################################################################
#pdf(file = "Fig1.pdf", height = 6, width = 12, pointsize = 16)
tiff(filename = "~/Campcl/scenarios/out/Fig_Prev_ASMM.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(3,3,2.5,5), mgp = c(2,1,0))
xticks <- seq(0, 780, 52)
yticks <- seq(0,40,5)
plot(base.prev.all, type = "n", ylim = c(0, 30), lwd = 3, col="black", axes=FALSE,
     main = "HIV Prevalence among ASMM under PrEP interventions", xlab = "Years", ylab = "HIV Prevalence")
axis(1, at = xticks, labels = c("-5","-4","-3","-2","-1","0","1","2","3","4","5","6","7","8","9","10"), col.axis="black", las=1, cex.axis=.6)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.6)

##Baseline
xx <- c(1:(length(base.prev.asmm)), (length(base.prev.asmm)):1)
yy <- c(base.prev.asmm_low, rev(base.prev.asmm_hi))
polygon(xx, yy, col = EpiModel::transco("grey", alpha = 0.3), border = NA)
lines(base.prev.asmm, lwd = 2, col = "black")

#INT1.
aa <- c(1:(length(inter.1.prev.asmm)), (length(inter.1.prev.asmm)):1)
bb <- c(inter.1.prev.asmm_low, rev(inter.1.prev.asmm_hi))
polygon(aa, bb, col = EpiModel::transco("red", alpha = 0.3), border = NA)
lines(inter.1.prev.asmm, lwd = 2, col = "red")

#INT2.
#aa <- c(1:(length(inter.2.prev.asmm)), (length(inter.2.prev.asmm)):1)
#bb <- c(inter.2.prev.asmm_low, rev(inter.2.prev.asmm_hi))
#polygon(aa, bb, col = EpiModel::transco("orange", alpha = 0.3), border = NA)
#lines(inter.2.prev.asmm, lwd = 2, col = "orange")

#INT3.
#aa <- c(1:(length(inter.3.prev.asmm)), (length(inter.3.prev.asmm)):1)
#bb <- c(inter.3.prev.asmm_low, rev(inter.3.prev.asmm_hi))
#polygon(aa, bb, col = EpiModel::transco("green", alpha = 0.3), border = NA)
#lines(inter.3.prev.asmm, lwd = 2, col = "green")

#INT4.
aa <- c(1:(length(inter.4.prev.asmm)), (length(inter.4.prev.asmm)):1)
bb <- c(inter.4.prev.asmm_low, rev(inter.4.prev.asmm_hi))
polygon(aa, bb, col = EpiModel::transco("blue", alpha = 0.3), border = NA)
lines(inter.4.prev.asmm, lwd = 2, col = "blue")

#INT5.
#aa <- c(1:(length(inter.5.prev.msm)), (length(inter.5.prev.msm)):1)
#bb <- c(inter.5.prev.msm_low, rev(inter.5.prev.msm_hi))
#polygon(aa, bb, col = EpiModel::transco("violet", alpha = 0.3), border = NA)
#lines(inter.5.prev.msm, lwd = 2, col = "violet")

legend("topright",c("No PreP","PreP for Adult MSM", "+ ASMM PrEP"),text.col=c("black","red", "blue") )

dev.off()


##Prevalence Table

base.prev.all <- tail(base.prev.all,1)
base.prev.all_hi 
base.prev.all_low 

base.prev.msm 
base.prev.msm_hi 
base.prev.msm_low 

base.prev.asmm 
base.prev.asmm_hi
base.prev.asmm_low 

############# 1. 
inter.1.prev.all 
inter.1.prev.all_hi 
inter.1.prev.all_low 

inter.1.prev.msm 
inter.1.prev.msm_hi 
inter.1.prev.msm_low 

inter.1.prev.asmm
inter.1.prev.asmm_hi
inter.1.prev.asmm_low 





#############################################   TESTS  ##################

time<-1:500
plot(time,tail(s1$epi$i.prev[,4],500),type="l",col='black',ylim=c(.20,.3))
lines(time,tail(s2$epi$i.prev[,4],500),col='yellow')
      lines(time,tail(s3$epi$i.prev[,4],500),col='orange')
                 lines(time,tail(s4$epi$i.prev[,4],500),col='red')






