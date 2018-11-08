
## whamp analysis file

library(EpiModelHIV)
library(EpiModelHPC)


s2 <- merge_simfiles(2, indir = "~/Campcl/scenarios/data/")
s3 <- merge_simfiles(3, indir = "~/Campcl/scenarios/data/")
s4 <- merge_simfiles(4, indir = "~/Campcl/scenarios/data/")
s5 <- merge_simfiles(5, indir = "~/Campcl/scenarios/data/")

# Figure 1 ----------------------------------------------------------------

# Line plots of cumulative PIA

steps <- 1:520



##Intervention 1 adult only.
inter.1.N<-rep(NA,520)

inter.1.PrEP.all<-rep(NA,520)
inter.1.PrEP.all_hi<-rep(NA,520)
inter.1.PrEP.all_low<-rep(NA,520)

inter.1.PrEP.msm<-rep(NA,520)
inter.1.PrEP.msm_hi<-rep(NA,520)
inter.1.PrEP.msm_low<-rep(NA,520)

inter.1.PrEP.asmm<-rep(NA,520)
inter.1.PrEP.asmm_hi<-rep(NA,520)
inter.1.PrEP.asmm_low<-rep(NA,520)


##Intervention 2 Adults plus ASMM @ 10%
inter.2.N<-rep(NA,520)

inter.2.PrEP.all<-rep(NA,520)
inter.2.PrEP.all_hi<-rep(NA,520)
inter.2.PrEP.all_low<-rep(NA,520)

inter.2.PrEP.msm<-rep(NA,520)
inter.2.PrEP.msm_hi<-rep(NA,520)
inter.2.PrEP.msm_low<-rep(NA,520)

inter.2.PrEP.asmm<-rep(NA,520)
inter.2.PrEP.asmm_hi<-rep(NA,520)
inter.2.PrEP.asmm_low<-rep(NA,520)


##Intervention 3 Adults plus ASMM @ 20%
inter.3.N<-rep(NA,520)

inter.3.PrEP.all<-rep(NA,520)
inter.3.PrEP.all_hi<-rep(NA,520)
inter.3.PrEP.all_low<-rep(NA,520)

inter.3.PrEP.msm<-rep(NA,520)
inter.3.PrEP.msm_hi<-rep(NA,520)
inter.3.PrEP.msm_low<-rep(NA,520)

inter.3.PrEP.asmm<-rep(NA,520)
inter.3.PrEP.asmm_hi<-rep(NA,520)
inter.3.PrEP.asmm_low<-rep(NA,520)


##Intervention 4 Adults plus ASMM @ 30%
inter.4.N<-rep(NA,520)

inter.4.PrEP.all<-rep(NA,520)
inter.4.PrEP.all_hi<-rep(NA,520)
inter.4.PrEP.all_low<-rep(NA,520)

inter.4.PrEP.msm<-rep(NA,520)
inter.4.PrEP.msm_hi<-rep(NA,520)
inter.4.PrEP.msm_low<-rep(NA,520)

inter.4.PrEP.asmm<-rep(NA,520)
inter.4.PrEP.asmm_hi<-rep(NA,520)
inter.4.PrEP.asmm_low<-rep(NA,520)

##Intervention 5 Adults plus ASMM @ 40%
inter.5.N<-rep(NA,520)

inter.5.PrEP.all<-rep(NA,520)
inter.5.PrEP.all_hi<-rep(NA,520)
inter.5.PrEP.all_low<-rep(NA,520)

inter.5.PrEP.msm<-rep(NA,520)
inter.5.PrEP.msm_hi<-rep(NA,520)
inter.5.PrEP.msm_low<-rep(NA,520)

inter.5.PrEP.asmm<-rep(NA,520)
inter.5.PrEP.asmm_hi<-rep(NA,520)
inter.5.PrEP.asmm_low<-rep(NA,520)


x.all <- x.1.all <- x.2.all <- x.3.all <- x.4.all <- x.5.all <- NULL
x.msm <- x.1.msm <- x.2.msm <- x.3.msm <- x.4.msm <- x.5.msm <- NULL 
x.asmm <- x.1.asmm <- x.2.asmm <- x.3.asmm <- x.4.asmm <- x.5.asmm  <- NULL
x.N <- x.1.N <- x.2.N <- x.3.N <- x.4.N <- x.5.N <- NULL 

for (i in seq_along(steps)) {

  sim.int1 <- truncate_sim(s2, at = 8261)
  mn <- head(as.data.frame(sim.int1), steps[i])

  x.1.N<-sort(as.numeric(sim.int1$epi$num[i,1:100]))
  inter.1.N[i]<-mean(x.1.all)

  x.1.all<-sort(as.numeric(sim.int1$epi$prepCurr[i,1:100]))
  inter.1.PrEP.all[i]<-mean(x.1.all)
  inter.1.PrEP.all_hi[i]<-mean(x.1.all[97],x.1.all[98])
  inter.1.PrEP.all_low[i]<-mean(x.1.all[3],x.1.all[2])
  
  x.1.msm<-sort(as.numeric(sim.int1$epi$prepCurr.msm[i,1:100]))
  inter.1.PrEP.msm[i]<-mean(x.1.msm)
  inter.1.PrEP.msm_hi[i]<-mean(x.1.msm[97],x.1.msm[98])
  inter.1.PrEP.msm_low[i]<-mean(x.1.msm[3],x.1.msm[2])
  
  x.1.asmm<-sort(as.numeric(sim.int1$epi$prepCurr.asmm[i,1:100]))
  inter.1.PrEP.asmm[i]<-mean(x.1.asmm)
  inter.1.PrEP.asmm_hi[i]<-mean(x.1.asmm[97],x.1.asmm[98])
  inter.1.PrEP.asmm_low[i]<-mean(x.1.asmm[3],x.1.asmm[2])


  ##############.
  sim.int2 <- truncate_sim(s3, at = 8261)
  mn <- head(as.data.frame(sim.int2), steps[i])
  
  x.2.N<-sort(as.numeric(sim.int2$epi$num[i,1:100]))
  inter.2.N[i]<-mean(x.2.all)
  
  x.2.all<-sort(as.numeric(sim.int2$epi$prepCurr[i,1:100]))
  inter.2.PrEP.all[i]<-mean(x.2.all)
  inter.2.PrEP.all_hi[i]<-mean(x.2.all[97],x.2.all[98])
  inter.2.PrEP.all_low[i]<-mean(x.2.all[3],x.2.all[2])
  
  x.2.msm<-sort(as.numeric(sim.int2$epi$prepCurr.msm[i,1:100]))
  inter.2.PrEP.msm[i]<-mean(x.2.msm)
  inter.2.PrEP.msm_hi[i]<-mean(x.2.msm[97],x.2.msm[98])
  inter.2.PrEP.msm_low[i]<-mean(x.2.msm[3],x.2.msm[2])
  
  x.2.asmm<-sort(as.numeric(sim.int2$epi$prepCurr.asmm[i,1:100]))
  inter.2.PrEP.asmm[i]<-mean(x.2.asmm)
  inter.2.PrEP.asmm_hi[i]<-mean(x.2.asmm[97],x.2.asmm[98])
  inter.2.PrEP.asmm_low[i]<-mean(x.2.asmm[3],x.2.asmm[2])
  

  ##############.
  sim.int3 <- truncate_sim(s4, at = 8261)
  mn <- head(as.data.frame(sim.int3), steps[i])
  
  x.3.N<-sort(as.numeric(sim.int3$epi$num[i,1:100]))
  inter.3.N[i]<-mean(x.3.all)
  
  x.3.all<-sort(as.numeric(sim.int3$epi$prepCurr[i,1:100]))
  inter.3.PrEP.all[i]<-mean(x.3.all)
  inter.3.PrEP.all_hi[i]<-mean(x.3.all[97],x.3.all[98])
  inter.3.PrEP.all_low[i]<-mean(x.3.all[3],x.3.all[2])
  
  x.3.msm<-sort(as.numeric(sim.int3$epi$prepCurr.msm[i,1:100]))
  inter.3.PrEP.msm[i]<-mean(x.3.msm)
  inter.3.PrEP.msm_hi[i]<-mean(x.3.msm[97],x.3.msm[98])
  inter.3.PrEP.msm_low[i]<-mean(x.3.msm[3],x.3.msm[2])
  
  x.3.asmm<-sort(as.numeric(sim.int3$epi$prepCurr.asmm[i,1:100]))
  inter.3.PrEP.asmm[i]<-mean(x.3.asmm)
  inter.3.PrEP.asmm_hi[i]<-mean(x.3.asmm[97],x.3.asmm[98])
  inter.3.PrEP.asmm_low[i]<-mean(x.3.asmm[3],x.3.asmm[2])
  

  
##############.
  sim.int4 <- truncate_sim(s5, at = 8261)
  mn <- head(as.data.frame(sim.int4), steps[i])
  
  x.4.N<-sort(as.numeric(sim.int4$epi$num[i,1:100]))
  inter.4.N[i]<-mean(x.4.all)
  
  x.4.all<-sort(as.numeric(sim.int4$epi$prepCurr[i,1:100]))
  inter.4.PrEP.all[i]<-mean(x.4.all)
  inter.4.PrEP.all_hi[i]<-mean(x.4.all[97],x.4.all[98])
  inter.4.PrEP.all_low[i]<-mean(x.4.all[3],x.4.all[2])
  
  x.4.msm<-sort(as.numeric(sim.int4$epi$prepCurr.msm[i,1:100]))
  inter.4.PrEP.msm[i]<-mean(x.4.msm)
  inter.4.PrEP.msm_hi[i]<-mean(x.4.msm[97],x.4.msm[98])
  inter.4.PrEP.msm_low[i]<-mean(x.4.msm[3],x.4.msm[2])
  
  x.4.asmm<-sort(as.numeric(sim.int4$epi$prepCurr.asmm[i,1:100]))
  inter.4.PrEP.asmm[i]<-mean(x.4.asmm)
  inter.4.PrEP.asmm_hi[i]<-mean(x.4.asmm[97],x.4.asmm[98])
  inter.4.PrEP.asmm_low[i]<-mean(x.4.asmm[3],x.4.asmm[2])
  

}



############# 1. 
inter.1.N <- mean(inter.1.N)

inter.1.PrEP.all <- mean(inter.1.PrEP.all)
inter.1.PrEP.all_hi <- mean(inter.1.PrEP.all_hi)
inter.1.PrEP.all_low <- mean(inter.1.PrEP.all_low)

inter.1.PrEP.msm <- mean(inter.1.PrEP.msm)
inter.1.PrEP.msm_hi <- mean(inter.1.PrEP.msm_hi)
inter.1.PrEP.msm_low <- mean(inter.1.PrEP.msm_low)

inter.1.PrEP.asmm <- mean(inter.1.PrEP.asmm)
inter.1.PrEP.asmm_hi <- mean(inter.1.PrEP.asmm_hi)
inter.1.PrEP.asmm_low <- mean(inter.1.PrEP.asmm_low)

################# 2.
inter.2.N <- mean(inter.2.N)

inter.2.PrEP.all <- mean(inter.2.PrEP.all)
inter.2.PrEP.all_hi <- mean(inter.2.PrEP.all_hi)
inter.2.PrEP.all_low <- mean(inter.2.PrEP.all_low)

inter.2.PrEP.msm <- mean(inter.2.PrEP.msm)
inter.2.PrEP.msm_hi <- mean(inter.2.PrEP.msm_hi)
inter.2.PrEP.msm_low <- mean(inter.2.PrEP.msm_low)

inter.2.PrEP.asmm <- mean(inter.2.PrEP.asmm)
inter.2.PrEP.asmm_hi <- mean(inter.2.PrEP.asmm_hi)
inter.2.PrEP.asmm_low <- mean(inter.2.PrEP.asmm_low)

################# 3.
inter.3.N <- mean(inter.3.N)

inter.3.PrEP.all <- mean(inter.3.PrEP.all)
inter.3.PrEP.all_hi <- mean(inter.3.PrEP.all_hi)
inter.3.PrEP.all_low <- mean(inter.3.PrEP.all_low)

inter.3.PrEP.msm <- mean(inter.3.PrEP.msm)
inter.3.PrEP.msm_hi <- mean(inter.3.PrEP.msm_hi)
inter.3.PrEP.msm_low <- mean(inter.3.PrEP.msm_low)

inter.3.PrEP.asmm <- mean(inter.3.PrEP.asmm)
inter.3.PrEP.asmm_hi <- mean(inter.3.PrEP.asmm_hi)
inter.3.PrEP.asmm_low <- mean(inter.3.PrEP.asmm_low)

################# 4.
inter.4.N <- mean(inter.4.N)

inter.4.PrEP.all <- mean(inter.4.PrEP.all)
inter.4.PrEP.all_hi <- mean(inter.4.PrEP.all_hi)
inter.4.PrEP.all_low <- mean(inter.4.PrEP.all_low)

inter.4.PrEP.msm <- mean(inter.4.PrEP.msm)
inter.4.PrEP.msm_hi <- mean(inter.4.PrEP.msm_hi)
inter.4.PrEP.msm_low <- mean(inter.4.PrEP.msm_low)

inter.4.PrEP.asmm <- mean(inter.4.PrEP.asmm)
inter.4.PrEP.asmm_hi <- mean(inter.4.PrEP.asmm_hi)
inter.4.PrEP.asmm_low <- mean(inter.4.PrEP.asmm_low)


PrEP.counts.table <-cbind(

  inter.1.PrEP.all,
  inter.1.PrEP.all_low,
  inter.1.PrEP.all_hi,
  inter.2.PrEP.all,
  inter.2.PrEP.all_low,
  inter.2.PrEP.all_hi,
  inter.3.PrEP.all,
  inter.3.PrEP.all_low,
  inter.3.PrEP.all_hi,
  inter.4.PrEP.all,
  inter.4.PrEP.all_low,
  inter.4.PrEP.all_hi,
  inter.1.PrEP.msm,
  inter.1.PrEP.msm_low,
  inter.1.PrEP.msm_hi,
  inter.2.PrEP.msm,
  inter.2.PrEP.msm_low,
  inter.2.PrEP.msm_hi,
  inter.3.PrEP.msm,
  inter.3.PrEP.msm_low,
  inter.3.PrEP.msm_hi,
  inter.4.PrEP.msm,
  inter.4.PrEP.msm_low,
  inter.4.PrEP.msm_hi,
  inter.1.PrEP.asmm,
  inter.1.PrEP.asmm_low,
  inter.1.PrEP.asmm_hi,
  inter.2.PrEP.asmm,
  inter.2.PrEP.asmm_low,
  inter.2.PrEP.asmm_hi,
  inter.3.PrEP.asmm,
  inter.3.PrEP.asmm_low,
  inter.3.PrEP.asmm_hi,
  inter.4.PrEP.asmm,
  inter.4.PrEP.asmm_low,
  inter.4.PrEP.asmm_hi,
  inter.1.N,
  inter.2.N,
  inter.3.N,
  inter.4.N
  )



library(xlsx) #load the package

write.xlsx(x = PrEP.counts.table, file = "/homes/dth2/Campcl/scenarios/out/PrEP_counts_table.xlsx",
           sheetName = "output", row.names = FALSE)

PrEP.counts.table


