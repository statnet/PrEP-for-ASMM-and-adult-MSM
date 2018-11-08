
## whamp analysis file

library(EpiModelHIV)
library(EpiModelHPC)


s1 <- merge_simfiles(1, indir = "~/Campcl/scenarios/data/")
s2 <- merge_simfiles(2, indir = "~/Campcl/scenarios/data/")
s3 <- merge_simfiles(3, indir = "~/Campcl/scenarios/data/")
s4 <- merge_simfiles(4, indir = "~/Campcl/scenarios/data/")
s5 <- merge_simfiles(5, indir = "~/Campcl/scenarios/data/")
s6 <- merge_simfiles(6, indir = "~/Campcl/scenarios/data/")

# Tables ----------------------------------------------------------------

# Line plots of cumulative PIA

nsims <- 1:100

base.incid<-rep(NA,100)
base.pt<-rep(NA,100)
base.prept<-rep(NA,100)
base.HIVt<-rep(NA,100)
inc.base_p10K <-rep(NA,100)
postime.base_p10K <-rep(NA,100)
inc.base_prep.pt.10K <-rep(NA,100)

##Intervention 1 adult only.
int.1.incid<-rep(NA,100)
int.1.pt<-rep(NA,100)
int.1.prept<-rep(NA,100)
int.1.HIVt<-rep(NA,100)
inc.int.1_p10K <-rep(NA,100)
postime.int.1_p10K <-rep(NA,100)
inc.int.1_prep.pt.10K <-rep(NA,100)

##Intervention 2 Adults plus ASMM @ 10%

int.2.incid<-rep(NA,100)
int.2.pt<-rep(NA,100)
int.2.prept<-rep(NA,100)
int.2.HIVt<-rep(NA,100)
inc.int.2_p10K <-rep(NA,100)
postime.int.2_p10K <-rep(NA,100)
inc.int.2_prep.pt.10K <-rep(NA,100)

##Intervention 3 Adults plus ASMM @ 20%

int.3.incid<-rep(NA,100)
int.3.pt<-rep(NA,100)
int.3.prept<-rep(NA,100)
int.3.HIVt<-rep(NA,100)
inc.int.3_p10K <-rep(NA,100)
postime.int.3_p10K <-rep(NA,100)
inc.int.3_prep.pt.10K <-rep(NA,100)


##Intervention 4 Adults plus ASMM @ 30%

int.4.incid<-rep(NA,100)
int.4.pt<-rep(NA,100)
int.4.prept<-rep(NA,100)
int.4.HIVt<-rep(NA,100)
inc.int.4_p10K <-rep(NA,100)
postime.int.4_p10K <-rep(NA,100)
inc.int.4_prep.pt.10K <-rep(NA,100)


####################################################################################

x <- x.1 <- x.2 <- x.3 <- x.4 <- x.5 <- NULL
x.msm <- x.1.msm <- x.2.msm <- x.3.msm <- x.4.msm <- x.5.msm <- NULL 
x.asmm <- x.1.asmm <- x.2.asmm <- x.3.asmm <- x.4.asmm <- x.5.asmm  <- NULL

####TEN YEAR PREP

for (i in seq_along(nsims)) {
  sim.base <- truncate_sim(s1, at = 8261)
  
  base.incid[i] <-sum(as.numeric(sim.base$epi$incid[1:520,i]))
  base.pt[i] <- sum(as.numeric(sim.base$epi$num.deb[1:520,i]))
  base.prept[i] <- sum(as.numeric(sim.base$epi$prepCurr[1:520,i]))
  base.HIVt[i] <- sum(as.numeric(sim.base$epi$i.num[1:520,i]))

  inc.base_p10K[i] <- base.incid[i] / ((base.pt[i]-base.HIVt[i])/52 /10000 )
  postime.base_p10K[i] <- (base.HIVt[i]/52) / ((base.pt[i]/52)/10000)
  
  inc.base_prep.pt.10K[i] <- NA
  
  
##################.
  sim.int.1 <- truncate_sim(s2, at = 8261)

  int.1.incid[i] <-sum(as.numeric(sim.int.1$epi$incid[1:520,i]))
  int.1.pt[i] <- sum(as.numeric(sim.int.1$epi$num.deb[1:520,i]))
  int.1.prept[i] <- sum(as.numeric(sim.int.1$epi$prepCurr[1:520,i]))
  int.1.HIVt[i] <- sum(as.numeric(sim.int.1$epi$i.num[1:520,i]))
  
  inc.int.1_p10K[i] <- int.1.incid[i] / (((int.1.pt[i]-int.1.HIVt[i])/52)/10000 )
  postime.int.1_p10K[i] <- (int.1.HIVt[i]/52) / ((int.1.pt[i]/52)/10000)
  
  inc.int.1_prep.pt.10K[i] <- (base.incid[i] - int.1.incid[i]) / ((int.1.prept[i])/52 /10000 )
  
  

  ##############.
  sim.int.2 <- truncate_sim(s3, at = 8261)
  
  int.2.incid[i] <-sum(as.numeric(sim.int.2$epi$incid[1:520,i]))
  int.2.pt[i] <- sum(as.numeric(sim.int.2$epi$num.deb[1:520,i]))
  int.2.prept[i] <- sum(as.numeric(sim.int.2$epi$prepCurr[1:520,i]))
  int.2.HIVt[i] <- sum(as.numeric(sim.int.2$epi$i.num[1:520,i]))

  inc.int.2_p10K[i] <- int.2.incid[i] / (((int.2.pt[i]-int.2.HIVt[i])/52)/10000 )
  postime.int.2_p10K[i] <- (int.2.HIVt[i]/52) / ((int.2.pt[i]/52)/10000)
  
  inc.int.2_prep.pt.10K[i] <- (base.incid[i] - int.2.incid[i]) / ((int.2.prept[i])/52 /10000 )
  
  

  ##############.
  sim.int.3 <- truncate_sim(s4, at = 8261)
  
  int.3.incid[i] <-sum(as.numeric(sim.int.3$epi$incid[1:520,i]))
  int.3.pt[i] <- sum(as.numeric(sim.int.3$epi$num.deb[1:520,i]))
  int.3.prept[i] <- sum(as.numeric(sim.int.3$epi$prepCurr[1:520,i]))
  int.3.HIVt[i] <- sum(as.numeric(sim.int.3$epi$i.num[1:520,i]))
  
  inc.int.3_p10K[i] <- int.3.incid[i] / (((int.3.pt[i]-int.3.HIVt[i])/52)/10000 )
  postime.int.3_p10K[i] <- (int.3.HIVt[i]/52) / ((int.3.pt[i]/52)/10000)  
  
  inc.int.3_prep.pt.10K[i] <- (base.incid[i] - int.3.incid[i]) / ((int.3.prept[i])/52 /10000 )
  
  

  
##############.
  sim.int.4 <- truncate_sim(s5, at = 8261)
  
  int.4.incid[i] <-sum(as.numeric(sim.int.4$epi$incid[1:520,i]))
  int.4.pt[i] <- sum(as.numeric(sim.int.4$epi$num.deb[1:520,i]))
  int.4.prept[i] <- sum(as.numeric(sim.int.4$epi$prepCurr[1:520,i]))
  int.4.HIVt[i] <- sum(as.numeric(sim.int.4$epi$i.num[1:520,i]))
  
  inc.int.4_p10K[i] <- int.4.incid[i] / (((int.4.pt[i]-int.4.HIVt[i])/52)/10000 )
  postime.int.4_p10K[i] <- (int.4.HIVt[i]/52) / ((int.4.pt[i]/52)/10000)

  inc.int.4_prep.pt.10K[i] <- (base.incid[i] - int.4.incid[i]) / ((int.4.prept[i])/52 /10000 )
  
}



####Create 95% CrI.
base.incid <-sort(base.incid)
base.incid_hi<-mean(base.incid[97],base.incid[98])
base.incid_low<-mean(base.incid[2],base.incid[3])
base.incid <-mean(base.incid)

base.pt <-sort(base.pt)
base.pt_hi<-mean(base.pt[97],base.pt[98])
base.pt_low<-mean(base.pt[2],base.pt[3])
base.pt <-mean(base.pt)

base.prept <-sort(base.prept)
base.prept_hi<-mean(base.prept[97],base.prept[98])
base.prept_low<-mean(base.prept[2],base.prept[3])
base.prept <-mean(base.prept)

base.HIVt <-sort(base.HIVt)
base.HIVt_hi<-mean(base.HIVt[97],base.HIVt[98])
base.HIVt_low<-mean(base.HIVt[2],base.HIVt[3])
base.HIVt <-mean(base.HIVt)

inc.base_p10K <- sort(inc.base_p10K) 
inc.base_p10K_hi <- mean(inc.base_p10K[97],inc.base_p10K[98])
inc.base_p10K_low <- mean(inc.base_p10K[2],inc.base_p10K[3])
inc.base_p10K <- mean(inc.base_p10K)

postime.base_p10K <- sort(postime.base_p10K) 
postime.base_p10K_hi <- mean(postime.base_p10K[97],postime.base_p10K[98])
postime.base_p10K_low <- mean(postime.base_p10K[2],postime.base_p10K[3])
postime.base_p10K <- mean(postime.base_p10K)

inc.base_prep.pt.10K <- sort(inc.base_prep.pt.10K)
inc.base_prep.pt.10K_hi <- mean(inc.base_prep.pt.10K[97],inc.base_prep.pt.10K[98])
inc.base_prep.pt.10K_low <- mean(inc.base_prep.pt.10K[2],inc.base_prep.pt.10K[3])
inc.base_prep.pt.10K <- mean(inc.base_prep.pt.10K)


###int.1.
int.1.incid <-sort(int.1.incid)
int.1.incid_hi<-mean(int.1.incid[97],int.1.incid[98])
int.1.incid_low<-mean(int.1.incid[2],int.1.incid[3])
int.1.incid <-mean(int.1.incid)

int.1.pt <-sort(int.1.pt)
int.1.pt_hi<-mean(int.1.pt[97],int.1.pt[98])
int.1.pt_low<-mean(int.1.pt[2],int.1.pt[3])
int.1.pt <-mean(int.1.pt)

int.1.prept <-sort(int.1.prept)
int.1.prept_hi<-mean(int.1.prept[97],int.1.prept[98])
int.1.prept_low<-mean(int.1.prept[2],int.1.prept[3])
int.1.prept <-mean(int.1.prept)

int.1.HIVt <-sort(int.1.HIVt)
int.1.HIVt_hi<-mean(int.1.HIVt[97],int.1.HIVt[98])
int.1.HIVt_low<-mean(int.1.HIVt[2],int.1.HIVt[3])
int.1.HIVt <-mean(int.1.HIVt)


inc.int.1_p10K <- sort(inc.int.1_p10K) 
inc.int.1_p10K_hi <- mean(inc.int.1_p10K[97],inc.int.1_p10K[98])
inc.int.1_p10K_low <- mean(inc.int.1_p10K[2],inc.int.1_p10K[3])
inc.int.1_p10K <- mean(inc.int.1_p10K)

postime.int.1_p10K <- sort(postime.int.1_p10K) 
postime.int.1_p10K_hi <- mean(postime.int.1_p10K[97],postime.int.1_p10K[98])
postime.int.1_p10K_low <- mean(postime.int.1_p10K[2],postime.int.1_p10K[3])
postime.int.1_p10K <- mean(postime.int.1_p10K)

inc.int.1_prep.pt.10K <- sort(inc.int.1_prep.pt.10K)
inc.int.1_prep.pt.10K_hi <- mean(inc.int.1_prep.pt.10K[97],inc.int.1_prep.pt.10K[98])
inc.int.1_prep.pt.10K_low <- mean(inc.int.1_prep.pt.10K[2],inc.int.1_prep.pt.10K[3])
inc.int.1_prep.pt.10K <- mean(inc.int.1_prep.pt.10K)


###int.2.

int.2.incid <-sort(int.2.incid)
int.2.incid_hi<-mean(int.2.incid[97],int.2.incid[98])
int.2.incid_low<-mean(int.2.incid[2],int.2.incid[3])
int.2.incid <-mean(int.2.incid)

int.2.pt <-sort(int.2.pt)
int.2.pt_hi<-mean(int.2.pt[97],int.2.pt[98])
int.2.pt_low<-mean(int.2.pt[2],int.2.pt[3])
int.2.pt <-mean(int.2.pt)

int.2.prept <-sort(int.2.prept)
int.2.prept_hi<-mean(int.2.prept[97],int.2.prept[98])
int.2.prept_low<-mean(int.2.prept[2],int.2.prept[3])
int.2.prept <-mean(int.2.prept)

int.2.HIVt <-sort(int.2.HIVt)
int.2.HIVt_hi<-mean(int.2.HIVt[97],int.2.HIVt[98])
int.2.HIVt_low<-mean(int.2.HIVt[2],int.2.HIVt[3])
int.2.HIVt <-mean(int.2.HIVt)

inc.int.2_p10K <- sort(inc.int.2_p10K) 
inc.int.2_p10K_hi <- mean(inc.int.2_p10K[97],inc.int.2_p10K[98])
inc.int.2_p10K_low <- mean(inc.int.2_p10K[2],inc.int.2_p10K[3])
inc.int.2_p10K <- mean(inc.int.2_p10K)

postime.int.2_p10K <- sort(postime.int.2_p10K) 
postime.int.2_p10K_hi <- mean(postime.int.2_p10K[97],postime.int.2_p10K[98])
postime.int.2_p10K_low <- mean(postime.int.2_p10K[2],postime.int.2_p10K[3])
postime.int.2_p10K <- mean(postime.int.2_p10K)

inc.int.2_prep.pt.10K <- sort(inc.int.2_prep.pt.10K)
inc.int.2_prep.pt.10K_hi <- mean(inc.int.2_prep.pt.10K[97],inc.int.2_prep.pt.10K[98])
inc.int.2_prep.pt.10K_low <- mean(inc.int.2_prep.pt.10K[2],inc.int.2_prep.pt.10K[3])
inc.int.2_prep.pt.10K <- mean(inc.int.2_prep.pt.10K)

###int.3.

int.3.incid <-sort(int.3.incid)
int.3.incid_hi<-mean(int.3.incid[97],int.3.incid[98])
int.3.incid_low<-mean(int.3.incid[2],int.3.incid[3])
int.3.incid <-mean(int.3.incid)

int.3.pt <-sort(int.3.pt)
int.3.pt_hi<-mean(int.3.pt[97],int.3.pt[98])
int.3.pt_low<-mean(int.3.pt[2],int.3.pt[3])
int.3.pt <-mean(int.3.pt)

int.3.prept <-sort(int.3.prept)
int.3.prept_hi<-mean(int.3.prept[97],int.3.prept[98])
int.3.prept_low<-mean(int.3.prept[2],int.3.prept[3])
int.3.prept <-mean(int.3.prept)

int.3.HIVt <-sort(int.3.HIVt)
int.3.HIVt_hi<-mean(int.3.HIVt[97],int.3.HIVt[98])
int.3.HIVt_low<-mean(int.3.HIVt[2],int.3.HIVt[3])
int.3.HIVt <-mean(int.3.HIVt)

inc.int.3_p10K <- sort(inc.int.3_p10K) 
inc.int.3_p10K_hi <- mean(inc.int.3_p10K[97],inc.int.3_p10K[98])
inc.int.3_p10K_low <- mean(inc.int.3_p10K[2],inc.int.3_p10K[3])
inc.int.3_p10K <- mean(inc.int.3_p10K)

postime.int.3_p10K <- sort(postime.int.3_p10K) 
postime.int.3_p10K_hi <- mean(postime.int.3_p10K[97],postime.int.3_p10K[98])
postime.int.3_p10K_low <- mean(postime.int.3_p10K[2],postime.int.3_p10K[3])
postime.int.3_p10K <- mean(postime.int.3_p10K)

inc.int.3_prep.pt.10K <- sort(inc.int.3_prep.pt.10K)
inc.int.3_prep.pt.10K_hi <- mean(inc.int.3_prep.pt.10K[97],inc.int.3_prep.pt.10K[98])
inc.int.3_prep.pt.10K_low <- mean(inc.int.3_prep.pt.10K[2],inc.int.3_prep.pt.10K[3])
inc.int.3_prep.pt.10K <- mean(inc.int.3_prep.pt.10K)


###int.4.

int.4.incid <-sort(int.4.incid)
int.4.incid_hi<-mean(int.4.incid[97],int.4.incid[98])
int.4.incid_low<-mean(int.4.incid[2],int.4.incid[3])
int.4.incid <-mean(int.4.incid)

int.4.pt <-sort(int.4.pt)
int.4.pt_hi<-mean(int.4.pt[97],int.4.pt[98])
int.4.pt_low<-mean(int.4.pt[2],int.4.pt[3])
int.4.pt <-mean(int.4.pt)

int.4.prept <-sort(int.4.prept)
int.4.prept_hi<-mean(int.4.prept[97],int.4.prept[98])
int.4.prept_low<-mean(int.4.prept[2],int.4.prept[3])
int.4.prept <-mean(int.4.prept)

int.4.HIVt <-sort(int.4.HIVt)
int.4.HIVt_hi<-mean(int.4.HIVt[97],int.4.HIVt[98])
int.4.HIVt_low<-mean(int.4.HIVt[2],int.4.HIVt[3])
int.4.HIVt <-mean(int.4.HIVt)

inc.int.4_p10K <- sort(inc.int.4_p10K) 
inc.int.4_p10K_hi <- mean(inc.int.4_p10K[97],inc.int.4_p10K[98])
inc.int.4_p10K_low <- mean(inc.int.4_p10K[2],inc.int.4_p10K[3])
inc.int.4_p10K <- mean(inc.int.4_p10K)

postime.int.4_p10K <- sort(postime.int.4_p10K) 
postime.int.4_p10K_hi <- mean(postime.int.4_p10K[97],postime.int.4_p10K[98])
postime.int.4_p10K_low <- mean(postime.int.4_p10K[2],postime.int.4_p10K[3])
postime.int.4_p10K <- mean(postime.int.4_p10K)

inc.int.4_prep.pt.10K <- sort(inc.int.4_prep.pt.10K)
inc.int.4_prep.pt.10K_hi <- mean(inc.int.4_prep.pt.10K[97],inc.int.4_prep.pt.10K[98])
inc.int.4_prep.pt.10K_low <- mean(inc.int.4_prep.pt.10K[2],inc.int.4_prep.pt.10K[3])
inc.int.4_prep.pt.10K <- mean(inc.int.4_prep.pt.10K)


inc.base_p10K <- c(inc.base_p10K, inc.base_p10K_low,inc.base_p10K_hi)
inc.int.1_p10K <- c(inc.int.1_p10K, inc.int.1_p10K_low,inc.int.1_p10K_hi)
inc.int.2_p10K <- c(inc.int.2_p10K, inc.int.2_p10K_low,inc.int.2_p10K_hi)
inc.int.3_p10K<-c(inc.int.3_p10K, inc.int.3_p10K_low,inc.int.3_p10K_hi)
inc.int.4_p10K<-c(inc.int.4_p10K, inc.int.4_p10K_low,inc.int.4_p10K_hi)
postime.base_p10K<-c(postime.base_p10K,postime.base_p10K_low, postime.base_p10K_hi)
postime.int.1_p10K<-c(postime.int.1_p10K,postime.int.1_p10K_low, postime.int.1_p10K_hi)
postime.int.2_p10K<-c(postime.int.2_p10K,postime.int.2_p10K_low, postime.int.2_p10K_hi)
postime.int.3_p10K<-c(postime.int.3_p10K,postime.int.3_p10K_low, postime.int.3_p10K_hi)
postime.int.4_p10K<-c(postime.int.4_p10K,postime.int.4_p10K_low, postime.int.4_p10K_hi)
                     
inc.base_prep.pt.10K<-c(inc.base_prep.pt.10K, inc.base_prep.pt.10K_hi, inc.base_prep.pt.10K_low)
inc.int.1_prep.pt.10K<-c(inc.int.1_prep.pt.10K, inc.int.1_prep.pt.10K_hi, inc.int.1_prep.pt.10K_low)
inc.int.2_prep.pt.10K<-c(inc.int.2_prep.pt.10K, inc.int.2_prep.pt.10K_hi, inc.int.2_prep.pt.10K_low)
inc.int.3_prep.pt.10K<-c(inc.int.3_prep.pt.10K, inc.int.3_prep.pt.10K_hi, inc.int.3_prep.pt.10K_low)
inc.int.4_prep.pt.10K<-c(inc.int.4_prep.pt.10K, inc.int.4_prep.pt.10K_hi, inc.int.4_prep.pt.10K_low)

Incidence.HIVpt.table <-rbind(inc.base_p10K, inc.int.1_p10K, inc.int.2_p10K, inc.int.3_p10K, inc.int.4_p10K,
                              postime.base_p10K, postime.int.1_p10K, postime.int.2_p10K, postime.int.3_p10K, postime.int.4_p10K,
                              inc.base_prep.pt.10K, inc.int.1_prep.pt.10K, inc.int.2_prep.pt.10K, inc.int.3_prep.pt.10K, inc.int.4_prep.pt.10K)

names <- c("inc.base_p10K", "inc.int.1_p10K", "inc.int.2_p10K", "inc.int.3_p10K", "inc.int.4_p10K",
                              "postime.base_p10K", "postime.int.1_p10K", "postime.int.2_p10K", "postime.int.3_p10K", "postime.int.4_p10K",
                              "inc.base_prep.pt.10K", "inc.int.1_prep.pt.10K", "inc.int.2_prep.pt.10K", "inc.int.3_prep.pt.10K", "inc.int.4_prep.pt.10K")

Incidence.HIVpt.table <- cbind(names,Incidence.HIVpt.table)

library(xlsx) #load the package

write.xlsx(x = Incidence.HIVpt.table, file = "/homes/dth2/Campcl/scenarios/out/Incidence_HIVpt_table.xlsx",
           sheetName = "output", row.names = FALSE)



###########################

#####40 years of PREP


for (i in seq_along(nsims)) {
  sim.base <- truncate_sim(s1, at = 8261)
  
  base.incid[i] <-sum(as.numeric(sim.base$epi$incid[1:2080,i]))
  base.pt[i] <- sum(as.numeric(sim.base$epi$num.deb[1:2080,i]))
  base.prept[i] <- sum(as.numeric(sim.base$epi$prepCurr[1:2080,i]))
  base.HIVt[i] <- sum(as.numeric(sim.base$epi$i.num[1:2080,i]))
  
  inc.base_p10K[i] <- base.incid[i] / ((base.pt[i]-base.HIVt[i])/52 /10000 )
  postime.base_p10K[i] <- (base.HIVt[i]/52) / ((base.pt[i]/52)/10000)
  
  inc.base_prep.pt.10K[i]<-NA
  
  ##################.
  sim.int.1 <- truncate_sim(s2, at = 8261)
  
  int.1.incid[i] <-sum(as.numeric(sim.int.1$epi$incid[1:2080,i]))
  int.1.pt[i] <- sum(as.numeric(sim.int.1$epi$num.deb[1:2080,i]))
  int.1.prept[i] <- sum(as.numeric(sim.int.1$epi$prepCurr[1:2080,i]))
  int.1.HIVt[i] <- sum(as.numeric(sim.int.1$epi$i.num[1:2080,i]))
  
  inc.int.1_p10K[i] <- int.1.incid[i] / (((int.1.pt[i]-int.1.HIVt[i])/52)/10000 )
  postime.int.1_p10K[i] <- (int.1.HIVt[i]/52) / ((int.1.pt[i]/52)/10000)
  
  inc.int.1_prep.pt.10K[i] <- (base.incid[i] - int.1.incid[i]) / ((int.1.prept[i])/52 /10000 )
  
  ##############.
  sim.int.2 <- truncate_sim(s3, at = 8261)
  
  int.2.incid[i] <-sum(as.numeric(sim.int.2$epi$incid[1:2080,i]))
  int.2.pt[i] <- sum(as.numeric(sim.int.2$epi$num.deb[1:2080,i]))
  int.2.prept[i] <- sum(as.numeric(sim.int.2$epi$prepCurr[1:2080,i]))
  int.2.HIVt[i] <- sum(as.numeric(sim.int.2$epi$i.num[1:2080,i]))
  
  inc.int.2_p10K[i] <- int.2.incid[i] / (((int.2.pt[i]-int.2.HIVt[i])/52)/10000 )
  postime.int.2_p10K[i] <- (int.2.HIVt[i]/52) / ((int.2.pt[i]/52)/10000)
  
  inc.int.2_prep.pt.10K[i] <- (base.incid[i] - int.2.incid[i]) / ((int.2.prept[i])/52 /10000 )
  
  ##############.
  sim.int.3 <- truncate_sim(s4, at = 8261)
  
  int.3.incid[i] <-sum(as.numeric(sim.int.3$epi$incid[1:2080,i]))
  int.3.pt[i] <- sum(as.numeric(sim.int.3$epi$num.deb[1:2080,i]))
  int.3.prept[i] <- sum(as.numeric(sim.int.3$epi$prepCurr[1:2080,i]))
  int.3.HIVt[i] <- sum(as.numeric(sim.int.3$epi$i.num[1:2080,i]))
  
  inc.int.3_p10K[i] <- int.3.incid[i] / (((int.3.pt[i]-int.3.HIVt[i])/52)/10000 )
  postime.int.3_p10K[i] <- (int.3.HIVt[i]/52) / ((int.3.pt[i]/52)/10000) 
  
  inc.int.3_prep.pt.10K[i] <- (base.incid[i] - int.3.incid[i]) / ((int.3.prept[i])/52 /10000 )
  
  
  ##############.
  sim.int.4 <- truncate_sim(s5, at = 8261)
  
  int.4.incid[i] <-sum(as.numeric(sim.int.4$epi$incid[1:2080,i]))
  int.4.pt[i] <- sum(as.numeric(sim.int.4$epi$num.deb[1:2080,i]))
  int.4.prept[i] <- sum(as.numeric(sim.int.4$epi$prepCurr[1:2080,i]))
  int.4.HIVt[i] <- sum(as.numeric(sim.int.4$epi$i.num[1:2080,i]))
  
  inc.int.4_p10K[i] <- int.4.incid[i] / (((int.4.pt[i]-int.4.HIVt[i])/52)/10000 )
  postime.int.4_p10K[i] <- (int.4.HIVt[i]/52) / ((int.4.pt[i]/52)/10000)
  
  inc.int.4_prep.pt.10K[i] <- (base.incid[i] - int.4.incid[i]) / ((int.4.prept[i])/52 /10000 )
  
}



####Create 95% CrI.
base.incid <-sort(base.incid)
base.incid_hi<-mean(base.incid[97],base.incid[98])
base.incid_low<-mean(base.incid[2],base.incid[3])
base.incid <-mean(base.incid)

base.pt <-sort(base.pt)
base.pt_hi<-mean(base.pt[97],base.pt[98])
base.pt_low<-mean(base.pt[2],base.pt[3])
base.pt <-mean(base.pt)

base.prept <-sort(base.prept)
base.prept_hi<-mean(base.prept[97],base.prept[98])
base.prept_low<-mean(base.prept[2],base.prept[3])
base.prept <-mean(base.prept)

base.HIVt <-sort(base.HIVt)
base.HIVt_hi<-mean(base.HIVt[97],base.HIVt[98])
base.HIVt_low<-mean(base.HIVt[2],base.HIVt[3])
base.HIVt <-mean(base.HIVt)

inc.base_p10K <- sort(inc.base_p10K) 
inc.base_p10K_hi <- mean(inc.base_p10K[97],inc.base_p10K[98])
inc.base_p10K_low <- mean(inc.base_p10K[2],inc.base_p10K[3])
inc.base_p10K <- mean(inc.base_p10K)

postime.base_p10K <- sort(postime.base_p10K) 
postime.base_p10K_hi <- mean(postime.base_p10K[97],postime.base_p10K[98])
postime.base_p10K_low <- mean(postime.base_p10K[2],postime.base_p10K[3])
postime.base_p10K <- mean(postime.base_p10K)

inc.base_prep.pt.10K <- sort(inc.base_prep.pt.10K)
inc.base_prep.pt.10K_hi <- mean(inc.base_prep.pt.10K[97],inc.base_prep.pt.10K[98])
inc.base_prep.pt.10K_low <- mean(inc.base_prep.pt.10K[2],inc.base_prep.pt.10K[3])
inc.base_prep.pt.10K <- mean(inc.base_prep.pt.10K)


###int.1.
int.1.incid <-sort(int.1.incid)
int.1.incid_hi<-mean(int.1.incid[97],int.1.incid[98])
int.1.incid_low<-mean(int.1.incid[2],int.1.incid[3])
int.1.incid <-mean(int.1.incid)

int.1.pt <-sort(int.1.pt)
int.1.pt_hi<-mean(int.1.pt[97],int.1.pt[98])
int.1.pt_low<-mean(int.1.pt[2],int.1.pt[3])
int.1.pt <-mean(int.1.pt)

int.1.prept <-sort(int.1.prept)
int.1.prept_hi<-mean(int.1.prept[97],int.1.prept[98])
int.1.prept_low<-mean(int.1.prept[2],int.1.prept[3])
int.1.prept <-mean(int.1.prept)

int.1.HIVt <-sort(int.1.HIVt)
int.1.HIVt_hi<-mean(int.1.HIVt[97],int.1.HIVt[98])
int.1.HIVt_low<-mean(int.1.HIVt[2],int.1.HIVt[3])
int.1.HIVt <-mean(int.1.HIVt)


inc.int.1_p10K <- sort(inc.int.1_p10K) 
inc.int.1_p10K_hi <- mean(inc.int.1_p10K[97],inc.int.1_p10K[98])
inc.int.1_p10K_low <- mean(inc.int.1_p10K[2],inc.int.1_p10K[3])
inc.int.1_p10K <- mean(inc.int.1_p10K)

postime.int.1_p10K <- sort(postime.int.1_p10K) 
postime.int.1_p10K_hi <- mean(postime.int.1_p10K[97],postime.int.1_p10K[98])
postime.int.1_p10K_low <- mean(postime.int.1_p10K[2],postime.int.1_p10K[3])
postime.int.1_p10K <- mean(postime.int.1_p10K)

inc.int.1_prep.pt.10K <- sort(inc.int.1_prep.pt.10K)
inc.int.1_prep.pt.10K_hi <- mean(inc.int.1_prep.pt.10K[97],inc.int.1_prep.pt.10K[98])
inc.int.1_prep.pt.10K_low <- mean(inc.int.1_prep.pt.10K[2],inc.int.1_prep.pt.10K[3])
inc.int.1_prep.pt.10K <- mean(inc.int.1_prep.pt.10K)


###int.2.

int.2.incid <-sort(int.2.incid)
int.2.incid_hi<-mean(int.2.incid[97],int.2.incid[98])
int.2.incid_low<-mean(int.2.incid[2],int.2.incid[3])
int.2.incid <-mean(int.2.incid)

int.2.pt <-sort(int.2.pt)
int.2.pt_hi<-mean(int.2.pt[97],int.2.pt[98])
int.2.pt_low<-mean(int.2.pt[2],int.2.pt[3])
int.2.pt <-mean(int.2.pt)

int.2.prept <-sort(int.2.prept)
int.2.prept_hi<-mean(int.2.prept[97],int.2.prept[98])
int.2.prept_low<-mean(int.2.prept[2],int.2.prept[3])
int.2.prept <-mean(int.2.prept)

int.2.HIVt <-sort(int.2.HIVt)
int.2.HIVt_hi<-mean(int.2.HIVt[97],int.2.HIVt[98])
int.2.HIVt_low<-mean(int.2.HIVt[2],int.2.HIVt[3])
int.2.HIVt <-mean(int.2.HIVt)

inc.int.2_p10K <- sort(inc.int.2_p10K) 
inc.int.2_p10K_hi <- mean(inc.int.2_p10K[97],inc.int.2_p10K[98])
inc.int.2_p10K_low <- mean(inc.int.2_p10K[2],inc.int.2_p10K[3])
inc.int.2_p10K <- mean(inc.int.2_p10K)

postime.int.2_p10K <- sort(postime.int.2_p10K) 
postime.int.2_p10K_hi <- mean(postime.int.2_p10K[97],postime.int.2_p10K[98])
postime.int.2_p10K_low <- mean(postime.int.2_p10K[2],postime.int.2_p10K[3])
postime.int.2_p10K <- mean(postime.int.2_p10K)

inc.int.2_prep.pt.10K <- sort(inc.int.2_prep.pt.10K)
inc.int.2_prep.pt.10K_hi <- mean(inc.int.2_prep.pt.10K[97],inc.int.2_prep.pt.10K[98])
inc.int.2_prep.pt.10K_low <- mean(inc.int.2_prep.pt.10K[2],inc.int.2_prep.pt.10K[3])
inc.int.2_prep.pt.10K <- mean(inc.int.2_prep.pt.10K)



###int.3.

int.3.incid <-sort(int.3.incid)
int.3.incid_hi<-mean(int.3.incid[97],int.3.incid[98])
int.3.incid_low<-mean(int.3.incid[2],int.3.incid[3])
int.3.incid <-mean(int.3.incid)

int.3.pt <-sort(int.3.pt)
int.3.pt_hi<-mean(int.3.pt[97],int.3.pt[98])
int.3.pt_low<-mean(int.3.pt[2],int.3.pt[3])
int.3.pt <-mean(int.3.pt)

int.3.prept <-sort(int.3.prept)
int.3.prept_hi<-mean(int.3.prept[97],int.3.prept[98])
int.3.prept_low<-mean(int.3.prept[2],int.3.prept[3])
int.3.prept <-mean(int.3.prept)

int.3.HIVt <-sort(int.3.HIVt)
int.3.HIVt_hi<-mean(int.3.HIVt[97],int.3.HIVt[98])
int.3.HIVt_low<-mean(int.3.HIVt[2],int.3.HIVt[3])
int.3.HIVt <-mean(int.3.HIVt)

inc.int.3_p10K <- sort(inc.int.3_p10K) 
inc.int.3_p10K_hi <- mean(inc.int.3_p10K[97],inc.int.3_p10K[98])
inc.int.3_p10K_low <- mean(inc.int.3_p10K[2],inc.int.3_p10K[3])
inc.int.3_p10K <- mean(inc.int.3_p10K)

postime.int.3_p10K <- sort(postime.int.3_p10K) 
postime.int.3_p10K_hi <- mean(postime.int.3_p10K[97],postime.int.3_p10K[98])
postime.int.3_p10K_low <- mean(postime.int.3_p10K[2],postime.int.3_p10K[3])
postime.int.3_p10K <- mean(postime.int.3_p10K)

inc.int.3_prep.pt.10K <- sort(inc.int.3_prep.pt.10K)
inc.int.3_prep.pt.10K_hi <- mean(inc.int.3_prep.pt.10K[97],inc.int.3_prep.pt.10K[98])
inc.int.3_prep.pt.10K_low <- mean(inc.int.3_prep.pt.10K[2],inc.int.3_prep.pt.10K[3])
inc.int.3_prep.pt.10K <- mean(inc.int.3_prep.pt.10K)


###int.4.

int.4.incid <-sort(int.4.incid)
int.4.incid_hi<-mean(int.4.incid[97],int.4.incid[98])
int.4.incid_low<-mean(int.4.incid[2],int.4.incid[3])
int.4.incid <-mean(int.4.incid)

int.4.pt <-sort(int.4.pt)
int.4.pt_hi<-mean(int.4.pt[97],int.4.pt[98])
int.4.pt_low<-mean(int.4.pt[2],int.4.pt[3])
int.4.pt <-mean(int.4.pt)

int.4.prept <-sort(int.4.prept)
int.4.prept_hi<-mean(int.4.prept[97],int.4.prept[98])
int.4.prept_low<-mean(int.4.prept[2],int.4.prept[3])
int.4.prept <-mean(int.4.prept)

int.4.HIVt <-sort(int.4.HIVt)
int.4.HIVt_hi<-mean(int.4.HIVt[97],int.4.HIVt[98])
int.4.HIVt_low<-mean(int.4.HIVt[2],int.4.HIVt[3])
int.4.HIVt <-mean(int.4.HIVt)

inc.int.4_p10K <- sort(inc.int.4_p10K) 
inc.int.4_p10K_hi <- mean(inc.int.4_p10K[97],inc.int.4_p10K[98])
inc.int.4_p10K_low <- mean(inc.int.4_p10K[2],inc.int.4_p10K[3])
inc.int.4_p10K <- mean(inc.int.4_p10K)

postime.int.4_p10K <- sort(postime.int.4_p10K) 
postime.int.4_p10K_hi <- mean(postime.int.4_p10K[97],postime.int.4_p10K[98])
postime.int.4_p10K_low <- mean(postime.int.4_p10K[2],postime.int.4_p10K[3])
postime.int.4_p10K <- mean(postime.int.4_p10K)

inc.int.4_prep.pt.10K <- sort(inc.int.4_prep.pt.10K)
inc.int.4_prep.pt.10K_hi <- mean(inc.int.4_prep.pt.10K[97],inc.int.4_prep.pt.10K[98])
inc.int.4_prep.pt.10K_low <- mean(inc.int.4_prep.pt.10K[2],inc.int.4_prep.pt.10K[3])
inc.int.4_prep.pt.10K <- mean(inc.int.4_prep.pt.10K)


inc.base_p10K <- c(inc.base_p10K, inc.base_p10K_low,inc.base_p10K_hi)
inc.int.1_p10K <- c(inc.int.1_p10K, inc.int.1_p10K_low,inc.int.1_p10K_hi)
inc.int.2_p10K <- c(inc.int.2_p10K, inc.int.2_p10K_low,inc.int.2_p10K_hi)
inc.int.3_p10K<-c(inc.int.3_p10K, inc.int.3_p10K_low,inc.int.3_p10K_hi)
inc.int.4_p10K<-c(inc.int.4_p10K, inc.int.4_p10K_low,inc.int.4_p10K_hi)
postime.base_p10K<-c(postime.base_p10K,postime.base_p10K_low, postime.base_p10K_hi)
postime.int.1_p10K<-c(postime.int.1_p10K,postime.int.1_p10K_low, postime.int.1_p10K_hi)
postime.int.2_p10K<-c(postime.int.2_p10K,postime.int.2_p10K_low, postime.int.2_p10K_hi)
postime.int.3_p10K<-c(postime.int.3_p10K,postime.int.3_p10K_low, postime.int.3_p10K_hi)
postime.int.4_p10K<-c(postime.int.4_p10K,postime.int.4_p10K_low, postime.int.4_p10K_hi)

inc.base_prep.pt.10K<-c(inc.base_prep.pt.10K, inc.base_prep.pt.10K_hi, inc.base_prep.pt.10K_low)
inc.int.1_prep.pt.10K<-c(inc.int.1_prep.pt.10K, inc.int.1_prep.pt.10K_hi, inc.int.1_prep.pt.10K_low)
inc.int.2_prep.pt.10K<-c(inc.int.2_prep.pt.10K, inc.int.2_prep.pt.10K_hi, inc.int.2_prep.pt.10K_low)
inc.int.3_prep.pt.10K<-c(inc.int.3_prep.pt.10K, inc.int.3_prep.pt.10K_hi, inc.int.3_prep.pt.10K_low)
inc.int.4_prep.pt.10K<-c(inc.int.4_prep.pt.10K, inc.int.4_prep.pt.10K_hi, inc.int.4_prep.pt.10K_low)

Incidence.HIVpt.table40 <-rbind(inc.base_p10K, inc.int.1_p10K, inc.int.2_p10K, inc.int.3_p10K, inc.int.4_p10K,
                              postime.base_p10K, postime.int.1_p10K, postime.int.2_p10K, postime.int.3_p10K, postime.int.4_p10K,
                              inc.base_prep.pt.10K, inc.int.1_prep.pt.10K, inc.int.2_prep.pt.10K, inc.int.3_prep.pt.10K, inc.int.4_prep.pt.10K)

names <- c("inc.base_p10K", "inc.int.1_p10K", "inc.int.2_p10K", "inc.int.3_p10K", "inc.int.4_p10K",
           "postime.base_p10K", "postime.int.1_p10K", "postime.int.2_p10K", "postime.int.3_p10K", "postime.int.4_p10K",
           "inc.base_prep.pt.10K", "inc.int.1_prep.pt.10K", "inc.int.2_prep.pt.10K", "inc.int.3_prep.pt.10K", "inc.int.4_prep.pt.10K")

Incidence.HIVpt.table40 <- cbind(names,Incidence.HIVpt.table40)


Incidence.HIVpt.table40

library(xlsx) #load the package

write.xlsx(x = Incidence.HIVpt.table40, file = "/homes/dth2/Campcl/scenarios/out/Incidence_HIVpt_table40.xlsx",
           sheetName = "output", row.names = FALSE)


