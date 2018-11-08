
## whamp analysis file

library(EpiModelHIV)
library(EpiModelHPC)
library(ggplot2)
library(ggridges)


s1 <- merge_simfiles(1, indir = "~/Campcl/scenarios/data/")
s2 <- merge_simfiles(2, indir = "~/Campcl/scenarios/data/")
s3 <- merge_simfiles(3, indir = "~/Campcl/scenarios/data/")
s4 <- merge_simfiles(4, indir = "~/Campcl/scenarios/data/")
s5 <- merge_simfiles(5, indir = "~/Campcl/scenarios/data/")
s6 <- merge_simfiles(6, indir = "~/Campcl/scenarios/data/")


##Base case
incid.base.asmm_10 <- rep(NA,100)
incid.base.msm_10 <- rep(NA,100)
time <- 520


      for(i in 1:100){
        
        temp.asmm <- s1$age.inf.vec[[i]][s1$age.inf.vec[[i]]$age < 19]
        temp.asmm <- temp.asmm[temp.asmm$time > 8260]
        temp.asmm <- temp.asmm[temp.asmm$time < 8781]
        incid.base.asmm[i] <- length(temp.asmm$age)/time
        
        temp.msm <- s1$age.inf.vec[[i]][s1$age.inf.vec[[i]]$age >= 19]
        temp.msm <- temp.msm[temp.msm$time > 8260]
        temp.msm <- temp.msm[temp.msm$time < 8781]
        incid.base.msm[i] <- length(temp.msm$age)/time
        
      }

incid.base.asmm_40 <- rep(NA,100)
incid.base.msm_40 <- rep(NA,100)
time <- 2080


for(i in 1:100){
  
  temp.asmm <- s1$age.inf.vec[[i]][s1$age.inf.vec[[i]]$age < 19]
  temp.asmm <- temp.asmm[temp.asmm$time > 8260]
  temp.asmm <- temp.asmm[temp.asmm$time < 10340]
  incid.base.asmm_40[i] <- length(temp.asmm$age)/time
  
  temp.msm <- s1$age.inf.vec[[i]][s1$age.inf.vec[[i]]$age >= 19]
  temp.msm <- temp.msm[temp.msm$time > 8260]
  temp.msm <- temp.msm[temp.msm$time < 10340]
  incid.base.msm_40[i] <- length(temp.msm$age)/time
  
}


##int1
int1 <- NULL
age<-NULL
time<-NULL
for(i in 1:100){
  age <- c(age,s2$age.inf.vec[[i]]$age) 
  time <- c(time,s2$age.inf.vec[[i]]$time)
}
int1<-as.data.frame(cbind(age,time))
int1<-int1[int1$time > 10080,]



##int2
int2 <- NULL
age<-NULL
time<-NULL
for(i in 1:100){
  age <- c(age,s3$age.inf.vec[[i]]$age) 
  time <- c(time,s3$age.inf.vec[[i]]$time)
}
int2<-as.data.frame(cbind(age,time))
int2<-int2[int2$time > 10080,]


##int3
int3 <- NULL
age<-NULL
time<-NULL
for(i in 1:100){
  age <- c(age,s4$age.inf.vec[[i]]$age) 
  time <- c(time,s4$age.inf.vec[[i]]$time)
}
int3<-as.data.frame(cbind(age,time))
int3<-int3[int3$time > 10080,]


##int4
int4 <- NULL
age<-NULL
time<-NULL
for(i in 1:100){
  age <- c(age,s5$age.inf.vec[[i]]$age) 
  time <- c(time,s5$age.inf.vec[[i]]$time)
}
int4<-as.data.frame(cbind(age,time))
int4<-int4[int4$time > 10080,]









age.at.infection<-cbind(base.age, base.age.sd, int1.age, int1.age.sd, int2.age, int2.age.sd, 
                        int3.age, int3.age.sd, int4.age, int4.age.sd, base.age_10, base.age.sd_10,
                        int1.age_10, int1.age.sd_10, int2.age_10, int2.age.sd_10, 
                        int3.age_10, int3.age.sd_10, int4.age_10, int4.age.sd_10)


library(xlsx) #load the package
library(rJava)
        
write.xlsx(x = age.at.infection, file = "/homes/dth2/Campcl/scenarios/out/age_at_infection.xlsx",
           sheetName = "output", row.names = FALSE)







