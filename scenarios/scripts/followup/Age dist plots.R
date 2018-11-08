
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
base <- NULL
age<-NULL
time<-NULL
      for(i in 1:100){
        age <- c(age,s1$age.inf.vec[[i]]$age) 
        time <- c(time,s1$age.inf.vec[[i]]$time)
      }
base<-as.data.frame(cbind(age,time))
base<-base[base$time > 10080,]

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





#Now, combine your two dataframes into one.  First make a new column in each that will be a variable to identify where they came from later.
base$Intervention <- ' No PrEP'
int1$Intervention <- 'AMSM - 40%, ASMM - 0%'
int2$Intervention <- 'AMSM - 40%, ASMM - 10%'
int3$Intervention <- 'AMSM - 40%, ASMM - 20%'
int4$Intervention <- 'AMSM - 40%, ASMM - 30%'



#and combine into your new data frame vegLengths
ages <- rbind(base, int1, int2, int3, int4)
ggplot(ages, aes(age, fill = Intervention)) + geom_density(alpha = 0.2)


###############################################################################################
tiff(filename = "Age at infection dist.tiff", height = 7, width = 9, units = "in", res = 250)
ggplot(ages, aes (y=Intervention)) +
  geom_density_ridges(aes(x = age, fill = Intervention), 
                      alpha = .4, color = "black", from = 10, to = 45) +
  labs(x = "Age at infection",
       y = "Intervention",
       title = "Age distribution of HIV aquisition",
       name = "Option", guide = "legend") +
  theme_ridges(grid = FALSE)

dev.off()

################################################################





#############   10 YEAR MEANS.

##Base case
base_b <- NULL
age<-NULL
time<-NULL
for(i in 1:100){
  age <- c(age,s1$age.inf.vec[[i]]$age) 
  time <- c(time,s1$age.inf.vec[[i]]$time)
}
base_10<-as.data.frame(cbind(age,time))
base_10<-base_10[base_10$time > 8260,]
base_10<-base_10[base_10$time < 8780,]

##int1
int1_10 <- NULL
age<-NULL
time<-NULL
for(i in 1:100){
  age <- c(age,s2$age.inf.vec[[i]]$age) 
  time <- c(time,s2$age.inf.vec[[i]]$time)
}
int1_10<-as.data.frame(cbind(age,time))
int1_10<-int1_10[int1_10$time > 8260,]
int1_10<-int1_10[int1_10$time < 8780,]

##int2
int2_10 <- NULL
age<-NULL
time<-NULL
for(i in 1:100){
  age <- c(age,s3$age.inf.vec[[i]]$age) 
  time <- c(time,s3$age.inf.vec[[i]]$time)
}
int2_10<-as.data.frame(cbind(age,time))
int2_10<-int2_10[int2_10$time > 8260,]
int2_10<-int2_10[int2_10$time < 8780,]


##int3
int3_10 <- NULL
age<-NULL
time<-NULL
for(i in 1:100){
  age <- c(age,s4$age.inf.vec[[i]]$age) 
  time <- c(time,s4$age.inf.vec[[i]]$time)
}
int3_10<-as.data.frame(cbind(age,time))
int3_10<-int3_10[int3_10$time > 8260,]
int3_10<-int3_10[int3_10$time < 8780,]

##int4
int4_10 <- NULL
age<-NULL
time<-NULL
for(i in 1:100){
  age <- c(age,s5$age.inf.vec[[i]]$age) 
  time <- c(time,s5$age.inf.vec[[i]]$time)
}
int4_10<-as.data.frame(cbind(age,time))
int4_10<-int4_10[int4_10$time > 8260,]
int4_10<-int4_10[int4_10$time < 8780,]



base.age<-mean(base$age)
base.age.sd<-sd(base$age)
int1.age<-mean(int1$age)
int1.age.sd<-sd(int1$age)
int2.age<-mean(int2$age)
int2.age.sd<-sd(int2$age)
int3.age<-mean(int3$age)
int3.age.sd<-sd(int3$age)
int4.age<-mean(int4$age)
int4.age.sd<-sd(int4$age)

base.age_10<-mean(base_10$age)
base.age.sd_10<-sd(base_10$age)
int1.age_10<-mean(int1_10$age)
int1.age.sd_10<-sd(int1_10$age)
int2.age_10<-mean(int2_10$age)
int2.age.sd_10<-sd(int2_10$age)
int3.age_10<-mean(int3_10$age)
int3.age.sd_10<-sd(int3_10$age)
int4.age_10<-mean(int4_10$age)
int4.age.sd_10<-sd(int4_10$age)


base.age
base.age.sd
int1.age
int1.age.sd
int2.age
int2.age.sd
int3.age
int3.age.sd
int4.age
int4.age.sd

base.age_10
base.age.sd_10
int1.age_10
int1.age.sd_10
int2.age_10
int2.age.sd_10
int3.age_10
int3.age.sd_10
int4.age_10
int4.age.sd_10

age.at.infection<-cbind(base.age, base.age.sd, int1.age, int1.age.sd, int2.age, int2.age.sd, 
                        int3.age, int3.age.sd, int4.age, int4.age.sd, base.age_10, base.age.sd_10,
                        int1.age_10, int1.age.sd_10, int2.age_10, int2.age.sd_10, 
                        int3.age_10, int3.age.sd_10, int4.age_10, int4.age.sd_10)


library(xlsx) #load the package
library(rJava)
        
write.xlsx(x = age.at.infection, file = "/homes/dth2/Campcl/scenarios/out/age_at_infection.xlsx",
           sheetName = "output", row.names = FALSE)







