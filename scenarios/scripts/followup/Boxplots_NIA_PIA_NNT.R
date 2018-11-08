


library(EpiModelHIV)
library(EpiModelHPC)



s1 <- merge_simfiles(1, indir = "Campcl/scenarios/data/")
s2 <- merge_simfiles(2, indir = "Campcl/scenarios/data/")
s3 <- merge_simfiles(3, indir = "Campcl/scenarios/data/")
s4 <- merge_simfiles(4, indir = "Campcl/scenarios/data/")
s5 <- merge_simfiles(5, indir = "Campcl/scenarios/data/")
s6 <- merge_simfiles(6, indir = "Campcl/scenarios/data/")


data<-c("s1","s2","s3","s4","s5","s6") 
 


##Basline (no prep)
incid.total.base.all<-rep(NA,100)
prep.pt.base.all<-rep(NA,100)
person.time.base.all<-rep(NA,100)
person.time.deb.base.all<-rep(NA,100)
prev.all

incid.total.base.msm<-rep(NA,100)
prep.pt.base.msm<-rep(NA,100)
person.time.base.msm<-rep(NA,100)
prev.msm

incid.total.base.asmm<-rep(NA,100)
prep.pt.base.asmm<-rep(NA,100)
person.time.base.asmm<-rep(NA,100)
person.time.deb.base.asmm<-rep(NA,100)
prev.asmm.all
prev.asmm.18

  for (j in 1:100){  
    incid.total.base.all[j]<-sum(s1$epi$incid[8261:8520,j])
    prep.pt.base.all[j]<-sum(s1$epi$prepCurr[8261:8520,j])
    person.time.base.all[j]<-sum(s1$epi$num[8261:8520,j]) - sum(s1$epi$i.num[8261:8520,j])
    person.time.deb.base.all[j]<-sum(s1$epi$debuted[8261:8520,j]) - sum(s1$epi$i.num[8261:8520,j])
    prev.all[j]<-s1$epi$i.prev[8520,j]

   incid.base.all<-mean(incid.total.base.all)
   
   incid.total.base.msm[j]<-sum(s1$epi$incid.msm[8261:8520,j])
   prep.pt.base.msm[j]<-sum(s1$epi$prepCurr.msm[8261:8520,j])
   person.time.base.msm[j]<-sum(s1$epi$num.msm[8261:8520,j]) - sum(s1$epi$i.num.msm[8261:8520,j])
   prev.msm[j]<-s1$epi$i.prev.msm[8520,j]
   
   incid.base.msm<-mean(incid.total.base.msm)
   
   incid.total.base.asmm[j]<-sum(s1$epi$incid.asmm[8261:8520,j])
   prep.pt.base.asmm[j]<-sum(s1$epi$prepCurr.asmm[8261:8520,j])
   person.time.base.asmm[j]<-sum(s1$epi$num.asmm[8261:8520,j]) - sum(s1$epi$i.num.asmm[8261:8520,j])
   person.time.deb.base.asmm[j]<-sum(s1$epi$debuted.asmm[8261:8520,j]) - sum(s1$epi$i.num.asmm[8261:8520,j])
   prev.asmm[j]<-s1$epi$i.prev.asmm[8520,j]
   
   incid.base.asmm<-mean(incid.total.base.asmm)
   

   
   }


## Condition 1
## Adult PrEP

incid.total.c1.all<-rep(NA,100)
prep.pt.c1.all<-rep(NA,100)
person.time.c1.all<-rep(NA,100)
person.time.deb.c1.all<-rep(NA,100)
prev.c1.all<-rep(NA,100)
NIA.c1.all<-rep(NA,100)
PIA.c1.all<-rep(NA,100)
NNT.c1.all<-rep(NA,100)

incid.total.c1.msm<-rep(NA,100)
prep.pt.c1.msm<-rep(NA,100)
person.time.c1.msm<-rep(NA,100)
prev.c1.msm<-rep(NA,100)
NIA.c1.msm<-rep(NA,100)
PIA.c1.msm<-rep(NA,100)
NNT.c1.msm<-rep(NA,100)

incid.total.c1.asmm<-rep(NA,100)
prep.pt.c1.asmm<-rep(NA,100)
person.time.c1.asmm<-rep(NA,100)
person.time.deb.c1.asmm<-rep(NA,100)
prev.c1.asmm<-rep(NA,100)
prev.c1.age18<-rep(NA,100)
NIA.c1.asmm<-rep(NA,100)
PIA.c1.asmm<-rep(NA,100)
NNT.c1.asmm<-rep(NA,100)

for (j in 1:100){  
  incid.total.c1.all[j]<-sum(s2$epi$incid[8261:8520,j])
  prep.pt.c1.all[j]<-sum(s2$epi$prepCurr[8261:8520,j])
  person.time.c1.all[j]<-sum(s2$epi$num[8261:8520,j]) - sum(s2$epi$i.num[8261:8520,j])
  person.time.deb.c1.all[j]<-sum(s2$epi$debuted[8261:8520,j])  - sum(s2$epi$i.num[8261:8520,j])
  prev.c1.all[j]<-s2$epi$i.prev[2080,j]
 
    incid.total.c1.msm[j]<-sum(s2$epi$incid.msm[8261:8520,j])
  prep.pt.c1.msm[j]<-sum(s2$epi$prepCurr.msm[8261:8520,j])
  person.time.c1.msm[j]<-sum(s2$epi$num.msm[8261:8520,j]) - sum(s2$epi$i.num.msm[8261:8520,j])
  prev.c1.msm[j]<-s2$epi$i.prev.msm[2080,j]

  incid.total.c1.asmm[j]<-sum(s2$epi$incid.asmm[8261:8520,j])
  prep.pt.c1.asmm[j]<-sum(s2$epi$prepCurr.asmm[8261:8520,j])
  person.time.c1.asmm[j]<-sum(s2$epi$num.asmm[8261:8520,j]) - sum(s2$epi$i.num.asmm[8261:8520,j])
  person.time.deb.c1.asmm[j]<-sum(s2$epi$debuted.asmm[8261:8520,j]) - sum(s2$epi$i.num.asmm[8261:8520,j])
  prev.c1.asmm[j]<-s2$epi$i.prev.asmm[2080,j]
  prev.c1.age18[j]<-s2$epi$i.prev.age18[2080,j]
}

  

#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c1.all[i]<-((incid.base.all-incid.total.c1.all[i])/person.time.deb.c1.all[i])*52*100000
  NIA.c1.msm[i]<-((incid.base.msm-incid.total.c1.msm[i])/person.time.c1.msm[i])*52*100000
  NIA.c1.asmm[i]<-((incid.base.asmm-incid.total.c1.asmm[i])/person.time.deb.c1.asmm[i])*52*100000
  
#Percent of infection averted.

  PIA.c1.all[i]<-(incid.base.all-incid.total.c1.all[i])/incid.base.all
  PIA.c1.msm[i]<-(incid.base.msm-incid.total.c1.msm[i])/incid.base.msm
  PIA.c1.asmm[i]<-(incid.base.asmm-incid.total.c1.asmm[i])/incid.base.asmm
  
#NNT .
  
  NNT.c1.all[i]<-(prep.pt.c1.all[i]/52)/(incid.base.all-incid.total.c1.all[i])
  NNT.c1.msm[i]<-(prep.pt.c1.msm[i]/52)/(incid.base.msm-incid.total.c1.msm[i])
  NNT.c1.asmm[i]<-(prep.pt.c1.asmm[i]/52)/(incid.base.asmm-incid.total.c1.asmm[i])
  
}


##Condition 2.
##Adult PrEP & 10% ASMM PrEP

incid.total.c2.all<-rep(NA,100)
prep.pt.c2.all<-rep(NA,100)
person.time.c2.all<-rep(NA,100)
person.time.deb.c2.all<-rep(NA,100)
prev.c2.all<-rep(NA,100)
NIA.c2.all<-rep(NA,100)
PIA.c2.all<-rep(NA,100)
NNT.c2.all<-rep(NA,100)

incid.total.c2.msm<-rep(NA,100)
prep.pt.c2.msm<-rep(NA,100)
person.time.c2.msm<-rep(NA,100)
prev.c2.msm<-rep(NA,100)
NIA.c2.msm<-rep(NA,100)
PIA.c2.msm<-rep(NA,100)
NNT.c2.msm<-rep(NA,100)

incid.total.c2.asmm<-rep(NA,100)
prep.pt.c2.asmm<-rep(NA,100)
person.time.c2.asmm<-rep(NA,100)
person.time.deb.c2.asmm<-rep(NA,100)
prev.c2.asmm<-rep(NA,100)
prev.c2.age18<-rep(NA,100)
NIA.c2.asmm<-rep(NA,100)
PIA.c2.asmm<-rep(NA,100)
NNT.c2.asmm<-rep(NA,100)

for (j in 1:100){  
  incid.total.c2.all[j]<-sum(s3$epi$incid[8261:8520,j])
  prep.pt.c2.all[j]<-sum(s3$epi$prepCurr[8261:8520,j])
  person.time.c2.all[j]<-sum(s3$epi$num[8261:8520,j]) - sum(s3$epi$i.num[8261:8520,j])
  person.time.deb.c2.all[j]<-sum(s3$epi$debuted[8261:8520,j])  - sum(s3$epi$i.num[8261:8520,j])
  prev.c2.all[j]<-s3$epi$i.prev[2080,j]
  
  incid.total.c2.msm[j]<-sum(s3$epi$incid.msm[8261:8520,j])
  prep.pt.c2.msm[j]<-sum(s3$epi$prepCurr.msm[8261:8520,j])
  person.time.c2.msm[j]<-sum(s3$epi$num.msm[8261:8520,j]) - sum(s3$epi$i.num.msm[8261:8520,j])
  prev.c2.msm[j]<-s3$epi$i.prev.msm[2080,j]
  
  incid.total.c2.asmm[j]<-sum(s3$epi$incid.asmm[8261:8520,j])
  prep.pt.c2.asmm[j]<-sum(s3$epi$prepCurr.asmm[8261:8520,j])
  person.time.c2.asmm[j]<-sum(s3$epi$num.asmm[8261:8520,j]) - sum(s3$epi$i.num.asmm[8261:8520,j])
  person.time.deb.c2.asmm[j]<-sum(s3$epi$debuted.asmm[8261:8520,j]) - sum(s3$epi$i.num.asmm[8261:8520,j])
  prev.c2.asmm[j]<-s3$epi$i.prev.asmm[2080,j]
  prev.c2.age18[j]<-s3$epi$i.prev.age18[2080,j]
}



#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c2.all[i]<-((incid.base.all-incid.total.c2.all[i])/person.time.deb.c2.all[i])*52*100000
  NIA.c2.msm[i]<-((incid.base.msm-incid.total.c2.msm[i])/person.time.c2.msm[i])*52*100000
  NIA.c2.asmm[i]<-((incid.base.asmm-incid.total.c2.asmm[i])/person.time.deb.c2.asmm[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c2.all[i]<-(incid.base.all-incid.total.c2.all[i])/incid.base.all
  PIA.c2.msm[i]<-(incid.base.msm-incid.total.c2.msm[i])/incid.base.msm
  PIA.c2.asmm[i]<-(incid.base.asmm-incid.total.c2.asmm[i])/incid.base.asmm
  
  #NNT .
  
  NNT.c2.all[i]<-(prep.pt.c2.all[i]/52)/(incid.base.all-incid.total.c2.all[i])
  NNT.c2.msm[i]<-(prep.pt.c2.msm[i]/52)/(incid.base.msm-incid.total.c2.msm[i])
  NNT.c2.asmm[i]<-(prep.pt.c2.asmm[i]/52)/(incid.base.asmm-incid.total.c2.asmm[i])
  
}

##Condition 3.
##Adult PrEP $ 20% ASMM PrEP

incid.total.c3.all<-rep(NA,100)
prep.pt.c3.all<-rep(NA,100)
person.time.c3.all<-rep(NA,100)
person.time.deb.c3.all<-rep(NA,100)
prev.c3.all<-rep(NA,100)
NIA.c3.all<-rep(NA,100)
PIA.c3.all<-rep(NA,100)
NNT.c3.all<-rep(NA,100)

incid.total.c3.msm<-rep(NA,100)
prep.pt.c3.msm<-rep(NA,100)
person.time.c3.msm<-rep(NA,100)
prev.c3.msm<-rep(NA,100)
NIA.c3.msm<-rep(NA,100)
PIA.c3.msm<-rep(NA,100)
NNT.c3.msm<-rep(NA,100)

incid.total.c3.asmm<-rep(NA,100)
prep.pt.c3.asmm<-rep(NA,100)
person.time.c3.asmm<-rep(NA,100)
person.time.deb.c3.asmm<-rep(NA,100)
prev.c3.asmm<-rep(NA,100)
prev.c3.age18<-rep(NA,100)
NIA.c3.asmm<-rep(NA,100)
PIA.c3.asmm<-rep(NA,100)
NNT.c3.asmm<-rep(NA,100)

for (j in 1:100){  
  incid.total.c3.all[j]<-sum(s4$epi$incid[8261:8520,j])
  prep.pt.c3.all[j]<-sum(s4$epi$prepCurr[8261:8520,j])
  person.time.c3.all[j]<-sum(s4$epi$num[8261:8520,j]) - sum(s4$epi$i.num[8261:8520,j])
  person.time.deb.c3.all[j]<-sum(s4$epi$debuted[8261:8520,j])  - sum(s4$epi$i.num[8261:8520,j])
  prev.c3.all[j]<-s4$epi$i.prev[2080,j]
  
  incid.total.c3.msm[j]<-sum(s4$epi$incid.msm[8261:8520,j])
  prep.pt.c3.msm[j]<-sum(s4$epi$prepCurr.msm[8261:8520,j])
  person.time.c3.msm[j]<-sum(s4$epi$num.msm[8261:8520,j]) - sum(s4$epi$i.num.msm[8261:8520,j])
  prev.c3.msm[j]<-s4$epi$i.prev.msm[2080,j]
  
  incid.total.c3.asmm[j]<-sum(s4$epi$incid.asmm[8261:8520,j])
  prep.pt.c3.asmm[j]<-sum(s4$epi$prepCurr.asmm[8261:8520,j])
  person.time.c3.asmm[j]<-sum(s4$epi$num.asmm[8261:8520,j]) - sum(s4$epi$i.num.asmm[8261:8520,j])
  person.time.deb.c3.asmm[j]<-sum(s4$epi$debuted.asmm[8261:8520,j]) - sum(s4$epi$i.num.asmm[8261:8520,j])
  prev.c3.asmm[j]<-s4$epi$i.prev.asmm[2080,j]
  prev.c3.age18[j]<-s4$epi$i.prev.age18[2080,j]
}



#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c3.all[i]<-((incid.base.all-incid.total.c3.all[i])/person.time.deb.c3.all[i])*52*100000
  NIA.c3.msm[i]<-((incid.base.msm-incid.total.c3.msm[i])/person.time.c3.msm[i])*52*100000
  NIA.c3.asmm[i]<-((incid.base.asmm-incid.total.c3.asmm[i])/person.time.deb.c3.asmm[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c3.all[i]<-(incid.base.all-incid.total.c3.all[i])/incid.base.all
  PIA.c3.msm[i]<-(incid.base.msm-incid.total.c3.msm[i])/incid.base.msm
  PIA.c3.asmm[i]<-(incid.base.asmm-incid.total.c3.asmm[i])/incid.base.asmm
  
  #NNT .
  
  NNT.c3.all[i]<-(prep.pt.c3.all[i]/52)/(incid.base.all-incid.total.c3.all[i])
  NNT.c3.msm[i]<-(prep.pt.c3.msm[i]/52)/(incid.base.msm-incid.total.c3.msm[i])
  NNT.c3.asmm[i]<-(prep.pt.c3.asmm[i]/52)/(incid.base.asmm-incid.total.c3.asmm[i])
  
}


##Condition 4.
##Adult PrEP and 30% ASMM PrEP

incid.total.c4.all<-rep(NA,100)
prep.pt.c4.all<-rep(NA,100)
person.time.c4.all<-rep(NA,100)
person.time.deb.c4.all<-rep(NA,100)
prev.c4.all<-rep(NA,100)
NIA.c4.all<-rep(NA,100)
PIA.c4.all<-rep(NA,100)
NNT.c4.all<-rep(NA,100)

incid.total.c4.msm<-rep(NA,100)
prep.pt.c4.msm<-rep(NA,100)
person.time.c4.msm<-rep(NA,100)
prev.c4.msm<-rep(NA,100)
NIA.c4.msm<-rep(NA,100)
PIA.c4.msm<-rep(NA,100)
NNT.c4.msm<-rep(NA,100)

incid.total.c4.asmm<-rep(NA,100)
prep.pt.c4.asmm<-rep(NA,100)
person.time.c4.asmm<-rep(NA,100)
person.time.deb.c4.asmm<-rep(NA,100)
prev.c4.asmm<-rep(NA,100)
prev.c4.age18<-rep(NA,100)
NIA.c4.asmm<-rep(NA,100)
PIA.c4.asmm<-rep(NA,100)
NNT.c4.asmm<-rep(NA,100)

for (j in 1:100){  
  incid.total.c4.all[j]<-sum(s5$epi$incid[8261:8520,j])
  prep.pt.c4.all[j]<-sum(s5$epi$prepCurr[8261:8520,j])
  person.time.c4.all[j]<-sum(s5$epi$num[8261:8520,j]) - sum(s5$epi$i.num[8261:8520,j])
  person.time.deb.c4.all[j]<-sum(s5$epi$debuted[8261:8520,j])  - sum(s5$epi$i.num[8261:8520,j])
  prev.c4.all[j]<-s5$epi$i.prev[2080,j]
  
  incid.total.c4.msm[j]<-sum(s5$epi$incid.msm[8261:8520,j])
  prep.pt.c4.msm[j]<-sum(s5$epi$prepCurr.msm[8261:8520,j])
  person.time.c4.msm[j]<-sum(s5$epi$num.msm[8261:8520,j]) - sum(s5$epi$i.num.msm[8261:8520,j])
  prev.c4.msm[j]<-s5$epi$i.prev.msm[2080,j]
  
  incid.total.c4.asmm[j]<-sum(s5$epi$incid.asmm[8261:8520,j])
  prep.pt.c4.asmm[j]<-sum(s5$epi$prepCurr.asmm[8261:8520,j])
  person.time.c4.asmm[j]<-sum(s5$epi$num.asmm[8261:8520,j]) - sum(s5$epi$i.num.asmm[8261:8520,j])
  person.time.deb.c4.asmm[j]<-sum(s5$epi$debuted.asmm[8261:8520,j]) - sum(s5$epi$i.num.asmm[8261:8520,j])
  prev.c4.asmm[j]<-s5$epi$i.prev.asmm[2080,j]
  prev.c4.age18[j]<-s5$epi$i.prev.age18[2080,j]
}



#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c4.all[i]<-((incid.base.all-incid.total.c4.all[i])/person.time.deb.c4.all[i])*52*100000
  NIA.c4.msm[i]<-((incid.base.msm-incid.total.c4.msm[i])/person.time.c4.msm[i])*52*100000
  NIA.c4.asmm[i]<-((incid.base.asmm-incid.total.c4.asmm[i])/person.time.deb.c4.asmm[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c4.all[i]<-(incid.base.all-incid.total.c4.all[i])/incid.base.all
  PIA.c4.msm[i]<-(incid.base.msm-incid.total.c4.msm[i])/incid.base.msm
  PIA.c4.asmm[i]<-(incid.base.asmm-incid.total.c4.asmm[i])/incid.base.asmm
  
  #NNT .
  
  NNT.c4.all[i]<-(prep.pt.c4.all[i]/52)/(incid.base.all-incid.total.c4.all[i])
  NNT.c4.msm[i]<-(prep.pt.c4.msm[i]/52)/(incid.base.msm-incid.total.c4.msm[i])
  NNT.c4.asmm[i]<-(prep.pt.c4.asmm[i]/52)/(incid.base.asmm-incid.total.c4.asmm[i])
  
}




###################################################################################################################.
##Figure 1 Boxplot of % of infections averted number need to treat to prevent 1 new infection.
##  Boxes are interquartile rannge and whiskers 95% credibility intervals for 100 simulations of each scenario.
##  Scenarios are across the range of coverage with the same adherence.
##Eligibility is AI + 6 months.

PIA.all <- cbind(PIA.c1.all,PIA.c2.all,PIA.c3.all,PIA.c4.all)
PIA.msm <- cbind(PIA.c1.msm,PIA.c2.msm,PIA.c3.msm,PIA.c4.msm)
PIA.asmm <- cbind(PIA.c1.asmm,PIA.c2.asmm,PIA.c3.asmm,PIA.c4.asmm)

NIA.all <- cbind(NIA.c1.all,NIA.c2.all,NIA.c3.all,NIA.c4.all)
NIA.msm <- cbind(NIA.c1.msm,NIA.c2.msm,NIA.c3.msm,NIA.c4.msm)
NIA.asmm <- cbind(NIA.c1.asmm,NIA.c2.asmm,NIA.c3.asmm,NIA.c4.asmm)

NNT.all <- cbind(NNT.c1.all,NNT.c2.all,NNT.c3.all,NNT.c4.all)
NNT.msm <- cbind(NNT.c1.msm,NNT.c2.msm,NNT.c3.msm,NNT.c4.msm)
NNT.asmm <- cbind(NNT.c1.asmm,NNT.c2.asmm,NNT.c3.asmm,NNT.c4.asmm)

library(wesanderson)
pal <- wes_palette("Zissou")[c(1, 9)]

 #pdf(file = "", height = 6, width = 12, pointsize = 16)
tiff(filename = "Fig1 PIA.tiff", height = 4, width = 8, units = "in", res = 250)
par(mfrow = c(1,3), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.9)

# Left Panel: PIA.all
PIA.all<-PIA.all*100
boxplot(PIA.all, outline = FALSE, medlwd = 1.1, ylim = c(0,60),
        col = c(rep(pal[1], 6), rep(pal[2], 3)),csi=.5,
        names=c("40% / 0%","40% / 10%","40% / 20%","40% / 30%"),las=2,
        #main = "",
        ylab = "Percent Infections Averted (pop)",
        xlab = "Coverage among Adult MSM / ASMM")

# Middle Panel: PIA.msm
PIA.msm<-PIA.msm*100
boxplot(PIA.msm, outline = FALSE, medlwd = 1.1,  ylim = c(0,60),
        col = c(rep(pal[1], 6), rep(pal[2], 3)),csi=.5,
        names=c("40% / 0%","40% / 10%","40% / 20%","40% / 30%"),las=2,
        #main = "",
        ylab = "Percent Infections Averted (Adult MSM)",
        xlab = "Coverage among Adult MSM / ASMM")

# Right Panel: PIA.asmm
PIA.asmm<-PIA.asmm*100
boxplot(PIA.asmm, outline = FALSE, medlwd = 1.1,  ylim = c(0,60),
        col = c(rep(pal[1], 6), rep(pal[2], 3)),csi=.5,
        names=c("40% / 0%","40% / 10%","40% / 20%","40% / 30%"),las=2,
        #main = "",
        ylab = "Percent Infections Averted (ASMM)",
        xlab = "Coverage among Adult MSM / ASMM")


dev.off()

#pdf(file = "", height = 6, width = 12, pointsize = 16)
tiff(filename = "Fig2 NIA", height = 4, width = 8, units = "in", res = 250)
par(mfrow = c(1,3), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.9)

# Left Panel: NIA ALL
boxplot(NIA.all, outline = FALSE, medlwd = 1.1, ylim = c(0,1500),
        col = c(rep(pal[1], 6), rep(pal[2], 3)),csi=.5,
        names=c("40% / 0%","40% / 10%","40% / 20%","40% / 30%"),las=2,
        #main = "NIA for Black ASMM with increasing coverage",
        ylab = "Number of Infections Averted (pop)",
        xlab = "Coverage among Adult MSM / ASMM")

# Middle Panel: NIA MSM
boxplot(NIA.msm, outline = FALSE, medlwd = 1.1,  ylim = c(0,1500),
        col = c(rep(pal[1], 6), rep(pal[2], 3)),csi=.5,
        names=c("40% / 0%","40% / 10%","40% / 20%","40% / 30%"),las=2,
        #main = "NIA for White ASMM with increasing coverage",
        ylab = "Number of Infections Averted (Adult MSM)",
        xlab = "Coverage among Adult MSM / ASMM")

# Right Panel: NIA ASMM
boxplot(NIA.asmm, outline = FALSE, medlwd = 1.1,  ylim = c(0,1500),
        col = c(rep(pal[1], 6), rep(pal[2], 3)),csi=.5,
        names=c("40% / 0%","40% / 10%","40% / 20%","40% / 30%"),las=2,
        #main = "",
        ylab = "Number of Infections Averted (ASMM)",
        xlab = "Coverage among Adult MSM / ASMM")


dev.off()

tiff(filename = "Fig3 NNT.tiff", height = 4, width = 8, units = "in", res = 250)
par(mfrow = c(1, 3), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.9)

# Left Panel: NNT all
boxplot(NNT.all, outline = FALSE, medlwd = 1.1, ylim = c(0,100),
        col = c(rep(pal[1], 7), rep(pal[2], 3)),
        names=c("40% / 0%","40% / 10%","40% / 20%","40% / 30%"),las=2,
        #main = "NNT for Black ASMM with increasing coverage",
        ylab = "Number Needed to Treat (pop)",
        xlab = "Coverage among Adult MSM / ASMM")

# Middle Panel: NNT MSM
boxplot(NNT.msm, outline = FALSE, medlwd = 1.1, ylim = c(0,100),
        col = c(rep(pal[1], 7), rep(pal[2], 3)),
        names=c("40% / 0%","40% / 10%","40% / 20%","40% / 30%"),las=2,
        #main = "NNT for Black ASMM with increasing coverage",
        ylab = "Number Needed to Treat (Adult MSM)",
        xlab = "Coverage among Adult MSM / ASMM")

# Right Panel: NNT ASMM
boxplot(NNT.asmm, outline = FALSE, medlwd = 1.1,  ylim = c(0,1000),
        col = c(rep(pal[1], 7), rep(pal[2], 3)),csi=.5,
        names=c("40% / 0%","40% / 10%","40% / 20%","40% / 30%"),las=2,
        #main = "NNT for White ASMM with increasing coverage",
        ylab = "Number Needed to Treat (ASMM)",
        xlab = "Coverage among Adult MSM / ASMM")

dev.off()


