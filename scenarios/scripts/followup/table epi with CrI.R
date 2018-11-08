
## CAMP Adol analysis file

library(EpiModelHIV)
library(EpiModelHPC)


s1 <- merge_simfiles(1, indir = "~/Campcl/scenarios/data/")
s2 <- merge_simfiles(2, indir = "~/Campcl/scenarios/data/")
s3 <- merge_simfiles(3, indir = "~/Campcl/scenarios/data/")
s4 <- merge_simfiles(4, indir = "~/Campcl/scenarios/data/")
s5 <- merge_simfiles(5, indir = "~/Campcl/scenarios/data/")



#Base incidence.
incid.total.base.all<-rep(NA,100)
incid.total.base.msm<-rep(NA,100)
incid.total.base.asmm<-rep(NA,100)

for (i in 1:100){
  
  incid.total.base.all[i]<-sum(s1$epi$incid[8261:8520,i])
  incid.total.base.asmm[i]<-sum(s1$epi$incid.asmm[8261:8520,i])
  incid.total.base.msm[i]<-incid.total.base.all[i] - incid.total.base.asmm[i]
}

incid.total.base.all<-mean(incid.total.base.all)
incid.total.base.msm<-mean(incid.total.base.msm)
incid.total.base.asmm<-mean(incid.total.base.asmm)


data<-c("s1","s2","s3","s4","s5") 

coverage<-rep(NA,length(data))

#set up palceholders

##ALL
incid.total.all<-rep(NA,length(data))
incid.rate.all<-rep(NA,length(data))
incid.total.all.up95<-rep(NA,length(data))
incid.total.all.low95<-rep(NA,length(data))


prep.pt.all<-rep(NA,length(data))
prep.pt.all.up95<-rep(NA,length(data))
prep.pt.all.low95<-rep(NA,length(data))

person.time.all.deb<-rep(NA,length(data))
prev.pop.all<-rep(NA,length(data))
prev.pop.all.up95<-rep(NA,length(data))
prev.pop.all.low95<-rep(NA,length(data))

PIA.all<-rep(NA,length(data))
PIA.all.low95<-rep(NA,length(data))
PIA.all.up95<-rep(NA,length(data))

NIA.all<-rep(NA,length(data))
NIA.all.low95<-rep(NA,length(data))
NIA.all.up95<-rep(NA,length(data))

NNT.all<-rep(NA,length(data))
NNT.all.low95<-rep(NA,length(data))
NNT.all.up95<-rep(NA,length(data))

##MSM
incid.total.msm<-rep(NA,length(data))
incid.rate.msm<-rep(NA,length(data))
incid.total.msm.up95<-rep(NA,length(data))
incid.total.msm.low95<-rep(NA,length(data))


prep.pt.msm<-rep(NA,length(data))
prep.pt.msm.up95<-rep(NA,length(data))
prep.pt.msm.low95<-rep(NA,length(data))

person.time.msm.deb<-rep(NA,length(data))
prev.pop.msm<-rep(NA,length(data))
prev.pop.msm.up95<-rep(NA,length(data))
prev.pop.msm.low95<-rep(NA,length(data))

PIA.msm<-rep(NA,length(data))
PIA.msm.low95<-rep(NA,length(data))
PIA.msm.up95<-rep(NA,length(data))

NIA.msm<-rep(NA,length(data))
NIA.msm.low95<-rep(NA,length(data))
NIA.msm.up95<-rep(NA,length(data))

NNT.msm<-rep(NA,length(data))
NNT.msm.low95<-rep(NA,length(data))
NNT.msm.up95<-rep(NA,length(data))


##ASMM

incid.total.asmm<-rep(NA,length(data))
incid.rate.asmm<-rep(NA,length(data))
incid.total.asmm.up95<-rep(NA,length(data))
incid.total.asmm.low95<-rep(NA,length(data))


prep.pt.asmm<-rep(NA,length(data))
prep.pt.asmm.up95<-rep(NA,length(data))
prep.pt.asmm.low95<-rep(NA,length(data))

person.time.asmm.deb<-rep(NA,length(data))
prev.pop.asmm<-rep(NA,length(data))
prev.pop.asmm.up95<-rep(NA,length(data))
prev.pop.asmm.low95<-rep(NA,length(data))

PIA.asmm<-rep(NA,length(data))
PIA.asmm.low95<-rep(NA,length(data))
PIA.asmm.up95<-rep(NA,length(data))

NIA.asmm<-rep(NA,length(data))
NIA.asmm.low95<-rep(NA,length(data))
NIA.asmm.up95<-rep(NA,length(data))

NNT.asmm<-rep(NA,length(data))
NNT.asmm.low95<-rep(NA,length(data))
NNT.asmm.up95<-rep(NA,length(data))

##Cycyle through scenarios


for (i in 1:length(data)){

  coverage<-rep(NA,100)

    #All population
  incid.total.all.temp<-rep(NA,100)
  incid.total.all.up95.temp<-rep(NA,100)
  incid.total.all.low95.temp<-rep(NA,100)
  prep.pt.all.temp<-rep(NA,100)
  prep.pt.all.up95.temp<-rep(NA,100)
  prep.pt.all.low95.temp<-rep(NA,100)
  person.time.all.deb.temp<-rep(NA,100)

  prev.pop.all.temp<-rep(NA,100)

  PIA.all.temp<-rep(NA,100)
  NIA.all.temp<-rep(NA,100)
  NNT.all.temp<-rep(NA,100)

  #MSM
  incid.total.msm.temp<-rep(NA,100)
  incid.total.msm.up95.temp<-rep(NA,100)
  incid.total.msm.low95.temp<-rep(NA,100)
  prep.pt.msm.temp<-rep(NA,100)
  prep.pt.msm.up95.temp<-rep(NA,100)
  prep.pt.msm.low95.temp<-rep(NA,100)
  person.time.msm.deb.temp<-rep(NA,100)
  
  prev.pop.msm.temp<-rep(NA,100)
  
  PIA.msm.temp<-rep(NA,100)
  NIA.msm.temp<-rep(NA,100)
  NNT.msm.temp<-rep(NA,100)
  
  #ASMM
  incid.total.asmm.temp<-rep(NA,100)
  incid.total.asmm.up95.temp<-rep(NA,100)
  incid.total.asmm.low95.temp<-rep(NA,100)
  prep.pt.asmm.temp<-rep(NA,100)
  prep.pt.asmm.up95.temp<-rep(NA,100)
  prep.pt.asmm.low95.temp<-rep(NA,100)
  person.time.asmm.deb.temp<-rep(NA,100)
  
  prev.pop.asmm.temp<-rep(NA,100)
  
  PIA.asmm.temp<-rep(NA,100)
  NIA.asmm.temp<-rep(NA,100)
  NNT.asmm.temp<-rep(NA,100)

  
## ALL Population  
    for (j in 1:100){  
      incid.total.all.temp[j]<-sum(get(data[i])$epi$incid[8261:8520,j])
      prep.pt.all.temp[j]<-sum(get(data[i])$epi$prepCurr[8261:8520,j])
      person.time.all.deb.temp[j]<-sum(get(data[i])$epi$debuted[8261:8520,j])
      prev.pop.all.temp[j]<-get(data[i])$epi$i.prev[8520,j]

      
      PIA.all.temp[j]<-(incid.total.base.all-incid.total.all.temp[j])/incid.total.base.all
      NIA.all.temp[j]<-((incid.total.base.all-incid.total.all.temp[j])/person.time.all.deb.temp[j])*52*100000
      NNT.all.temp[j]<-(prep.pt.all.temp[j]/52)/(incid.total.base.all-incid.total.all.temp[j])
    }
  
  ## MSM Population  
  for (j in 1:100){  
    incid.total.msm.temp[j]<-sum(get(data[i])$epi$incid[8261:8520,j]) - sum(get(data[i])$epi$incid.asmm[8261:8520,j])
    prep.pt.msm.temp[j]<-sum(get(data[i])$epi$prepCurr.msm[8261:8520,j])
    person.time.msm.deb.temp[j]<-sum(get(data[i])$epi$num.msm[8261:8520,j])
    prev.pop.msm.temp[j]<-get(data[i])$epi$i.prev.msm[8520,j]
    
    
    PIA.msm.temp[j]<-(incid.total.base.msm-incid.total.msm.temp[j])/incid.total.base.msm
    NIA.msm.temp[j]<-((incid.total.base.msm-incid.total.msm.temp[j])/person.time.msm.deb.temp[j])*52*100000
    NNT.msm.temp[j]<-(prep.pt.msm.temp[j]/52)/(incid.total.base.msm-incid.total.msm.temp[j])
  }
  
  ## ASMM Population  
  for (j in 1:100){  
    incid.total.asmm.temp[j]<-sum(get(data[i])$epi$incid[8261:8520,j])
    prep.pt.asmm.temp[j]<-sum(get(data[i])$epi$prepCurr.asmm[8261:8520,j])
    person.time.asmm.deb.temp[j]<-sum(get(data[i])$epi$debuted.asmm[8261:8520,j])
    prev.pop.asmm.temp[j]<-get(data[i])$epi$i.prev.asmm[8520,j]
    
    
    PIA.asmm.temp[j]<-(incid.total.base.asmm-incid.total.asmm.temp[j])/incid.total.base.asmm
    NIA.asmm.temp[j]<-((incid.total.base.asmm-incid.total.asmm.temp[j])/person.time.asmm.deb.temp[j])*52*100000
    NNT.asmm.temp[j]<-(prep.pt.asmm.temp[j]/52)/(incid.total.base.asmm-incid.total.asmm.temp[j])

  }
  
  
  
  coverage<-get(data[i])$param$prep.coverage

  ##ALL population
  incid.total.all[i]<-mean(incid.total.all.temp)
  incid.rate.all[i]<-mean(incid.total.all.temp)/260
  incid.total.all.temp.ordered<-sort(incid.total.all.temp)
  incid.total.all.up95[i]<-incid.total.all.temp.ordered[length(incid.total.all.temp.ordered)*.97]
  incid.total.all.low95[i]<-incid.total.all.temp.ordered[length(incid.total.all.temp.ordered)*.03]
  
  prep.pt.all[i]<-mean(prep.pt.all.temp)
  prep.pt.all.temp.ordered<-sort(prep.pt.all.temp)
  prep.pt.all.up95[i]<-prep.pt.all.temp.ordered[length(prep.pt.all.temp.ordered)*.97]
  prep.pt.all.low95[i]<-prep.pt.all.temp.ordered[length(prep.pt.all.temp.ordered)*.03]
  
  prev.pop.all[i]<-mean(prev.pop.all.temp)
  prev.pop.all.temp.ordered<-sort(prev.pop.all.temp)
  prev.pop.all.up95[i]<-prev.pop.all.temp.ordered[length(prev.pop.all.temp.ordered)*.97]
  prev.pop.all.low95[i]<-prev.pop.all.temp.ordered[length(prev.pop.all.temp.ordered)*.03]
 
  person.time.all.deb[i]<-mean(person.time.all.deb.temp)

  if(i==1){
    PIA.all[i] <- PIA.all.low95[i] <- PIA.all.up95[i] <- 0
    NIA.all[i] <- NIA.all.low95[i] <- NIA.all.up95[i] <- 0
    NNT.all[i] <- NNT.all.low95[i] <- NNT.all.up95[i] <- 0
  }
  
  if(i>1){
  PIA.all[i]<-mean(PIA.all.temp)
  PIA.all.temp<-sort(PIA.all.temp)
  PIA.all.low95[i]<-PIA.all.temp[length(PIA.all.temp)*.03]
  PIA.all.up95[i]<-PIA.all.temp[length(PIA.all.temp)*.97]
  
  NIA.all[i]<-mean(NIA.all.temp)
  NIA.all.temp<-sort(NIA.all.temp)
  NIA.all.low95[i]<-NIA.all.temp[length(NIA.all.temp)*.03]
  NIA.all.up95[i]<-NIA.all.temp[length(NIA.all.temp)*.97]
  
  NNT.all[i]<-mean(NNT.all.temp)
  NNT.all.temp<-sort(NNT.all.temp)
  NNT.all.low95[i]<-NNT.all.temp[length(NNT.all.temp)*.03]
  NNT.all.up95[i]<-NNT.all.temp[length(NNT.all.temp)*.97]
  }

  ##MSM population
#  incid.total.msm[i]<-mean(incid.total.msm.temp)
#  incid.rate.msm[i]<-mean(incid.total.msm.temp)/260
#  incid.total.msm.temp.ordered<-sort(incid.total.msm.temp)
#  incid.total.msm.up95[i]<-incid.total.msm.temp.ordered[length(incid.total.msm.temp.ordered)*.97]
#  incid.total.msm.low95[i]<-incid.total.msm.temp.ordered[length(incid.total.msm.temp.ordered)*.03]
  
#  prep.pt.msm[i]<-mean(prep.pt.msm.temp)
#  prep.pt.msm.temp.ordered<-sort(prep.pt.msm.temp)
#  prep.pt.msm.up95[i]<-prep.pt.msm.temp.ordered[length(prep.pt.msm.temp.ordered)*.97]
#  prep.pt.msm.low95[i]<-prep.pt.msm.temp.ordered[length(prep.pt.msm.temp.ordered)*.03]
  
#  prev.pop.msm[i]<-mean(prev.pop.msm.temp)
#  prev.pop.msm.temp.ordered<-sort(prev.pop.msm.temp)
#  prev.pop.msm.up95[i]<-prev.pop.msm.temp.ordered[length(prev.pop.msm.temp.ordered)*.97]
#  prev.pop.msm.low95[i]<-prev.pop.msm.temp.ordered[length(prev.pop.msm.temp.ordered)*.03]
  
#  person.time.msm.deb[i]<-mean(person.time.msm.deb.temp)
  
#  if(i==1){
#    PIA.msm[i] <- PIA.msm.low95[i] <- PIA.msm.up95[i] <- 0
#    NIA.msm[i] <- NIA.msm.low95[i] <- NIA.msm.up95[i] <- 0
#    NNT.msm[i] <- NNT.msm.low95[i] <- NNT.msm.up95[i] <- 0
#  }
  
#  if(i>1){
#  PIA.msm[i]<-mean(PIA.msm.temp)
#  PIA.msm.temp<-sort(PIA.msm.temp)
#  PIA.msm.low95[i]<-PIA.msm.temp[length(PIA.msm.temp)*.03]
#  PIA.msm.up95[i]<-PIA.msm.temp[length(PIA.msm.temp)*.97]
  
#  NIA.msm[i]<-mean(NIA.msm.temp)
#  NIA.msm.temp<-sort(NIA.msm.temp)
#  NIA.msm.low95[i]<-NIA.msm.temp[length(NIA.msm.temp)*.03]
#  NIA.msm.up95[i]<-NIA.msm.temp[length(NIA.msm.temp)*.97]
  
#  NNT.msm[i]<-mean(NNT.msm.temp)
#  NNT.msm.temp<-sort(NNT.msm.temp)
#  NNT.msm.low95[i]<-NNT.msm.temp[length(NNT.msm.temp)*.03]
#  NNT.msm.up95[i]<-NNT.msm.temp[length(NNT.msm.temp)*.97]
#  }
  
  
  ##ASMM population
#  incid.total.asmm[i]<-mean(incid.total.asmm.temp)
#  incid.rate.asmm[i]<-mean(incid.total.asmm.temp)/260
#  incid.total.asmm.temp.ordered<-sort(incid.total.asmm.temp)
#  incid.total.asmm.up95[i]<-incid.total.asmm.temp.ordered[length(incid.total.asmm.temp.ordered)*.97]
#  incid.total.asmm.low95[i]<-incid.total.asmm.temp.ordered[length(incid.total.asmm.temp.ordered)*.03]
  
#  prep.pt.asmm[i]<-mean(prep.pt.asmm.temp)
#  prep.pt.asmm.temp.ordered<-sort(prep.pt.asmm.temp)
#  prep.pt.asmm.up95[i]<-prep.pt.asmm.temp.ordered[length(prep.pt.asmm.temp.ordered)*.97]
#  prep.pt.asmm.low95[i]<-prep.pt.asmm.temp.ordered[length(prep.pt.asmm.temp.ordered)*.03]
  
#  prev.pop.asmm[i]<-mean(prev.pop.asmm.temp)
#  prev.pop.asmm.temp.ordered<-sort(prev.pop.asmm.temp)
#  prev.pop.asmm.up95[i]<-prev.pop.asmm.temp.ordered[length(prev.pop.asmm.temp.ordered)*.97]
#  prev.pop.asmm.low95[i]<-prev.pop.asmm.temp.ordered[length(prev.pop.asmm.temp.ordered)*.03]
  
#  person.time.asmm.deb[i]<-mean(person.time.asmm.deb.temp)
  
#  if(i == 1){
#    PIA.asmm[i] <- PIA.asmm.low95[i] <- PIA.asmm.up95[i] <- 0
#    NIA.asmm[i] <- NIA.asmm.low95[i] <- NIA.asmm.up95[i] <- 0
#    NNT.asmm[i] <- NNT.asmm.low95[i] <- NNT.asmm.up95[i] <- 0
#  }
  
#  if (i > 1){
#  PIA.asmm[i]<-mean(PIA.asmm.temp)
#  PIA.asmm.temp<-sort(PIA.asmm.temp)
#  PIA.asmm.low95[i]<-PIA.asmm.temp[length(PIA.asmm.temp)*.03]
#  PIA.asmm.up95[i]<-PIA.asmm.temp[length(PIA.asmm.temp)*.97]
  
#  NIA.asmm[i]<-mean(NIA.asmm.temp)
#  NIA.asmm.temp<-sort(NIA.asmm.temp)
#  NIA.asmm.low95[i]<-NIA.asmm.temp[length(NIA.asmm.temp)*.03]
#  NIA.asmm.up95[i]<-NIA.asmm.temp[length(NIA.asmm.temp)*.97]
  
#  if(i==2){NNT.asmm[i] <- NNT.asmm.low95[i] <- NNT.asmm.up95[i] <- 0}
#  if(i > 2){
#  NNT.asmm[i]<-mean(NNT.asmm.temp)
#  NNT.asmm.temp<-sort(NNT.asmm.temp)
#  NNT.asmm.low95[i]<-NNT.asmm.temp[length(NNT.asmm.temp)*.03]
#  NNT.asmm.up95[i]<-NNT.asmm.temp[length(NNT.asmm.temp)*.97]
#  }
#  }
  
}

#campcl_NIA_PIA_NNT_table<-cbind(PIA.all, PIA.all.low95, PIA.all.up95, NIA.all, NIA.all.low95, NIA.all.up95, 
#                       NNT.all, NNT.all.low95, NNT.all.up95,
#                       PIA.msm, PIA.msm.low95, PIA.msm.up95, NIA.msm, NIA.msm.low95, NIA.msm.up95, 
#                       NNT.msm, NNT.msm.low95, NNT.msm.up95,
#                       PIA.asmm, PIA.asmm.low95, PIA.asmm.up95, NIA.asmm, NIA.asmm.low95, NIA.asmm.up95, 
#                       NNT.asmm, NNT.asmm.low95, NNT.asmm.up95,
#                       data)
#campcl_NIA_PIA_NNT_table


campcl_NIA_PIA_NNT_table<-cbind(PIA.all, PIA.all.low95, PIA.all.up95, NIA.all, NIA.all.low95, NIA.all.up95, 
                                NNT.all, NNT.all.low95, NNT.all.up95,
                                data)
campcl_NIA_PIA_NNT_table

library(rJava)
library(xlsx) #load the package
write.xlsx(x = campcl_NIA_PIA_NNT_table, file = "/homes/dth2/Campcl/scenarios/out/campcl_NIA_PIA_NNT_table.xlsx",
           sheetName = "Epidemic results", row.names = FALSE)

