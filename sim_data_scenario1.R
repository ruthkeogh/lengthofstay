#----------------------------------
#----------------------------------
#simulate data with 2 event types PLUS a transient state
#----------------------------------
#----------------------------------

lambda.12=0.05 #hazard for healthy->illness

lambda.13=0.1 #hazard for healthy->death 

lambda.23=0.3 #hazard for illness->death

#---
#censoring time
unif=runif(n,0,1)
t.cens=ceiling(-log(unif)/lambda.cens)+0.1

if(lambda.cens==0){
  t.cens=10000
}

#---
#times: 1->3
unif=runif(n,0,1)
t.13=ceiling(-log(unif)/lambda.13)

#---
#times: 1->2
unif=runif(n,0,1)
t.12=ceiling(-log(unif)/lambda.12)

#---
#times: 2->3
unif=runif(n,0,1)
t.23=ceiling(-log(unif)/lambda.23)

# kappa.13<-2
# kappa.12<-0.5
# kappa.23<-4
# 
# #---
# #times: 1->3 
# unif=runif(n,0,1)
# t.13=ceiling((-log(unif)/lambda.13)^(1/kappa.13))
# 
# #---
# #times: 1->2 
# unif=runif(n,0,1)
# t.12=ceiling((-log(unif)/lambda.12)^(1/kappa.12))
# 
# #---
# #times: 2->3 
# unif=runif(n,0,1)
# t.23=ceiling((-log(unif)/lambda.23)^(1/kappa.23))

#-----
#status for each event - approach A
stat_2=ifelse(t.12<=t.13 & t.12<=t.cens,1,0)
stat_3=ifelse((t.13<t.12 & t.13<=t.cens)|
                    (t.12<=t.13 & t.12<=t.cens & (t.12+t.23)<=t.cens),1,0)

#-----
#observed event/censoring time for each transition - approach A
time_2=ifelse(t.12<=t.13 & t.12<=t.cens,t.12,NA)

time_3=ifelse(t.13<t.12 & t.13<=t.cens,t.13,NA)
time_3=ifelse(t.12<=t.13 & t.12<=t.cens & (t.12+t.23)<=t.cens,t.12+t.23,time_3)

time_2=ifelse(stat_2==1,time_2,pmin(time_3,t.cens,na.rm=T))
time_3=ifelse(stat_3==1,time_3,t.cens)

time_3=ifelse(time_3==time_2 & stat_2==1 & stat_3==0,time_3+1,time_3)#to avoid tstart=tstop in msfit

#-----
#status for each event - approach B
stat_31=ifelse(t.13<t.12 & t.13<=t.cens,1,0)
stat_32=ifelse(t.12<=t.13 & t.12<=t.cens & (t.12+t.23)<=t.cens,1,0)

#-----
#observed event/censoring time for each transition - approach B

time_31=ifelse(stat_31==1,t.13,NA)

time_32=ifelse(stat_32==1,t.12+t.23,NA)

time_31=ifelse(stat_31==1,time_31,pmin(time_2,t.cens,na.rm=T))

time_32=ifelse(stat_32==1 & stat_2==1,time_32,NA)
time_32=ifelse(stat_32==0 & stat_2==1,t.cens,time_32)

time_32=ifelse(time_32==time_2 & stat_2==1 & stat_32==0,time_32+1,time_32)#to avoid tstart=tstop in msfit

#---
#simulated data set - approach A

data.A=data.frame(id=1:n,stat_2,stat_3,time_2,time_3)

#---
#simulated data set - approach B

data.B=data.frame(id=1:n,stat_2,stat_31,stat_32,time_2,time_31,time_32)


