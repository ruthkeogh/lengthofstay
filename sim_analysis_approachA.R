#############################################
#Illness-death model: estimation of conditional distribution of length of stay
#Applies approach A to data generated in sim_data_scenario1/2/3
#############################################

#----------------------------------
#storage for simulation results
#----------------------------------

rcelos.1.13=rep(NA,nsim)
rcelos.1.123=rep(NA,nsim)
rcelos.2.123=rep(NA,nsim)
rcelos.2.123.star=rep(NA,nsim)

p.1.13=matrix(NA,nrow=nsim,ncol=tau)
p.1.123=matrix(NA,nrow=nsim,ncol=tau)
p.2.123=matrix(NA,nrow=nsim,ncol=tau)
p.2.123.star=matrix(NA,nrow=nsim,ncol=tau)

#----------------------------------
#start simulation loop
#----------------------------------

set.seed(10)

for(k in 1:nsim){
  print(k)

  #----------------------------------
  #simulate data
  #----------------------------------
  
  if(scenario==1){
    source("sim_data_scenario1.R")
  }
  if(scenario==2){
    source("sim_data_scenario2.R")
  }
  if(scenario==3){
    source("sim_data_scenario3.R")
  }
  
  #use data format required for approach A
  data=data.A
  
  #----------------------------------
  #create lists of times at which transitions occur
  #----------------------------------
  
  #times at which any transition occurs out of state 1 (excluding censoring times)
  event.times=sort(unique(c(data$time_2[data$stat_2==1],data$time_3[data$stat_3==1])))
  n.event.times=length(event.times)
  
  #times at which any transition occurs out of state 1: restricted to times<=tau
  event.times.tau=event.times[event.times<=tau]
  n.event.times.tau=length(event.times.tau)
  
  #gaps between event.times.tau
  event.times.diff=c(event.times.tau[1],diff(event.times.tau))
 
  #times at which any transition occurs out of state 2 (excluding censoring times), using clock-reset approach
  event.times.cr=sort(unique(data$time_3[data$stat_2==1 & data$stat_3==1]-data$time_2[data$stat_2==1 & data$stat_3==1]))
  n.event.times.cr=length(event.times.cr)
  
  #times at which any transition occurs out of state 2, using clock-reset approach: restricted to times<=tau
  event.times.cr.tau=event.times.cr[event.times.cr<=tau]
  n.event.times.cr.tau=length(event.times.cr.tau)
  
  #gaps between event.times.cr.tau
  event.times.cr.diff=c(event.times.cr.tau[1],diff(event.times.cr.tau))

  #----------------------------------
  #transition hazards
  #----------------------------------
  
  #---
  #numbers of transitions at each time
  
  #number of transitions 1->2 at each time
  n.12=sapply(1:n.event.times,FUN=function(x){sum(data$time_2[data$stat_2==1]==event.times[x])})
  
  #number of transitions 1->3 at each time
  n.13=sapply(1:n.event.times,FUN=function(x){sum(data$time_3[data$stat_3==1 & data$stat_2==0]==event.times[x])})
  
  #number of transitions 2->3 at each time, CLOCK FORWARD
  n.23=sapply(1:n.event.times,FUN=function(x){sum(data$time_3[data$stat_3==1 & data$stat_2==1]==event.times[x])})
  
  #number of transitions 2->3 at each time, CLOCK RESET
  n.23.cr=sapply(1:n.event.times.cr,FUN=function(x){sum((data$time_3[data$stat_3==1 & data$stat_2==1]-data$time_2[data$stat_3==1 & data$stat_2==1])==event.times.cr[x])})
  
  #---
  #numbers at risk at each time
  
  #number at risk of transition 1->2 at each time
  n.risk.12=sapply(1:n.event.times,FUN=function(x){sum(data$time_2>=event.times[x])})
  
  #number at risk of 1->3 at each time
  n.risk.13=sapply(1:n.event.times,FUN=function(x){sum(data$time_3[data$stat_2==0]>=event.times[x])+sum(data$time_2[data$stat_2==1]>=event.times[x])})
  
  #number at risk of 2->3 at each time
  n.risk.23=sapply(1:n.event.times,FUN=function(x){sum(data$time_3[data$stat_2==1 & data$time_2<=event.times[x]]>=event.times[x])})
  
  #number at risk of 2->3 at each time, CLOCK RESET
  n.risk.23.cr=sapply(1:n.event.times.cr,FUN=function(x){sum((data$time_3[data$stat_2==1]-data$time_2[data$stat_2==1])>=event.times.cr[x])})
  
  #---
  #transition hazards
  
  haz.12=n.12/n.risk.12
  haz.13=n.13/n.risk.13
  haz.23=n.23/n.risk.23
  haz.23.cr=n.23.cr/n.risk.23.cr
  
  #replace NaN and Inf with 0 (these occur at times when there are no people at risk)
  haz.12=ifelse(is.nan(haz.12)|haz.12==Inf,0,haz.12)
  haz.13=ifelse(is.nan(haz.13)|haz.13==Inf,0,haz.13)
  haz.23=ifelse(is.nan(haz.23)|haz.23==Inf,0,haz.23)
  haz.23.cr=ifelse(is.nan(haz.23.cr)|haz.23.cr==Inf,0,haz.23.cr)
  
  #----------------------------------
  #RCELOS 1|13
  #time spent in state 1 condition on pathway 1->3
  #----------------------------------
  
  #prob of not transitioning out of state 1 before time tj
  surv.1=c(1,sapply(1:(n.event.times.tau-1),FUN=function(x){prod(1-haz.12[1:x]-haz.13[1:x])}))

  #Pr(T3>=t,T2>T3)
  p.1.13.num1=sapply(1:n.event.times.tau,FUN=function(x){surv.1[x]*haz.13[x]})

  p.1.13.num2=sapply(1:n.event.times.tau,FUN=function(x){sum(p.1.13.num1[x:n.event.times.tau])})

  #Pr(T2>T3)
  p.1.13.denom=sum(p.1.13.num1)
  if(scenario==1){
    #Pr(T3>=t|T2>T3)
    p.1.13[k,event.times.tau]=p.1.13.num2[1:n.event.times.tau]/p.1.13.denom
    
    #RCELOS
    rcelos.1.13[k]=sum(event.times.diff*p.1.13[k,event.times.tau])
  }else if(scenario==2|scenario==3){
    #Pr(T3>=t|T2>T3)
    p.1.13=p.1.13.num2[1:n.event.times.tau]/p.1.13.denom
    
    #RCELOS
    rcelos.1.13[k]=sum(event.times.diff*p.1.13)
  }
  
  #----------------------------------
  #RCELOS 1|123
  #time spent in state 1 condition on pathway 1->2->3
  #----------------------------------
  
  #Pr(T2>=t,T2<T3)
  p.1.123.num1=sapply(1:n.event.times.tau,FUN=function(x){surv.1[x]*haz.12[x]})
  
  p.1.123.num2=sapply(1:n.event.times.tau,FUN=function(x){sum(p.1.123.num1[x:n.event.times.tau])})
  
  #Pr(T2<T3)
  p.1.123.denom=sum(p.1.123.num1)
  
  if(scenario==1){
    #Pr(T2>=t|T2<T3)
    p.1.123[k,event.times.tau]=p.1.123.num2/p.1.123.denom
    
    #RCELOS
    rcelos.1.123[k]=sum(event.times.diff*p.1.123[k,event.times.tau])
  }else if(scenario==2|scenario==3){
    #Pr(T2>=t|T2<T3)
    p.1.123=p.1.123.num2/p.1.123.denom
    
    #RCELOS
    rcelos.1.123[k]=sum(event.times.diff*p.1.123)
  }

  #----------------------------------  
  #RCELOS 2|123
  #time spent in state 2 conditional on pathway 1->2->3
  #----------------------------------
  
  #prob of not transitioning out of state 2 before time tj (using clock-reset)
  surv.2=c(1,sapply(1:(n.event.times.cr.tau-1),FUN=function(x){prod(1-haz.23.cr[1:x])}))
  
  #Pr(T3-T2>=t|T3>T2)
  p.2.123.num1=sapply(1:n.event.times.cr.tau,FUN=function(x){surv.2[x]*haz.23.cr[x]})
  
  p.2.123.num2=sapply(1:n.event.times.cr.tau,FUN=function(x){sum(p.2.123.num1[x:n.event.times.cr.tau])})
  
  #Pr(T2>T3)
  p.2.123.denom=sum(p.2.123.num1)
  
  if(scenario==1){
    #Pr(T3>t|T2>T3)
    p.2.123[k,event.times.cr.tau]=p.2.123.num2/p.2.123.denom
    
    #RCELOS
    rcelos.2.123[k]=sum(event.times.cr.diff*p.2.123[k,event.times.cr.tau])
  }else if(scenario==2|scenario==3){
    #Pr(T3>t|T2>T3)
    p.2.123=p.2.123.num2/p.2.123.denom
    
    #RCELOS
    rcelos.2.123[k]=sum(event.times.cr.diff*p.2.123)
  }
  
  #----------------------------------  
  #RCELOS 2|123 *
  #time spent in state 2 conditional on pathway 1->2->3
  #expected time spent in state 2 up to time tau (because we know everyone will eventually go to state 3)
  #----------------------------------
  
  #prob of remaining in state 2 at time tj (using clock-reset)
  surv.2=c(1,sapply(1:(n.event.times.cr.tau-1),FUN=function(x){prod(1-haz.23.cr[1:x])}))
  # dat.2=data[data$stat_2==1,]
  # dat.2$time_new=dat.2$time_3-dat.2$time_2
  # surv.2km.fit=survfit(Surv(time_new,stat_3)~1,dat=dat.2)
  # surv.2km=c(1,summary(surv.2km.fit)$surv[summary(surv.2km.fit)$time<=tau])
  
  if(scenario==1){
    #Pr(T3>=t|T3>T2)
    p.2.123.star[k,event.times.cr.tau]=surv.2
    
    #RCELOS
    rcelos.2.123.star[k]=sum(event.times.cr.diff*p.2.123.star[k,event.times.cr.tau])
  }else if(scenario==2|scenario==3){
    #Pr(T3>=t|T3>T2)
    p.2.123.star=surv.2
    
    #RCELOS
    rcelos.2.123.star[k]=sum(event.times.cr.diff*p.2.123.star)
  }
  
}
  
#----------------------------------  
#save results: for scenario 1
#----------------------------------  

if(scenario==1 & lambda.cens==0 & tau==10000){
  
  p.A=array(dim=c(4,1000,10000))
  p.A[1,,]=p.1.13
  p.A[2,,]=p.1.123
  p.A[3,,]=p.2.123
  p.A[4,,]=p.2.123.star
  save(p.A,file="results/scen1_A_p_nocens_taumax.RData")
  
  rcelos.A=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.A,file="results/scen1_A_rcelos_nocens_taumax.RData")
}

if(scenario==1 & lambda.cens==0 & tau==5){
  
  p.A=array(dim=c(4,1000,10000))
  p.A[1,,]=p.1.13
  p.A[2,,]=p.1.123
  p.A[3,,]=p.2.123
  p.A[4,,]=p.2.123.star
  save(p.A,file="results/scen1_A_p_nocens_tau5.RData")
  
  rcelos.A=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.A,file="results/scen1_A_rcelos_nocens_tau5.RData")
}

if(scenario==1 & lambda.cens==0.2 & tau==10000){

  p.A=array(dim=c(4,1000,10000))
  p.A[1,,]=p.1.13
  p.A[2,,]=p.1.123
  p.A[3,,]=p.2.123
  p.A[4,,]=p.2.123.star
  save(p.A,file="results/scen1_A_p_cens_taumax.RData")
  
  rcelos.A=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.A,file="results/scen1_A_rcelos_cens_taumax.RData")
}

if(scenario==1 & lambda.cens==0.2 & tau==5){

  p.A=array(dim=c(4,1000,10000))
  p.A[1,,]=p.1.13
  p.A[2,,]=p.1.123
  p.A[3,,]=p.2.123
  p.A[4,,]=p.2.123.star
  save(p.A,file="results/scen1_A_p_cens_tau5.RData")
  
  rcelos.A=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.A,file="results/scen1_A_rcelos_cens_tau5.RData")
}

#----------------------------------  
#save results: for scenario 2
#---------------------------------- 

if(scenario==2 & lambda.cens==0 & tau==10000){

  rcelos.A=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.A,file="results/scen2_A_rcelos_nocens_taumax.RData")

}

if(scenario==2 & lambda.cens==0.2 & tau==10000){

  rcelos.A=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.A,file="results/scen2_A_rcelos_cens_taumax.RData")
  
}

if(scenario==2 & lambda.cens==0 & tau==5){

  rcelos.A=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.A,file="results/scen2_A_rcelos_nocens_tau5.RData")
}

if(scenario==2 & lambda.cens==0.2 & tau==5){

  rcelos.A=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.A,file="results/scen2_A_rcelos_cens_tau5.RData")
}

#----------------------------------  
#save results: for scenario 3
#---------------------------------- 

if(scenario==3 & lambda.cens==0 & tau==10000){

  rcelos.A=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.A,file="results/scen3_A_rcelos_nocens_taumax.RData")
  
}

if(scenario==3 & lambda.cens==0.2 & tau==10000){

  rcelos.A=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.A,file="results/scen3_A_rcelos_cens_taumax.RData")
  
}

if(scenario==3 & lambda.cens==0 & tau==5){

  rcelos.A=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.A,file="results/scen3_A_rcelos_nocens_tau5.RData")
}

if(scenario==3 & lambda.cens==0.2 & tau==5){

  rcelos.A=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.A,file="results/scen3_A_rcelos_cens_tau5.RData")
}


