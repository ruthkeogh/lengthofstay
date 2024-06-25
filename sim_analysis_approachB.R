#############################################
#Illness-death model: estimation of conditional distribution of length of stay
#Applies approach B to data generated in sim_data_scenario1/2/3
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

# max.event.times=rep(NA,nsim)
# max.event.times.cr=rep(NA,nsim)
# ncens=rep(NA,nsim)

#----------------------------------
#start simulation loop
#----------------------------------

set.seed(10)

pb <- progress_bar$new(total = nsim) # set up progress bar

for(k in 1:nsim){
  pb$tick() # add progress to bar
  # print(k)

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
  data=data.B
  
  #ncens[k]=sum(data$stat_2==0 & data$stat_3==0)
  
  #----------------------------------
  #create lists of times at which transitions occur
  #----------------------------------

  #times at which any transition occurs out of state 1 (excluding censoring times)
  event.times=sort(unique(c(data$time_31[data$stat_31==1],data$time_2[data$stat_2==1])))
  n.event.times=length(event.times)
  
  #times at which any transition occurs out of state 1: restricted to times<=tau
  event.times.tau=event.times[event.times<=tau]
  n.event.times.tau=length(event.times.tau)
  
  #gaps between event.times.tau
  event.times.diff=c(event.times.tau[1],diff(event.times.tau))
  #event.times.diff=c(event.times.tau[1],diff(event.times.tau),tau-event.times.tau[n.event.times.tau])
  
  #times at which any transition occurs out of state 2 (excluding censoring times), using clock-reset approach
  event.times.cr=sort(unique((data$time_32-data$time_2)[data$stat_32==1]))
  n.event.times.cr=length(event.times.cr)
  
  #times at which any transition occurs out of state 2, using clock-reset approach: restricted to times<=tau
  event.times.cr.tau=event.times.cr[event.times.cr<=tau]
  n.event.times.cr.tau=length(event.times.cr.tau)
  
  #gaps between event.times.cr.tau
  event.times.cr.diff=c(event.times.cr.tau[1],diff(event.times.cr.tau))
  #event.times.cr.diff=c(event.times.cr.tau[1],diff(event.times.cr.tau),tau-event.times.tau[n.event.times.cr.tau])
  
  # max.event.times[k]=max(event.times)
  # max.event.times.cr[k]=max(event.times.cr)

  #----------------------------------
  #multistate setup
  #----------------------------------
  
  trans.mat=transMat(x=list(c(2,3),c(4),c(),c()),names = c("healthy","illness","death_1","death_2"))
  
  data.msprep<- msprep(time = c(NA,"time_2","time_31","time_32"), 
                       status = c(NA,"stat_2","stat_31","stat_32"), 
                       data = data, trans = trans.mat )
  
  ms.mod<-coxph(Surv(Tstart,Tstop,status)~strata(trans),data=data.msprep,ties="breslow") #clock forward
  
  ms.fit<-msfit(ms.mod,trans=trans.mat)
  
  prob.fit=probtrans(ms.fit,predt=0)

  #----------------------------------
  #RCELOS 1|13
  #time spent in state 1 condition on pathway 1->3
  #----------------------------------

  #maximum observed time <= tau
  temp.tau=ifelse(prob.fit[[1]]$time>tau,0,prob.fit[[1]]$time)
  temp.tau=prob.fit[[1]]$time[which.min(tau-temp.tau)]
  
  #---
  #prob of being in state 3_1 (called state 3 in prob.fit) at time tau
  
  p.tau.31= prob.fit[[1]]$pstate3[prob.fit[[1]]$time==temp.tau]
  
  #prob of being in state 1 just before time t_j, for each t_j in event.times.tau
  
  event.times.tau.lag=c(0,event.times.tau[-length(event.times.tau)])
  p.1=rep(0,length(event.times.tau))
  temp=prob.fit[[1]]
  for(j in 1:length(event.times.tau.lag)){
    try(p.1[j]<-temp$pstate1[temp$time==event.times.tau.lag[j]],silent=T)
  }
  #sapply(1:length(event.times.tau.lag),FUN=function(x){temp$pstate1[temp$time==event.times.tau.lag[x]]}) #alternative code
  
  #prob of being in state 3_1 (called state 3 in prob.fit) at time tau given in state 1 just before time t_j
  p.31=rep(0,length(event.times.tau))
  for(j in 1:length(event.times.tau.lag)){
    temp<-probtrans(ms.fit,predt=event.times.tau.lag[j])[[1]]
    try(p.31[j]<-temp$pstate3[temp$time==temp.tau],silent=T)
  }

  # if(scenario==1){
  #   #Pr(T3>=t|T2>T3)
  #   p.1.13[k,event.times.tau]=p.31*p.1/p.tau.31
  #   
  #   #RCELOS
  #   rcelos.1.13[k]=sum(event.times.diff*p.1.13[k,event.times.tau])
  # }else if(scenario==2|scenario==3){
  #   #Pr(T3>=t|T2>T3)
  #   p.1.13=p.1.13.num2[1:n.event.times.tau]/p.1.13.denom
  #   
  #   #RCELOS
  #   rcelos.1.13[k]=sum(event.times.diff*p.1.13)
  # }
  
  p.1.13=p.31*p.1/p.tau.31
  
  #RCELOS
  rcelos.1.13[k]=sum(event.times.diff*p.1.13)
  
  #----------------------------------
  #RCELOS 1|123
  #time spent in state 1 condition on pathway 1->2->3
  #----------------------------------

  #---
  #prob of being in state 2 or state 3_2 (called state 4 in prob.fit) at time tau
  
  p.tau.2or32= prob.fit[[1]]$pstate2[prob.fit[[1]]$time==temp.tau]+prob.fit[[1]]$pstate4[prob.fit[[1]]$time==temp.tau]
  
  #prob of being in state 2 or state 3_2 (called state 4 in prob.fit) at time tau given in state 1 just before time t_j
  p.2or32=rep(0,length(event.times.tau))
  for(j in 1:length(event.times.tau.lag)){
    temp<-probtrans(ms.fit,predt=event.times.tau.lag[j])[[1]]
    p.2or32[j]<-temp$pstate2[temp$time==temp.tau]+temp$pstate4[temp$time==temp.tau]
  }
  
  # if(scenario==1){
  #   #Pr(T2>=t|T2<T3)
  #   p.1.123[k,event.times.tau]=p.2or32*p.1/p.tau.2or32
  #   
  #   #RCELOS
  #   rcelos.1.123[k]=sum(event.times.diff*p.1.123[k,event.times.tau])
  # }else if(scenario==2|scenario==3){
  #   #Pr(T2>=t|T2<T3)
  #   p.1.123=p.2or32*p.1/p.tau.2or32
  #   
  #   #RCELOS
  #   rcelos.1.123[k]=sum(event.times.diff*p.1.123)
  # }
  
  #Pr(T2>=t|T2<T3)
  p.1.123=p.2or32*p.1/p.tau.2or32
  
  #RCELOS
  rcelos.1.123[k]=sum(event.times.diff*p.1.123)

  #----------------------------------
  #multistate setup - use clock-reset now for time after going to state 2
  #----------------------------------
  
  ms.mod<-coxph(Surv(Tstop-Tstart,status)~strata(trans),data=data.msprep,ties="breslow") #clock reset
  
  ms.fit<-msfit(ms.mod,trans=trans.mat)
  
  prob.fit=probtrans(ms.fit,predt=0)

  #----------------------------------  
  #RCELOS 2|123
  #time spent in state 2 conditional on pathway 1->2->3
  #----------------------------------
  
  #maximum observed time <= tau
  temp.tau=ifelse(prob.fit[[2]]$time>tau,0,prob.fit[[2]]$time)
  temp.tau=prob.fit[[2]]$time[which.min(tau-temp.tau)]

  #prob of being in state 3_2 (called state 4 in prob.fit) at time tau after entering state 2
  p.tau.32.2= prob.fit[[2]]$pstate4[prob.fit[[2]]$time==temp.tau]

  #prob of being in state 2 just before time t_j
  event.times.cr.tau.lag=c(0,event.times.cr.tau[-length(event.times.cr.tau)])
  p.2=rep(0,length(event.times.cr.tau))
  temp=prob.fit[[2]]
  for(j in 1:length(event.times.cr.tau.lag)){
    try(p.2[j]<-temp$pstate2[temp$time==event.times.cr.tau.lag[j]],silent=T)
  }
  
  #prob of being in state 3_2 (called state 4 in prob.fit) at time tau given in state 2 just before time t_j
  p.32.2=rep(0,length(event.times.cr.tau))
  for(j in 1:length(event.times.cr.tau.lag)){
    temp<-probtrans(ms.fit,predt=event.times.cr.tau.lag[j])[[2]]
    try(p.32.2[j]<-temp$pstate4[temp$time==temp.tau],silent=T)
  }

  # if(scenario==1){
  #   #Pr(T3>t|T2>T3)
  #   p.2.123[k,event.times.cr.tau]=p.32.2*p.2/p.tau.32.2
  #   
  #   #RCELOS
  #   rcelos.2.123[k]=sum(event.times.cr.diff*p.2.123[k,event.times.cr.tau])
  # }else if(scenario==2|scenario==3){
  #   #Pr(T3>t|T2>T3)
  #   p.2.123=p.2.123.num2/p.2.123.denom
  #   
  #   #RCELOS
  #   rcelos.2.123[k]=sum(event.times.cr.diff*p.2.123)
  # }
  
  #Pr(T3>t|T2>T3)
  p.2.123=p.32.2*p.2/p.tau.32.2
  
  #RCELOS
  rcelos.2.123[k]=sum(event.times.cr.diff*p.2.123)
  
}

#----------------------------------  
#save results: for scenario 1
#----------------------------------  

if(scenario==1 & lambda.cens==0 & tau==10000){
  
  p.B=array(dim=c(4,1000,10000))
  p.B[1,,]=p.1.13
  p.B[2,,]=p.1.123
  p.B[3,,]=p.2.123
  p.B[4,,]=p.2.123.star
  save(p.B,file="results/scen1_B_p_nocens_taumax.RData")
  
  rcelos.B=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.B,file="results/scen1_B_rcelos_nocens_taumax.RData")
}

if(scenario==1 & lambda.cens==0 & tau==5){
  
  p.B=array(dim=c(4,1000,10000))
  p.B[1,,]=p.1.13
  p.B[2,,]=p.1.123
  p.B[3,,]=p.2.123
  p.B[4,,]=p.2.123.star
  save(p.B,file="results/scen1_B_p_nocens_tau5.RData")
  
  rcelos.B=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.B,file="results/scen1_B_rcelos_nocens_tau5.RData")
}

if(scenario==1 & lambda.cens==0.2 & tau==10000){
  
  p.B=array(dim=c(4,1000,10000))
  p.B[1,,]=p.1.13
  p.B[2,,]=p.1.123
  p.B[3,,]=p.2.123
  p.B[4,,]=p.2.123.star
  save(p.B,file="results/scen1_B_p_cens_taumax.RData")
  
  rcelos.B=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.B,file="results/scen1_B_rcelos_cens_taumax.RData")
}

if(scenario==1 & lambda.cens==0.2 & tau==5){
  
  p.B=array(dim=c(4,1000,10000))
  p.B[1,,]=p.1.13
  p.B[2,,]=p.1.123
  p.B[3,,]=p.2.123
  p.B[4,,]=p.2.123.star
  save(p.B,file="results/scen1_B_p_cens_tau5.RData")
  
  rcelos.B=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.B,file="results/scen1_B_rcelos_cens_tau5.RData")
}

#----------------------------------  
#save results: for scenario 2
#---------------------------------- 

if(scenario==2 & lambda.cens==0 & tau==10000){

  rcelos.B=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.B,file="results/scen2_B_rcelos_nocens_taumax.RData")
  
}

if(scenario==2 & lambda.cens==0.2 & tau==10000){
 
  rcelos.B=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.B,file="results/scen2_B_rcelos_cens_taumax.RData")
  
}

if(scenario==2 & lambda.cens==0 & tau==5){

  rcelos.B=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.B,file="results/scen2_B_rcelos_nocens_tau5.RData")
}

if(scenario==2 & lambda.cens==0.2 & tau==5){
 
  rcelos.B=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.B,file="results/scen2_B_rcelos_cens_tau5.RData")
}

#----------------------------------  
#save results: for scenario 3
#---------------------------------- 

if(scenario==3 & lambda.cens==0 & tau==10000){

  rcelos.B=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.B,file="results/scen3_B_rcelos_nocens_taumax.RData")
  
}

if(scenario==3 & lambda.cens==0.2 & tau==10000){

  rcelos.B=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.B,file="results/scen3_B_rcelos_cens_taumax.RData")
  
}

if(scenario==3 & lambda.cens==0 & tau==5){

  rcelos.B=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.B,file="results/scen3_B_rcelos_nocens_tau5.RData")
}

if(scenario==3 & lambda.cens==0.2 & tau==5){

  rcelos.B=cbind(rcelos.1.13,rcelos.1.123,rcelos.2.123,rcelos.2.123.star)
  save(rcelos.B,file="results/scen3_B_rcelos_cens_tau5.RData")
}


