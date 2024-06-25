#############################################
#Illness-death model: estimation of conditional distribution of length of stay
#Applies approach A to data generated in sim_data_scenario1/2/3
#############################################

#----------------------------------
#storage for simulation results
#----------------------------------

rcelos.naive.1.13=rep(NA,nsim)
rcelos.naive.1.123=rep(NA,nsim)
rcelos.naive.2.123=rep(NA,nsim)
rcelos.naive.2.123.star=rep(NA,nsim)

p.naive.1.13=matrix(NA,nrow=nsim,ncol=tau)
p.naive.1.123=matrix(NA,nrow=nsim,ncol=tau)
p.naive.2.123=matrix(NA,nrow=nsim,ncol=tau)
p.naive.2.123.star=matrix(NA,nrow=nsim,ncol=tau)

# max.event.times=rep(NA,nsim)
# max.event.times.cr=rep(NA,nsim)
# ncens=rep(NA,nsim)

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
  
  #ncens[k]=sum(data$stat_2==0 & data$stat_3==0)
  
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
  #naive approach: empirical estimates ignoring censoring
  #----------------------------------
  
  #RCELOS estimates 
  
  rcelos.naive.1.13[k]=mean(data$time_3[data$stat_2==0 & data$stat_3==1 & data$time_3<=tau])
  rcelos.naive.1.123[k]=mean(data$time_2[data$stat_2==1 & data$stat_3==1 & data$time_2<=tau])
  rcelos.naive.2.123[k]=mean((data$time_3-data$time_2)[data$stat_2==1 & data$stat_3==1 & (data$time_3-data$time_2)<=tau])
  rcelos.naive.2.123.star[k]=mean(pmin(tau,(data$time_3-data$time_2))[data$stat_2==1 & data$stat_3==1])
  
  #estimates of conditional probabilities (scenario (i) only)
  
  if(scenario==1){
    p.naive.1.13[k,event.times.tau]=sapply(event.times.tau,FUN=function(x){sum(data$time_3[data$stat_2==0 & data$stat_3==1 & data$time_3<=tau]>=x)/sum(data$stat_2==0 & data$stat_3==1 & data$time_3<=tau)})
    
    p.naive.1.123[k,event.times.tau]=sapply(event.times.tau,FUN=function(x){sum(data$time_2[data$stat_2==1 & data$stat_3==1 & data$time_3<=tau]>=x)/sum(data$stat_2==1 & data$stat_3==1 & data$time_3<=tau)})
    
    p.naive.1.123[k,event.times.tau]=sapply(event.times.tau,FUN=function(x){sum(data$time_2[data$stat_2==1 & data$time_2<=tau]>=x)/sum(data$stat_2==1 & data$time_2<=tau)})
    
    p.naive.2.123[k,event.times.cr.tau]=sapply(event.times.cr.tau,FUN=function(x){sum((data$time_3-data$time_2)[data$stat_2==1 & data$stat_3==1 & (data$time_3-data$time_2)<=tau]>=x)/
        sum(data$stat_2==1 & data$stat_3==1 & (data$time_3-data$time_2)<=tau)})
  }
  
}

#----------------------------------  
#save results: for scenario 1
#----------------------------------  

if(scenario==1 & lambda.cens==0 & tau==10000){
  
  p.naive=array(dim=c(4,1000,10000))
  p.naive[1,,]=p.naive.1.13
  p.naive[2,,]=p.naive.1.123
  p.naive[3,,]=p.naive.2.123
  p.naive[4,,]=p.naive.2.123.star
  save(p.naive,file="results/scen1_naive_p_nocens_taumax.RData")
  
  rcelos.naive=cbind(rcelos.naive.1.13,rcelos.naive.1.123,rcelos.naive.2.123,rcelos.naive.2.123.star)
  save(rcelos.naive,file="results/scen1_naive_rcelos_nocens_taumax.RData")
  
}

if(scenario==1 & lambda.cens==0 & tau==5){
  
  p.naive=array(dim=c(4,1000,10000))
  p.naive[1,,]=p.naive.1.13
  p.naive[2,,]=p.naive.1.123
  p.naive[3,,]=p.naive.2.123
  p.naive[4,,]=p.naive.2.123.star
  save(p.naive,file="results/scen1_naive_p_nocens_tau5.RData")
  
  rcelos.naive=cbind(rcelos.naive.1.13,rcelos.naive.1.123,rcelos.naive.2.123,rcelos.naive.2.123.star)
  save(rcelos.naive,file="results/scen1_naive_rcelos_nocens_tau5.RData")

}

if(scenario==1 & lambda.cens==0.2 & tau==10000){
  
  p.naive=array(dim=c(4,1000,10000))
  p.naive[1,,]=p.naive.1.13
  p.naive[2,,]=p.naive.1.123
  p.naive[3,,]=p.naive.2.123
  p.naive[4,,]=p.naive.2.123.star
  save(p.naive,file="results/scen1_naive_p_cens_taumax.RData")
  
  rcelos.naive=cbind(rcelos.naive.1.13,rcelos.naive.1.123,rcelos.naive.2.123,rcelos.naive.2.123.star)
  save(rcelos.naive,file="results/scen1_naive_rcelos_cens_taumax.RData")

}

if(scenario==1 & lambda.cens==0.2 & tau==5){

  p.naive=array(dim=c(4,1000,10000))
  p.naive[1,,]=p.naive.1.13
  p.naive[2,,]=p.naive.1.123
  p.naive[3,,]=p.naive.2.123
  p.naive[4,,]=p.naive.2.123.star
  save(p.naive,file="results/scen1_naive_p_cens_tau5.RData")
  
  rcelos.naive=cbind(rcelos.naive.1.13,rcelos.naive.1.123,rcelos.naive.2.123,rcelos.naive.2.123.star)
  save(rcelos.naive,file="results/scen1_naive_rcelos_cens_tau5.RData")
 
}

#----------------------------------  
#save results: for scenario 2
#---------------------------------- 

if(scenario==2 & lambda.cens==0 & tau==10000){
  
  rcelos.naive=cbind(rcelos.naive.1.13,rcelos.naive.1.123,rcelos.naive.2.123,rcelos.naive.2.123.star)
  save(rcelos.naive,file="results/scen2_naive_rcelos_nocens_taumax.RData")
  
}

if(scenario==2 & lambda.cens==0.2 & tau==10000){
  
  rcelos.naive=cbind(rcelos.naive.1.13,rcelos.naive.1.123,rcelos.naive.2.123,rcelos.naive.2.123.star)
  save(rcelos.naive,file="results/scen2_naive_rcelos_cens_taumax.RData")
  
}

if(scenario==2 & lambda.cens==0 & tau==5){

  rcelos.naive=cbind(rcelos.naive.1.13,rcelos.naive.1.123,rcelos.naive.2.123,rcelos.naive.2.123.star)
  save(rcelos.naive,file="results/scen2_naive_rcelos_nocens_tau5.RData")

}

if(scenario==2 & lambda.cens==0.2 & tau==5){
  
  rcelos.naive=cbind(rcelos.naive.1.13,rcelos.naive.1.123,rcelos.naive.2.123,rcelos.naive.2.123.star)
  save(rcelos.naive,file="results/scen2_naive_rcelos_cens_tau5.RData")

}

#----------------------------------  
#save results: for scenario 3
#---------------------------------- 

if(scenario==3 & lambda.cens==0 & tau==10000){

  rcelos.naive=cbind(rcelos.naive.1.13,rcelos.naive.1.123,rcelos.naive.2.123,rcelos.naive.2.123.star)
  save(rcelos.naive,file="results/scen3_naive_rcelos_nocens_taumax.RData")
  
}

if(scenario==3 & lambda.cens==0.2 & tau==10000){
  
  rcelos.naive=cbind(rcelos.naive.1.13,rcelos.naive.1.123,rcelos.naive.2.123,rcelos.naive.2.123.star)
  save(rcelos.naive,file="results/scen3_naive_rcelos_cens_taumax.RData")
  
}

if(scenario==3 & lambda.cens==0 & tau==5){

  rcelos.naive=cbind(rcelos.naive.1.13,rcelos.naive.1.123,rcelos.naive.2.123,rcelos.naive.2.123.star)
  save(rcelos.naive,file="results/scen3_naive_rcelos_nocens_tau5.RData")

}

if(scenario==3 & lambda.cens==0.2 & tau==5){

  rcelos.naive=cbind(rcelos.naive.1.13,rcelos.naive.1.123,rcelos.naive.2.123,rcelos.naive.2.123.star)
  save(rcelos.naive,file="results/scen3_naive_rcelos_cens_tau5.RData")

}


