#############################################
#Illness-death model: estimation of conditional distribution of length of stay
#True values obtained using a  very large sample size with no censoring
#############################################

set.seed(100)

n=1000000 #very large sample size to get (approximate) true values

lambda.cens= 0 #no censoring

p.true.1.13=rep(NA,tau)
p.true.1.123=rep(NA,tau)
p.true.2.123=rep(NA,tau)
p.true.2.123.star=rep(NA,tau)

#----------------------------------  
#scenario 1
#----------------------------------  

if(scenario==1){
  source("sim_data_scenario1.R")
  
  data=data.B #this could be done using data.A to give identical results
  
  #observed times at which any transition occurs out of state 1 (excluding censoring times)
  t1.obs=sort(unique(c(data$time_31[data$stat_31==1], data$time_2[data$stat_2==1])))
  t1.obs= t1.obs[t1.obs<=tau]
  
  #observed times at which any transition occurs out of state 2 (excluding censoring times), using clock-reset approach
  t2.obs=sort(unique((data$time_32-data$time_2)[data$stat_32==1]))
  t2.obs= t2.obs[t2.obs<=tau]
  
  #estimates of conditional probabilities (scenario (i) only)
  p.true.1.13[t1.obs]=sapply(t1.obs,FUN=function(x){sum(data$time_31[data$stat_31==1 & data$time_31<=tau]>=x)/sum(data$stat_31==1 & data$time_31<=tau)})
  p.true.1.123[t1.obs]=sapply(t1.obs,FUN=function(x){sum(data$time_2[data$stat_2==1 & data$time_2<=tau]>=x)/sum(data$stat_2==1 & data$time_2<=tau)})
  p.true.2.123[t2.obs]=sapply(t2.obs,FUN=function(x){sum((data$time_32-data$time_2)[data$stat_32==1 & (data$time_32-data$time_2)<=tau]>=x)/sum(data$stat_32==1 & (data$time_32-data$time_2)<=tau)})
  p.true.2.123.star[t2.obs]=sapply(t2.obs,FUN=function(x){sum((data$time_32-data$time_2)[data$stat_32==1]>=x)/sum(data$stat_32==1)})
  
  p.true=cbind(p.true.1.13,p.true.1.123,p.true.2.123,p.true.2.123.star)
  
  #RCELOS estimates 
  rcelos.true.1.13=mean(data$time_31[data$stat_31==1 & data$time_31<=tau])
  rcelos.true.1.123=mean(data$time_2[data$stat_2==1 & data$time_2<=tau])
  rcelos.true.2.123=mean((data$time_32-data$time_2)[data$stat_32==1 & (data$time_32-data$time_2)<=tau])
  rcelos.true.2.123.star=mean(pmin(tau,(data$time_32-data$time_2))[data$stat_32==1])
  
  rcelos.true=c(rcelos.true.1.13,rcelos.true.1.123,rcelos.true.2.123,rcelos.true.2.123.star)
  
  if(tau==10000){
    save(p.true,file="results/scen1_true_p_taumax.RData")
    save(rcelos.true,file="results/scen1_true_rcelos_taumax.RData")
  }
  
  if(tau==5){
    save(p.true,file="results/scen1_true_p_tau5.RData")
    save(rcelos.true,file="results/scen1_true_rcelos_tau5.RData")
  }
}

#----------------------------------  
#scenario 2
#----------------------------------  

if(scenario==2){
  source("sim_data_scenario2.R")
  
  data=data.B #this could be done using data.A to give identical results
  
  #RCELOS estimates 
  rcelos.true.1.13=mean(data$time_31[data$stat_31==1 & data$time_31<=tau])
  rcelos.true.1.123=mean(data$time_2[data$stat_2==1 & data$time_2<=tau])
  rcelos.true.2.123=mean((data$time_32-data$time_2)[data$stat_32==1 & (data$time_32-data$time_2)<=tau])
  rcelos.true.2.123.star=mean(pmin(tau,(data$time_32-data$time_2))[data$stat_32==1])
  
  rcelos.true=c(rcelos.true.1.13,rcelos.true.1.123,rcelos.true.2.123,rcelos.true.2.123.star)
  
  if(tau==10000){
    save(rcelos.true,file="results/scen2_true_rcelos_taumax.RData")
  }
  
  if(tau==5){
    save(rcelos.true,file="results/scen2_true_rcelos_tau5.RData")
  }
}

#----------------------------------  
#scenario 3
#----------------------------------  

if(scenario==3){
  source("sim_data_scenario3.R")
  
  data=data.B #this could be done using data.A to give identical results
  
  #RCELOS estimates 
  rcelos.true.1.13=mean(data$time_31[data$stat_31==1 & data$time_31<=tau])
  rcelos.true.1.123=mean(data$time_2[data$stat_2==1 & data$time_2<=tau])
  rcelos.true.2.123=mean((data$time_32-data$time_2)[data$stat_32==1 & (data$time_32-data$time_2)<=tau])
  rcelos.true.2.123.star=mean(pmin(tau,(data$time_32-data$time_2))[data$stat_32==1])
  
  rcelos.true=c(rcelos.true.1.13,rcelos.true.1.123,rcelos.true.2.123,rcelos.true.2.123.star)
  
  if(tau==10000){
    save(rcelos.true,file="results/scen3_true_rcelos_taumax.RData")
  }
  
  if(tau==5){
    save(rcelos.true,file="results/scen3_true_rcelos_tau5.RData")
  }
}
