
#----
#transition matrix
tmat <- transMat(x = list( c(2, 3), c(3), c() ),
                 names = c("Healthy", "Illness", "Death"))
tmat # for mstate
tra <- trans2tra(tmat) # need mstate 0.3.1 or higher for this!
tra # for etm

source("simmstate.R") # contains the code

# ----
# Clock forward
shape <- c(0.75, 0.75, 1.25) # length is number of transitions
rate <- c(0.05, 0.1, 0.3) # length is number of transitions
sigma <- 1 * diag(4) # diag implies independent frailties

msmodel <- list(
  trans = tmat,
  shape = shape, # one for each transition
  rate = rate, # one for each transition
  censrate = c(lambda.cens, lambda.cens, lambda.cens), # one for each state, last one not used because last state is absorbing
  # rates are so small that there is effectively no censoring
  censshape = c(1, 1, 1), # one for each state, last one not used
  clock = "reset"
  # frailty = list(
  #   mean = rep(0, 4),
  #   sigma = sigma
  # )
)

simmsm <- simmstate(n=n, msmodel, startprobs=c(1, 0, 0)) # in etm format
simmstat <- etm2msdata(simmsm, "id", tra) # convert to mstate format

#---
#put data in format of 1 row per individual
#format required for Approach A

simmstat$time_2=simmstat$Tstop

simmstat$stat_2=ifelse(simmstat$from==1 & simmstat$to==2 & simmstat$status==1,1,0)

simmstat$time_3=simmstat$Tstop

simmstat$stat_3=ifelse(simmstat$from==1 & simmstat$to==3 & simmstat$status==1,1,0)
simmstat$stat_3=ifelse(simmstat$from==2 & simmstat$to==3 & simmstat$status==1,1,simmstat$stat_3)

simmstat=simmstat%>%group_by(id)%>%mutate(time_2=min(time_2))
simmstat=simmstat%>%group_by(id)%>%mutate(stat_2=sum(stat_2))
simmstat=simmstat%>%group_by(id)%>%mutate(time_3=max(time_3))
simmstat=simmstat%>%group_by(id)%>%mutate(stat_3=sum(stat_3))

simmstat=simmstat%>%mutate(ones=1)%>%group_by(id)%>%mutate(rownum=cumsum(ones))

data.A=simmstat[simmstat$rownum==1,]

data.A$time_2=ifelse(data.A$stat_2==0,data.A$time_3,data.A$time_2)
data.A=data.A[,c("id","stat_2","stat_3","time_2","time_3")]

#---
#put data in format of 1 row per individual
#format required for Approach B

data.B=data.A

data.B$time_31=ifelse(data.B$stat_2==0 & data.B$stat_3==1,data.B$time_3,NA)
data.B$time_32=ifelse(data.B$stat_2==1 & data.B$stat_3==1,data.B$time_3,NA)

data.B$stat_31=ifelse(data.B$stat_2==0 & data.B$stat_3==1,1,0)
data.B$stat_32=ifelse(data.B$stat_2==1 & data.B$stat_3==1,1,0)

data.B$time_31=ifelse(is.na(data.B$time_31),data.B$time_2,data.B$time_31)

data.B$time_32=ifelse(is.na(data.B$time_32),data.B$time_3,data.B$time_32)


