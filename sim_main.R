
#simulation scenario
#scenario (i): transition times generated from exponential model 
#scenario (ii): transition times generated from Weibull model 
#scenario (iii): transition times generated from Weibull model with individual frailties

#lambda.cens: hazard for censoring - set to 0 for no censoring or 0.2 for censoring

#Approach A is coded 'by hand'. 
#Approach B performs the same analysis using mstate. 
#They give identical results. Method A is quicker. 

#----------------------------------
#packages
#----------------------------------

library(survival)
library(mstate)
library(tidyr)
library(tidyverse)
library(mvtnorm)
library(etm)
library(xtable)
library(ggplot2)
library(gridExtra)
library(progress) 

#number of simulations
nsim=1000

#----------------------------------
#scenario 1: tau=10000
#Estimating conditional probabilities and corresponding CELOS
#----------------------------------

scenario=1

tau=10000

#---
#obtain true values for estimands

source("sim_truevalues.R")

#---
#apply analysis

#sample size for the simulated data set
n=200

#no censoring
lambda.cens=0
source("sim_analysis_naive.R")
source("sim_analysis_approachA.R")
source("sim_analysis_approachB.R")

#with censoring
lambda.cens=0.2
source("sim_analysis_approachA.R")

#----------------------------------
#scenario 1: tau=5
#Estimating conditional probabilities and corresponding CELOS
#----------------------------------

scenario=1

tau=5

#---
#obtain true values for estimands

source("sim_truevalues.R")

#sample size for the simulated data set
n=200

#no censoring
lambda.cens=0
source("sim_analysis_approachA.R")

#with censoring
lambda.cens=0.2
source("sim_analysis_approachA.R")

#----------------------------------
#scenario 2: tau=10000
#Estimating conditional probabilities and corresponding CELOS
#----------------------------------

scenario=2

tau=10000

#---
#obtain true values for estimands

source("sim_truevalues.R")

#sample size for the simulated data set
n=200

#no censoring
lambda.cens=0
source("sim_analysis_naive.R")
source("sim_analysis_approachA.R")
source("sim_analysis_approachB.R")

#with censoring
lambda.cens=0.2
source("sim_analysis_approachA.R")

#----------------------------------
#scenario 2: tau=5
#Estimating conditional probabilities and corresponding CELOS
#----------------------------------

scenario=2

tau=5

#---
#obtain true values for estimands

source("sim_truevalues.R")

#sample size for the simulated data set
n=200

#no censoring
lambda.cens=0
source("sim_analysis_approachA.R")

#with censoring
lambda.cens=0.2
source("sim_analysis_approachA.R")

#----------------------------------
#scenario 3: tau=10000
#Estimating conditional probabilities and corresponding CELOS
#----------------------------------

scenario=3

tau=10000

#---
#obtain true values for estimands

source("sim_truevalues.R")

#sample size for the simulated data set
n=200

#no censoring
lambda.cens=0
source("sim_analysis_naive.R")
source("sim_analysis_approachA.R")
source("sim_analysis_approachB.R")

#with censoring
lambda.cens=0.2
source("sim_analysis_approachA.R")

#----------------------------------
#scenario 3: tau=5
#Estimating conditional probabilities and corresponding CELOS
#----------------------------------

scenario=3

tau=5

#---
#obtain true values for estimands

source("sim_truevalues.R")

#sample size for the simulated data set
n=200

#no censoring
lambda.cens=0
source("sim_analysis_approachA.R")

#with censoring
lambda.cens=0.2
source("sim_analysis_approachA.R")



