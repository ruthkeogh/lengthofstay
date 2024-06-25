This repository contains R code for conducting the simulation study reported in the following paper:

Keogh, R.H., Diaz-Ordaz, K., Jewell, N.P. et al. Estimating distribution of length of stay in a multi-state model conditional on the pathway, with an application to patients hospitalised with Covid-19. Lifetime Data Anal 29, 288–317 (2023). https://doi.org/10.1007/s10985-022-09586-0

sim_main.R: This is the master file.
sim_data_scenario1/2/3.R: Generates data under different scenarios.
sim_analysis_approachA.R and sim_analysis_approachB.R: These are two equivalent ways of conducting the analyses described in the paper. The notation of Approach B matches the way the methods are described in the paper. 
sim_truevalues.R: Obtains true values of the target quantities.
sim_analysis_naive.R: Implements the 'naive' analysis.
simmstate.R: Used to generate data under scenario 3. 
sim_results.R: Compiles results and creates tables and figures.
summary_stat_func.R: This is called from sim_results. 

