#######################################
#This is called from the sim_results.R#
#######################################

rcelos.naive.mean=colMeans(rcelos.naive)
rcelos.naive.sd=c(sd(rcelos.naive[,1]),sd(rcelos.naive[,2]),sd(rcelos.naive[,3]),sd(rcelos.naive[,4]))

rcelos.naive.bias=rcelos.naive.mean-rcelos.true
rcelos.naive.mc=rcelos.naive.sd/sqrt(nsim)

rcelos.A.mean=colMeans(rcelos.A)
rcelos.A.sd=c(sd(rcelos.A[,1]),sd(rcelos.A[,2]),sd(rcelos.A[,3]),sd(rcelos.A[,4]))

rcelos.A.bias=rcelos.A.mean-rcelos.true
rcelos.A.mc=rcelos.A.sd/sqrt(nsim)

rcelos.B.mean=colMeans(rcelos.B)
rcelos.B.sd=c(sd(rcelos.B[,1]),sd(rcelos.B[,2]),sd(rcelos.B[,3]),sd(rcelos.B[,4]))

rcelos.B.bias=rcelos.B.mean-rcelos.true
rcelos.B.mc=rcelos.B.sd/sqrt(nsim)