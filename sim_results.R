
############################################################################
#simulation results: tabulates results and plots probability distributions
############################################################################

sp=function(x){sprintf("%.3f",x)}

#----------------------------------
#scenario 1, lambda.cens=0, tau=10000
#----------------------------------

load(file="results/scen1_true_rcelos_taumax.RData")
load(file="results/scen1_naive_rcelos_nocens_taumax.RData")
load(file="results/scen1_A_rcelos_nocens_taumax.RData")
load(file="results/scen1_B_rcelos_nocens_taumax.RData")


source(file="summary_stat_func.R")

res.naive.scen1.nocens.taumax=cbind(paste0(sp(rcelos.naive.mean)," (",sp(rcelos.naive.sd),")"),paste0(sp(rcelos.naive.bias)," (",sp(rcelos.naive.mc),")"))
res.A.scen1.nocens.taumax=cbind(paste0(sp(rcelos.A.mean)," (",sp(rcelos.A.sd),")"),paste0(sp(rcelos.A.bias)," (",sp(rcelos.A.mc),")"))
res.B.scen1.nocens.taumax=cbind(paste0(sp(rcelos.B.mean)," (",sp(rcelos.B.sd),")"),paste0(sp(rcelos.B.bias)," (",sp(rcelos.B.mc),")"))

#----------------------------------
#scenario 1, lambda.cens=0, tau=5
#----------------------------------

load(file="results/scen1_true_rcelos_tau5.RData")
load(file="results/scen1_naive_rcelos_nocens_tau5.RData")
load(file="results/scen1_A_rcelos_nocens_tau5.RData")

source(file="summary_stat_func.R")

res.naive.scen1.nocens.tau5=cbind(paste0(sp(rcelos.naive.mean)," (",sp(rcelos.naive.sd),")"),paste0(sp(rcelos.naive.bias)," (",sp(rcelos.naive.mc),")"))
res.A.scen1.nocens.tau5=cbind(paste0(sp(rcelos.A.mean)," (",sp(rcelos.A.sd),")"),paste0(sp(rcelos.A.bias)," (",sp(rcelos.A.mc),")"))

#----------------------------------
#scenario 1, lambda.cens=0.2, tau=10000
#----------------------------------

load(file="results/scen1_true_rcelos_taumax.RData")
load(file="results/scen1_naive_rcelos_cens_taumax.RData")
load(file="results/scen1_A_rcelos_cens_taumax.RData")

source(file="summary_stat_func.R")

res.naive.scen1.cens.taumax=cbind(paste0(sp(rcelos.naive.mean)," (",sp(rcelos.naive.sd),")"),paste0(sp(rcelos.naive.bias)," (",sp(rcelos.naive.mc),")"))
res.A.scen1.cens.taumax=cbind(paste0(sp(rcelos.A.mean)," (",sp(rcelos.A.sd),")"),paste0(sp(rcelos.A.bias)," (",sp(rcelos.A.mc),")"))

#----------------------------------
#scenario 1, lambda.cens=0.2, tau=5
#----------------------------------

load(file="results/scen1_true_rcelos_tau5.RData")
load(file="results/scen1_naive_rcelos_cens_tau5.RData")
load(file="results/scen1_A_rcelos_cens_tau5.RData")

source(file="summary_stat_func.R")

res.naive.scen1.cens.tau5=cbind(paste0(sp(rcelos.naive.mean)," (",sp(rcelos.naive.sd),")"),paste0(sp(rcelos.naive.bias)," (",sp(rcelos.naive.mc),")"))
res.A.scen1.cens.tau5=cbind(paste0(sp(rcelos.A.mean)," (",sp(rcelos.A.sd),")"),paste0(sp(rcelos.A.bias)," (",sp(rcelos.A.mc),")"))

#----------------------------------
#scenario 2, lambda.cens=0, tau=10000
#----------------------------------

load(file="results/scen2_true_rcelos_taumax.RData")
load(file="results/scen2_naive_rcelos_nocens_taumax.RData")
load(file="results/scen2_A_rcelos_nocens_taumax.RData")

source(file="summary_stat_func.R")

res.naive.scen2.nocens.taumax=cbind(paste0(sp(rcelos.naive.mean)," (",sp(rcelos.naive.sd),")"),paste0(sp(rcelos.naive.bias)," (",sp(rcelos.naive.mc),")"))
res.A.scen2.nocens.taumax=cbind(paste0(sp(rcelos.A.mean)," (",sp(rcelos.A.sd),")"),paste0(sp(rcelos.A.bias)," (",sp(rcelos.A.mc),")"))

#----------------------------------
#scenario 2, lambda.cens=0, tau=5
#----------------------------------

load(file="results/scen2_true_rcelos_tau5.RData")
load(file="results/scen2_naive_rcelos_nocens_tau5.RData")
load(file="results/scen2_A_rcelos_nocens_tau5.RData")

source(file="summary_stat_func.R")

res.naive.scen2.nocens.tau5=cbind(paste0(sp(rcelos.naive.mean)," (",sp(rcelos.naive.sd),")"),paste0(sp(rcelos.naive.bias)," (",sp(rcelos.naive.mc),")"))
res.A.scen2.nocens.tau5=cbind(paste0(sp(rcelos.A.mean)," (",sp(rcelos.A.sd),")"),paste0(sp(rcelos.A.bias)," (",sp(rcelos.A.mc),")"))

#----------------------------------
#scenario 2, lambda.cens=0.2, tau=10000
#----------------------------------

load(file="results/scen2_true_rcelos_taumax.RData")
load(file="results/scen2_naive_rcelos_cens_taumax.RData")
load(file="results/scen2_A_rcelos_cens_taumax.RData")

source(file="summary_stat_func.R")

res.naive.scen2.cens.taumax=cbind(paste0(sp(rcelos.naive.mean)," (",sp(rcelos.naive.sd),")"),paste0(sp(rcelos.naive.bias)," (",sp(rcelos.naive.mc),")"))
res.A.scen2.cens.taumax=cbind(paste0(sp(rcelos.A.mean)," (",sp(rcelos.A.sd),")"),paste0(sp(rcelos.A.bias)," (",sp(rcelos.A.mc),")"))

#----------------------------------
#scenario 2, lambda.cens=0.2, tau=5
#----------------------------------

load(file="results/scen2_true_rcelos_tau5.RData")
load(file="results/scen2_naive_rcelos_cens_tau5.RData")
load(file="results/scen2_A_rcelos_cens_tau5.RData")

source(file="summary_stat_func.R")

res.naive.scen2.cens.tau5=cbind(paste0(sp(rcelos.naive.mean)," (",sp(rcelos.naive.sd),")"),paste0(sp(rcelos.naive.bias)," (",sp(rcelos.naive.mc),")"))
res.A.scen2.cens.tau5=cbind(paste0(sp(rcelos.A.mean)," (",sp(rcelos.A.sd),")"),paste0(sp(rcelos.A.bias)," (",sp(rcelos.A.mc),")"))

#----------------------------------
#scenario 3, lambda.cens=0, tau=10000
#----------------------------------

load(file="results/scen3_true_rcelos_taumax.RData")
load(file="results/scen3_naive_rcelos_nocens_taumax.RData")
load(file="results/scen3_A_rcelos_nocens_taumax.RData")

source(file="summary_stat_func.R")

res.naive.scen3.nocens.taumax=cbind(paste0(sp(rcelos.naive.mean)," (",sp(rcelos.naive.sd),")"),paste0(sp(rcelos.naive.bias)," (",sp(rcelos.naive.mc),")"))
res.A.scen3.nocens.taumax=cbind(paste0(sp(rcelos.A.mean)," (",sp(rcelos.A.sd),")"),paste0(sp(rcelos.A.bias)," (",sp(rcelos.A.mc),")"))

#----------------------------------
#scenario 3, lambda.cens=0, tau=5
#----------------------------------

load(file="results/scen3_true_rcelos_tau5.RData")
load(file="results/scen3_naive_rcelos_nocens_tau5.RData")
load(file="results/scen3_A_rcelos_nocens_tau5.RData")

source(file="summary_stat_func.R")

res.naive.scen3.nocens.tau5=cbind(paste0(sp(rcelos.naive.mean)," (",sp(rcelos.naive.sd),")"),paste0(sp(rcelos.naive.bias)," (",sp(rcelos.naive.mc),")"))
res.A.scen3.nocens.tau5=cbind(paste0(sp(rcelos.A.mean)," (",sp(rcelos.A.sd),")"),paste0(sp(rcelos.A.bias)," (",sp(rcelos.A.mc),")"))

#----------------------------------
#scenario 3, lambda.cens=0.2, tau=10000
#----------------------------------

load(file="results/scen3_true_rcelos_taumax.RData")
load(file="results/scen3_naive_rcelos_cens_taumax.RData")
load(file="results/scen3_A_rcelos_cens_taumax.RData")

source(file="summary_stat_func.R")

res.naive.scen3.cens.taumax=cbind(paste0(sp(rcelos.naive.mean)," (",sp(rcelos.naive.sd),")"),paste0(sp(rcelos.naive.bias)," (",sp(rcelos.naive.mc),")"))
res.A.scen3.cens.taumax=cbind(paste0(sp(rcelos.A.mean)," (",sp(rcelos.A.sd),")"),paste0(sp(rcelos.A.bias)," (",sp(rcelos.A.mc),")"))

#----------------------------------
#scenario 3, lambda.cens=0.2, tau=5
#----------------------------------

load(file="results/scen3_true_rcelos_tau5.RData")
load(file="results/scen3_naive_rcelos_cens_tau5.RData")
load(file="results/scen3_A_rcelos_cens_tau5.RData")

source(file="summary_stat_func.R")

res.naive.scen3.cens.tau5=cbind(paste0(sp(rcelos.naive.mean)," (",sp(rcelos.naive.sd),")"),paste0(sp(rcelos.naive.bias)," (",sp(rcelos.naive.mc),")"))
res.A.scen3.cens.tau5=cbind(paste0(sp(rcelos.A.mean)," (",sp(rcelos.A.sd),")"),paste0(sp(rcelos.A.bias)," (",sp(rcelos.A.mc),")"))

#----------------------------------
#results table
#----------------------------------

#scenario 1

res.scen1.taumax=rbind(cbind(res.naive.scen1.nocens.taumax,res.naive.scen1.cens.taumax),
                      cbind(res.A.scen1.nocens.taumax,res.A.scen1.cens.taumax))
xtable(res.scen1.taumax)

res.scen1.tau5=rbind(cbind(res.naive.scen1.nocens.tau5,res.naive.scen1.cens.tau5),
                       cbind(res.A.scen1.nocens.tau5,res.A.scen1.cens.tau5))
xtable(res.scen1.tau5)

#scenario 2

res.scen2.taumax=rbind(cbind(res.naive.scen2.nocens.taumax,res.naive.scen2.cens.taumax),
                       cbind(res.A.scen2.nocens.taumax,res.A.scen2.cens.taumax))
xtable(res.scen2.taumax)

res.scen2.tau5=rbind(cbind(res.naive.scen2.nocens.tau5,res.naive.scen2.cens.tau5),
                     cbind(res.A.scen2.nocens.tau5,res.A.scen2.cens.tau5))
xtable(res.scen2.tau5)

#scenario 3

res.scen3.taumax=rbind(cbind(res.naive.scen3.nocens.taumax,res.naive.scen3.cens.taumax),
                       cbind(res.A.scen3.nocens.taumax,res.A.scen3.cens.taumax))
xtable(res.scen3.taumax)

res.scen3.tau5=rbind(cbind(res.naive.scen3.nocens.tau5,res.naive.scen3.cens.tau5),
                     cbind(res.A.scen3.nocens.tau5,res.A.scen3.cens.tau5))
xtable(res.scen3.tau5)


#----------------------------------
#results plot set-up
#----------------------------------

addlinetoplot.lty <- function(dataset, varx, vary,vcol) { 
  list(
    geom_step(data=dataset, aes_string(x=varx, y=vary,colour=vcol),size=1) 
  )
}

cols=c("true"="#000000","naive"="#999999","multistate"="#D55E00")

#------------------------
#plot: no censoring, taumax
#------------------------

load(file="results/scen1_true_p_taumax.RData")

load(file="results/scen1_naive_p_nocens_taumax.RData")

load(file="results/scen1_A_p_nocens_taumax.RData")

myt=30

dat=data.frame(time=1:myt,p.true.1.13=p.true[1:myt,1],p.true.1.123=p.true[1:myt,2],p.true.2.123=p.true[1:myt,3],
               p.naive.1.13.mean=colMeans(p.naive[1,,1:myt],na.rm=T),p.naive.1.123.mean=colMeans(p.naive[2,,1:myt],na.rm=T),p.naive.2.123.mean=colMeans(p.naive[3,,1:myt],na.rm=T),
               p.A.1.13.mean=colMeans(p.A[1,,1:myt],na.rm=T),p.A.1.123.mean=colMeans(p.A[2,,1:myt],na.rm=T),p.A.2.123.mean=colMeans(p.A[3,,1:myt],na.rm=T))

plot.1.13.nocens=ggplot(dat,aes(x=time,y=p.true.1.13))+
  addlinetoplot.lty(dat,"time","p.true.1.13",vcol='"true"')+
  addlinetoplot.lty(dat,"time","p.naive.1.13.mean",vcol='"naive"')+
  addlinetoplot.lty(dat,"time","p.A.1.13.mean",vcol='"multistate"')+
  scale_x_continuous(breaks=seq(0,30,10),limits=c(0,30))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true="True values",naive="Naive analysis",multistate="Multi-state analysis"),breaks=c("true","naive","multistate"))+
  ylab("Probability")+xlab("Time")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        title = element_text(size=10))+
  theme(legend.position=c(0.7,0.8))+
  ggtitle(expression(paste("Time spent in state 1: pathway 1->3. ", P[1~"|"~13]~(t))))


plot.1.123.nocens=ggplot(dat,aes(x=time,y=p.true.1.123))+
  addlinetoplot.lty(dat,"time","p.true.1.123",vcol='"true"')+
  addlinetoplot.lty(dat,"time","p.naive.1.123.mean",vcol='"naive"')+
  addlinetoplot.lty(dat,"time","p.A.1.123.mean",vcol='"multistate"')+
  scale_x_continuous(breaks=seq(0,30,10),limits=c(0,30))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true="True values",naive="Naive analysis",multistate="Multi-state analysis"),breaks=c("true","naive","multistate"))+
  ylab("Probability")+xlab("Time")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        title = element_text(size=10))+
  theme(legend.position=c(0.7,0.8))+
  ggtitle(expression(paste("Time spent in state 1: pathway 1->2->3. ", P[1~"|"~123]~(t))))

plot.2.123.nocens=ggplot(dat,aes(x=time,y=p.true.2.123))+
  addlinetoplot.lty(dat,"time","p.true.2.123",vcol='"true"')+
  addlinetoplot.lty(dat,"time","p.naive.2.123.mean",vcol='"naive"')+
  addlinetoplot.lty(dat,"time","p.A.2.123.mean",vcol='"multistate"')+
  scale_x_continuous(breaks=seq(0,30,10),limits=c(0,30))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true="True values",naive="Naive analysis",multistate="Multi-state analysis"),breaks=c("true","naive","multistate"))+
  ylab("Probability")+xlab("Time")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        title = element_text(size=10))+
  theme(legend.position=c(0.7,0.8))+
  ggtitle(expression(paste("Time spent in state 2: pathway 1->2->3. ", P[2~"|"~123]~(t))))

#windows(15,5)
grid.arrange(plot.1.13.nocens,plot.1.123.nocens,plot.2.123.nocens,ncol=3)
ggsave("results/plot_scen1_nocens_taumax.png")

#------------------------
#plot: no censoring, tau=5
#------------------------

load(file="results/scen1_true_p_tau5.RData")

load(file="results/scen1_naive_p_nocens_tau5.RData")

load(file="results/scen1_A_p_nocens_tau5.RData")

myt=5

dat=data.frame(time=1:myt,p.true.1.13=p.true[1:myt,1],p.true.1.123=p.true[1:myt,2],p.true.2.123=p.true[1:myt,3],p.true.2.123.star=p.true[1:myt,4],
               p.naive.1.13.mean=colMeans(p.naive[1,,1:myt],na.rm=T),p.naive.1.123.mean=colMeans(p.naive[2,,1:myt],na.rm=T),
               p.naive.2.123.mean=colMeans(p.naive[3,,1:myt],na.rm=T),p.naive.2.123.star.mean=colMeans(p.naive[4,,1:myt],na.rm=T),
               p.A.1.13.mean=colMeans(p.A[1,,1:myt],na.rm=T),p.A.1.123.mean=colMeans(p.A[2,,1:myt],na.rm=T),
               p.A.2.123.mean=colMeans(p.A[3,,1:myt],na.rm=T),p.A.2.123.star.mean=colMeans(p.A[4,,1:myt],na.rm=T))

plot.1.13.nocens5=ggplot(dat,aes(x=time,y=p.true.1.13))+
  addlinetoplot.lty(dat,"time","p.true.1.13",vcol='"true"')+
  addlinetoplot.lty(dat,"time","p.naive.1.13.mean",vcol='"naive"')+
  addlinetoplot.lty(dat,"time","p.A.1.13.mean",vcol='"multistate"')+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true="True values",naive="Naive analysis",multistate="Multi-state analysis"),breaks=c("true","naive","multistate"))+
  ylab("Probability")+xlab("Time")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        title = element_text(size=10))+
  theme(legend.position=c(0.7,0.8))+
  ggtitle(expression(paste("Time spent in state 1: pathway 1->3. ", P[1~"|"~13]^{tau}~(t))))


plot.1.123.nocens5=ggplot(dat,aes(x=time,y=p.true.1.123))+
  addlinetoplot.lty(dat,"time","p.true.1.123",vcol='"true"')+
  addlinetoplot.lty(dat,"time","p.naive.1.123.mean",vcol='"naive"')+
  addlinetoplot.lty(dat,"time","p.A.1.123.mean",vcol='"multistate"')+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true="True values",naive="Naive analysis",multistate="Multi-state analysis"),breaks=c("true","naive","multistate"))+
  ylab("Probability")+xlab("Time")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        title = element_text(size=10))+
  theme(legend.position=c(0.7,0.8))+
  ggtitle(expression(paste("Time spent in state 1: pathway 1->2->3. ", P[1~"|"~123]^{tau}~(t))))

plot.2.123.nocens5=ggplot(dat,aes(x=time,y=p.true.2.123))+
  addlinetoplot.lty(dat,"time","p.true.2.123",vcol='"true"')+
  addlinetoplot.lty(dat,"time","p.naive.2.123.mean",vcol='"naive"')+
  addlinetoplot.lty(dat,"time","p.A.2.123.mean",vcol='"multistate"')+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true="True values",naive="Naive analysis",multistate="Multi-state analysis"),breaks=c("true","naive","multistate"))+
  ylab("Probability")+xlab("Time")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        title = element_text(size=10))+
  theme(legend.position=c(0.7,0.8))+
  ggtitle(expression(paste("Time spent in state 2: pathway 1->2->3. ", P[2~"|"~123]^{tau}~(t))))

plot.2.123.star.nocens5=ggplot(dat,aes(x=time,y=p.true.2.123))+
  addlinetoplot.lty(dat,"time","p.true.2.123.star",vcol='"true"')+
  addlinetoplot.lty(dat,"time","p.naive.2.123.star.mean",vcol='"naive"')+
  addlinetoplot.lty(dat,"time","p.A.2.123.star.mean",vcol='"multistate"')+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true="True values",naive="Naive analysis",multistate="Multi-state analysis"),breaks=c("true","naive","multistate"))+
  ylab("Probability")+xlab("Time")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        title = element_text(size=10))+
  theme(legend.position=c(0.7,0.8))+
  ggtitle(expression(paste("Time spent in state 2: pathway 1->2->3. ", P[2~"|"~123]^{tau^"*"}~(t))))

#windows(10,10)
grid.arrange(plot.1.13.nocens5,plot.1.123.nocens5,plot.2.123.nocens5,plot.2.123.star.nocens5,ncol=2)
ggsave("results/plot_scen1_nocens_tau5.png")

#------------------------
#plot: censoring, taumax
#------------------------

load(file="results/scen1_true_p_taumax.RData")

load(file="results/scen1_naive_p_cens_taumax.RData")

load(file="results/scen1_A_p_cens_taumax.RData")

myt=30

dat=data.frame(time=1:myt,p.true.1.13=p.true[1:myt,1],p.true.1.123=p.true[1:myt,2],p.true.2.123=p.true[1:myt,3],
               p.naive.1.13.mean=colMeans(p.naive[1,,1:myt],na.rm=T),p.naive.1.123.mean=colMeans(p.naive[2,,1:myt],na.rm=T),p.naive.2.123.mean=colMeans(p.naive[3,,1:myt],na.rm=T),
               p.A.1.13.mean=colMeans(p.A[1,,1:myt],na.rm=T),p.A.1.123.mean=colMeans(p.A[2,,1:myt],na.rm=T),p.A.2.123.mean=colMeans(p.A[3,,1:myt],na.rm=T))

plot.1.13.cens=ggplot(dat,aes(x=time,y=p.true.1.13))+
  addlinetoplot.lty(dat,"time","p.true.1.13",vcol='"true"')+
  addlinetoplot.lty(dat,"time","p.naive.1.13.mean",vcol='"naive"')+
  addlinetoplot.lty(dat,"time","p.A.1.13.mean",vcol='"multistate"')+
  scale_x_continuous(breaks=seq(0,30,10),limits=c(0,30))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true="True values",naive="Naive analysis",multistate="Multi-state analysis"),breaks=c("true","naive","multistate"))+
  ylab("Probability")+xlab("Time")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        title = element_text(size=10))+
  theme(legend.position=c(0.7,0.8))+
  ggtitle(expression(paste("Time spent in state 1: pathway 1->3. ", P[1~"|"~13]~(t))))


plot.1.123.cens=ggplot(dat,aes(x=time,y=p.true.1.123))+
  addlinetoplot.lty(dat,"time","p.true.1.123",vcol='"true"')+
  addlinetoplot.lty(dat,"time","p.naive.1.123.mean",vcol='"naive"')+
  addlinetoplot.lty(dat,"time","p.A.1.123.mean",vcol='"multistate"')+
  scale_x_continuous(breaks=seq(0,30,10),limits=c(0,30))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true="True values",naive="Naive analysis",multistate="Multi-state analysis"),breaks=c("true","naive","multistate"))+
  ylab("Probability")+xlab("Time")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        title = element_text(size=10))+
  theme(legend.position=c(0.7,0.8))+
  ggtitle(expression(paste("Time spent in state 1: pathway 1->2->3. ", P[1~"|"~123]~(t))))

plot.2.123.cens=ggplot(dat,aes(x=time,y=p.true.2.123))+
  addlinetoplot.lty(dat,"time","p.true.2.123",vcol='"true"')+
  addlinetoplot.lty(dat,"time","p.naive.2.123.mean",vcol='"naive"')+
  addlinetoplot.lty(dat,"time","p.A.2.123.mean",vcol='"multistate"')+
  scale_x_continuous(breaks=seq(0,30,10),limits=c(0,30))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true="True values",naive="Naive analysis",multistate="Multi-state analysis"),breaks=c("true","naive","multistate"))+
  ylab("Probability")+xlab("Time")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        title = element_text(size=10))+
  theme(legend.position=c(0.7,0.8))+
  ggtitle(expression(paste("Time spent in state 2: pathway 1->2->3. ", P[2~"|"~123]~(t))))

#windows(15,5)
grid.arrange(plot.1.13.cens,plot.1.123.cens,plot.2.123.cens,ncol=3)
ggsave("results/plot_scen1_cens.png")
            
#------------------------
#plot: censoring, tau=5
#------------------------

load(file="results/scen1_true_p_tau5.RData")

load(file="results/scen1_naive_p_cens_tau5.RData")

load(file="results/scen1_A_p_cens_tau5.RData")

myt=5

dat=data.frame(time=1:myt,p.true.1.13=p.true[1:myt,1],p.true.1.123=p.true[1:myt,2],p.true.2.123=p.true[1:myt,3],p.true.2.123.star=p.true[1:myt,4],
               p.naive.1.13.mean=colMeans(p.naive[1,,1:myt],na.rm=T),p.naive.1.123.mean=colMeans(p.naive[2,,1:myt],na.rm=T),
               p.naive.2.123.mean=colMeans(p.naive[3,,1:myt],na.rm=T),p.naive.2.123.star.mean=colMeans(p.naive[4,,1:myt],na.rm=T),
               p.A.1.13.mean=colMeans(p.A[1,,1:myt],na.rm=T),p.A.1.123.mean=colMeans(p.A[2,,1:myt],na.rm=T),
               p.A.2.123.mean=colMeans(p.A[3,,1:myt],na.rm=T),p.A.2.123.star.mean=colMeans(p.A[4,,1:myt],na.rm=T))

plot.1.13.cens5=ggplot(dat,aes(x=time,y=p.true.1.13))+
  addlinetoplot.lty(dat,"time","p.true.1.13",vcol='"true"')+
  addlinetoplot.lty(dat,"time","p.naive.1.13.mean",vcol='"naive"')+
  addlinetoplot.lty(dat,"time","p.A.1.13.mean",vcol='"multistate"')+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true="True values",naive="Naive analysis",multistate="Multi-state analysis"),breaks=c("true","naive","multistate"))+
  ylab("Probability")+xlab("Time")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        title = element_text(size=10))+
  theme(legend.position=c(0.7,0.8))+
  ggtitle(expression(paste("Time spent in state 1: pathway 1->3. ", P[1~"|"~13]^{tau}~(t))))


plot.1.123.cens5=ggplot(dat,aes(x=time,y=p.true.1.123))+
  addlinetoplot.lty(dat,"time","p.true.1.123",vcol='"true"')+
  addlinetoplot.lty(dat,"time","p.naive.1.123.mean",vcol='"naive"')+
  addlinetoplot.lty(dat,"time","p.A.1.123.mean",vcol='"multistate"')+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true="True values",naive="Naive analysis",multistate="Multi-state analysis"),breaks=c("true","naive","multistate"))+
  ylab("Probability")+xlab("Time")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        title = element_text(size=10))+
  theme(legend.position=c(0.7,0.8))+
  ggtitle(expression(paste("Time spent in state 1: pathway 1->2->3. ", P[1~"|"~123]^{tau}~(t))))

plot.2.123.cens5=ggplot(dat,aes(x=time,y=p.true.2.123))+
  addlinetoplot.lty(dat,"time","p.true.2.123",vcol='"true"')+
  addlinetoplot.lty(dat,"time","p.naive.2.123.mean",vcol='"naive"')+
  addlinetoplot.lty(dat,"time","p.A.2.123.mean",vcol='"multistate"')+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true="True values",naive="Naive analysis",multistate="Multi-state analysis"),breaks=c("true","naive","multistate"))+
  ylab("Probability")+xlab("Time")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        title = element_text(size=10))+
  theme(legend.position=c(0.7,0.8))+
  ggtitle(expression(paste("Time spent in state 2: pathway 1->2->3. ", P[2~"|"~123]^{tau}~(t))))

plot.2.123.star.cens5=ggplot(dat,aes(x=time,y=p.true.2.123))+
  addlinetoplot.lty(dat,"time","p.true.2.123.star",vcol='"true"')+
  addlinetoplot.lty(dat,"time","p.naive.2.123.star.mean",vcol='"naive"')+
  addlinetoplot.lty(dat,"time","p.A.2.123.star.mean",vcol='"multistate"')+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true="True values",naive="Naive analysis",multistate="Multi-state analysis"),breaks=c("true","naive","multistate"))+
  ylab("Probability")+xlab("Time")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),
        title = element_text(size=10))+
  theme(legend.position=c(0.7,0.8))+
  ggtitle(expression(paste("Time spent in state 2: pathway 1->2->3. ", P[2~"|"~123]^{tau^"*"}~(t))))

#windows(10,10)
grid.arrange(plot.1.13.cens5,plot.1.123.cens5,plot.2.123.cens5,plot.2.123.star.cens5,ncol=2)
ggsave("results/plot_scen1_cens_tau5.png")
