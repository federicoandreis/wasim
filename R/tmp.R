rm(list=ls())
gc()
############ load libraries
setwd('C:/Users/Federico/Dropbox/wasim/wasim/R')
library(sampling)
library(HotDeckImputation)
library(parallel)
############ load tools
source('tmptools.R')
############ population setup
N <- 1000
n <- 100
set.seed(42)
y <- rgamma(N,1,2)
x <- exp(y+1)*.25+abs(rnorm(N,sd=2))
plot(x,y)
cor(x,y)
pop <- cbind(y,x)
############ designs setup
source('des_setup.R')
############ sim run
set.seed(42)
J <- 5000
res0 <- comp_sim(J,des,des_was,pop)
res <- res_seq_to_res(res0)
############ empirical inclusion probabilities, first and second order
emp_pik <- apply(res,2,rowMeans)
# emp_pik2 <- array(NA,dim=c(N,N,7))
# for (i in 1:7) {
#
#   emp_pik2[,,i] <- est_pik2(res[,i,])
#
#   diag(emp_pik2[,,i]) <- emp_pik[,i] # the diagonal contains the first order inclusion probs
#
# }
############ checks and plots
boxplot(emp_pik)
apply(emp_pik,2,sd)
apply(emp_pik,2,mean)
plot(emp_pik[,1],emp_pik[,7],xlim=c(0,.5),ylim=c(0,.5));abline(a=0,b=1,col='red')
# MC checks

############ bootstrapping
B <- 100 # maybe subset the total number of runs J
# system.time(bootstrap_was1 <- bootstrap_sim(B,res[,5,],pop,des_was[[1]]))
# system.time(bootstrap_was2 <- bootstrap_sim(B,res[,6,],pop,des_was[[2]]))
# system.time(bootstrap_was2 <- bootstrap_sim(B,res[,7,],pop,des_was[[3]]))
# go parallel
cores <- detectCores()
cluster <- makePSOCKcluster(cores)
clusterExport(cluster,c('res','bootstrap_pik','pop','B',
                        'des_was','check_pik','par_sample',
                        'target_foo','y','x'))
clusterEvalQ(cluster,library(HotDeckImputation))

system.time(bootstrap_was1 <- parApply(cluster,res[,5,],2,function(x) bootstrap_pik(B,x,pop,des_was[[1]])))
system.time(bootstrap_was2 <- parApply(cluster,res[,6,],2,function(x) bootstrap_pik(B,x,pop,des_was[[2]])))
system.time(bootstrap_was3 <- parApply(cluster,res[,7,],2,function(x) bootstrap_pik(B,x,pop,des_was[[3]])))

stopCluster(cluster)
############ estimation
# HT and Hajek estimators for total, mean, variance, ...

mean_srswor <- apply(res[,1,],2,function(z) mean(y[z==1]))
mean_pareto <- apply(res[,2,],2,function(z) HT_mean(y[z==1],(n*x/sum(x))[z==1],N))
mean_systematic <- apply(res[,3,],2,function(z) mean(y[z==1]))
mean_systematic_pps <- apply(res[,4,],2,function(z) HT_mean(y[z==1],(n*x/sum(x))[z==1],N))
mean_was1 <- mean_was2 <- mean_was3 <- numeric(J)
for (i in 1:J) {

  mean_was1[i] <- crossprod(y[res[,5,i]==1],1/emp_pik[res[,5,i]==1,5])/N#bootstrap_was1[res[,5,i]==1,i])
  mean_was2[i] <- crossprod(y[res[,6,i]==1],1/emp_pik[res[,6,i]==1,6])/N#bootstrap_was2[res[,6,i]==1,i])
  mean_was3[i] <- crossprod(y[res[,7,i]==1],1/emp_pik[res[,7,i]==1,7])/N#bootstrap_was3[res[,7,i]==1,i])

}

mean_est <- cbind(mean_srswor,mean_pareto,mean_systematic,mean_systematic_pps,mean_was1,mean_was2,mean_was3)
boxplot(mean_est); abline(h=mean(y),col='red')


#
#
# hd.hj.est.ba <- sapply(ss.res,function(x) sum(y[(x$ss1+x$ss2a)==1,1]/pik.est.hd.ba[(x$ss1+x$ss2a)==1])/
#                          sum(1/pik.est.hd.ba[(x$ss1+x$ss2a)==1]))
# hd.hj.est.bb <- sapply(ss.res,function(x) sum(y[(x$ss1+x$ss2b)==1,1]/pik.est.hd.bb[(x$ss1+x$ss2b)==1])/
#                          sum(1/pik.est.hd.bb[(x$ss1+x$ss2b)==1]))
# hd.hj.est.bc <- sapply(ss.res,function(x) sum(y[(x$ss1+x$ss2c)==1,1]/pik.est.hd.bc[(x$ss1+x$ss2c)==1])/
#                          sum(1/pik.est.hd.bc[(x$ss1+x$ss2c)==1]))
#
# srs.est <- sapply(ss.full,function(x) mean(y[x$ssrs==1,1]))

# Variance estimation: approximations

# Oversampling & costs - possibly radar charts
