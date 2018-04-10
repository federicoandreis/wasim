# design setup

des <- vector('list',4)

des[[1]]$foo <- des1
des[[2]]$foo <- des2
des[[3]]$foo <- des3
des[[4]]$foo <- des3

des[[1]]$n <- des[[2]]$n <- des[[3]]$n <- des[[4]]$n <- n

des[[1]]$pik <- NA
des[[2]]$pik <- n*x/sum(x)
des[[3]]$pik <- rep(n,N)/N
des[[4]]$pik <- n*x/sum(x)

des_was <- vector('list',3)

des_was[[1]]$foo <- des_was1
des_was[[2]]$foo <- des_was1
des_was[[3]]$foo <- des_was1

des_was[[1]]$n <- des_was[[2]]$n <- des_was[[3]]$n <- c(floor(n/2),n-floor(n/2))

des_was[[1]]$pred <- impute.NN_HD
des_was[[2]]$pred <- impute.NN_HD
des_was[[3]]$pred <- impute.NN_HD

des_was[[1]]$tar <- function(x) target_foo(x,median(y),2)
des_was[[2]]$tar <- function(x) target_foo(x,median(y),15)
des_was[[3]]$tar <- function(x) target_foo(x,quantile(y,.75),2)
