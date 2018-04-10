# tools
par_sample <- function(lambda) {

  stopifnot(!any(lambda<0|lambda>1))
  N <- length(lambda)
  n <- sum(lambda)
  d <- sum(lambda*(1-lambda))

  u <- runif(N)

  Q <- u/(1-u)/(lambda/(1-lambda))*exp(1/d^2*lambda*(1-lambda)*(lambda-.5))

  ss <- numeric(N)

  ss[order(Q)[1:n]] <- 1

  ss

}

check_pik <- function(x,nn) {

  if(sum(x)!=nn)  x <- nn*x/sum(x)

  if(any(x>1)) {

    while(any(x>1)) {

      ii <- x>1

      x[ii] <- 1

      x[!ii] <- (nn-sum(ii))*x[!ii]/sum(x[!ii])
    }
  }

  x

}

target_foo <- function(x,tthr,boost=2) {

  cond <- x>tthr

  return(x*(cond*(boost-1/boost)+1/boost))    # threshold-boosting

}

#################################

des1 <- function(pop,n,pik) { # SRSWOR

  ss <- numeric(nrow(pop))

  ss[sample(nrow(pop),n)] <- 1

  ss

}

des2 <- function(pop,n,pik) { # Pareto Sampling

  par_sample(pik)

}

des3 <- function(pop,n,pik) { # UP Systematic Sampling

  UPsystematic(pik)

}

des_was1 <- function(pop,n,tar,pred) { # WAS Sampling

  N <- nrow(pop)

  ss <- numeric(N)
  ss[sample(N,size=n[1])] <- 1

  ss.pop <- cbind(pop[,-1],NA)
  ss.pop[ss==1,ncol(ss.pop)] <- pop[ss==1,1]
  hd.pop <- pred(ss.pop)

  hd.pop <- cbind(hd.pop,ss,0)

  uu <- tar(hd.pop[ss!=1,ncol(ss.pop)])

  hd.pop[ss!=1,ncol(hd.pop)] <- check_pik(n[2]*uu/sum(uu),n[2])


  ss2 <- par_sample(hd.pop[,ncol(hd.pop)])

  ss_res <- numeric(N)
  ss_res[ss==1] <- 1
  ss_res[ss2==1] <- 2

  ss_res

}

#################################

comp_sim <- function(J,des,des_was,pop) {

  ncomp <- length(des)
  nwas <- length(des_was)

  N <- nrow(pop)

  replicate(J,{

    int_res <- matrix(0,N,ncomp)
    int_res_was <- matrix(0,N,nwas)

    for (i in 1:ncomp) {

      tmp <- des[[i]]

      int_res[,i] <- tmp$foo(pop,tmp$n,tmp$pik)

    }

    for (i in 1:nwas) {

      tmp <- des_was[[i]]

      int_res_was[,i] <- tmp$foo(pop,tmp$n,tmp$tar,tmp$pred)

    }

    cbind(int_res,int_res_was)

  })


}

#########################

res_seq_to_res <- function(x) {

  x[which(x>1)] <- 1

  x

}

est_pik2 <- function(x) {

  tmp <- outer(x[,1],x[,1])

  for (i in 2:ncol(x)) {

    tmp <- tmp + outer(x[,i],x[,i])

  }

  pikl <- tmp/ncol(x)
  diag(pikl) <- NA

  pikl

}

#########################
# only HD for now, needs X, two steps

#function for the single run
bootstrap_pik <- function(B, # number of Bootstrap runs
                          ss, # sample from was (0/1)
                          pop, # population file
                          des_was) {

  N <- nrow(res)

  tmp.pop <- cbind(pop[,-1],NA)
  tmp.pop[ss==1,ncol(tmp.pop)] <- pop[ss==1,1]

  hd.pop <- data.frame(impute.NN_HD(tmp.pop))

  ssamples <- replicate(B,{

    ss.0 <- sample(N,size=des_was$n[1])
    ss0 <- numeric(N)
    ss0[ss.0] <- 1

    tmp.pop0 <- cbind(pop[,-1],NA)
    tmp.pop0[ss0==1,ncol(tmp.pop0)] <- pop[ss0==1,1]

    hd.pop0 <- cbind(impute.NN_HD(tmp.pop0))

    uu <- (1-ss0)*des_was$tar(hd.pop[,2])

    pik2 <- check_pik(des_was$n[2]*uu/sum(uu),des_was$n[2])

    s1 <- par_sample(pik2)

    ss0+s1

  })

  #w.tmp <- rowMeans(ssamples)
  w.tmp <- (rowSums(ssamples)+1)/(B+1)

  w.tmp

}

# function to Bootstrap whole sim
bootstrap_sim <- function(B,x,pop,des_was) apply(x,2,function(x) bootstrap_pik(B,x,pop,des_was))

# estimators

HT_mean <- function(y,pik,N) crossprod(y,1/pik)/N

Hajek.Ff <- function(y,obs,pik) sapply(y,Hajek.F,obs=obs,pik=pik)

HajekF <- function (x,pik) {
  ord <- order(x)
  x <- sort(x)
  n <- length(x)
  if (n < 1)
    stop("'x' must have 1 or more non-missing values")
  vals <- unique(x)
  rval <- approxfun(vals, cumsum(tabulate(match(x, vals))/pik[ord])/sum(pik^-1),
                    method = "constant", yleft = 0, yright = 1, f = 0, ties = "ordered")
  class(rval) <- c("Hajekcdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  rval
}

hajek.origin <- function(y,pik,k=1) sum(y^k/pik)/sum(1/pik)
hajek.central <- function(y,pik,k=2) sum((y-hajek.origin(y,pik,k=1))^k/pik)/sum(1/pik)

hajek.quant <- function(y,pik,p) {

  stopifnot(p>=0&p<=1)

  o.s <- order(y)

  tj <- cumsum(1/pik[o.s])/sum(1/pik[o.s])

  p.pos <- which.min(tj<p)

  y[o.s[p.pos]]

}

hajek.gamma1 <- function(y,pik) hajek.central(y,pik,k=3)/hajek.central(y,pik,k=2)^1.5
hajek.gamma2 <- function(y,pik) hajek.central(y,pik,k=4)/hajek.central(y,pik,k=2)^2

hajek.galton <- function(y,pik) (hajek.quant(y,pik,.25)+hajek.quant(y,pik,.75)-2*hajek.quant(y,pik,.5))/(hajek.quant(y,pik,.75)-hajek.quant(y,pik,.25))

hajek.qsr <- function(y,pik) {

  q20 <- hajek.quant(y,pik,.2)
  q80 <- hajek.quant(y,pik,.8)

  sum((y/pik)[y<=q20])/sum((y/pik)[y>=q80])

}
