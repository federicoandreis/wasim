#' Estimate inclusion probabilities via HD-Bootstrap.
#'
#' @param res A list containing the simulation results.
#' @param B The number of Bootstrap runs.
#' @param pars A list of parameters.
#' @return The estimated inclusion probabilities.
#' @examples
#' set.seed(42)



pik_est_HD <- function(res, # result file from simulation
                       B, # number of Bootstrap runs
                       pars # parameters
                       ) {

  N <- nrow(res)

  # parameters, ss, bboost, thresholds, ...

  y.tmp <- y[ss==1]

  tmp.pop <- cbind(x,NA)
  tmp.pop[ss==1,2] <- y[ss==1]

  hd.pop <- data.frame(impute.NN_HD(tmp.pop))

  ssamples <- replicate(B,{

    ss.0 <- sample(N,size=ni[1])
    ss0 <- numeric(N)
    ss0[ss.0] <- 1

    tmp.pop0 <- cbind(x,NA)
    tmp.pop0[ss0==1,2] <- y[ss0==1]

    hd.pop0 <- cbind(impute.NN_HD(tmp.pop0))

    uu <- (1-ss0)*target_foo(hd.pop[,2],tthr=thr,boost=bboost)

    pik2 <- check_pik(ni[2]*uu/sum(uu),ni[2])

    s1 <- par_sample(pik2)

    ss0+s1

  })

  #w.tmp <- rowMeans(ssamples)
  w.tmp <- (rowSums(ssamples)+1)/(B+1)

  w.tmp

}
