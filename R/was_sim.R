#' Runs the WAS simulation.
#'
#' @param J The number of runs.
#' @param S The number of steps.
#' @param des A list containing the sampling designs for each step.
#' @param des_pars A list containing the parameters for the sampling designs in \code{des}.
#' @param pre A list containing the predictive tools to compare.
#' @param pre_pars A list containing the parameters for the predictive tools in \code{pre}.
#' @param tar A list containing the targeting functions for each step/to compare?.
#' @param tar_pars A list containing the parameters for the targeting functions in \code{tar}.
#' @param B The number of Bootstrap runs to estimate the inclusion probabilities.
#' @param est_var Should the variance be also estimated via bootstrapped second order inclusion probabilities? Defaults to FALSE.
#' @param pop The population [...].
#' @return The simulation.
#' @examples
#' set.seed(42)

was_sim <- function(J, # number of sims (scalar)
                    S, # number of steps (scalar)
                    des, # sampling designs for each step (list)
                    des_pars, # parameters for the designs (list, specify structure)
                    pre, # predictive tools (list)
                    pre_pars, # parameters for the predictions (list)
                    tar, # targeting functions (list)
                    tar_pars, # parameters for the targeting functions  (list)
                    B, # number or Bootstrap runs to estimate pis
                    est_var=FALSE, # if FALSE, use estimated second order inclusion probabilities, else use approximations
                    pop # population
                    ) {

  # test J, S, B, N, pop

  N <- length(des_pars[[1]]$pik)


  # test designs

  # test prediction functions

  # test targeting functions

  # text message to recap sim parameters

  # actual sim

  res <- replicate(J,{

    pik0 <- rep(ni[1]/N,N)

    ss <- sample(N,size=ni[1],prob=pik0)
    ss1 <- numeric(N)
    ss1[ss] <- 1

    spips <- par_sample(pik)

    #spips <- UPsampford(pik)

    ss.pop <- cbind(x,NA)
    ss.pop[ss1==1,2] <- dd[ss1==1,3]
    hd.pop <- impute.NN_HD(ss.pop)

    hd.pop <- cbind(hd.pop,ss1,0,0,0)

    uu1 <- target_foo(hd.pop[ss1!=1,2],tthr=thr,boost=c1)
    uu2 <- target_foo(hd.pop[ss1!=1,2],tthr=thr,boost=c2)
    uu3 <- target_foo(hd.pop[ss1!=1,2],tthr=thr,boost=c3)

    hd.pop[ss1!=1,4] <- check_pik(ni[2]*uu1/sum(uu1),ni[2])
    hd.pop[ss1!=1,5] <- check_pik(ni[2]*uu2/sum(uu2),ni[2])
    hd.pop[ss1!=1,6] <- check_pik(ni[2]*uu3/sum(uu3),ni[2])

    ss2 <- par_sample(hd.pop[,4])
    ss2b <- par_sample(hd.pop[,5])
    ss2c <- par_sample(hd.pop[,6])

    # ss2 <- UPsampford(hd.pop[,4])
    # ss2b <- UPsampford(hd.pop[,5])

    hd.pop <- data.frame(hd.pop,ss2,ss2b,ss2c)
    names(hd.pop) <- c('x','yhat1','ss1','pikk','pikb','pikc','ss2','ss2b','ss2c')

    ssrs <- numeric(N)
    ssrs[sample(N,n)] <- 1

    list(cbind(hd.pop,ssrs,spips,pik0))

  })

  }
