#' Runs the WAS simulation.
#'
#' @param R The number of runs.
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

was_sim <- function(R, # number of sims (scalar)
                    S, # number of steps (scalar)
                    des, # sampling designs for each step (list)
                    des_pars, # parameters for the designs (list)
                    pre, # predictive tools (list)
                    pre_pars, # parameters for the predictions (list)
                    tar, # targeting functions (list)
                    tar_pars, # parameters for the targeting functions  (list)
                    B, # number or Bootstrap runs to estimate pis
                    est_var=FALSE, # if FALSE, use estimated second order inclusion probabilities, else use approximations
                    pop # population
                    ) {

  # test R, S, B, N, n, pop

  # test designs

  # test prediction functions

  # test targeting functions

  # actual sim

  }
