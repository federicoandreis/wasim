#' Extracts a Pareto sample.
#'
#' @param lambda An \code{N}-vector of inclusion probabilities summing to \code{n}.
#' @return A Pareto sample of size \code{n} from a population of size \code{N}.
#' @examples
#' N <- 100
#' n <- 10
#' set.seed(42)
#' x <- runif(N)
#' pik <- n*x/sum(x)
#' par_sample(pik)

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
