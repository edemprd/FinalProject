#' Mixture normal distribution
#'
#' @description Generate n observations from a mixture of two normal distributionsGenerate n observations from a mixture of two normal distributions
#'
#' @param p numeric positive integer
#' @param mu1 numeric positive integer
#' @param mu2 numeric positive integer
#' @param s1 numeric positive integer
#' @param s2 numeric positive integer
#' @param n numeric positive integer
#' @export
#' @author Edem Defor
#' @examples
#' Y1 = mixnorm(0.5,-2,3,2,1.5,1000)



mixnorm <- function(p,mu1,mu2,s1,s2,n){

  # set.seed(65432)

  U = stats::rbinom(n,1,p)         # generate U from a Bernoulli(p) distribution and
  n1 = length(which(U==1))  # if U=1, get the length of U
  n2 = length(which(U==0))  # if U=0, get the length of U

  Y1 = stats::rnorm(n1,mu1,s1)     # if U=1, then draw Y from a normal distribution with mean μ1 and standard deviation σ1,
  Y2 = stats::rnorm(n2,mu2,s2)     # if U=0, draw Y from the other normal distribution with mean μ2 and standard deviation σ2.

  U[U==1] = Y1              # assign the values of Y1 if U=1
  U[U==0] = Y2              # assign the values of Y2 if U=0

  return(U)
}

