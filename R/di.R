#' @title Function for DI
#' @description The function computes the univariate Poisson dispersion index for a count random variable.
#' @param X A count random variable
#' @details
#' \code{di.fun} provides the univariate Poisson  dispersion index (Fisher, 1934). We can refer to Touré et al. (2020) for more details on the Poisson dispersion index.
#'
#' @importFrom stats var
#' @return Returns
#' \item{di}{The Poisson dispersion index}
#' @author
#' Aboubacar Y. Touré and Célestin C. Kokonendji
#' @references
#' Fisher, R.A. (1934). The effects of methods of ascertainment upon the estimation of frequencies, \emph{Annals of Eugenics} \bold{6}, 13-25.\cr
#' \cr
#' Touré, A.Y., Dossou-Gbété, S. and Kokonendji, C.C. (2020). Asymptotic normality of the test statistics for relative dispersion and relative variation indexes, \emph{Journal of Applied Statistics} \bold{47}, 2479-2491.
#' @export di.fun
#'
#' @examples
#'X<-c(6,7,8,9,8,4,7,6,12,8,0)
#' di.fun(X)
#'T<-c(61,72,83,94,85,46,77,68,129,80,10,12,12,3,4,5)
#'di.fun(T)
di.fun<-function(X){
  data.frame(di=var(X)/mean(X))
}
