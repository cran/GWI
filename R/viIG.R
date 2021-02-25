#' @title Function for VIiG
#' @description The function computes the inverse Gaussian variation index with shape parameter \eqn{l\in (0,\infty)}.
#' @param X A positive continuous random variable
#' @param l The shape parameter of the inverse Gaussian distribution
#' @details
#' \code{viiG.fun} computes the variation index with respect to the inverse Gaussian distribution. See Touré et al. (2020) for more details.
#' @importFrom stats var
#' @return Returns
#' \item{viiG}{The inverse Gaussian variation index}
#' @author
#' Aboubacar Y. Touré and Célestin C. Kokonendji
#' @references
#' Touré, A.Y., Dossou-Gbété, S. and Kokonendji, C.C. (2020). Asymptotic normality of the test statistics for relative dispersion and relative variation indexes, \emph{Journal of Applied Statistics} \bold{47}, 2479-2491.
#' @export viiG.fun
#'
#' @examples
#' X<-c(0.12,9.11,0.03,8.71,5.02,7.12,6.42,5.73)
#' viiG.fun(X,0.05)
#' Y<-c(0.003,6.283,1.001,3.112,4.342,2.890,5.005)
#' viiG.fun(Y,0.3)
viiG.fun<-function(X,l){
  data.frame(viIG=(l^2*var(X))/mean(X)^3)
}
