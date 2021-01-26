#' @title Function for VI
#' @description The function calculates the univariate exponential variation index for a positive continuous random variable.
#' @param X a positive continuous random variable
#' @details
#' vi.fun computes the univariate exponential variation index defined by Abid et al. (2020). See also Touré et al. (2020) for more details.
#' @importFrom stats var
#' @return VI (The exponential variation index)
#' @author
#' Aboubacar Y. Touré and Célestin C. Kokonendji
#' @references
#' Abid, R., Kokonendji, C.C. and Masmoudi, A. (2020). Geometric Tweedie regression models for continuous and semicontinuous data with variation phenomenon, \emph{AStA Advances in Statistical Analysis} \bold{104}, 33-58.\cr
#' \cr
#' Touré, A.Y., Dossou-Gbété, S. and Kokonendji, C.C. (2020). Asymptotic normality of the test statistics for relative dispersion and relative variation indexes, \emph{Journal of Applied Statistics} \bold{47}, 2479-2491.
#' @export
#'
#' @examples
#' X<-c(6.23,7.02,8.94,9.56,8.01,4.34,7.44,6.66,12.72,8.34,0)
#' vi.fun(X)
#' T<-c(6.231,7.022,8.943,9.789,8.014,4.423)
#' vi.fun(T)
vi.fun<-function(X){
  data.frame(VI=var(X)/mean(X)^2)
}
