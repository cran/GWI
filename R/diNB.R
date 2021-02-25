#' @title Function for DInb
#' @description The function computes the negative binomial dispersion index for a given dispersion parameter \eqn{l\in (0,\infty)}.
#' @param X A count random variable
#' @param l The dispersion parameter of negative binomial distribution
#' @details
#' \code{dinb.fun} computes the dispersion index with respect to negative binomial distribution. See Touré et al. (2020) and Abid et al. (2021) for more details.
#' @importFrom stats var
#' @return Returns
#' \item{dinb}{The negative binomial dispersion index}
#' @author
#' Aboubacar Y. Touré and Célestin C. Kokonendji
#' @references
#' Abid, R.,Kokonendji, C.C. and Masmoudi, A. (2021). On Poisson-exponential-Tweedie models for ultra-overdispersed count data, \emph{AStA Advances in Statistical Analysis} \bold{105}, 1-23.\cr
#' \cr
#' Touré, A.Y., Dossou-Gbété, S. and Kokonendji, C.C. (2020). Asymptotic normality of the test statistics for relative dispersion and relative variation indexes, \emph{Journal of Applied Statistics} \bold{47}, 2479-2491.
#' @export dinb.fun
#'
#' @examples
#' X<-c(12,9,0,8,5,7,6,5,3,4,9,4)
#' dinb.fun(X,12)
#' Y<-c(0,6,1,3,4,2,5)
#' dinb.fun(Y,7)
dinb.fun<-function(X,l){
  data.frame(dinb=var(X)/(mean(X)*(1+mean(X)/l)))
}
