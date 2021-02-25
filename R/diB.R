#' @title Function for DIb
#' @description The function computes the binomial dispersion index for a given number of trials \eqn{N\in \{1,2,\ldots\}}.
#' @param X A count random variable
#' @param N The number of trials of binomial distribution
#' @details
#' \code{dib.fun} computes the dispersion index with respect to the binomial distribution. See Touré et al. (2020) and Weiss (2018) for more details.
#' @importFrom stats var
#' @return Returns
#' \item{dib}{The binomial dispersion index}
#' @author
#' Aboubacar Y. Touré and Célestin C. Kokonendji
#' @references
#' Touré, A.Y., Dossou-Gbété, S. and Kokonendji, C.C. (2020). Asymptotic normality of the test statistics for relative dispersion and relative variation indexes, \emph{Journal of Applied Statistics} \bold{47}, 2479-2491.\cr
#' \cr
#' Weiss, C.H. (2018). An Introduction to Discrete-Valued Times Series. \emph{Wiley}, Hoboken NJ.
#' @export dib.fun
#'
#' @examples
#' X<-c(12,9,0,8,5,7,6,5,3,4,9,4)
#' dib.fun(X,12)
#' Y<-c(0,0,1,1,0,1,1)
#' dib.fun(Y,7)
dib.fun<-function(X,N){

  data.frame(dib=var(X)/(mean(X)*(1-mean(X)/N)))
}
