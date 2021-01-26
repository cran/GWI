aux_esp<-function(data){
  res<-c()
  n=dim(data)
  n=n[2]
  for (i in 1:n){
    res[i]=mean(data[,i])
  }
  return(res)
}
aux_var<-function(data){
  res<-c()
  n=dim(data)
  n=n[2]
  for (i in 1:n){
    res[i]=var(data[,i])
  }
  return(res)
}
aux_diag_var<-function(data){
  res<-c()
  n=dim(data)
  n=n[2]
  for (i in 1:n){
    res[i]=var(data[,i])
  }
  return(diag(res))
}

MVI<-function(data){
  tmp_E=aux_esp(data)
  tmp_D=aux_diag_var(data)
  res=(tmp_E%*%tmp_D%*%(tmp_E))/(tmp_E%*%(tmp_E))^2
  return(res)
}

GVI<-function(data){
  tmp_E=aux_esp(data)
  tmp_C=cov(data)
  res=(tmp_E%*%tmp_C%*%(tmp_E))/(tmp_E%*%(tmp_E))^2
  return(res)
}
#' @title Function for GVI and MVI
#' @description The function computes GVI and MVI indexes for multivariate positive continuous data.
#' @param Y  a matrix of positive continuous random variables
#' @details
#'gmvi.fun computes the GVI and MVI indexes defined in Kokonendji et al. (2020).
#' @importFrom stats cov
#' @return Returns a vector containing :\cr
#' \cr
#' GVI : The generalized variation index\cr
#'\cr
#'MVI : The marginal variation index
#' @author
#' Aboubacar Y. Touré and Célestin C. Kokonendji
#' @export
#' @references
#' Kokonendji, C.C., Touré, A.Y. and Sawadogo, A. (2020). Relative variation indexes for multivariate continuous distributions on \eqn{[0,\infty)^k} and extensions, \emph{AStA Advances in Statistical Analysis} \bold{104}, 285-307.
#' @examples
#' Y<-cbind(c(2.3 ,26.1 ,8.7 ,10.9 ,1.2,1.4),c(9.7 ,7.3,9.3 ,9.4 ,10.5 ,9.8))
#' gmvi.fun(Y)
#' Z<-cbind(c(2.3 ,26.1 ,8.7),c(9.7 ,7.3,9.3),c(9.7 ,7.3,9.3),c(9.7 ,7.3,9.3))
#' gmvi.fun(Z)
gmvi.fun<-function(Y){
  data.frame(GVI=GVI(Y),MVI=MVI(Y))
}
