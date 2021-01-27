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
MDI<-function(data){
  tmp_E=aux_esp(data)
  tmp_D=aux_diag_var(data)
  res=sqrt(tmp_E)%*%tmp_D%*%sqrt((tmp_E))/(tmp_E%*%(tmp_E))
  return(res)
}
GDI<-function(data){
  tmp_E=aux_esp(data)
  tmp_C=cov(data)
  res=sqrt(tmp_E)%*%tmp_C%*%sqrt((tmp_E))/(tmp_E%*%(tmp_E))
  return(res)
}
#' @title Function for GDI and MDI
#' @description The function computes the GDI and MDI indexes for multivariate count data.
#' @param Y  a matrix of count random variables
#' @details
#'\code{gmdi.fun} computes GDI and MDI indexes introduced by Kokonendji and Puig (2018).
#' @importFrom stats var cov
#' @return Returns a vector containing:
#'\item{gdi}{The generalized dispersion index}
#'\item{mdi}{The marginal dispersion index}
#' @author
#' Aboubacar Y. Touré and Célestin C. Kokonendji
#' @export gmdi.fun
#' @references
#' Kokonendji, C.C. and Puig, P. (2018). Fisher dispersion index for multivariate count distributions : A review and a new proposal, \emph{Journal of Multivariate Analysis} \bold{165}, 180-193.
#' @examples
#' Y<-cbind(c(1,2,3,4,5,6,7,8),c(1,2,3,4,5,6,7,8))
#' gmdi.fun(Y)
#' Z<-cbind(c(1,2,3,4,5,6,7,8),c(1,2,3,4,5,6,7,8),c(1,2,3,4,5,6,7,8),c(1,2,3,4,5,6,7,8))
#' gmdi.fun(Z)
gmdi.fun<-function(Y){
  data.frame(gdi=GDI(Y),mdi=MDI(Y))
}
