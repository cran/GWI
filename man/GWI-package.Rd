\encoding{UTF-8}
\name{GWI-package}
\alias{GWI-package}
\alias{GWI}
\docType{package}
\title{
 Count and continuous generalized variability indexes
}
\description{
 Univariate Poisson dispersion index \code{\link{di.fun}}, univariate exponential variation index \code{\link{vi.fun}} functions are performed. Also, the generalized dispersion index and the marginal dispersion index \code{\link{gmdi.fun}} functions are carried out. Finally, the generalized variation index and the marginal variation index \code{\link{gmvi.fun}} functions are displayed.
}



\details{

\describe{
\item{The univariate Poisson dispersion index (DI):}{The Poisson dispersion phenomenon is well-known and very widely used in practice; see, e.g., Kokonendji (2014) for a review of count (or discrete integer-valued) models. There are many interpretable mechanisms leading to this phenomenon which makes it possible to classify count distributions and make inference; see, e.g., Mizère et al. (2006) and Touré et al. (2020) for approximative statistical tests. Introduced from Fisher (1934), the Poisson dispersion index,  also called the Fisher dispersion index, of a count random variable \eqn{X} on \eqn{\{0,1,2,\ldots\}} can be defined as \deqn{DI(X)=\frac{VarX}{EX},} the ratio of variance to mean.
    }
}

\describe{
\item{The univariate variation index (VI):}{More recently, Abid et al. (2020) have introduced the exponential variation index, so-called Jorgensen variation index, for positive continuous random variable \eqn{X} on \eqn{[0,\infty)} as \deqn{VI(X)=\frac{VarX}{(EX)^2}.} It can be viewed as the squared coefficient of variation. It is used in the framework of reliability to discriminate distribution of increasing/decreasing failure rate on the average (IFRA/DFRA); see, e.g., Barlow and Proschan (1981) in the sense of the coefficient of variation. See also Touré et al. (2020) for more details.
   }
}

Next, consider the following notations. Let \eqn{Y} = \eqn{(Y_1,\ldots,Y_k)^{\top}} be a nondegenerate count or continuous \eqn{k}-variate random vector, \eqn{k\ge 1}. Let also \eqn{EY} be the mean vector of \eqn{Y} and \eqn{covY}= \eqn{(cov(Y_i,Y_j) )_{i,j\in \{1,\ldots,k\}}} the covariance matrix of \eqn{Y}.

	\describe{

	        \item{The generalized dispersion index (GDI) and marginal dispersion index (MVI):}{Kokonendji and Puig (2018) have introduced the \emph{generalized dispersion index} for count vector \eqn{Y} on \eqn{\{0,1,2,\ldots\}^k} by

\deqn{GDI(Y) =\frac{\sqrt{EY}^{\top} ( covY)\sqrt{EY}}{EY^{\top}EY}.}
Note that when \eqn{k=1}, \eqn{GDI(Y)} is just the classical Fisher dispersion index DI.
\eqn{GDI}(\eqn{Y}) makes it possible to compare the full variability of \eqn{Y} (in the numerator) with respect to its expected uncorrelated Poissonian variability (in the denominator) which depends only on \eqn{EY}. \eqn{GDI(Y)} takes into account the correlation between variables.
For only taking into account the dispersion information coming from the margins, the authors defined the "marginal dispersion index":
\deqn{MDI(Y) = \frac{\sqrt{EY}^{\top}( diag varY )\sqrt{EY}}{EY^{\top}EY}=\sum_{j=1}^k\frac{\{E(Y_j)\}^2}{EY^{\top}EY} DI(Y_j).}}

}
\describe{
  \item{The generalized variation index (GVI) and marginal variation index (MVI):}{Similarly, Kokonendji et al. (2020) defined the \emph{generalized variation index} for positive continuous vector \eqn{Y} on \eqn{[0, \infty)^k} by
\deqn{GVI(Y) =\frac{EY^{\top} ( covY) EY}{(EY^{\top}EY)^2}.}
Remark that when \eqn{k=1}, \eqn{GVI} is the univariate variation index VI.
\eqn{GVI(Y)} makes it possible to compare the full variability of \eqn{Y} (in the numerator) with respect to its expected uncorrelated exponential variability (in the denominator) which depends only on \eqn{EY}. Also, \eqn{GVI(Y)} takes into account the correlation between variables.
To only take into account the variation information coming from the margins, Kokonendji et al. (2020) defined the "marginal variation index":
\deqn{MVI(Y) = \frac{EY^{\top}( diag varY )EY}{(EY^{\top}EY)^2}=\sum_{j=1}^k\frac{(EY_j)^4}{(EY^{\top}EY)^2} VI(Y_j).
}
 }
  }

}

\author{

Aboubacar Y. Touré and Célestin C. Kokonendji

Maintainer: Aboubacar Y. Touré <aboubacaryacoubatoure.ussgb@gmail.com>

}
\references{

 Abid, R., Kokonendji, C.C. and Masmoudi, A. (2020). Geometric Tweedie regression models for continuous and semicontinuous data with variation phenomenon, \emph{AStA Advances in Statistical Analysis} \bold{104}, 33-58.


  Barlow, R.A. and Proschan, F. (1981). Statistical Theory of Reliability and Life Testing : Probability Models, \emph{Silver Springs}, Maryland.


  Fisher, R.A. (1934). The effects of methods of ascertainment upon the estimation of frequencies, \emph{Annals of Eugenics} \bold{6}, 13-25.


  Kokonendji, C.C., Over- and underdispersion models. In: N. Balakrishnan (Ed.) The Wiley Encyclopedia of Clinical Trials- Methods and Applications of Statistics in Clinical Trials, \bold{Vol.2} (Chap.30), pp. 506-526. \emph{Wiley}, New York (2014).


  Kokonendji, C.C. and Puig, P. (2018). Fisher dispersion index for multivariate count distributions : A review and a new proposal, \emph{Journal of Multivariate Analysis} \bold{165}, 180-193.


  Kokonendji, C.C., Touré, A.Y. and Sawadogo, A. (2020). Relative variation indexes for multivariate continuous distributions on \eqn{[0,\infty)^k} and extensions, \emph{AStA Advances in Statistical Analysis} \bold{104}, 285-307.


  Mizère, D., Kokonendji, C.C. and Dossou-Gbété, S. (2006). Quelques tests de la loi de Poisson contre des alternatives géenérales basées sur l'indice de dispersion de Fisher, \emph{Revue de Statistique Appliquée} \bold{54}, 61-84.


Touré, A.Y., Dossou-Gbété, S. and Kokonendji, C.C. (2020). Asymptotic normality of the test statistics for relative dispersion and relative variation indexes, \emph{Journal of Applied Statistics} \bold{47}, 2479-2491.


}

\keyword{ package }

