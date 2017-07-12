\name{DL_coeff_pred}
\alias{DL_coeff_pred}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DL_coeff_pred(dlm_object, dist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dlm_object}{
%%     ~~Describe \code{dlm_object} here~~
}
  \item{dist}{
%%     ~~Describe \code{dist} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dlm_object, dist) 
{
    Lag = dlm_object$Lag
    L = length(Lag)
    Const = LagFunc(Lag)
    Lag_ct = Lag - mean(Lag)
    Lag_ct_pred = dist - mean(Lag)
    C0_pred = cbind(1, Lag_ct_pred)
    C1_pred = matrix(nrow = 1, ncol = L)
    n_z = length(dlm_object$betas_Z)
    idx_lag = (n_z + 1):(n_z + L)
    for (j in 1:L) C1_pred[1, j] = abs(Lag_ct[j] - Lag_ct_pred)^3
    Omg_pred = cbind(C0_pred, C1_pred \%*\% Const$M1 \%*\% Const$inv_M2_sqrt)
    DL_coeff_pred = Omg_pred \%*\% dlm_object$thetas[idx_lag]
    var_DL_coeff_pred = Omg_pred \%*\% dlm_object$cov_thetas[-(1:n_z), 
        -(1:n_z)] \%*\% t(Omg_pred)
    CI_DL_coeff_pred = c(DL_coeff_pred - 1.96 * sqrt(var_DL_coeff_pred), 
        DL_coeff_pred + 1.96 * sqrt(var_DL_coeff_pred))
    summary_DL_coeff_pred = c(DL_coeff_pred, CI_DL_coeff_pred)
    names(summary_DL_coeff_pred) = c("est", "2.5\%", "97.5\%")
    return(list(DL_coeff_pred = DL_coeff_pred, var_DL_coeff_pred = var_DL_coeff_pred, 
        summary_DL_coeff_pred = summary_DL_coeff_pred))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
