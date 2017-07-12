\name{dlm}
\alias{dlm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dlm(Y, X, Z, Lag)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{Z}{
%%     ~~Describe \code{Z} here~~
}
  \item{Lag}{
%%     ~~Describe \code{Lag} here~~
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
function (Y, X, Z, Lag) 
{
    L = length(Lag)
    Const = LagFunc(Lag)
    Y = as.matrix(Y)
    Z = as.matrix(Z)
    X = as.matrix(X)
    X_star = X \%*\% Const$C0
    Z_star = X \%*\% Const$K1
    D_star = cbind(Z, X_star, Z_star)
    n_z = ncol(Z)
    n_fix = ncol(X_star) + n_z
    n_random = ncol(Z_star)
    n = length(Y)
    sub = rep(1, n)
    fit = lme(Y ~ -1 + Z + X_star, random = list(sub = pdIdent(~Z_star - 
        1)))
    b1 = unlist(fit$coef$random)
    thetas = c(fit$coef$fixed, b1)
    sigma_b_sq = (fit$sigma * exp(unlist(fit$modelStruct)))^2
    tau_sq = fit$sigma^2
    G = diag(c(rep(0, n_fix), rep(1/sigma_b_sq, n_random)))
    cov_thetas = solve(t(D_star) \%*\% D_star/tau_sq + G)
    betas_Z = thetas[1:n_z]
    cov_betas_Z = cov_thetas[1:n_z, 1:n_z]
    summary_betas_Z = summary(fit)$tTable[1:n_z, ]
    cov_DL_coeff = Const$Omg \%*\% cov_thetas[-(1:n_z), -(1:n_z)] \%*\% 
        t(Const$Omg)
    DL_coeff = Const$Omg \%*\% thetas[-(1:n_z)]
    CI_DL_coeff = cbind(DL_coeff - 1.96 * sqrt(diag(cov_DL_coeff)), 
        DL_coeff + 1.96 * sqrt(diag(cov_DL_coeff)))
    summary_DL_coeff = cbind(DL_coeff, CI_DL_coeff)
    colnames(summary_DL_coeff) = c("est", "2.5\%", "97.5\%")
    rownames(DL_coeff) = rownames(cov_DL_coeff) = colnames(cov_DL_coeff) = rownames(summary_DL_coeff) = Const$Lag
    return(list(thetas = thetas, cov_thetas = cov_thetas, Table = summary(fit), 
        Lag = Const$Lag, betas_Z = betas_Z, cov_betas_Z = cov_betas_Z, 
        summary_betas_Z = summary_betas_Z, DL_coeff = DL_coeff, 
        cov_DL_coeff = cov_DL_coeff, summary_DL_coeff = summary_DL_coeff))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
