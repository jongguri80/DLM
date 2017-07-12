\name{LagFunc}
\alias{LagFunc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LagFunc(Lag)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
function (Lag) 
{
    Lag_ct = Lag - mean(Lag)
    L = length(Lag)
    C0 = cbind(rep(1, L), Lag_ct)
    C1 = matrix(nrow = L, ncol = L)
    for (i in 1:L) for (j in 1:L) C1[i, j] = abs(Lag_ct[i] - 
        Lag_ct[j])^3
    qr_Q = qr.Q(qr(cbind(C0, C1)))
    M1 = qr_Q[, -c(1, 2)]
    M2 = t(M1) \%*\% C1 \%*\% M1
    inv_M2_sqrt = svd(M2)$v \%*\% diag(1/sqrt(svd(M2)$d)) \%*\% t(svd(M2)$u)
    Omg = cbind(C0, C1 \%*\% M1 \%*\% inv_M2_sqrt)
    return(list(Lag = Lag, C0 = C0, K1 = C1 \%*\% M1 \%*\% inv_M2_sqrt, 
        Omg = Omg, M1 = M1, inv_M2_sqrt = inv_M2_sqrt))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
