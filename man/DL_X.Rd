\name{DL_X}
\alias{DL_X}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
This function generate distributed lag covariates
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DL_X(pts_outcome, pts_X, Lag, unit = "ED")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pts_outcome}{
%%     ~~Describe \code{pts_outcome} here~~
}
  \item{pts_X}{
%%     ~~Describe \code{pts_X} here~~
}
  \item{Lag}{
%%     ~~Describe \code{Lag} here~~
}
  \item{unit}{
%%     ~~Describe \code{unit} here~~
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
function (pts_outcome, pts_X, Lag, unit = "ED") 
{
    pts_outcome = as.matrix(pts_outcome)
    pts_X = as.matrix(pts_X)
    n = nrow(pts_outcome)
    a_mile = 1.609344
    L = length(Lag)
    X = matrix(nrow = n, ncol = L)
    for (i in 1:n) {
        if (unit == "ED") 
            dist = spDistsN1(pts_X, pts_outcome[i, ])
        if (unit == "km") 
            dist = spDistsN1(pts_X, pts_outcome[i, ], longlat = TRUE)
        if (unit == "mile") 
            dist = spDistsN1(pts_X, pts_outcome[i, ], longlat = TRUE)/a_mile
        X[i, 1] = sum(dist <= Lag[1])
        for (j in 2:L) X[i, j] = sum(dist > Lag[j - 1] & dist <= 
            Lag[j])
    }
    return(X)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
