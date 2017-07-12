\name{plot.dlm}
\alias{plot.dlm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
This function plots the estimated distributed lag effects from dlm_object or Bayes_dlm_object.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.dlm(dlm_object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dlm_object}{
%%     ~~Describe \code{dlm_object} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (dlm_object, ...) 
{
    DL_coeff = dlm_object$summary_DL_coeff[, 1]
    DL_coeff_LCI = dlm_object$summary_DL_coeff[, 2]
    DL_coeff_UCI = dlm_object$summary_DL_coeff[, 3]
    ylim = c(min(dlm_object$summary_DL_coeff), max(dlm_object$summary_DL_coeff))
    Lag = rep(dlm_object$Lag, 3)
    plot(Lag, c(DL_coeff, DL_coeff_LCI, DL_coeff_UCI), ylab = "DL coefficients", 
        type = "n", lwd = 2, ...)
    lines(dlm_object$Lag, DL_coeff, lwd = 2)
    lines(dlm_object$Lag, DL_coeff_LCI, lty = 2, col = "grey", 
        lwd = 2)
    lines(dlm_object$Lag, DL_coeff_UCI, lty = 2, col = "grey", 
        lwd = 2)
    abline(h = 0, lty = 2, col = "2")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
