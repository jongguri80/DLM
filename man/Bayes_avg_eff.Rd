\name{Bayes_avg_eff}
\alias{Bayes_avg_eff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayes_avg_eff uses the object from Bayes_dlm and calculates the average buffer effects up to a certain distance
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Bayes_avg_eff(Bayes_dlm_object, dist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Bayes_dlm_object}{
%%     ~~Describe \code{Bayes_dlm_object} here~~
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
function (Bayes_dlm_object, dist) 
{
    Lag = Bayes_dlm_object$Lag
    Const = LagFunc(Lag)
    L = length(Lag)
    n_post = nrow(Bayes_dlm_object$post_thetas)
    if (min(Lag) < dist) {
        idx_p = tail(which(Lag < dist), n = 1)
        wts = pi * (c(Lag[1:idx_p], dist)^2 - c(0, Lag[1:idx_p])^2)/(pi * 
            dist^2)
        tmp_beta = Bayes_DL_coeff_pred(Bayes_dlm_object, dist)$post_DL_coeff_pred
        avg_eff = rowSums(cbind(Bayes_dlm_object$post_DL_coeff[, 
            1:idx_p], tmp_beta) * matrix(wts, nrow = n_post, 
            ncol = idx_p + 1, byrow = T))
    }
    if (min(Lag) >= dist) 
        avg_eff = c(Bayes_DL_coeff_pred(Bayes_dlm_object, dist)$post_DL_coeff_pred)
    summary_avg_eff = quantile(avg_eff, prob = c(0.5, 0.025, 
        0.975))
    return(list(post_avg_eff = avg_eff, summary_avg_eff = summary_avg_eff))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
