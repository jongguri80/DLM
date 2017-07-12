\name{Bayes_DL_coeff_pred}
\alias{Bayes_DL_coeff_pred}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayes_DL_coeff_pred uses Bayes_dlm_object and predicts the DL effect at a specific distance.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Bayes_DL_coeff_pred(Bayes_dlm_object, dist)
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
    L = length(Lag)
    n_post = nrow(Bayes_dlm_object$post_thetas)
    Const = LagFunc(Lag)
    Lag_ct = Lag - mean(Lag)
    Lag_ct_pred = dist - mean(Lag)
    C0_pred = cbind(1, Lag_ct_pred)
    C1_pred = matrix(nrow = 1, ncol = L)
    n_z = ncol(Bayes_dlm_object$post_betas_Z)
    idx_lag = (n_z + 1):(n_z + L)
    for (j in 1:L) C1_pred[1, j] = abs(Lag_ct[j] - Lag_ct_pred)^3
    Omg_pred = cbind(C0_pred, C1_pred \%*\% Const$M1 \%*\% Const$inv_M2_sqrt)
    post_DL_coeff_pred = matrix(nrow = n_post, ncol = 1)
    for (i in 1:n_post) post_DL_coeff_pred[i, ] = Omg_pred \%*\% 
        Bayes_dlm_object$post_thetas[i, idx_lag]
    summary_DL_coeff_pred = quantile(post_DL_coeff_pred, prob = c(0.5, 
        0.025, 0.975))
    return(list(post_DL_coeff_pred = post_DL_coeff_pred, summary_DL_coeff_pred = summary_DL_coeff_pred))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
