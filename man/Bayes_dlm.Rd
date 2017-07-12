\name{Bayes_dlm}
\alias{Bayes_dlm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fits a Bayesian distributed lag model.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Bayes_dlm(Y, X, Z, Lag, n_sim = 5000, n_save = 1000, burn_in = 2000, per_print = 1000, prior_list = list(a_tau = 0.1, b_tau = 1e-06, a_sigma_b = 0.1, b_sigma_b = 1e-06), int_list = list(thetas = rep(0, length(Lag) + ncol(Z)), sigma_b_sq = 0.1))
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
  \item{n_sim}{
%%     ~~Describe \code{n_sim} here~~
}
  \item{n_save}{
%%     ~~Describe \code{n_save} here~~
}
  \item{burn_in}{
%%     ~~Describe \code{burn_in} here~~
}
  \item{per_print}{
%%     ~~Describe \code{per_print} here~~
}
  \item{prior_list}{
%%     ~~Describe \code{prior_list} here~~
}
  \item{int_list}{
%%     ~~Describe \code{int_list} here~~
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
function (Y, X, Z, Lag, n_sim = 5000, n_save = 1000, burn_in = 2000, 
    per_print = 1000, prior_list = list(a_tau = 0.1, b_tau = 1e-06, 
        a_sigma_b = 0.1, b_sigma_b = 1e-06), int_list = list(thetas = rep(0, 
        length(Lag) + ncol(Z)), sigma_b_sq = 0.1)) 
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
    n_thetas = n_fix + n_random
    n = length(Y)
    idx_ran = (n_fix + 1):n_thetas
    thinning = floor((n_sim - burn_in)/n_save)
    save = kk = 1
    post_tau_sq = post_sigma_b_sq = post_lb = rep(0, n_save)
    post_thetas = matrix(0, nrow = n_save, ncol = n_thetas)
    thetas = int_list$thetas
    sigma_b_sq = int_list$sigma_b_sq
    G = diag(c(rep(0, n_fix), rep(1, n_random)))
    XX = t(D_star) \%*\% D_star
    XY = t(D_star) \%*\% Y
    for (s in 1:n_sim) {
        res = Y - D_star \%*\% thetas
        tau_sq = 1/rgamma(1, prior_list$a_tau + n/2, t(res) \%*\% 
            res/2 + prior_list$b_tau)
        Sigma = solve(XX/tau_sq + G/sigma_b_sq)
        mu = Sigma \%*\% (XY/tau_sq)
        thetas = t(chol(Sigma)) \%*\% rnorm(n_thetas) + mu
        sigma_b_sq = 1/rgamma(1, prior_list$a_sigma_b + (L - 
            2)/2, prior_list$b_sigma_b + t(thetas) \%*\% G \%*\% 
            thetas/2)
        if (kk == s/per_print) {
            print(paste("iteration #:", s))
            kk = kk + 1
        }
        if (s > burn_in && s\%\%thinning == 0 && save <= n_save) {
            res = Y - D_star \%*\% thetas
            post_thetas[save, ] = thetas
            post_tau_sq[save] = tau_sq
            post_sigma_b_sq[save] = sigma_b_sq
            post_lb[save] = n * log(tau_sq) + t(res) \%*\% res/tau_sq + 
                n * log(2 * pi)
            save = save + 1
        }
    }
    thetas_bar = colMeans(post_thetas)
    tau_sq_bar = mean(post_tau_sq)
    res = Y - D_star \%*\% thetas_bar
    D_bar = mean(post_lb)
    D_hat = n * log(tau_sq_bar) + t(res) \%*\% res/tau_sq_bar + 
        n * log(2 * pi)
    DIC = 2 * D_bar - D_hat
    summary_thetas = matrix(nrow = n_thetas, ncol = 3)
    for (i in 1:n_thetas) summary_thetas[i, ] = quantile(post_thetas[, 
        i], prob = c(0.5, 0.025, 0.975))
    rownames(summary_thetas) = c(paste("Z", 1:n_z, sep = ""), 
        "X_star1", "X_star2", paste("b", 1:n_random, sep = ""))
    colnames(summary_thetas) = c("50\%", "2.5\%", "97.5\%")
    post_betas_Z = post_thetas[, 1:n_z]
    summary_betas_Z = summary_thetas[1:n_z, ]
    post_DL_coeff = matrix(nrow = n_save, ncol = L)
    for (s in 1:n_save) post_DL_coeff[s, ] = Const$Omg \%*\% post_thetas[s, 
        -(1:ncol(Z))]
    summary_DL_coeff = matrix(nrow = L, ncol = 3)
    for (i in 1:L) summary_DL_coeff[i, ] = quantile(post_DL_coeff[, 
        i], prob = c(0.5, 0.025, 0.975))
    rownames(summary_DL_coeff) = c(Const$Lag)
    colnames(summary_DL_coeff) = c("50\%", "2.5\%", "97.5\%")
    return(list(post_thetas = post_thetas, summary_thetas = summary_thetas, 
        post_betas_Z = post_betas_Z, summary_betas_Z = summary_betas_Z, 
        post_tau_sq = post_tau_sq, post_sigma_b_sq = post_sigma_b_sq, 
        post_DL_coeff = post_DL_coeff, summary_DL_coeff = summary_DL_coeff, 
        DIC = DIC, Lag = Const$Lag))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
