# DLM
Distributed Lag Model (DLM) to examine associations between the built environment and health.


####### Load nlme library which is used by some of the functions

library(nlme)

library(sp)


#################### Create built environment factors ####################
####### We assume that the user has to have already calculated DL covariate matrix and distance lag

set.seed(10)
par(mfrow = c(2,2))

####### Generate locations of outcome and locations of environmental factors (e.g., convenience stores)

n_X = 3000;n_Y = 1000

####### 3000 locations of environmental factors #######

pts_X = cbind(runif(n_X, -25, 75), runif(n_X, -25, 75)) 

####### 1000 locations of outcome #######

pts_Y = cbind(runif(n_Y, -25, 75), runif(n_Y, -25, 75)) 
colnames(pts_X) = colnames(pts_Y) = c("x", "y") 
head(pts_Y)
head(pts_X)

####### Black dots are locations of environmental factors and red dots are locations of outcome #######

plot(pts_X, cex=.5, pch=20)
points(pts_Y, col="red", cex = .5, pch=20)

####### A vector of distance lag #######

Lag = seq(from=0.1, to=10, length=100)

####### Generate DL covariates #######

X = DL_X(pts_Y, pts_X, Lag)

####### Generate individual factors, e.g., gender and age #######

Gender = rbinom(n_Y, 1, 0.5)
Age = rpois(n_Y, 65)

####### True DL coefficients assuming a normal kernel function #######

true_DL_coeff = 0.1*dnorm(Lag, sd=5/3)/dnorm(0, sd=5/3)
plot(Lag, true_DL_coeff, type="l", ylab = "DL coefficients", main="True DL coefficient over distance")

########## True variance of residual errors #######

true_tau_sq = 1

####### Generate residuals #######

e = rnorm(n_Y, 0, sd=sqrt(true_tau_sq))

####### Generate true outcome Y, using a below true function #######

Y = 0.5*Gender -0.5*Age + X %*% true_DL_coeff + e

####### Realign a covariate matrix for other fixed effects #######

Z = model.matrix( ~ Gender + Age)   

####### Fit a DLM in Frequentist framework #######

fm1 = dlm(Y, X, Z, Lag)

####### Summary for fixed effects of Z #######

fm1$summary_betas_Z 			

####### Summary for DL coefficients #######

head(fm1$summary_DL_coeff) 			

####### Fit DLM in Bayesian framework #######

Bm1 = Bayes_dlm(Y, X, Z, Lag)

####### Summary for fixed effects of Z #######

Bm1$summary_betas_Z  			

####### Summary for DL coefficients #######

head(Bm1$summary_DL_coeff) 			

####### Plot fitted models #######

plot.dlm(fm1, main="Estimated DL coefficients \n in a frequentist framework" )
plot.dlm(Bm1, main="Estimated DL coefficients \n in a Bayesian framework" )

####### Estimated DL coefficients at distance 2 #######

DL_coeff_pred(fm1, dist=2)
Bayes_DL_coeff_pred(Bm1, dist=2)$summary_DL_coeff_pred

####### Estimated DL coefficients at distance 2.34 #######

DL_coeff_pred(fm1, dist=2.34)
Bayes_DL_coeff_pred(Bm1, dist=2.34)$summary_DL_coeff_pred

####### Estimated average effects up to distance 2 #######

Avg_eff(fm1, dist=2)
Bayes_avg_eff(Bm1, dist=2)$summary_avg_eff

####### Estimated average effects up to distance 2.34 #######

Avg_eff(fm1, dist=2.34)
Bayes_avg_eff(Bm1, dist=2.34)$summary_avg_eff

