tol <- 0.01

###################################################################
# 1.3 Reproduce loglik, parameter estimates and covariance on page 85
#    of Coles. Will not be exact because fitting routines differ:
#    gpd works with phi=log(sigma). Can only reproduce cell [2,2]
#    of covariance.

cparas <- c(7.44, 0.184)
cse <- c(0.958432, 0.101151)

ccov <- matrix(c(.9188, -.0655, -.0655, .0102), nrow=2)
cloglik <- -485.1

mod <- evm(rain, th=30, penalty="none")
mod.coef <- coef(mod)

mod.coef[1] <- exp(mod.coef[1])
names(mod.coef) <- c("sigma", "xi")

mod.loglik <- mod$loglik
mod.cov22 <- mod$cov[2, 2]

# equals function checks for names. is_equivalent_to doesn't, but doesn't take
# a tolerance argument.
names(cparas) <- names(cse) <- names(mod$se) <- c("sigma", "xi")

expect_equal(cparas, mod.coef, tolerance=tol,
             info="gpd: parameter ests page 85 Coles")
expect_equal(cse[2], mod$se[2], tolerance=tol,
             info="gpd: standard errors page 85 Coles")
expect_equal(ccov[2, 2], mod$cov[2,2], tolerance=tol,
             info="gpd: Covariance page 85 Coles")
expect_equal(cloglik, mod.loglik, tolerance=tol,
             info="gpd: loglik page 85 Coles")

###################################################################
#   Logical checks on the effect of Gaussian penalization. The smaller the
#    variance, the more the parameter should be drawn towards the
#    mean.

# 2.1 Tests for xi being drawn to 0

gp1 <- list(c(0, 0), diag(c(10^4, .25)))
gp2 <- list(c(0, 0), diag(c(10^4, .05)))

mod1 <- evm(rain, th=30, priorParameters=gp1, penalty="gaussian")
mod2 <- evm(rain, th=30, priorParameters=gp2, penalty="gaussian")

## gpd: Gaussian penalization xi being drawn to 0
expect_true(coef(mod)[2] > coef(mod1)[2])

## gpd: Gaussian penalization xi being drawn to 0
expect_true(coef(mod1)[2]>coef(mod2)[2])

# 2.2 Tests for phi being drawn to 0

gp3 <- list(c(0, 0), diag(c(1, 10^4)))
gp4 <- list(c(0, 0), diag(c(.1, 10^4)))

mod3 <- evm(rain, th=30, priorParameters=gp3, penalty="gaussian")
mod4 <- evm(rain, th=30, priorParameters=gp4, penalty="gaussian")

## gpd: Gaussian penalization phi being drawn to 0
expect_true(coef(mod)[1]>coef(mod3)[1])

##gpd: Gaussian penalization phi being drawn to 0
expect_true(coef(mod3)[1]>coef(mod4)[1])

# 2.3 Tests for xi being drawn to 1
gp5 <- list(c(0, 1), diag(c(10^4, .25)))
gp6 <- list(c(0, 1), diag(c(10^4, .05)))

mod5 <- evm(rain, th=30, priorParameters=gp5, penalty="gaussian")
mod6 <- suppressWarnings(evm(rain, th=30, priorParameters=gp6, penalty="gaussian"))

## gpd: Gaussian penalization xi being drawn to 1
expect_true(1-coef(mod)[2]>1-coef(mod5)[2])

##gpd: Gaussian penalization xi being drawn to 1
expect_true(1-coef(mod1)[2]>1-coef(mod6)[2])

# 2.4 Tests for phi being drawn to 4 (greater than mle for phi)

gp7 <- list(c(4, 0), diag(c(1, 10^4)))
gp8 <- list(c(4, 0), diag(c(.1, 10^4)))

mod7 <- evm(rain, th=30, priorParameters=gp7, penalty="gaussian")
mod8 <- evm(rain, th=30, priorParameters=gp8, penalty="gaussian")

## gpd: Gaussian penalization phi being drawn to 4
expect_true(4-coef(mod)[1]>4-coef(mod7)[1])

## gpd: Gaussian penalization phi being drawn to 4
expect_true(4-coef(mod3)[1]>4-coef(mod8)[1])

###################################################################
#   Logical checks on the effect of penalization using lasso or L1 penalization.
#   The smaller the variance, the more the parameter should be drawn towards the
#    mean.

# 2a.1 Tests for xi being drawn to 0

gp1 <- list(c(0, 0), solve(diag(c(10^4, .25))))
gp2 <- list(c(0, 0), solve(diag(c(10^4, .05))))

mod1 <- evm(rain, th=30, priorParameters=gp1, penalty="lasso")
mod2 <- evm(rain, th=30, priorParameters=gp2, penalty="lasso")

## gpd: lasso penalization xi being drawn to 0
expect_true(coef(mod)[2]>coef(mod1)[2])

## gpd: lasso penalization xi being drawn to 0
expect_true(coef(mod1)[2]>coef(mod2)[2])

# 2a.2 Tests for phi being drawn to 0

gp3 <- list(c(0, 0), solve(diag(c(1, 10^4))))
gp4 <- list(c(0, 0), solve(diag(c(.1, 10^4))))

mod3 <- evm(rain, th=30, priorParameters=gp3, penalty="lasso")
mod4 <- evm(rain, th=30, priorParameters=gp4, penalty="lasso")

expect_true(coef(mod)[1]>coef(mod3)[1],
            info="gpd: lasso penalization phi being drawn to 0")
expect_true(coef(mod3)[1]>coef(mod4)[1],
            info="gpd: lasso penalization phi being drawn to 0")

# 2a.3 Tests for xi being drawn to 1
gp5 <- list(c(0, 1), solve(diag(c(10^4, .25))))
gp6 <- list(c(0, 1), solve(diag(c(10^4, .05))))

mod5 <- evm(rain, th=30, priorParameters=gp5, penalty="lasso")
mod6 <- evm(rain, th=30, priorParameters=gp6, penalty="lasso")

## gpd: lasso penalization xi being drawn to 1
expect_true(1-coef(mod)[2]>1-coef(mod5)[2])

## gpd: lasso penalization xi being drawn to 1
expect_true(1-coef(mod1)[2]>1-coef(mod6)[2])

# 2a.4 Tests for phi being drawn to 4 (greater than mle for phi)

gp7 <- list(c(4, 0), solve(diag(c(1, 10^4))))
gp8 <- list(c(4, 0), solve(diag(c(.1, 10^4))))

mod7 <- evm(rain, th=30, priorParameters=gp7, penalty="lasso")
mod8 <- evm(rain, th=30, priorParameters=gp8, penalty="lasso")

## gpd: lasso penalization phi being drawn to 4
expect_true(4-coef(mod)[1]>4-coef(mod7)[1])

## gpd: lasso penalization phi being drawn to 4
expect_true(4-coef(mod3)[1]>4-coef(mod8)[1])

########################################################
# Tests on including covariates. Once more, gpd.fit in ismev
# works with sigma inside the optimizer, so we need to tolerate
# some differences and standard errors might be a bit out.

# 3.0 Reproduce Coles, page 119. Reported log-likelihood is -484.6.

rtime <- (1:length(rain))/1000
d <- data.frame(rainfall = rain, time = rtime)

mod <- evm(rainfall, th=30, data = d, phi= ~ time, penalty="none")

expect_equal(-484.6, mod$loglik, tolerance=tol,
             info="gpd: loglik Coles page 119")

####################################################################
# 3.1 Use liver data, compare with ismev.
#     These are not necessarily sensible models!
#     Start with phi alone.

mod <- evm(ALT.M, qu=.7, data=liver,
           phi = ~ ALT.B + dose, xi = ~1,
           penalty="none", cov="observed")

m <- model.matrix(~ ALT.B + dose, liver)

.ismev.gpd.fit <- texmex:::.ismev.gpd.fit
ismod <- .ismev.gpd.fit(liver$ALT.M,
                        threshold=quantile(liver$ALT.M, .7),
                        ydat = m, sigl=2:ncol(m),
                        siglink=exp, show=FALSE)

expect_equal(ismod$mle, unname(coef(mod)), tolerance=tol,
             info="gpd: covariates in phi only, point ests")

# SEs for phi will not be same as for sigma, but we can test xi
expect_equal(ismod$se[length(ismod$se)],
             mod$se[length(mod$se)], tolerance=tol,
             info="gpd: covariates in phi only, standard errors")

######################################################################
# 3.2 Test xi alone.
mod <- evm(log(ALT.M / ALT.B), qu=.7, data=liver,
           phi = ~1, xi = ~ ALT.B + dose,
           penalty="none")

m <- model.matrix(~ ALT.B + dose, liver)

ismod <- .ismev.gpd.fit(log(liver$ALT.M / liver$ALT.B),
                        threshold=quantile(log(liver$ALT.M / liver$ALT.B), .7),
                        ydat = m, shl=2:ncol(m), show=FALSE)
mco <- coef(mod)
mco[1] <- exp(mco[1])

expect_equal(ismod$mle, unname(mco), tolerance=tol,
             info="gpd: covariates in xi only: point ests")
# SEs for phi will not be same as for sigma, but we can test xi
expect_equal(ismod$se[-1], mod$se[-1], tolerance = tol,
             info="gpd: covariates in xi only: standard errors")

######################################################################
# 3.3 Test phi & xi simultaneously. Use simulated data.

set.seed(25111970)

makeDataGpd <- function(a,b,n=500,u=10)
  # lengths of a and b should divide n exactly
  # returns data set size 2n made up of uniform variates (size n) below threshold u and
  # gpd (size n) with scale parameter exp(a) and shape b above threshold u
{
  gpd <- rgpd(n,exp(a),b,u=u)
  unif <- runif(n,u-10,u)
  as.data.frame(cbind(a=a,b=b,y=c(gpd,unif)))
}

mya <- seq(0.1,1,len=10)
myb <- rep(c(-0.2,0.2),each=5)
data <- makeDataGpd(mya,myb)
m <- model.matrix(~ a+b, data)

mod <- evm(y,qu=0.7,data=data,phi=~a,xi=~b,penalty="none")
ismod <- .ismev.gpd.fit(data$y,
                        threshold=quantile(data$y,0.7),
                        ydat=m,shl=3,sigl=2,
                        siglink=exp,
                        show=FALSE)

expect_equal(ismod$mle, unname(coef(mod)), tolerance = tol,
             info="gpd: covariates in phi and xi: point ests")
expect_equal(ismod$se, sqrt(diag(mod$cov)), tolerance = tol,
             info="gpd: covariates in phi and xi: std errs")

####################################################################
# Check that using priors gives expected behaviour when covariates are included.

# 2.1 Tests for xi being drawn to 0

myb <- rep(c(0.5,1.5),each=5)
data <- makeDataGpd(a=1,b=myb,n=3000)

gp1 <- list(c(0, 0, 0), diag(c(10^4, 0.25, 0.25)))
gp2 <- list(c(0, 0, 0), diag(c(10^4, 0.25, 0.01)))

mod0 <- evm(y,qu=0.6,data=data,phi=~1,xi=~b,penalty="none")
mod1 <- evm(y,qu=0.6,data=data,phi=~1,xi=~b,priorParameters=gp1)
mod2 <- suppressWarnings(evm(y,qu=0.6,data=data,phi=~1,xi=~b,priorParameters=gp2))

## gpd: with covariates, xi drawn to zero
expect_true(all(abs(coef(mod0)[2:3])>abs(coef(mod1)[2:3])))

## gpd: with covariates, xi drawn to zero
expect_true(abs(coef(mod1)[3])>abs(coef(mod2)[3]))

# 2.2 Tests for phi being drawn to 0

# HS. Changed a to mya due to scoping problems in S+. The issue is very general
# and affects (for example) lm(~a, data, method="model.frame"), so it's kind of
# by design.
mya <- seq(0.1,1,len=10)
data <- makeDataGpd(-3 + mya,b=-0.1,n=3000)
data$a <- rep(mya, len=nrow(data))

gp4 <- list(c(0, 0, 0), diag(c(1, 1, 10^4)))
gp5 <- list(c(0, 0, 0), diag(c(0.1, 0.1, 10^4)))

mod3 <- evm(y,qu=0.6,data=data,phi=~a,xi=~1,penalty="none")
mod4 <- evm(y,qu=0.6,data=data,phi=~a,xi=~1,priorParameters=gp4)
mod5 <- evm(y,qu=0.6,data=data,phi=~a,xi=~1,priorParameters=gp5)

## gpd: with covariates, phi being drawn to 0
expect_true(all(abs(coef(mod3)[1:2])>abs(coef(mod4)[1:2])))

## gpd: with covariates, phi being drawn to 0
expect_true(all(abs(coef(mod4)[1:2])>abs(coef(mod5)[1:2])))

# 2.3 Tests for xi being drawn to 2
myb <- rep(c(-0.5,0.5),each=5)
data <- makeDataGpd(a=1,b=myb,n=3000)

gp7 <- list(c(0, 2, 2), diag(c(10^4, 0.25, 0.25)))
gp8 <- list(c(0, 2, 2), diag(c(10^4, 0.05, 0.05)))

mod6 <- evm(y,qu=0.6,data=data,phi=~1,xi=~b,penalty="none")
mod7 <- evm(y,qu=0.6,data=data,phi=~1,xi=~b,priorParameters=gp7)
mod8 <- evm(y,qu=0.6,data=data,phi=~1,xi=~b,priorParameters=gp8)

## gpd: with covariates, xi drawn to 2
expect_true(all(abs(2-coef(mod6)[2:3])>abs(2-coef(mod7)[2:3])))

## gpd: with covariates, xi drawn to 2
expect_true(all(abs(2-coef(mod7)[2:3])>abs(2-coef(mod8)[2:3])))

# 2.4 Tests for phi being drawn to 4

mya <- seq(0.1,1,len=10)
data <- makeDataGpd(2 + mya,b=-0.1,n=3000)
data$a <- rep(mya, len=nrow(data))

gp10 <- list(c(0, 4, 0), diag(c(10^4, 1,   10^4)))
gp11 <- list(c(0, 4, 0), diag(c(10^4, 0.1, 10^4)))

mod9 <- evm(y,qu=0.6,data=data,phi=~a,xi=~1,penalty="none")
mod10 <- evm(y,qu=0.6,data=data,phi=~a,xi=~1,priorParameters=gp10)
mod11 <- evm(y,qu=0.6,data=data,phi=~a,xi=~1,priorParameters=gp11)

expect_true(abs(4-coef(mod9)[2])>abs(4-coef(mod10)[2]),
            info="gpd: with covariates, phi drawn to 4")
expect_true(abs(4-coef(mod10)[2])>abs(4-coef(mod11)[2]),
            info="gpd: with covariates, phi drawn to 4")

#*************************************************************
# 4.1. Test reproducibility
set.seed(20101110)
save.seed <- .Random.seed

#set.seed(save.seed)
bmod <- evm(ALT.M, data=liver,
            th=quantile(liver$ALT.M, .7),
            iter=1000, thin=1, verbose=FALSE, method="sim")

#set.seed(save.seed)
set.seed(20101110)
bmod2 <- evm(ALT.M, data=liver,
             th=quantile(liver$ALT.M, .7),
             iter=1000, thin=1, verbose=FALSE, method="sim")

expect_equal(bmod$param, bmod2$param,
             info="gpd: test simulation reproducibility 1")

if (FALSE){
  set.seed(bmod$seed)
  evmSimSetSeed(bmod$seed)
  bmod3 <- evm(ALT.M, data=liver,
               th=quantile(liver$ALT.M, .7),
               iter=1000, thin=1, verbose=FALSE, method="sim")
  expect_equal(bmod$param, ebmod3$param,
               info="gpd: test simulation reproducibility 2")
}

#*************************************************************
# 4.2. Logical test of burn-in

expect_equal(nrow(bmod$chains[[1]]) - bmod$burn, nrow(bmod$param),
             info="gpd: Logical test of burn-in 1")

iter <- sample(500:1000,1)
burn <- sample(50,1)
bmod2 <- evm(ALT.M, data=liver, th=quantile(liver$ALT.M, .7),
             iter=iter, burn=burn, thin=1, verbose=FALSE, method="sim")

expect_equal(iter-burn, nrow(bmod2$param),
             info="gpd: Logical test of burn-in 2")

#*************************************************************
# 4.3. Logical test of thinning

thin <- 0.5
iter <- 1000
bmod <- evm(ALT.M, data=liver, th=quantile(liver$ALT.M, .7),
            iter=iter, thin = thin,verbose=FALSE, method="sim")

expect_equal((nrow(bmod$chains[[1]])-bmod$burn)*thin, nrow(bmod$param),
             info="gpd: Logical test of thinning 1")

thin <- 2
iter <- 1000
bmod <- evm(ALT.M, data=liver, th=quantile(liver$ALT.M, .7),
            iter=iter, thin = thin, verbose=FALSE, method="sim")

expect_equal((nrow(bmod$chains[[1]])-bmod$burn)/thin, nrow(bmod$param),
             info="gpd: Logical test of thinning 1")

#*************************************************************
# 5.1. Test of gev family: point estimates and cov matrices
coles <- c(3.87, .198, -.050) # From page 59 of Coles
mOpt <- evm(SeaLevel, data=portpirie, family=gev, penalty="none")
mSim <- evm(SeaLevel, data=portpirie, family=gev, method="sim",trace=100000)
coOpt <- coef(mOpt)
coSim <- coef(mSim)
coOpt[2] <- exp(coOpt[2])
coSim[2] <- exp(coSim[2])
# check point estimates
expect_equal(coles, unname(coOpt), tolerance=tol,
             info="gev: optimisation, parameter ests page 59 Coles")
expect_equal(coles, unname(coSim), tolerance=tol,
             info="gev: simulation, parameter ests page 59 Coles")

# Check non-sigma elements of covariance
coles <- matrix(c(.000780, -.00107,
                  -.00107, .00965), ncol=2)
coOpt <- mOpt$cov[c(1,3), c(1,3)]
coSim <- cov(mSim$param)[c(1,3), c(1,3)]
expect_equal(coles, coOpt, tolerance=tol,
             info="gev: optimisation, covariance page 59 coles")
expect_equal(coles, coSim, tolerance=tol,
             info="gev: simulation, covariance page 59 coles")
mcOpt <- max(abs(coles - coOpt))
mcSim <- max(abs(coles - coSim))
expect_equal(0, mcOpt, tolerance=tol,
             info="gev:optimisation,maxabscovariance")
expect_equal(0, mcSim, tolerance=tol,
             info="gev:simulation,maxabscovariance")

# Check log-likelihood
coles <- 4.34
co <- mOpt$loglik
expect_equal(coles, co, tolerance=tol,
             info="gev: optimisation, loglik page 59 coles")

###################################################################
# 5.2   GEV - Logical checks on the effect of Gaussian penalization. The smaller the
#    variance, the more the parameter should be drawn towards the
#    mean.

# Tests for xi being drawn to 0

gp1 <- list(c(0, 0, 0), diag(c(10^4, 10^4, .25)))
gp2 <- list(c(0, 0, 0), diag(c(10^4, 10^4, .05)))

mod1 <- evm(SeaLevel, data=portpirie, family=gev, priorParameters=gp1)
mod2 <- evm(SeaLevel, data=portpirie, family=gev, priorParameters=gp2)

expect_true(abs(coef(mOpt)[3])>abs(coef(mod1)[3]),
            info="gev: Gaussian penalization xi being drawn to 0")
expect_true(abs(coef(mOpt)[3])>abs(coef(mod2)[3]),
            info="gev: Gaussian penalization xi being drawn to 0")
expect_true(abs(coef(mod1)[3])>abs(coef(mod2)[3]),
            info="gev: Gaussian penalization xi being drawn to 0")

# Tests for phi being drawn to 0

gp3 <- list(c(0, 0, 0), diag(c(10^4, 1, 10^4)))
gp4 <- list(c(0, 0, 0), diag(c(10^4, .1, 10^4)))

mod3 <- evm(SeaLevel, data=portpirie, family=gev, priorParameters=gp3)
mod4 <- evm(SeaLevel, data=portpirie, family=gev, priorParameters=gp4)

expect_true(abs(coef(mOpt)[2])>abs(coef(mod3)[2]),
            info="gev: Gaussian penalization phi being drawn to 0")
expect_true(abs(coef(mOpt)[2])>abs(coef(mod4)[2]),
            info="gev: Gaussian penalization phi being drawn to 0")
expect_true(abs(coef(mod3)[2])>abs(coef(mod4)[2]),
            info="gev: Gaussian penalization phi being drawn to 0")

# Tests for xi being drawn to 1
gp5 <- list(c(0, 0, 1), diag(c(10^4, 10^4, .25)))
gp6 <- list(c(0, 0, 1), diag(c(10^4, 10^4, .05)))

mod5 <- evm(SeaLevel, data=portpirie, family=gev, priorParameters=gp5)
mod6 <- evm(SeaLevel, data=portpirie, family=gev, priorParameters=gp6)

expect_true(abs(1-coef(mOpt)[3])>abs(1-coef(mod5)[3]),
            info="gev: Gaussian penalization xi being drawn to 1")
expect_true(abs(1-coef(mOpt)[3])>abs(1-coef(mod6)[3]),
            info="gev: Gaussian penalization xi being drawn to 1")
expect_true(abs(1-coef(mod5)[3])>abs(1-coef(mod6)[3]),
            info="gev: Gaussian penalization xi being drawn to 1")

# Tests for phi being drawn to 0.5 (greater than mle for phi)

gp7 <- list(c(0, 0.5, 0), diag(c(10^4, 1, 10^4)))
gp8 <- list(c(0, 0.5, 0), diag(c(10^4, .1, 10^4)))

mod7 <- evm(SeaLevel, data=portpirie, family=gev, priorParameters=gp7)
mod8 <- evm(SeaLevel, data=portpirie, family=gev, priorParameters=gp8)

expect_true(abs(.5-coef(mOpt)[2])>abs(.5-coef(mod7)[2]),
            info="gpd: Gaussian penalization phi being drawn to .5")
expect_true(abs(.5-coef(mOpt)[2])>abs(.5-coef(mod8)[2]),
            info="gpd: Gaussian penalization phi being drawn to .5")

# test above but with lasso penalty

mod9 <- evm(SeaLevel, data=portpirie, family=gev, priorParameters=gp7,penalty="lasso")
mod10 <- evm(SeaLevel, data=portpirie, family=gev, priorParameters=gp8,penalty="lasso")

expect_true(abs(.5-coef(mOpt)[2])>abs(.5-coef(mod9)[2]),
            info="gpd: Gaussian lasso penalization phi being drawn to .5")
expect_true(abs(.5-coef(mOpt)[2])>abs(.5-coef(mod10)[2]),
            info="gpd: Gaussian lasso penalization phi being drawn to .5")

###################################################################
# 5.3  GEV - covariates in mu, phi and xi separately, use gev.fit from ismev package

set.seed(20130614)
makeDataGev <- function(u=0,a,b,n=500)
  # lengths of a and b should divide n exactly
  # returns data set size n distributed as GEV variates location parameter u,
  # scale parameter exp(a) and shape b
{
  gev <- rgev(n,mu=u,exp(a),b)
  as.data.frame(cbind(u=u,a=a,b=b,y=gev))
}

myu <- rnorm(10)
mya <- seq(0.1,1,len=10)
myb <- rep(c(-0.2,0.2),each=5)

data <- list(makeDataGev(myu,2,-0.1),
             makeDataGev(3,mya,-0.1),
             makeDataGev(3,2,myb))

.ismev.gev.fit <- texmex:::.ismev.gev.fit

start1 <- c(0,1,2,-0.1)
g1.fit <- .ismev.gev.fit(xdat=data[[1]]$y,ydat=data[[1]],mul=1, siglink=exp,show=FALSE,muinit=start1[1:2])
g2.fit <- .ismev.gev.fit(xdat=data[[2]]$y,ydat=data[[2]],sigl=2,siglink=exp,show=FALSE)
# siginit specified in call below. Otherwise, it converges to a bad location (as judged by looking at
# the likelihood from each fit)
g3.fit <- .ismev.gev.fit(xdat=data[[3]]$y,ydat=data[[3]],shl=3,siglink=exp, siginit=2, show=FALSE)

t1.fit <- evm(y,mu=~u, family=gev,data=data[[1]],start=start1)
t2.fit <- evm(y,phi=~a,family=gev,data=data[[2]])
t3.fit <- evm(y,xi=~b, family=gev,data=data[[3]])

expect_equal(g1.fit$mle, unname(coef(t1.fit)), tolerance=tol, info="gev:covariatesinmupointest")
expect_equal(g2.fit$mle, unname(coef(t2.fit)), tolerance=tol, info="gev:covariatesinphipointest")
expect_equal(g3.fit$mle, unname(coef(t3.fit)), tolerance=tol, info="gev:covariatesinxipointest")

expect_equal(g1.fit$nllh, -t1.fit$loglik, tolerance=tol, info="gev:covariatesinmu,log-lik")
expect_equal(g2.fit$nllh, -t2.fit$loglik, tolerance=tol, info="gev:covariatesinphi,log-lik")
expect_equal(g3.fit$nllh, -t3.fit$loglik, tolerance=tol, info="gev:covariatesinxi,log-lik")

expect_equal(g1.fit$cov, t1.fit$cov, tolerance=tol, info="gev:covariatesinmu,cov")
expect_equal(g2.fit$cov, t2.fit$cov, tolerance=tol, info="gev:covariatesinphi,cov")
expect_equal(g3.fit$cov, t3.fit$cov, tolerance=tol, info="gev:covariatesinxi,cov")

######################################################################
# 5.4 GEV - Test mu phi & xi simultaneously. Use simulated data.

set.seed(25111970)

data <- makeDataGev(5+2*myu,2-0.5*mya,0.1+2*myb)
data$u <- myu
data$a <- mya
data$b <- myb

mod <- evm(y,data=data,mu=~u,phi=~a,xi=~b,penalty="none",family=gev,start=c(5,2,2,-0.1,0,2))
ismod <- .ismev.gev.fit(data$y,
                        ydat=data,mul=1,shl=3,sigl=2,
                        siglink=exp,
                        show=FALSE,muinit=c(5,2),siginit=c(2,-0.1),shinit=c(0,2))

expect_equal(ismod$mle, unname(coef(mod)), tolerance=tol, info="gev:covariatesinmuphiandxi:pointests")
expect_equal(ismod$se, sqrt(diag(mod$cov)), tolerance=tol, info="gev:covariatesinmuphiandxi:stderrs")

####################################################################
# 5.5 GEV:  Check that using priors gives expected behaviour when covariates are included.

# Tests for xi being drawn to 0

myb <- rep(c(-0.1,0.1),each=5)
data <- makeDataGev(u=0,a=1,b=-0.5+myb,n=3000)
data$b <- myb

gp1 <- list(c(0, 0, 0, 0), diag(c(10^4, 10^4, 0.25, 0.25)))
gp2 <- list(c(0, 0, 0, 0), diag(c(10^4, 10^4, 0.25, 0.01)))

mod0 <- evm(y,family=gev,data=data,xi=~b,penalty="none")
mod1 <- evm(y,family=gev,data=data,xi=~b,priorParameters=gp1)
mod2 <- evm(y,family=gev,data=data,xi=~b,priorParameters=gp2)

expect_true(all(abs(coef(mod0)[3:4])>abs(coef(mod1)[3:4])),
            info="gev: with covariates, xi drawn to zero")
expect_true(abs(coef(mod1)[4])>abs(coef(mod2)[4]),
            info="gev: with covariates, xi drawn to zero")

# Tests for phi being drawn to 0

mya <- seq(0.1,1,len=10)
data <- makeDataGev(u=0,a=-3 + mya,b=-0.1,n=3000)
data$a <- rep(mya, len=nrow(data))

gp4 <- list(c(0, 0, 0, 0), diag(c(10^4, 1, 1, 10^4)))
gp5 <- list(c(0, 0, 0, 0), diag(c(10^4, 0.1, 0.1, 10^4)))

mod3 <- evm(y,family=gev,data=data,phi=~a,penalty="none")
mod4 <- evm(y,family=gev,data=data,phi=~a,priorParameters=gp4)
mod5 <- evm(y,family=gev,data=data,phi=~a,priorParameters=gp5)

expect_true(all(abs(coef(mod3)[2:3])>abs(coef(mod4)[2:3])),
            info="gev: with covariates, phi being drawn to 0")
expect_true(all(abs(coef(mod4)[2:3])>abs(coef(mod5)[2:3])),
            info="gev: with covariates, phi being drawn to 0")

# Tests for xi being drawn to 2
myb <- rep(c(-0.1,0.1),each=5)
data <- makeDataGev(u=0,a=1,b=myb,n=3000)

gp7 <- list(c(0, 0, 1, 1), diag(c(10^4, 10^4, 0.25, 0.25)))
gp8 <- list(c(0, 0, 1, 1), diag(c(10^4, 10^4, 0.05, 0.05)))

mod6 <- evm(y,family=gev,data=data,xi=~b,penalty="none")
mod7 <- evm(y,family=gev,data=data,xi=~b,priorParameters=gp7)
mod8 <- evm(y,family=gev,data=data,xi=~b,priorParameters=gp8)

expect_true(all(abs(1-coef(mod6)[3:4])>abs(1-coef(mod7)[3:4])),
            info="gev: with covariates, xi drawn to 1")
expect_true(all(abs(1-coef(mod7)[3:4])>abs(1-coef(mod8)[3:4])),
            info="gev: with covariates, xi drawn to 1")

# Tests for mu being drawn to 4

myu <- seq(0.1,1,len=10)
data <- makeDataGev(2 + myu,a=1,b=-0.1,n=3000)
data$u <- rep(myu, len=nrow(data))

gp10 <- list(c(4, 0, 0, 0), diag(c(1,   10^4, 10^4, 10^4)))
gp11 <- list(c(4, 0, 0, 0), diag(c(0.1, 10^4, 10^4, 10^4)))

mod9 <-  evm(y,family=gev,data=data,mu=~u,penalty="none")
mod10 <- evm(y,family=gev,data=data,mu=~u,priorParameters=gp10)
mod11 <- evm(y,family=gev,data=data,mu=~u,priorParameters=gp11)

expect_true(abs(4-coef(mod9)[1])>abs(4-coef(mod10)[1]),
            info="gev: with covariates, mu drawn to 4")
expect_true(abs(4-coef(mod10)[1])>abs(4-coef(mod11)[1]),
            info="gev: with covariates, mu drawn to 4")

####################################################################
# 6.1 gpdIntCensored:  Check that estimates for interval censored data approach those treating data as exact as reported precision of data increases

nReps <- 10
y.exact <- lapply(1:nReps,function(i)rgpd(100,sigma=1,xi=-0.1))
gpd.exact <- lapply(y.exact,function(x)evm(x,th=0))
nDP <- 4
DP <- seq(0,length=nDP)
gpd.cens <- lapply(y.exact,function(x)lapply(DP,function(dp)evm(round(x,digits=dp),th=0,family=gpdIntCensored)))

gpdCens.scale <- sapply(gpd.cens,function(x)sapply(x,function(y)y$par[1]))
gpdCens.shape <- sapply(gpd.cens,function(x)sapply(x,function(y)y$par[2]))
gpdExct.scale <- sapply(gpd.exact,function(x)x$par[1])
gpdExct.shape <- sapply(gpd.exact,function(x)x$par[2])

scaleCgce <- sapply(1:nDP,function(i)abs(mean(gpdCens.scale[i,] - gpdExct.scale)))
shapeCgce <- sapply(1:nDP,function(i)abs(mean(gpdCens.shape[i,] - gpdExct.shape)))

expect_true(all(diff(scaleCgce)<0), "gpdIntCensored")
expect_true(all(diff(shapeCgce)<0), "gpdIntCensored")

##############################################################################
## evmSim behaves as it should with multiple cores

pp <- list(c(0, 0, 0), diag(c(1e4, 1/16, 1/16)))

liver <- liver
liver$dose <- as.numeric(liver$dose) - 2

for (i in 1:3){
  set.seed(20200220)
  m1 <- evm(log(ALT.M / ALT.B), data = liver, qu = .7, xi = ~ dose,
            method = "sim", chains = i)

  set.seed(20200220)
  m2 <- evm(log(ALT.M / ALT.B), data = liver, qu = .7, xi = ~ dose,
            method = "sim", chains = i)

  expect_true(length(m1$chains) == i, info = "Correct number of chains are run")
  expect_equal(m1$chains, m2$chains, info = "Setting the seed results in multiple chains being reproducible")
}
