## Test that simBase returns a sample as expected
ll <- liver
ll$ndose <- as.numeric(ll$dose)
mm <- lmr(log(ALT.M) ~ log(ALT.B) + ndose, data=ll)
ll$r <- resid(mm)
nsim <- 10500; burn <- 500
gm <- evm(r, data=ll, qu=.5, xi=~ndose, method="sim",
          iter=nsim, burn=burn, thin=1, verbose=FALSE)
b <- simBase(mm, gm, baseline="log(ALT.B)")

# Check length is the same as the number of simulated
# parameters in the GPD model
expect_equal(length(b), nsim - burn)

# Check the simulated baselines are from the same distribution
# as the original. Don't want to be warned about ties in the data,
# so suppress warnings.
suppressWarnings(k <- ks.test(log(ll$ALT.B), b)$p.value)
expect_true(k > .2)


## Test distribution of expected values from robust regression is ok
ll <- liver
ll$ndose <- as.numeric(ll$dose)
mm <- lmr(log(ALT.M) ~ log(ALT.B) + ndose, data=ll)
ll$r <- resid(mm)
nsim <- 10500; burn <- 500
gm <- evm(r, data=ll, qu=.5, xi=~ndose, method="sim",
          iter=nsim, burn=burn, thin=1, verbose=FALSE)
nd <- data.frame(ndose=1:4)
sl <- simLinear(mm, gm, nd,
                baseline="log(ALT.B)", rawBaseline="ALT.B",
                invtrans=exp)

# Check number of simulations is same as number of GPD
# simualted parameters times number of rows in newdata
expect_equal(nrow(sl), nrow(nd) * (nsim - burn))

# Check the (nsim-burn) baseline values got replicated correctly
expect_equal(log(sl$ALT.B[1:(nsim-burn)]), log(sl$ALT.B[((nsim-burn)+1):(2*(nsim-burn))]))
expect_equal(log(sl$ALT.B[1:(nsim-burn)]), log(sl$ALT.B[(2*(nsim-burn)+1):(3*(nsim-burn))]))
expect_equal(log(sl$ALT.B[1:(nsim-burn)]), log(sl$ALT.B[(3*(nsim-burn)+1):nrow(sl)]))

# Check that coefficients match those from the original robust regression.
# The data have not been centred, so omit the intercept.
ols <- lm(fitted ~ log(ALT.B) + ndose, data=sl)
# Data not centred, so intercepts could differ
expect_equal(coef(ols)[-1], coef(mm)[-1], tol=.01)

# Check that the difference in fitted values is the same from one dose to
# the next (which checks the simulated regression coefficients have been
# replicated in the same way as the baselines).
ssl <- split(sl, sl$ndose)
diff1 <- ssl[[2]]$fitted - ssl[[1]]$fitted
diff2 <- ssl[[3]]$fitted - ssl[[2]]$fitted
diff3 <- ssl[[4]]$fitted - ssl[[3]]$fitted
expect_equal(diff1, diff2)
expect_equal(diff2, diff3)

# Check the differences correspond to the dose effect
expect_equal(mean(diff1), as.numeric(coef(mm)[3]), tol=.01)
co <- coef(summary(mm))
# tol is mean relative diff tol. Simulation suggests 0.025 should be fairly safe
expect_equal(sd(diff1), co[3,2], tol=.03)


## Test probabilities of threshold exceedance are calculated correctly
# margarita.rp and margarita.getProbs are just wrappers that allow for
# dealing with matrices and lists of parameters and return levels.
for (i in 1:10){
  sigma <- abs(rnorm(1, 10))
  xi <- rnorm(1, 0, .5)
  phi <- log(sigma)
  m <- 2
  mrp <- margarita:::margarita.rp(1, matrix(m), u = 0,
                                  par = t(c(phi, xi)), 1, NULL,
                                  family = gpd, th = NULL)
  p <- 1 - pgpd(2, u = 0, sigma = sigma, xi = xi)
  mgp <- margarita:::margarita.getProbs(1, list(matrix(c(phi, xi), ncol=2)),
                                        u = 0, p = 1, r = NULL, m=matrix(2),
                                        family = gpd, th = NULL)
  expect_equal(mrp, p)
  expect_equal(unlist(mgp), p)
}


## Test that return levels match those from pre-margarita code
# Compute return levels using old pre-margarita code that was submitted to a Git
# repo on 2013-06-19 (though the code predates that)

liver$ndose <- as.numeric(liver$dose)
rmod <- lmr(log(ALT.M) ~ log(ALT.B) + ndose, data=liver)
liver$r <- resid(rmod)
gmod <- evm(r, data=liver, qu=.7, xi=~ndose, method="sim", verbose=FALSE)

nsim <- nrow(gmod$param)

# Resample baselines
base <- sample(log(liver$ALT.B), size=nsim, replace=TRUE)
mycov <- rmod$cov
myloc <- coef(rmod)
mycoefs <- rmvnorm(4*nsim, mean=myloc, sigma=mycov)

ElogALT <- mycoefs[,1] + mycoefs[,2]*base + rep(1:4,each=nsim)*mycoefs[,3]
ElogALT <- matrix(ElogALT, ncol=4)

m <- c(100, 1000)
gg <- list()
for (i in 1:2){
  rl <- predict(gmod, M=m[i], all=TRUE)$obj[[1]]
  colnames(rl) <- LETTERS[1:4]

  logRL <- rl + ElogALT

  srl <- exp(apply(logRL, 2, quantile, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))) / 36
  r <- range(srl)

  srl <- data.frame(t(srl))
  srl$dose <- paste("Dose", LETTERS[1:4])

  gg[[i]] <- srl
} # Close for i
gg <- do.call("rbind", gg)

# Redo using margarita
mar <- margarita(rmod, gmod, newdata=data.frame(ndose=1:4), baseline="ALT.B",
                 arm = "ndose")
rl <- simulate(mar, M=c(100, 1000))
s <- summary(rl) / 36

res <- as.data.frame(s)[, 1:5] / as.data.frame(gg)[, 1:5]
expect_true(max(res) < 1.1)
expect_true(min(res) > 1/1.1)

# Medians should be more stable than 5th and 95th percentiles
expect_true(max(res[, 3]) < 1.05)
expect_true(min(res[, 3]) > 1/1.05)


## Test that eExcess probabilities match those from pre-margarita code

rp <- function(xm, u, phi, xi, p, r) {
  res <- p * (1 + xi/exp(phi) * (xm - u))^(-1/xi)
  if (any(u > xm)){
    res[u > xm] <- sapply(u[u>xm],
                          function(x,r,m,p) mean((r + x - quantile(r,1-p)) > m),
                          r=r,m=xm,p=p)
  }
  res[xi < 0 & xm > u - exp(phi)/xi] <- 0
  res
}

getProbs <- function(u, phi, xi, p, r, ULN, m = c(1, 3, 10, 20)) {
  m <- log(ULN * m)
  res <- t(sapply(m, rp, u = u, phi = phi, xi = xi, p = p, r=r))
  res <- apply(res, 1, quantile,  prob=c(.05, .25, .5, .75, .95))
  round(res, 4)
}

DoCalc <- function(gmod, ElogALT, xi){
  cnames <- paste("P(ALT > ", c("", "3x", "10x", "20x"), "ULN)",sep = "")
  out <- getProbs(u = ElogALT + gmod$map$threshold,
                  phi = gmod$param[, 1], xi = xi,
                  r=liver$r, p = gmod$map$rate, ULN = 36)
  colnames(out) <- cnames
  out
}

bmodParams <- predict(gmod, type="lp", all=TRUE)$obj

rpA <- DoCalc(gmod, ElogALT = ElogALT[,1], xi = bmodParams[[1]][[1]][,2])
rpB <- DoCalc(gmod, ElogALT = ElogALT[,2], xi = bmodParams[[1]][[2]][,2])
rpC <- DoCalc(gmod, ElogALT = ElogALT[,3], xi = bmodParams[[1]][[3]][,2])
rpD <- DoCalc(gmod, ElogALT = ElogALT[,4], xi = bmodParams[[1]][[4]][,2])

rpl <- list(rpA, rpB, rpC, rpD)
rpl <- do.call("rbind", lapply(rpl, t))

# Redo using margarita
mar <- margarita(rmod, gmod, newdata=data.frame(ndose=1:4), baseline="ALT.B",
                 arm = "ndose")
p <- simulate(mar, M=c(1, 3, 10, 20)*36, type="prob")
sp <- as.data.frame(summary(p)) %>%
  arrange(groups)

## Note that with weak priors and only 10000 sims, the 5th and 9th
## percentiles can be quite variable.

res <- sp[, 3:7] - rpl
expect_true(max(res[, 2:4]) < .01)
expect_true(min(res[, 2:4]) > -.01)
# image(as.matrix(res)); res
