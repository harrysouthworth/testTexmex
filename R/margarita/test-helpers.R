
## Test margaritaScale traps erroneous input and returns correct substring
margaritaScale <- margarita:::margaritaScale
expect_equal(margaritaScale("robots"), "r")
expect_equal(margaritaScale("pingpong"), "p")
expect_equal(margaritaScale("dumbdumb"), "d")
expect_error(margaritaScale(2))
expect_error(margaritaScale(letters))

f <- function(x){
  res <- try(margaritaScale(x), silent=TRUE)
  if (class(res) == "try-error"){ FALSE }
  else { TRUE }
}
s <- sapply(letters, f)
for (i in 1:sum(s)) # s is a logical vector
  expect_equal(letters[s][i], c("d", "p", "r")[i])


## Test getCIquantiles behaves as expected
getCIquantiles <- margarita:::getCIquantiles
expect_equal(getCIquantiles(.05), c(0.025, 0.50, 0.975))
expect_equal(getCIquantiles(.1), c(0.050, 0.50, 0.950))
expect_equal(getCIquantiles(c(.1, .5)), c(.050, .250, .50, .750, .950))
expect_equal(getCIquantiles(c(.5, .1)), sort(getCIquantiles(c(.5, .1))))
expect_error(getCIquantiles(0))
expect_error(getCIquantiles(1))
expect_error(getCIquantiles(c(.05, .1, .5)))


## Test segments are returned correctly
getSegmentData <- margarita:::getSegmentData
data <- data.frame(rmvnorm(100, rep(0, 3)))

expect_error(getSegmentData(data))

data$groups <- rep(LETTERS[1:2], each=50)

expect_error(getSegmentsData(data))

data[, 1:3] <- t(apply(data[, 1:3], 1, sort))

expect_equivalent(getSegmentData(data)[[1]], data[, c("X1", "X3", "groups")])
expect_equal(getSegmentData(data)[[2]], NULL)

data <- data.frame(rmvnorm(100, rep(0, 7)))
data$groups <- rep(LETTERS[1:2], each=50)
data[, 1:7] <- t(apply(data[, 1:7], 1, sort))

expect_equivalent(getSegmentData(data)[[1]], data[, c("X1", "X5", "groups")])
expect_equivalent(getSegmentData(data)[[2]], data[, c("X2", "X4", "groups")])

data <- data.frame(rmvnorm(100, rep(0, 2)))
data$groups <- rep(LETTERS[1:2], each=50)
data[, 1:2] <- t(apply(data[, 1:2], 1, sort))

expect_error(getSegmentData(data))


## Test summary.margarita arithmetic works
# Implicity tests as.data.frame.summary.margarita, too
liver$ndose <- as.numeric(liver$dose)
mm <- lmr(log(ALT.M) ~ log(ALT.B) + ndose, data=liver)
liver$r <- resid(mm)
em <- evm(r, data=liver, qu=.5, xi=~ndose, method="sim", iter=10500, verbose=FALSE)
mar <- margarita(mm, em, newdata=data.frame(ndose=1:4), baseline="ALT.B", arm = "ndose")


rl <- simulate(mar, M=c(100, 500, 1000))
srl <- summary(rl)

# division
d <- srl / 25
whd <- lapply(srl, function(x) x/25)
whd <- do.call("rbind", whd)
expect_equivalent(as.data.frame(whd), as.data.frame(d)[, 1:5])

# multiplication
d <- srl * 25
whd <- lapply(srl, function(x) x*25)
whd <- do.call("rbind", whd)
expect_equivalent(as.data.frame(whd), as.data.frame(d)[, 1:5])

# subtraction
d <- srl - 25
whd <- lapply(srl, function(x) x-25)
whd <- do.call("rbind", whd)
expect_equivalent(as.data.frame(whd), as.data.frame(d)[, 1:5])

# addition
d <- srl + 25
whd <- lapply(srl, function(x) x+25)
whd <- do.call("rbind", whd)
expect_equivalent(as.data.frame(whd), as.data.frame(d)[, 1:5])
