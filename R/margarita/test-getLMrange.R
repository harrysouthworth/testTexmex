library(tidyverse)
library(robustbase)
library(robust)
library(margarita)
library(lmr)

liver$ndose <- as.numeric(liver$dose)

rmod <- lmr(log(ALT.M) ~ log(ALT.B) + ndose, data = liver)
liver$r <- resid(rmod)
bmod <- evm(r, data = liver, qu = .7, xi = ~ndose, method = "sim")

mar <- margarita(rmod, bmod, newdata = data.frame(ndose = 1:4),
                 arm = "ndose", baseline = "ALT.B")

getLMrange <- margarita:::getLMrange

for (i in 1:5){
  n <- sample(10:25, size = 1)

  o <- getLMrange(mar, n = n)

  expect_equal(unique(table(o$ndose)), n,
               info = "getLMrange: ndose repeats correctly")

  expect_equal(nrow(o), n * length(unique(liver$ndose)))

  expect_equal(min(liver$ALT.B), o$ALT.B[1],
               info = "getLMrange: min ALT.B matches data")
  expect_equal(max(liver$ALT.B), o$ALT.B[nrow(o)],
               info = "getLMrange: max ALT.B matches data")

  br <- c(sample(4:10, size = 1), sample(30:60, size = 1))

  o <- getLMrange(mar, n = n, range = br)

  expect_equal(unique(table(o$ndose)), n,
               info = "getLMrange: specifying range results in ndose repeats correctly")
  expect_equal(nrow(o), n * length(unique(liver$ndose)),
               info = "getLMrange: specifying range results in correct size output")
  expect_equal(br[1], o$ALT.B[1],
               info = "getLMrange: specifying range results in correct minimum ALT.B")
  expect_equal(br[2], o$ALT.B[nrow(o)],
               info = "getLMrange: specifying range results in correct maximum ALT.B")


  co <- coef(rmod)

  ealt <- co[1] + co[2] * log(o$ALT.B) + co[3] * o$ndose
  expect_equal(cor(ealt, o$expected), 1,
               info = "getLMrange: expected values are correct")
}
