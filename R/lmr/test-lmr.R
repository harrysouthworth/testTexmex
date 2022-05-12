## Test that Pena-Yohai initial estimates are the same every time
liver <- texmex::liver

for (run in 1:10){
  i <- sample(1:nrow(liver), size = nrow(liver), replace = TRUE)
  d <- liver[i, ]

  m1 <- py(log(ALT.M) ~ log(ALT.B) + log(ALP.B) + log(AST.B) + as.numeric(dose),
           data = d)
  m2 <- py(log(ALT.M) ~ log(ALT.B) + log(ALP.B) + log(AST.B) + as.numeric(dose),
           data = d)

  expect_identical(m1, m2, info = "py produces the same output every time")

  m1 <- py(log(ALT.M) ~ log(ALT.B) + log(ALP.B) + log(AST.B) + as.numeric(dose),
           data = d, resid_keep_method = "proportion",
           psc_keep = .75, resid_keep_prop = .75)

  expect_false(all(m1$coefficients == m2$coefficients),
               info = "py produces different output with different settings")

  m2 <- py(log(ALT.M) ~ log(ALT.B) + log(ALP.B) + log(AST.B) + as.numeric(dose),
           data = d, resid_keep_method = "proportion",
           psc_keep = .75, resid_keep_prop = .75)

  expect_identical(m1, m2, info = "py produces the same output every time with user-supplied settings")
}

## Test that robustbase and MASS functions give the same result
liver <- texmex::liver

set.seed(20210615)
for (run in 1:10){
  i <- sample(1:nrow(liver), size = nrow(liver), replace = TRUE)
  d <- liver[i, ]

  mrb <- lmr(log(ALT.M) ~ log(ALT.B) + as.numeric(dose), data = d,
             engine = "lmrob")
  mr <- lmr(log(ALT.M) ~ log(ALT.B) + as.numeric(dose), data = d,
            engine = "rlm")
  expect_equal(coef(mr), coef(mrb), tol = .002,
               info = "lmrob and rlm give same coefficients")
  expect_equal(mr$init$coefficients, mrb$init$coefficients, tol = 1e-7,
               info = "lmrob and rlm use same initial coefficients")
  expect_equal(mr$init$scale, mrb$init$scale, tol = 1e-7,
               info = "lmrob and rlm use same initial scale")
  expect_true(cor(resid(mrb), resid(mr)) > .999,
              info = "lmrob and rlm produce the same residuals")

  mrr <- robust::lmRob(log(ALT.M) ~ log(ALT.B) + as.numeric(dose), data = d,
                       control = robust::lmRob.control(efficiency = .85,
                                                       weight = c("bisquare", "bisquare"),
                                                       mxr = 200))
  expect_equal(coef(mrr), coef(mrb), tol = .002,
               info = "lmRob gives same coefficients")
  expect_true(cor(resid(mrb), resid(mrr)) > .999,
              info = "lmRob and lmr produce same residuals")
}



## Test that rfpe works correctly
liver <- texmex::liver

set.seed(20210615)
for (i in 1:10){
  i <- sample(1:nrow(liver), size = nrow(liver), replace = TRUE)
  d <- liver[i, ]


  mrb0 <- lmr(log(ALT.M) ~ 1, data = d,
              engine = "lmrob")
  mrb1 <- lmr(log(ALT.M) ~ log(ALT.B), data = d,
              engine = "lmrob")
  mrb2 <- lmr(log(ALT.M) ~ log(ALT.B) + as.numeric(dose), data = d,
              engine = "lmrob")

  s <- mrb2$scale

  expect_error(rfpe(mrb2),
               info = "rfpe: failure if scale not provided")

  expect_true(rfpe(mrb1, scale = s) < rfpe(mrb0, scale = s),
              info = "rfpe: ok with lmrob, models 0 and 1")
  expect_true(rfpe(mrb2, scale = s) < rfpe(mrb1, scale = s),
              info = "rfpe: ok with lmrob, models 1 and 2")

  mrb0 <- lmr(log(ALT.M) ~ 1, data = d,
              engine = "rlm")
  mrb1 <- lmr(log(ALT.M) ~ log(ALT.B), data = d,
              engine = "rlm")
  mrb2 <- lmr(log(ALT.M) ~ log(ALT.B) + as.numeric(dose), data = d,
              engine = "rlm")

  s <- mrb2$scale

  expect_true(rfpe(mrb1, scale = s) < rfpe(mrb0, scale = s),
              info = "rfpe: ok with rlm, models 0 and 1")
  expect_true(rfpe(mrb2, scale = s) < rfpe(mrb1, scale = s),
              info = "rfpe: ok with rlm, models 1 and 2")
}




