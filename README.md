# Testing the texmex, margarita and lmr pachages

The texmex, margarita and lmr packages have test suites that are dependent
on the testthat package. Unfortunately, testthat has changed again and the
tests no longer run. This small project ports the test functions from
testthat to tinytest and executes them.

The test scripts are in the R/ directory under subdirectories texmex/
margarita/ and lmr/. The output from running them is generated using
Rmd/testTexmex.Rmd and is in the html and pdf files in Rmd/.

