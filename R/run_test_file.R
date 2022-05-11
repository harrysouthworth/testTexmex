#' Run a test file with tinytest, allowing a graphics device to be open.
#'
#' @param file,at_home,verbose,color,remove_side_effects,side_effects,set_env,... See
#'   the help file for \code{tinytest::run_test_file}.
#' @details \code{tinytest::run_test_file} sends all graphical output to a
#'   temporary file. Since we want to be able to view the graphical output,
#'   we overturn that functionality here. This function is a simple copy of
#'   the version in tinytest.
run_test_file <- function( file
                           , at_home=TRUE
                           , verbose = getOption("tt.verbose", 2)
                           , color   = getOption("tt.pr.color", TRUE)
                           , remove_side_effects = TRUE
                           , side_effects = FALSE
                           , set_env = list()
                           , ...){

  if (!file_test("-f", file)){
    stop(sprintf("'%s' does not exist or is a directory",file),call.=FALSE)
  }

  t0 <- Sys.time()

  # set environment variables (if any) to control the R environment during testing.
  if (length(set_env) > 0){
    # first, record current settings
    old_env_var <- sapply(names(set_env), Sys.getenv, unset=NA_character_, USE.NAMES=TRUE)
    # new settings
    do.call(Sys.setenv, set_env)
  }
  ## where to come back after running the file
  oldwd <- getwd()
  tinytest:::set_call_wd(oldwd)


  # make sure that plots get redirected to oblivion
  ##grDevices::pdf(file=nullfile())

  ## this will store the names of all environment
  ## variables created while running the file.
  envvar <- new.env()

  ## this will store option values that are overwritten by
  ## the user when running the file.
  oldop <- new.env()

  ## Store locale settings that may be overwritten
  ## by the user when running the file
  locale <- Sys.getlocale()

  ## clean up side effects
  on.exit({
    ## Clean up tinytest side effects
    # go back to the original working directory
    setwd(oldwd)
    tinytest:::set_call_wd("")
    # unset 'at_home' marker
    Sys.unsetenv("TT_AT_HOME")
    if ( remove_side_effects ){ ## Clean up user side effects
      # unset env vars set by the user in 'file'
      tinytest:::unset_envvar(envvar)
      # reset options to the state before running 'file'
      tinytest:::reset_options(oldop)
      # reset locale settings to starting values
      tinytest:::reset_locale(locale)
    }
    ##grDevices::dev.off()
    # return env var to values before running run_test_file
    if (exists("old_env_var")){
      unset <- is.na(old_env_var)
      Sys.unsetenv(names(old_env_var)[unset])
      if (any(!unset)) do.call(Sys.setenv, as.list(old_env_var)[!unset])
    }
  })


  setwd(dirname(file))
  file <- basename(file)

  if (at_home) Sys.setenv(TT_AT_HOME=TRUE)

  # An environment to capture the output in.
  o <- tinytest:::output()
  # An environment to run the test scripts in
  e <- new.env(parent=globalenv())
  # We locally mask expectation functions in the evaluation
  # environment 'e' so their output  will be captured in 'o'
  tinytest:::add_locally_masked_functions(envir = e, output=o)

  ## Reduce user side effects by making sure that any env var set
  ## in a test file is unset after running it.
  e$Sys.setenv <- tinytest:::capture_envvar(Sys.setenv, envvar)

  ## Reduce user side effects by capturing options that will be reset
  ## on exit
  e$options <- tinytest:::capture_options(options, oldop)


  ## Set useFancyQuotes, which is usually done by startup.Rs, the location
  ## of which is defined by envvar R_TESTS, which we set to empty now.
  ## See GH issues 36,37
  options(useFancyQuotes=FALSE)
  Sys.setenv(R_TESTS="")

  ## Make sure that we catch side-effects if the user asks for it.
  # an environment to store side-effects, and wheter we report them.
  sidefx <- new.env()
  e$report_side_effects <- tinytest:::capture_se(report_side_effects, sidefx)
  do.call(e$report_side_effects, as.list(side_effects))
  # internal side-effect tracker: make sure results are exported to user.
  local_report_envvar <- tinytest:::capture(tinytest:::report_envvar, o)
  local_report_cwd    <- tinytest:::capture(tinytest:::report_cwd, o)
  local_report_files  <- tinytest:::capture(tinytest:::report_files, o)
  local_report_locale <- tinytest:::capture(tinytest:::report_locale, o)

  # parse file, store source reference.
  tinytest:::check_double_colon(filename=file)
  parsed <- parse(file=file, keep.source=TRUE)
  src <- attr(parsed, "srcref")
  o$file <- file

  # format file name for printing while running.
  prfile <- basename(file)
  if (nchar(prfile) > 30 ){
    prfile <- paste0("..",substr(prfile, nchar(prfile)-27,nchar(prfile)))
  }
  prfile <- gsub(" ",".",sprintf("%-30s",basename(file)))


  for ( i in seq_along(parsed) ){
    expr   <- parsed[[i]]
    o$fst  <- src[[i]][1]
    o$lst  <- src[[i]][3]
    o$call <- expr

    if ( !o$exit ) eval(expr, envir=e) else break

    local_report_envvar(sidefx)
    local_report_cwd(sidefx)
    local_report_files(sidefx)
    local_report_locale(sidefx)

    if (verbose == 2) tinytest:::print_status(prfile, o, color, print=TRUE)
  }
  td <- abs(Sys.time() - t0)
  tx <- tinytest:::humanize(td, color=color)
  if (verbose == 1){
    # always when run in parallel. And we can only print once in that case
    str <- print_status(prfile, o, color, print=FALSE)
    if (o$exit) tinytest:::catf("%s %s %s\n", str, tx, o$exit_msg())
    else tinytest:::catf("%s %s\n", str, tx)
  }
  if (verbose >= 2){
    str <- if (o$exit) tinytest:::catf("%s %s\n", tx, o$exit_msg())
    else tinytest:::catf("%s\n", tx)
  }

  # returns a 'list' of 'tinytest' objects
  test_output <- o$gimme()
  structure(test_output, class="tinytests", duration=td)
}
