.onUnload <- function(libpath) {
  if (exists(".threadle_env", envir = asNamespace("threadleR"), inherits = FALSE)) {
    env <- get(".threadle_env", envir = asNamespace("threadleR"))
    proc <- env$proc
    if (!is.null(proc)) {
      try(proc$kill(), silent = TRUE)
      env$proc <- NULL
    }
  }
}
