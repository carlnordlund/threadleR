.threadle_env <- new.env(parent = emptyenv())

.th_set_proc <- function(proc) {
  .threadle_env$proc <- proc
  invisible(NULL)
}

.th_get_proc <- function() {
  proc <- .threadle_env$proc
  if (is.null(proc)) {
    stop("No Threadle process is running. Call `th_start_threadle()` first.", call. = FALSE)
  }
  proc
}

.th_has_proc <- function() {
  !is.null(.threadle_env$proc)
}

.th_clear_proc <- function() {
  .threadle_env$proc <- NULL
  invisible(NULL)
}
