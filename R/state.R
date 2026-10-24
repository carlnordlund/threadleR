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

#' Get the most recently measured command execution time
#'
#' Returns the execution time (in milliseconds) of the last Threadle command
#' sent via this session, as reported by Threadle when the `"executiontime"`
#' setting is enabled (see [th_setting()]). Every call to `.th_call()` updates
#' this, regardless of which `th_*()` wrapper it came from or what `return=`
#' mode it used, since most wrappers only return a command's payload and would
#' otherwise discard this value entirely.
#'
#' @return A single numeric value (milliseconds), or `NULL` if the
#'   `"executiontime"` setting is off, no command has been sent yet, or the
#'   connected Threadle version doesn't support it.
#' @examplesIf th_is_available()
#' th_start_threadle()
#'
#' th_setting("executiontime", TRUE)
#' ns <- th_create_nodeset("ns", createnodes = 3)
#' th_get_last_execution_time()
#' th_stop_threadle()
#' @export
th_get_last_execution_time <- function() {
  .threadle_env$last_execution_time_ms
}
