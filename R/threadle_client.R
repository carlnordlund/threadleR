# R client package for Threadle CLIconsole

#' Coerce Threadle structure inputs to a backend name
#'
#' Internal helper to normalize inputs that can be either:
#' a single character string (already a backend variable name),
#' or a Threadle structure object (a list with a scalar character `$name`).
#'
#' @param x A single character name, or a Threadle structure object with `$name`.
#' @returns A length-1 character string giving the backend variable name.
#' @keywords internal
.th_name <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.character(x) && length(x) == 1) return(x)
  if (is.list(x)) {
    if (!is.null(x$name) && is.character(x$name) && length(x$name) == 1) return(x$name) # backward compat
  }
  stop("Expected a Threadle structure object (with $name) or a single character name.")
}

#' Collect wrapper arguments for backend calls
#'
#' Internal helper that collects arguments from a wrapper's environment and
#' prepares them for sending to the Threadle backend.
#'
#' It keeps only the wrapper's formal arguments (optionally dropping some),
#' extracts their current values from `env`,
#' normalizes known structure parameters (`name`, `network`, `structure`)
#' to backend names via `.th_name()`.
#'
#' @param env An environment, usually `environment()` from the wrapper.
#' @param drop Character vector of argument names to drop.
#' @returns A named list of arguments suitable for `.th_json_cmd()`.
#' @keywords internal
.th_args <- function(env, drop = character()) {
  fmls <- names(formals(sys.function(sys.parent())))
  keep <- setdiff(fmls, drop)
  args <- as.list(env)[keep]

  if ("name" %in% names(args)) args$name <- .th_name(args$name)
  if ("network" %in% names(args)) args$network <- .th_name(args$network)
  if ("structure" %in% names(args)) args$structure <- .th_name(args$structure)
  if ("nodeid" %in% names(args)) {
    if (is.null(args$nodeid) || length(args$nodeid) == 0L) {
      args$nodeid <- ""}
  }
  args
}

#' Build a JSON command DTO for the Threadle CLI backend
#'
#' Internal helper that constructs the JSON object sent to the Threadle CLI
#' process.
#'
#' @param command Command name (character scalar).
#' @param args Named list of arguments, or `NULL`.
#' @param assign Optional backend variable name to assign output to, or `NULL`.
#' @returns A JSON string.
#' @keywords internal
.th_json_cmd <- function(command, args = NULL, assign = NULL) {
  mode <- getOption("threadle.command", default = "json")

  # plain cli mode
  if (identical(mode, "cli")) {
    cmd <- as.character(command)
    asg <- if (!is.null(assign) && nzchar(as.character(assign))) as.character(assign) else NULL

    fmt_val <- function(v) {
      if (is.null(v)) "" else as.character(v)
    }

    fmt_one <- function(nm, v) {
      v_out <- fmt_val(v)
      if (!is.null(nm) && nzchar(nm)) {
        paste0(nm, " = ", v_out)
      } else {
        v_out
      }
    }

    if (is.null(args) || length(args) == 0) {
      call_str <- paste0(cmd, "()")
    } else {
      nms <- names(args)
      if (is.null(nms)) nms <- rep("", length(args))

      parts <- Map(fmt_one, nms, args)

      # optional
      parts <- Filter(function(x) nzchar(x), parts)

      call_str <- paste0(cmd, "(", paste(parts, collapse = ", "), ")")
    }

    if (!is.null(asg)) return(paste0(asg, " = ", call_str))
    return(call_str)
  }

  # default json mode
  dto <- list(
    Assign  = if (!is.null(assign)) as.character(assign) else NULL,
    Command = as.character(command),
    Args    = NULL
  )

  if (!is.null(args)) {
    args <- lapply(args, function(x) {
      if (is.null(x)) "" else as.character(x)
    })
    dto$Args <- args
  }

  jsonlite::toJSON(dto, auto_unbox = TRUE, null = "null")
}

#' Null-coalescing operator
#'
#' Returns `a` if it is not `NULL`, otherwise returns `b`.
#'
#' @name op-null-coalesce
#' @aliases %||%
#' @param a First value.
#' @param b Fallback value.
#' @returns `a` if non-`NULL`, else `b`.
#' @keywords internal
NULL

#' @rdname op-null-coalesce
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Stop if backend response indicates failure
#'
#' Validates that the backend response is a structured JSON object with a
#' logical `Success` field and throws an R error when `Success` is not `TRUE`.
#'
#' @param resp Parsed JSON response (a list) from `.send_command()`.
#' @returns Invisibly returns `resp` when successful.
#' @keywords internal
.th_stop_if_fail <- function(resp) {
  mode <- getOption("threadle.command", "json")

  if (identical(mode, "cli")) {
    if (is.null(resp)) {
      stop("No response from Threadle (CLI mode).", call. = FALSE)
    }
    return(invisible(resp))
  }

  if (is.null(resp) || is.null(resp$Success)) {
    stop("Invalid JSON response from Threadle.", call. = FALSE)
  }
  if (!isTRUE(resp$Success)) {
    stop(sprintf("[%s] %s",
                 resp$Code %||% "Error",
                 resp$Message %||% "Threadle error"),
         call. = FALSE)
  }
  invisible(resp)
}

#' Send a command to the Threadle CLI
#'
#' Internal helper function used by all wrappers. Sends a JSON command and
#' waits for a complete one-line JSON response. It also support CLI command now.
#'
#' @param cmd A JSON command string to send to the Threadle CLI process.
#' @returns A parsed JSON response as a list.
#' @keywords internal
.send_command <- function(cmd) {
  if (length(cmd) == 0) return(NULL)
  if (!nzchar(cmd)) return(NULL)

  proc <- get(".threadle_proc", envir = .GlobalEnv)
  proc$write_input(paste0(cmd, "\n"))

  mode <- getOption("threadle.command", default = "json")

  if (identical(mode, "cli")){
    out <- character()
    idle_ticks <- 0L
    repeat {
      new <- proc$read_output_lines()

      if (length(new) > 0) {
        out <- c(out, new)
        idle_ticks <- 0L
      } else {
        Sys.sleep(0.01)
        idle_ticks <- idle_ticks + 1L

        # after ~200ms with no new output, treat as finished
        if (idle_ticks >= 20L) {
          return(out)
        }
      }
    }
  }

  out <- character()
  repeat {
    new <- proc$read_output_lines()

    if (length(new) > 0) {
      out <- c(out, new)

      for (line in new) {
        s <- sub("^\\s*>\\s*", "", line)
        s <- trimws(s)

        if (grepl("^\\{.*\\}$", s)) {
          resp <- tryCatch(
            jsonlite::fromJSON(s, simplifyVector = TRUE),
            error = function(e) NULL
          )
          if (!is.null(resp)) {
            # if debug raw
            # attr(resp, "raw_lines") <- out
            return(resp)
          }
        }
      }
    } else {
      Sys.sleep(0.01)
    }
  }
}

#' Call a Threadle CLI command and handle the response
#'
#' Internal helper for wrappers. Builds a JSON DTO, sends it to the running
#' Threadle CLI process, checks success, and returns either the payload
#' or the full response.
#'
#' @param cmd Command name (character scalar).
#' @param args Named list of command arguments, or `NULL`.
#' @param assign Optional backend variable name to assign the result to, or `NULL`.
#' @param return What to return: `"payload"` (default) or `"response"`.
#' @param .print_message Logical; if `TRUE`, print `resp$Message` when present.
#' @returns If `return = "payload"`, returns `resp$Payload`; otherwise returns the full response.
#' @keywords internal
.th_call <- function(cmd,
                     args = NULL,
                     assign = NULL,
                     return = getOption("threadle.return", "payload"),
                     .print_message = getOption("threadle.print_message", TRUE)) {

  mode <- getOption("threadle.command", "json")

  if (identical(mode, "cli")) {
    if (!isTRUE(getOption("threadle.warn_return_cli_shown", FALSE))) {
      if (!is.null(getOption("threadle.return", NULL)) || length(return) > 1L) {
        message("Note: `threadle.return` = \"payload\"/\"response\" applies only in JSON mode; it is ignored in CLI mode.")
        options(threadle.warn_return_cli_shown = TRUE)
      }
    }

    cmd_str <- .th_json_cmd(command = cmd, args = args, assign = assign)
    if (isTRUE(getOption("threadle.print_cmd", FALSE))) {
      print(cmd_str)
    }

    out <- .send_command(cmd_str)
    .th_stop_if_fail(out)
    if (is.null(out) || length(out) == 0L) return(invisible(NULL))
    return(out)
  }

  cmd_json <- .th_json_cmd(command = cmd, args = args, assign = assign)
  if (isTRUE(getOption("threadle.print_cmd", FALSE))) {
    print(cmd_json)
  }
  resp <- .send_command(cmd_json)
  if (identical(mode, "json")) {.th_stop_if_fail(resp)}

  if (.print_message && !is.null(resp$Message) && nzchar(resp$Message)) {
    message(resp$Message)
  }

  if (return == "payload") {
    p <- resp$Payload
    if (is.null(p) || (is.atomic(p) && length(p) == 0L)) {
      return(invisible(NULL))
    }
    return(p)
  }

  resp
}

#' Start a Threadle CLI process
#'
#' Launches the Threadle CLI process executable and stores the process handle
#' in the global environment as `.threadle_proc`. The process is started in
#' silent mode (and optionally JSON mode).
#'
#' @param path Optional path to the Threadle CLI executable. If `NULL`, tries to
#'   locate `threadle` on `PATH` via [Sys.which()].
#' @returns Invisibly returns the `processx` process object.
#' @export
th_start_threadle <- function(path = NULL) {
  if (is.null(path) || !nzchar(path)) {
    path <- Sys.which("threadle")
    if (!nzchar(path)) {
      stop("Cannot find 'threadle' on PATH.\n",
           "Please provide the full path, e.g.\n",
           "  th_start_threadle('/full/path/to/threadle')",
           call. = FALSE
      )
    }
  } else {
    p <- path
    if (!file.exists(p)) {
      stop("Threadle not found at: ", p, call. = FALSE)
    }
    path <- normalizePath(p, mustWork = TRUE)
  }

  mode <- getOption("threadle.command", "json")
  if (exists(".threadle_proc", envir=.GlobalEnv)) {
    stop("'.threadle_proc' process already running.")
  }

  if (identical(mode, "cli")) {
    args <- c("--silent")
  } else if (identical(mode, "json")) {
    args <- c("--json", "--silent")
  } else {
    stop("Invalid option threadle.command. Use 'json' or 'cli'.", call. = FALSE)
  }
  proc <- processx::process$new(path, args=args, stdin="|", stdout="|", stderr = "|")
  # proc <- processx::process$new(path, args=c("--json", "--silent"), stdin="|", stdout="|", stderr = "|")
  #proc <- processx::process$new(path, args=c("--endmarker"), stdin="|", stdout="|", stderr = "|")
  # assign(".threadle_proc", proc, envir=.GlobalEnv)
  assign(".threadle_proc", proc, envir=.GlobalEnv)
  invisible(proc)
}

#' Stop the running Threadle CLI process
#'
#' Terminates the Threadle process previously started with `.start_threadle()`.
#'
#' @returns None; prints status messages.
#' @export
th_stop_threadle <- function() {
  if (exists(".threadle_proc", envir=.GlobalEnv)) {
    proc <- get(".threadle_proc", envir=.GlobalEnv)

    if (proc$is_alive()) {
      proc$kill()
      message("'.threadle_proc' process terminated.")
    }
    else {
      message("'.threadle_proc' process is already not running.")
    }
    rm(".threadle_proc", envir = .GlobalEnv)
  }
  else {
    message("No '.threadle_proc' process found.")
  }
}

#' Synchronize Threadle working directory with the current R working directory
#'
#' Sets the working directory used by the Threadle process to the current R working directory returned by [getwd()].
#'
#' @details
#' This is useful when invoking Threadle from threadleR, as the Threadle process
#' may have a different working directory than the R session.
#'
#' @return
#' Invisibly returns the path to the synced working directory
#' @export
th_sync_wd <- function() {
  r_wd <- getwd()
  th_set_workdir(r_wd)
  message("Threadle working directory synced to: ", r_wd)
}

#' Stage threadleR example files into a subfolder of the current R working directory
#'
#' Copies example files shipped with \pkg{threadleR} into \code{file.path(getwd(), folder)}
#' and returns the destination path.
#'
#' @param folder Name of the destination subfolder under [getwd()].
#' @param overwrite Logical; overwrite existing files in the destination.
#' @returns Invisibly returns the normalized path to the staged examples folder.
#' @export
th_stage_examples_to_wd <- function(folder = "threadle_examples", overwrite = TRUE) {
  from <- system.file("extdata", "Examples", package = "threadleR")
  if (from == "") stop("Examples not found in threadleR extdata.", call. = FALSE)

  dest <- file.path(getwd(), folder)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  files <- list.files(from, full.names = TRUE)
  ok <- file.copy(files, to = dest, overwrite = overwrite)
  if (!all(ok)) warning("Some example files were not copied.", call. = FALSE)

  invisible(normalizePath(dest, mustWork = TRUE))
}

#' Add an affiliation to a two-mode layer
#'
#' `th_add_aff()` adds an affiliation (hyperedge membership) between a node and a hyperedge in a two-mode layer.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param nodeid Node ID.
#' @param hypername Name of the hyperedge.
#' @param addmissingnode Logical; if `TRUE`, missing nodes are created automatically.
#' @param addmissingaffiliation Logical; if `TRUE`, missing affiliations are created automatically.
#'
#' @returns `NULL`, invisibly.
#' @export
th_add_aff <- function(network, layername, nodeid, hypername,
                       addmissingnode = TRUE, addmissingaffiliation = TRUE) {
  args <- .th_args(environment())
  cmd <- "addaff"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Add an edge to a one-mode layer
#'
#' `th_add_edge()` adds an edge between two nodes in a one-mode layer.
#'
#' @details
#' If the layer is directional, node1id is the source (from) and node2id is the destination (to).
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param node1id Node ID of the first node.
#' @param node2id Node ID of the second node.
#' @param value Edge value, defaults to 1.
#' @param addmissingnodes Logical; if `TRUE`, missing nodes are created and added. Defaults to `TRUE`.
#'
#' @returns `NULL`, invisibly.
#' @export
th_add_edge <- function(network, layername, node1id, node2id,
                        value = 1, addmissingnodes = TRUE) {
  args <- .th_args(environment())
  cmd <- "addedge"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Add a hyperedge to a two-mode layer
#'
#' `th_add_hyper()` adds a hyperedge (affiliation) to the hyperedge set of a 2-mode layer.
#'
#' @details
#' If a hyperedge with the same name already exists, it is replaced. Duplicate node IDs in `nodes` are ignored.
#'
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param hypername Name of the hyperedge.
#' @param nodes Vector of node to attach to the hyperedge. If `NULL`,
#'   creates the hyperedge without adding nodes.
#' @param addmissingnodes Logical; if `TRUE`, missing nodes are created and added. Defaults to `TRUE`.
#'
#' @returns `NULL`, invisibly.
#' @export
th_add_hyper <- function(network, layername, hypername,
                         nodes = c(), addmissingnodes = TRUE) {
  args <- .th_args(environment())
  args$nodes <- if (is.null(args$nodes)) "" else paste(args$nodes, collapse = ";")
  cmd <- "addhyper"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = NULL)
}

#' Add a relational layer in a network
#'
#' `th_add_layer()` adds a new layer to a network, which can be either one-mode or two-mode.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param mode Layer mode (1 or 2).
#' @param directed Logical; whether ties are directed (only for 1-mode layers).
#' @param valuetype "binary" or "valued" (only for 1-mode layers).
#' @param selfties Logical; whether self-ties are allowed (only for 1-mode layers).
#'
#' @returns `NULL`, invisibly.
#' @export
th_add_layer <- function(network, layername, mode, directed=FALSE, valuetype = c("binary", "valued"), selfties=FALSE) {
  valuetype <- match.arg(valuetype)
  args <- .th_args(environment())
  cmd <- "addlayer"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Add a node to a nodeset
#'
#' `th_add_node()` adds a node to a nodeset (or the nodeset of the provided network).
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param id Node ID (unsigned integer).
#'
#' @returns `NULL`, invisibly.
#' @export
th_add_node <- function(structure, id) {
  args <- .th_args(environment())
  cmd <- "addnode"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Check if an edge exists between two nodes in a layer
#'
#' `th_check_edge()` checks if an edge exists between two nodes in a layer, which can be either one-mode or two-mode.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param node1id Node ID of the first node.
#' @param node2id Node ID of the second node.
#'
#' @returns A logical scalar indicating whether the specified tie exists.
#' @export
th_check_edge <- function(network, layername, node1id, node2id) {
  args <- .th_args(environment())
  cmd <- "checkedge"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Remove all edges in a layer
#'
#' `th_clear_layer()` removes all edges in a layer but keep the layer.
#' For two-mode layers, all hyperedges are removed.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @returns `NULL`, invisibly.
#' @export
th_clear_layer <- function(network, layername) {
  args <- .th_args(environment())
  cmd <- "clearlayer"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Calculate connected components for the specified layer and network
#'
#' `th_components()` computes connected components for the specified layer and network.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param attrname Attribute name.
#' @returns A named list with:
#' \describe{
#'  \item{NbrComponents}{Integer of length 1; number of connected components.}
#'  \item{ComponentSizes}{Integer vector; sizes of the connected components.}
#' }
#' @export
th_components <- function(network, layername, attrname) {
  args <- .th_args(environment())
  cmd <- "components"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}


#' Create a network using the provided nodeset
#'
#' `th_create_network()` creates a new network from an existing nodeset.
#'
#' @param var Name of the assigned variable in the Threadle CLI environment.
#' @param nodeset A `threadle_nodeset` object or a character string giving
#' the name of a nodeset in the Threadle CLI environment.
#' @param name Optional internal name stored in the Threadle object metadata.
#'   This does not affect how the object is referenced in commands; commands
#'   use 'var'.
#'
#' @returns A `threadle_network` object.
#' @export
th_create_network <- function(var, nodeset, name = NULL) {
  args <- .th_args(environment(), drop = "var")
  cmd <- "createnetwork"
  assign <- var
  .th_call(cmd = cmd, args = args, assign = assign)
  structure(list(name = var), class = "threadle_network")
}

#' Create a nodeset
#'
#' `th_create_nodeset()` creates an empty nodeset by default,
#' but you can create nodes by setting `createnodes`.
#'
#' @param var Name of the assigned variable in the Threadle CLI environment.
#' @param name Optional internal name stored in the Threadle object metadata.
#'   This does not affect how the object is referenced in commands.
#' @param createnodes Number of nodes to create in the nodeset. Default is 0.
#' @returns A `threadle_nodeset` object.
#' @export
th_create_nodeset <- function(var, name = NULL, createnodes = 0) {
  args <- .th_args(environment(), drop = "var")
  cmd <- "createnodeset"
  assign <- var
  .th_call(cmd = cmd, args = args, assign = assign)
  structure(list(name=var), class="threadle_nodeset")
}

#' Define an attribute for a nodeset
#'
#' `th_define_attr()` defines a node attribute for a nodeset or the nodeset of the provided network.
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param attrname Attribute name.
#' @param attrtype Attribute type ("int","char", "float", or "bool")
#'
#' @returns `NULL`, invisibly.
#' @export
th_define_attr <- function(structure, attrname, attrtype = c('int','char','float','bool')) {
  attrtype <- match.arg(attrtype)
  cmd <- "defineattr"
  args <- list(structure = .th_name(structure), attrname = attrname, attrtype = attrtype)
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Calculate node degree centrality in a layer
#'
#' `th_degree()` computes node degree centrality for a specified layer and stores the result as a node attribute.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Name of the layer for which degree centrality is computed.
#' @param attrname Optional name for the node attribute storing degree values.
#' @param direction Which ties to count: `"in"`, `"out"`, or `"both"`. Defaults to `"in"`.
#' @returns `NULL`, invisibly.
#' @export
th_degree <- function(network, layername, attrname = NULL, direction = "in") {
  direction <- match.arg(direction, c("in", "out", "both"))
  args <- .th_args(environment())
  cmd <- "degree"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Delete a structure
#'
#' `th_delete()` removes a nodeset or network.
#'
#' @details
#' A nodeset cannot be deleted if it is currently used by another structure; delete
#' the dependent structures first.
#'
#' Deleting a network does not delete its nodeset; the nodeset remains available.
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#'
#' @returns `NULL`, invisibly.
#' @export
th_delete <- function(structure) {
  args <- .th_args(environment())
  cmd <- "delete"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Delete all structures
#'
#' `th_delete_all()` removes all current structures.
#'
#' @returns `NULL`, invisibly.
th_delete_all <- function() {
  args <- NULL
  cmd <- "deleteall"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Calculate the density of a layer
#'
#' `th_density()` computes the density of a layer, treating all existing ties as binary.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Name of the layer for which density is computed.
#' @returns A numeric scalar giving the layer density.
#' @export
th_density <- function(network, layername) {
  args <- .th_args(environment())
  cmd <- "density"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Dichotomize a valued one-mode layer
#'
#' `th_dichotomize()` creates a recoded version of
#' a valued one-mode layer based on a threshold rule.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Name of the valued one-mode layer to dichotomize.
#' @param cond Comparison operator used for dichotomization: `"eq"`, `"ne"`, `"gt"`, `"lt"`, `"ge"`, or `"le"`.
#'   Defaults to `"ge"`.
#' @param threshold Numeric threshold used with `cond`. Defaults to `1`.
#' @param truevalue Value assigned when the condition is `TRUE`. Defaults to `1`.
#' Can also be `"keep"` to retain the original value.
#' @param falsevalue Value assigned when the condition is `FALSE`. Defaults to `0`.
#' Can also be `"keep"` to retain the original value.
#' @param newlayername Optional name for the dichotomized layer.
#' If `NULL`, a default name is used.
#' @returns `NULL`, invisibly.
#' @export
th_dichotomize <- function(network, layername,
                           cond = c('ge','eq','ne','gt','lt','le','isnull','notnull'),
                           threshold = 1,
                           truevalue = 1,
                           falsevalue = 0,
                           newlayername = NULL) {
  args <- .th_args(environment())
  cmd <- "dichotomize"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' List directory contents
#'
#' `th_dir()` gets files and folders in the current working directory used by Threadle,
#' or in a specified directory.
#'
#' @param path Optional directory path. If `NULL` or empty, lists the current Threadle working directory.
#' @returns A named list with the following elements:
#'   \describe{
#'     \item{Path}{Character scalar; the listed directory path.}
#'     \item{Directories}{A data frame with a `Name` column listing subdirectories.}
#'     \item{Files}{A data frame with a `Name` column listing files.}
#'     \item{TotalDirectories}{Integer scalar; number of subdirectories.}
#'     \item{TotalFiles}{Integer scalar; number of files.}
#'   }
#' @export
th_dir <- function(path = NULL) {
  args <- .th_args(environment())
  cmd <- "dir"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Create a nodeset by a node attribute
#'
#' `th_filter()` creates a new nodeset containing the nodes that satisfy a condition on a specified
#' node attribute.
#'
#' @param name Name of the new nodeset variable
#' @param nodeset A `threadle_nodeset` object or a character string giving
#' the name of a nodeset in the Threadle CLI environment.
#' @param attrname Name of the node attribute to filter on.
#' @param cond Condition operator: `"eq"`, `"ne"`, `"gt"`, `"lt"`, `"ge"`, `"le"`,
#'   `"isnull"`, or `"notnull"`.
#' @param attrvalue Attribute value used for the condition.
#' @returns A `threadle_nodeset` object.
#' @export
th_filter <- function(name, nodeset, attrname, cond, attrvalue) {
  args <- .th_args(environment(), drop = "name")
  cmd <- "filter"
  assign <- name
  .th_call(cmd = cmd, args = args, assign = assign)
  structure(list(name = name), class = "threadle_nodeset")
}

#' Generate random ties in a layer
#'
#' `th_generate()` creates random ties in an existing layer of a network.
#'
#' @details
#' For one-mode generators, the layer must be binary;
#' any existing ties in the layer are removed before generation.
#'
#' Four generators are supported:
#' \describe{
#'   \item{`type = "er"`}{Erdős–Rényi. Provide `p` (edge probability / density).}
#'   \item{`type = "ws"`}{Watts–Strogatz. Provide `k` (even mean degree) and
#'   `beta` (rewiring probability in `[0,1]`). The layer must be symmetric.}
#'   \item{`type = "ba"`}{Barabási–Albert. Provide `m` (number of edges each new node attaches; must be >= 1 and typically < number of nodes).}
#'   \item{`type = "2mode"`}{Two-mode affiliations. Provide `h` (number of hyperedges)
#'   and `a` (average number of affiliations per node; Poisson mean). The layer must be two-mode.}
#' }
#' @param network Name of the new network variable to create.
#' @param layername Name of the layer to create in `network`.
#' @param type Generator type: `"er"`, `"ws"`, `"ba"`, or `"2mode"`.
#' @param p For `type = "er"`: edge probability in `[0, 1]`. Required when `type = "er"`.
#' @param k For `type = "ws"`: mean degree (must be even). Required when `type = "ws"`.
#' @param beta For `type = "ws"`: rewiring probability in `[0, 1]`. Required when `type = "ws"`.
#' @param m For `type = "ba"`: attachment parameter. Required when `type = "ba"`.
#' @param h For `type = "2mode"`: number of hyperedges. Required when `type = "2mode"`.
#' @param a For `type = "2mode"`: average affiliations per node. Required when `type = "2mode"`.
#'
#' @returns `NULL`, invisibly.
#' @export
th_generate <- function(network, layername, type, p, k, beta, m, h, a) {
  args <- .th_args(environment())
  cmd <- "generate"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Generate random node attributes
#'
#' `th_generate_attr()` creates a new node attribute with the given name and type
#' for the specified `structure`, and sets a random attribute value to each node
#' according to the provided parameters.
#'
#' The meaning of the parameters depends on `attrtype`:
#' \describe{
#'   \item{`attrtype = "int"`}{Uses `min` and `max` (defaults 0 and 100).}
#'   \item{`attrtype = "float"`}{Uses `min` and `max` (defaults 0.0 and 1.0).}
#'   \item{`attrtype = "bool"`}{Uses `p` as the probability of `"true"` (default 0.5).}
#'   \item{`attrtype = "char"`}{Uses `chars` as a `;`-separated set of values (default `"m;f;o"`).}
#' }
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param attrname Attribute name to create/fill.
#' @param attrtype Attribute type: `"int"` (default), `"float"`, `"bool"`, or `"char"`. Defaults to `"int"`.
#' @param min Minimum value for `"int"`/`"float"` types. Defaults to `0`/`0.0`.
#' @param max Maximum value for `"int"`/`"float"` types. Defaults to `100`/`1.0`.
#' @param p For `"bool"` type: probability of `"true"`. Default `0.5`.
#' @param chars For `"char"` type: `;`-separated candidate values, e.g. `"a;c;f;g;z"`.
#'   Default `"m;f;o"`.
#' @returns `NULL`, invisibly.
#' @export
th_generate_attr <- function(structure,
                             attrname,
                             attrtype = c("int", "float", "bool", "char"),
                             min = NULL,
                             max = NULL,
                             p = 0.5,
                             chars = "m;f;o") {
  attrtype <- match.arg(attrtype)
  if (is.null(min)) min <- if (attrtype == "int") 0L else 0
  if (is.null(max)) max <- if (attrtype == "int") 100L else 1
  args <- .th_args(environment())
  cmd <- "generateattr"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get all edges from a one-mode layer
#'
#' `th_get_all_edges` gets the edges in a specified one-mode layer.
#'
#' @param network A `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param layername Name of the one-mode layer to query.
#' @param offset Starting index (0-based). Defaults to `0`.
#' @param limit Maximum number of edges to return. Defaults to `1000`.
th_get_all_edges <- function(network, layername, offset = 0, limit = 1000) {
  args <- .th_args(environment())
  cmd <- "getalledges"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get all hyperedges in a two-mode layer
#'
#' `th_get_all_hyperedges()` returns the hyperedge names that exist in the specified
#' 2-mode `layername` of `network`. Supports pagination via `offset`/`limit`.
#'
#' @param network A `threadle_network` object or a character string giving
#'   the name of a network in the Threadle CLI environment.
#' @param layername Layer name (must be a 2-mode layer).
#' @param offset Starting index (0-based). Defaults to `0`.
#' @param limit Maximum number of hyperedges to return. Defaults to `1000`.
#' @returns A character vector of hyperedge names (JSON mode payload), or raw CLI text.
#' @export
th_get_all_hyperedges <- function(network, layername, offset = 0, limit = 1000) {
  args <- .th_args(environment())
  cmd <- "getallhyperedges"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get nodes from a nodeset
#'
#' `th_get_all_nodes` gets node IDs from a nodeset (or the nodeset of the provided network).
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character string
#'   naming a structure in the Threadle CLI environment.
#' @param offset Starting index (0-based). Defaults to `0`.
#' @param limit Maximum number of nodes to return. Defaults to 1000.
#'
#' @returns An integer vector of node IDs.
#' @export
th_get_all_nodes <- function(structure, offset, limit = 1000) {
  args <- .th_args(environment())
  cmd <- "getallnodes"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get the value of a node attribute
#'
#' `th_get_attr` gets the value of a node attribute for a single node in a nodeset (or the nodeset of the provided network).
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param nodeid Node ID.
#' @param attrname Attribute name.
#'
#' @returns A scalar giving the attribute value for the specified node.
#' @export
th_get_attr <- function(structure, nodeid, attrname) {
  args <- .th_args(environment())
  cmd <- "getattr"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Summarize a node attribute
#'
#' `th_get_attr_summary()` calculates summary statistics for a node attribute
#' in a nodeset (or the nodeset of the provided network).
#' @details
#' The reported statistics depend on the attribute type:
#' \describe{
#'   \item{`int` / `float`}{Mean, Median, StdDev, Min, Max, Q1, Q3.}
#'   \item{`bool`}{Count_true, Count_false, Ratio_true.}
#'   \item{`char`}{Frequency distribution, Mode, Unique_values.}
#' }
#' All types include Count, Missing, and PercentageWithValue.
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param attrname Name of the node attribute to summarize.
#' @returns In JSON mode: a parsed summary payload (often a list); in CLI mode: raw text.
#' @export
th_get_attr_summary <- function(structure, attrname) {
  args <- .th_args(environment())
  cmd <- "getattrsummary"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get the value of an edge between two nodes
#'
#' `th_get_edge()` gets the edge value between
#' two nodes in a specified layer.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Name of the layer to query (one-mode or two-mode).
#' @param node1id Node ID of the first node.
#' @param node2id Node ID of the second node.
#' @returns A numeric scalar giving the edge value (0 if no edge exists).
#' @export
th_get_edge <- function(network, layername, node1id, node2id) {
  args <- .th_args(environment())
  cmd <- "getedge"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get nodes affiliated with a hyperedge
#'
#' `th_get_hyperedge_nodes()` gets the node IDs affiliated with a hyperedge in a two-mode layer.
#'
#' @param network A `threadle_network` object or a character string giving
#'   the name of a network in the Threadle CLI environment.
#' @param layername Name of the two-mode layer to query.
#' @param hypername Name of the hyperedge.
#' @returns An integer vector of node IDs.
#' @export
th_get_hyperedge_nodes <- function(network, layername, hypername) {
  args <- .th_args(environment())
  cmd <- "gethyperedgenodes"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get the number of nodes in a structure
#'
#' `th_get_nbr_nodes()` gets the number of nodes in the nodeset (or the nodeset of the provided network).
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#'
#' @returns An integer scalar giving the number of nodes.
#' @export
th_get_nbr_nodes <- function(structure) {
  args <- .th_args(environment())
  cmd <- "getnbrnodes"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get alters of a node within a layer or across layers
#'
#' @param network A `threadle_network` object or a character string giving
#'   the name of a network in the Threadle CLI environment.
#' @param nodeid Node ID.
#' @param layernames Optional layer names.
#'   If `NULL`, alters are collected across all layers.
#' @param direction Which ties to count: `"both"` (default), `"in"`, or `"out"`.
#' @param unique Logical; if `TRUE`, deduplicate alter IDs. Defaults to `FALSE`.
#'
#' @returns An integer vector giving alter node IDs.
#' @export
th_get_node_alters <- function(network, nodeid, layernames = "", direction="both", unique = FALSE) {
  direction <- match.arg(direction, c("both", "in", "out"))
  if (is.null(layernames) || length(layernames) == 0L) {
    layernames <- ""
  } else if (length(layernames) > 1L) {
    layernames <- paste(layernames, collapse = ";")
  }
  args <- .th_args(environment())
  cmd <- "getnodealters"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get hyperedges affiliated with a node
#'
#' `th_get_node_hyperedges()` gets the names of hyperedges
#' that a node is affiliated with in a two-mode layer.
#'
#' @param network A `threadle_network` object or a character string giving
#'   the name of a network in the Threadle CLI environment.
#' @param layername Name of the two-mode layer to query.
#' @param nodeid Node ID.
#' @returns A character vector of hyperedge names.
#' @export
th_get_node_hyperedges <- function(network, layername, nodeid) {
  args <- .th_args(environment())
  cmd <- "getnodehyperedges"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get node ID by index position
#'
#' `th_get_nodeid_by_index()` gets the node ID at a specified index position
#' in a nodeset (or the nodeset of the provided network).
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param index Zero-based index position of the node in the nodeset.
#'
#' @returns An integer scalar giving the node ID at the requested index position.
#' @export
th_get_nodeid_by_index <- function(structure, index) {
  cmd <- "getnodeidbyindex"
  args <- list(structure = .th_name(structure), index = index)
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get a random alter of a node
#'
#' `th_get_random_alter()` gets the node ID of a randomly selected alter of a given node.
#'
#' @param network A `threadle_network` object or a character string giving the name of
#'   a network in the Threadle CLI environment.
#' @param nodeid Node ID.
#' @param layername Optional layer name. If `NULL`, alters are considered across all layers.
#' @param direction Which ties to count: `"both"` (default), `"in"`, or `"out"`.
#' @param balanced Logical; only used when `layername` is `NULL`.
#' @returns An integer scalar giving the node ID of the sampled alter.
#' @export
th_get_random_alter <- function(network, nodeid, layername="", direction="both", balanced=FALSE) {
  direction <- match.arg(direction, c("both", "in", "out"))
  args <- .th_args(environment())
  cmd <- "getrandomalter"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get a random edge from a layer
#'
#' `th_get_random_edge()` gets a randomly sampled edge from a specified layer.
#'
#' @param network A `threadle_network` object or a character string giving the name of
#'   a network in the Threadle CLI environment.
#' @param layername Name of the layer to sample from (one-mode or two-mode).
#' @param maxattempts Maximum number of uniform random node-pair attempts before switching to
#'   the fallback heuristic (two-mode layers only). Defaults to 100.
#'
#' @returns A named list with:
#' \describe{
#'  \item{node1}{Integer scalar; ID of the first node.}
#'  \item{node2}{Integer scalar; ID of the second node.}
#'  \item{value}{Numeric scalar; edge value between `node1` and `node2`.}
#' }
#' @export
th_get_random_edge <- function(network, layername, maxattempts = 100) {
  args <- .th_args(environment())
  cmd <- "getrandomedge"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get a random node ID
#'
#' `th_get_random_node()` gets a randomly selected node ID from a nodeset (or the nodeset of the provided network).
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @returns An integer scalar giving the sampled node ID.
#' @export
th_get_random_node <- function(structure) {
  args <- .th_args(environment())
  cmd <- "getrandomnode"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Get Threadle working directory
#'
#' `th_get_workdir` gets the working directory currently used by the Threadle.
#'
#' @returns A character scalar giving the current working directory used by Threadle.
#' @export
th_get_workdir <- function() {
  .th_call(cmd = "getwd")
}

#' Import ties into an existing layer from file
#'
#' `th_import_layer()` imports one-mode edges or two-mode affiliations
#' into an existing layer of a network from a file.
#'
#' @param network Name of the network
#' @param layername Name of the layer to create inside `network`.
#' @param file Path to the input file.
#' @param format Input file format. One of `"edgelist"` or `"matrix"`.
#' @param node1col For one-mode edge lists: zero-based column index of the first node ID.
#'   Defaults to 0.
#' @param node2col For one-mode edge lists: zero-based column index of the second node ID.
#'   Defaults to 1.
#' @param valuecol For valued one-mode edge lists: zero-based column index of the tie value.
#'   Defaults to 2.
#' @param nodecol For two-mode edge lists: zero-based column index of the node ID. Defaults to 0.
#' @param affcol For two-mode edge lists: zero-based column index of the affiliation/hyperedge label.
#'   Defaults to 1.
#' @param header Logical; whether the edge list file has a header line. Defaults to `FALSE`.
#' @param sep Field separator used when `format = "edgelist"` (and for delimited
#' matrix formats, if applicable). Defaults to tab.
#' @param addmissingnodes Logical; if `TRUE`, create nodes referenced in the file
#' that are not yet present in `network`.
#'
#' @returns `NULL`, invisibly.
#' @export
th_import_layer <- function(network, layername, file,
                            format,
                            node1col = 0, node2col = 1, valuecol = 2,
                            nodecol = 0, affcol = 1,
                            header = FALSE, sep = "\t",
                            addmissingnodes = FALSE) {
  format <- match.arg(format, c("edgelist", "matrix"))

  args <- .th_args(environment())
  cmd <- "importlayer"
  .th_call(cmd = cmd, args = args, assign = NULL)
}

#' Get structure metadata
#'
#' `th_info()` gets metadata about a nodeset or network.
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#'
#' @returns A named list containing structure metadata.
#' @export
th_info <- function(structure) {
  args <- list(structure = .th_name(structure))
  cmd <- "info"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' List all stored data objects
#'
#' `th_i()` lists an inventory of the data objects currently stored in the Threadle.
#'
#' @details
#' This command is in honor of all 1970's text adventure games,
#' where 'i' was used to check what you were carrying.
#'
#' @returns A named list mapping object names to their types.
#' @export
th_i <- function() {
  cmd <- "i"
  args <- NULL
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Load and run a Threadle script
#'
#' `th_load_script()` loads and executes a script file containing Threadle CLI commands.
#'
#' @details
#' Execution aborts if an error is encountered.
#'
#' @param file Path to a script file containing CLI commands.
#' @returns `NULL`, invisibly.
#' @export
th_load_script <- function(file) {
  args <- .th_args(environment())
  cmd <- "loadscript"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Load a nodeset or network from file
#'
#' `th_load_file()` loads a nodeset or network from a file in Threadle's internal format.
#'
#' @details
#' When loading a network that refers to a nodeset file,
#' the referenced nodeset is also loaded.
#'
#' Supported file extensions:
#' \describe{
#'   \item{`.tsv`}{Text-based internal format.}
#'   \item{`.tsv.gz`}{Gzipped text-based internal format.}
#'   \item{`.bin`}{Threadle binary format (compact, not human-readable).}
#'   \item{`.bin.gz`}{Gzipped Threadle binary format.}
#' }
#'
#' @param name Name of the assigned variable in Threadle.
#' @param file Path to the input file.
#' @param type Structure type to load: `"nodeset"` or `"network"`.
#'
#' @returns An object with class corresponding to the loaded type.
#' @export
th_load_file <- function(name, file, type) {
  args <- .th_args(environment(), drop = "name")
  cmd <- "loadfile"
  assign <- name
  .th_call(cmd = cmd, args = args, assign = assign)
  structure(list(name=name), class=paste0("threadle_",type))
}

#' Preview a structure
#'
#' `th_preview()` previews the content of a structure stored in the Threadle CLI
#' environment under the variable name `structure`.
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param maxlines Maximum number of lines to output. Defaults to `50`.
#' @returns CLI output (typically text lines; JSON mode may return a payload depending on backend).
#' @export
th_preview <- function(structure, maxlines = 50) {
  args <- .th_args(environment())
  cmd <- "preview"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Remove an affiliation from a two-mode layer
#'
#' `th_remove_aff()` removes an affiliation between
#' a node and a hyperedge in a two-mode layer.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Name of the two-mode layer.
#' @param nodeid Node ID.
#' @param hypername Name of the hyperedge.
#' @returns `NULL`, invisibly.
#' @export
th_remove_aff <- function(network, layername, nodeid, hypername) {
  args <- .th_args(environment())
  cmd <- "removeaff"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Remove a node attribute value
#'
#' `th_remove_attr()` removes an attribute value from a specific node
#' in a nodeset (or in the nodeset of the provided network).
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param nodeid Node ID.
#' @param attrname Attribute name.
#'
#' @returns `NULL`, invisibly.
#' @export
th_remove_attr <- function(structure, nodeid, attrname) {
  args <- .th_args(environment())
  cmd <- "removeattr"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Remove an edge from a one-mode layer
#'
#' `th_remove_edge` removes an edge between two nodes in a one-mode layer.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Layer name.
#' @param node1id Node ID of the first node.
#' @param node2id Node ID of the second node.
#' @returns `NULL`, invisibly.
#' @export
th_remove_edge <- function(network, layername, node1id, node2id) {
  args <- .th_args(environment())
  cmd <- "removeedge"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Remove a hyperedge from a two-mode layer
#'
#' `th_remove_hyper()` remove a hyperedge from a two-mode layer.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Name of the two-mode layer.
#' @param hypername Name of the hyperedge to remove.
#' @returns `NULL`, invisibly.
#' @export
th_remove_hyper <- function(network, layername, hypername) {
  args <- .th_args(environment())
  cmd <- "removehyper"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Remove a layer from a network
#'
#' `th_remove_layer()` removes a layer from a network, including all ties in that layer.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param layername Name of the layer to remove.
#' @returns `NULL`, invisibly.
#' @export
th_remove_layer <- function(network, layername) {
  args <- .th_args(environment())
  cmd <- "removelayer"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Remove a node from a nodeset
#'
#' `th_remove_node()` removes a node from a nodeset (or the nodeset of the provided network).
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param nodeid Node ID to remove.
#'
#' @returns `NULL`, invisibly.
#' @export
th_remove_node <- function(structure, nodeid) {
  args <- .th_args(environment())
  cmd <- "removenode"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Save a structure to file
#'
#' `th_save_file` saves a nodeset or network to disk using Threadle's internal file formats.
#'
#' @details
#' Supported file extensions:
#' \describe{
#'   \item{`.tsv`}{Tab-separated text format.}
#'   \item{`.tsv.gz`}{Gzipped tab-separated text format.}
#'   \item{`.bin`}{Threadle binary format (compact, not human-readable).}
#'   \item{`.bin.gz`}{Gzipped Threadle binary format.}
#' }
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param file Optional path to save the structure to. If omitted, the existing filepath
#' of the structure is used. Defaults to `""`.
#' @returns `NULL`, invisibly.
#' @export
th_save_file <- function(structure, file = "") {
  args <- .th_args(environment())
  if (!nzchar(file)) args$file <- shQuote(paste0(args$structure, ".tsv"), "cmd2")
  cmd <- "savefile"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Set a node attribute value
#'
#' `th_set_attr()` sets the value of a node attribute for a specific node
#' in a nodeset (or the nodeset of the provided network).
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param nodeid Node ID.
#' @param attrname Attribute name.
#' @param attrvalue Attribute value to set.
#'
#' @returns `NULL`, invisibly.
#' @export
th_set_attr <- function(structure, nodeid, attrname, attrvalue) {
  args <- .th_args(environment())
  cmd <- "setattr"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Set a Threadle setting
#'
#' `th_setting()` enables or disables a Threadle backend setting.
#' @details
#' Available settings include:
#' \describe{
#'   \item{`"nodecache"`}{Use a node cache (lazy initialized).}
#'   \item{`"blockmultiedges"`}{Prohibit creation of multiple edges with identical endpoints
#'     and direction.}
#'   \item{`"onlyoutboundedges"`}{Store only outbound edges (no inbound edges) to reduce memory
#'     usage for walker-only applications.}
#' }
#' @param name Setting name.
#' @param value Logical; `TRUE` to enable, `FALSE` to disable.
#'
#' @returns `NULL`, invisibly.
#' @export
th_setting <- function(name, value) {
  args <- .th_args(environment())
  cmd <- "setting"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Set Threadle working directory
#'
#' `th_set_workdir()` sets the working directory used by the Threadle.
#' @details
#' In addition to absolute and relative paths, `dir` supports special values:
#' \describe{
#'   \item{`"~"`}{Set the working directory to the user's home directory.}
#'   \item{`"~documents"`}{Set the working directory to the user's Documents folder.}
#' }
#' An error is raised if the target directory does not exist.
#' @param dir Directory path, or one of the special values `"~"` or `"~documents"`.
#' @returns `NULL`, invisibly.
#' @examples
#' \dontrun{
#' th_set_workdir("~documents")
#' }
#' @export
th_set_workdir <- function(dir) {
  args <- .th_args(environment())
  .th_call(cmd = "setwd", args = args)
}

#' Calculate shortest path distance between two nodes
#' `th_shortest_path()` computes the shortest path distance from `node1id` to `node2id` in a network.
#'
#' @details
#' By default, all layers are used. If `layername` is provided, the shortest path is
#' computed using that layer only.
#'
#' Shortest path distance is directional: in directed layers, the distance from `node1id`
#' to `node2id` may differ from the distance in the reverse direction. For symmetric
#' layers, this distinction is moot.
#'
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param node1id Node ID of the first node.
#' @param node2id Node ID of the second node.
#' @param layername Optional layer name. If `NULL`, all layers are used.
#' @returns An integer scalar giving the shortest path distance.
#' @export
th_shortest_path <- function(network, node1id, node2id, layername) {
  args <- .th_args(environment())
  cmd <- "shortestpath"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}

#' Create a subnetwork from a network and nodeset
#'
#' `th_subnet()` creates a new network based on an existing network, restricted to the
#' nodes in a specified nodeset.
#'
#' @details
#' This is useful for creating a network subset after creating a subset nodeset (e.g.,
#' with `th_filter()`).
#'
#' @param name Name of the new network variable to create.
#' @param network A `threadle_network` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @param nodeset A `threadle_nodeset` object or a character string giving
#' the name of a network in the Threadle CLI environment.
#' @returns A `threadle_network` object.
#' @export
th_subnet <- function(name, network, nodeset) {
  args <- .th_args(environment(), drop = "name")
  cmd <- "subnet"
  assign <- name
  .th_call(cmd = cmd, args = args, assign = assign)
  structure(list(name = name), class = "threadle_network")
}

#' Create a symmetric version of a one-mode layer
#'
#' `th_symmetrize()` generates a symmetric version of a one-mode layer in a network.
#'
#' @param network A `threadle_network` object or a character string giving the name of
#'   a network in the Threadle CLI environment.
#' @param layername Name of the one-mode layer to symmetrize.
#' @param method Symmetrization method: `"max"`, `"min"`, `"minnonzero"`, `"average"`, `"sum"`,
#'   or `"product"`. Defaults to `"max"`.
#' @param newlayername Optional name for the newly created symmetrized layer.
#'
#' @returns `NULL`, invisibly.
#' @export
th_symmetrize <- function(network, layername,
                          method = c("max", "min", "minnonzero", "average", "sum", "product"),
                          newlayername = NULL) {
  method <- match.arg(method)
  args <- .th_args(environment())
  cmd <- "symmetrize"
  .th_call(cmd = cmd, args = args, assign = NULL)
}

#' Undefine a node attribute
#'
#' `th_undefine_attr()` removes the definition of a node attribute from a nodeset
#' (or the nodeset of the provided network).
#' @details
#' Undefining an attribute also removes its values from all nodes that currently have
#' a value set for that attribute.
#'
#' @param structure A `threadle_nodeset` or `threadle_network` object, or a character
#'   string naming a structure in the Threadle CLI environment.
#' @param attrname Attribute name to undefine.
#'
#' @returns `NULL`, invisibly.
#' @export
th_undefine_attr <- function(structure, attrname) {
  args <- .th_args(environment())
  cmd <- "undefineattr"
  assign <- NULL
  .th_call(cmd = cmd, args = args, assign = assign)
}
