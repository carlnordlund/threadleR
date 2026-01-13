#' Example Threadle network file (mynet)
#'
#' @description
#' A small example Threadle network input stored as a one-column tibble, where each row
#' corresponds to one line in the original network file (metadata header plus three layers).
#' The example includes:
#' \itemize{
#'   \item \strong{kinship}: undirected, binary ties (LayerMode 1)
#'   \item \strong{trade}: directed, valued ties (LayerMode 1), encoded as \code{target;value}
#'   \item \strong{work}: two-mode affiliation data (LayerMode 2)
#' }
#'
#' @format
#' A tibble with 34 rows and 1 variable:
#' \describe{
#'   \item{`# Network Metadata`}{A character vector containing one line of the Threadle network file per row.}
#' }
#'
#' @source Included with the package for demonstration purposes.
"mynet"
