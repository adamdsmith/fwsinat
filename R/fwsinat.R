#' \code{fwsinat} package
#'
#' See the \href{https://github.com/adamdsmith/fwsinat}{README} on GitHub
#'
#' @docType package
#' @name fwsinat
#' @import dplyr
#' @importFrom rlang .data
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
