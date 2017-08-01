## dplyr methods:

#' Dplyr verb methods for fwsinat objects
#'
#' @param .data data object of class \link{fwsinat}
#' @param .dots see corresponding function in package \code{dplyr}
#' @param ... other arguments
#' @name dplyr
#' @export
filter_.fwsinat <- function(.data, ..., .dots) {
  as_fwsinat(NextMethod())
}

#' @name dplyr
#' @export
filter.fwsinat <- filter_.fwsinat

#' @name dplyr
#' @export
arrange_.fwsinat <- function(.data, ..., .dots) {
  as_fwsinat(NextMethod())
}

#' @name dplyr
#' @export
arrange.fwsinat <- arrange_.fwsinat

#' @name dplyr
#' @export
mutate_.fwsinat <- function(.data, ..., .dots) {
  as_fwsinat(NextMethod())
}

#' @name dplyr
#' @export
mutate.fwsinat <- mutate_.fwsinat

#' @name dplyr
#' @export
select_.fwsinat <- function(.data, ..., .dots) {
  as_fwsinat(NextMethod())
}

#' @name dplyr
#' @export
select.fwsinat <- select_.fwsinat

#' @name dplyr
#' @param x see \link[dplyr]{left_join}
#' @param y see \link[dplyr]{left_join}
#' @param by see \link[dplyr]{left_join}
#' @param copy see \link[dplyr]{left_join}
#' @param suffix see \link[dplyr]{left_join}
#' @export
left_join.fwsinat = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  as_fwsinat(NextMethod())
}
