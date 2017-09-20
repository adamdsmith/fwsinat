## dplyr methods:

#' Dplyr verb methods for fwsinat objects
#'
#' @param .data data object of class \link{fwsinat}
#' @param ... other arguments
#' @name dplyr
#' @export
filter.fwsinat <- function(.data, ...) {
  as_fwsinat(NextMethod())
}

#' @name dplyr
#' @export
arrange.fwsinat <- function(.data, ...) {
  as_fwsinat(NextMethod())
}

#' @name dplyr
#' @export
mutate.fwsinat <- function(.data, ...) {
  as_fwsinat(NextMethod())
}

#' @name dplyr
#' @export
select.fwsinat <- function(.data, ...) {
  as_fwsinat(NextMethod())
}

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
