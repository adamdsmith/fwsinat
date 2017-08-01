#' Use dplyr::bind_rows on fwsinat objects
#'
#' Since \code{dplyr::bind_rows} is not currently an S3 method, this function
#'  masks \code{dplyr::bind_rows} (although it is called directly) for use on
#'  \code{fwsinat} objects.
#'
#' @param ... Inputs passed to dplyr::bind_rows
#' @return If all inputs are \code{fwsinat} objects, then the attributes
#'  are stacked and an \code{fwsinat} will be returned.  Otherwise, the expected
#'  outcome of \code{\link[dplyr]{left_join}} is returned and a message is displayed.
#' @seealso \code{\link[dplyr]{bind_rows}}
#' @seealso \url{https://github.com/tidyverse/dplyr/issues/2457}
#' @export
bind_rows <- function(...) {
  fwsinats <- list(...)
  is_fwsinats <- unlist(lapply(fwsinats, function(i) inherits(i, "fwsinat")))
  r <- dplyr::bind_rows(...)

  if (all(is_fwsinats)) {
    projs <- lapply(fwsinats, function(i) attr(i, "inat_proj")) %>%
      unlist() %>% unique()
    dts <- lapply(fwsinats, function(i) attr(i, "query_dt"))
    dt <- dts[[which.min(unlist(dts))]]
    attr(r, "inat_proj") <- projs
    attr(r, "query_dt") <- dt
  } else if (any(is_fwsinats)) {
    message('Some non-fwsinat objects.  Reverting to dplyr::bind_rows')
  }
  return(r)
}
