#' fwsinat summary
#'
#'
#'
#' @param object \code{fwsinat} \code{data.frame} of iNaturalist observations
#'  produced by running \code{\link{retrieve_inat}}, and (optionally) subsequently through
#'  \code{\link{assign_inat}} and/or \code{\link{update.fwsinat}}
#' @param ... see \link[base]{summary}
#' @return Same as \link[base]{summary.data.frame} but also prints information on contained
#'  iNaturalist projects and the date of the retrieval.
#' @export
summary.fwsinat <- function(object, ...) {
  message(deparse(substitute(object)), " contains the following iNaturalist project(s):")
  message(paste0("  ", attr(object, "inat_proj"), "\n"))
  message("iNaturalist data retrieval was initiated at ",
          strftime(attr(object, "query_dt"), format = "%Y-%m-%d %H:%M:%S", usetz = TRUE))
  NextMethod()
}
