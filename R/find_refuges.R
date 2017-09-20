#' Find refuges or properties available for query.
#'
#' @param refuge character string scalar or vector (i.e., multiple entries allowed and
#'  regular expressions allowed) with
#'  which to search and return valid refuge identifiers for use with
#'  \code{\link{inat_retrieve}}. Default (`NULL`) returns all available refuges.
#'  Case-insensitive search. Regular expressions allowed. See Examples.
#' @param ptype character string scalar or vector of types of USFWS properties to search.
#'  Default is to search all property types.  Other viable options include National Wildlife
#'  Refuges only (`ptype = "NWR"`), "WMD" (Wetland Managment District), "WMA" (Wildlife
#'  Management Area), and WPA" (Waterfowl Production Area).  Multiple options are
#'  permissible.
#' @param region integer vector indicating which USFWS Region to search; see
#'  \url{https://www.fws.gov/where}; valid values range from 1 to 8
#'
#' @return character scalar or vector of proper organizational name (ORGNAME) for USFWS
#'  properties meeting the search criteria.  This output can be passed directly as the
#'  \code{refuge} argument in \code{\link{inat_retrieve}}.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all available properties
#' all_props <- find_refuges()
#'
#' # Get all National Wildlife Refuges
#' all_refs <- find_refuges(ptype = "NWR")
#'
#' # Search for refuges using a partial name match
#' ml <- find_refuges("longleaf")
#'
#' # Search for refuges matching multiple strings
#' multi <- find_refuges(c("longleaf", "romain"))
#'
#' # Same search using regular expressions
#' multi <- find_refuges("longleaf|romain")
#'
#' # Return all southeast (region 4) refuges
#' r4 <- find_refuges(region = 4, ptype = "NWR")
#' }

find_refuges <- function(refuge = NULL,
                         ptype = c("NWR", "WMD", "WMA", "WPA"),
                         region = 1:8L) {

  if (!any(ptype %in% c("NWR", "WMD", "WMA", "WPA")))
    stop("Unknown property type (`ptype`).\n",
         "See available options in `?find_refuges`.")

  r <- utils::read.csv(system.file("extdata", "fws_place_ids.csv", package = "fwsinat"),
                       stringsAsFactors = FALSE)

  # Filter by region
  if (!all(region %in% 1:8L)) stop("Valid USFWS regions range from 1 to 8.\n",
                                   "See https://www.fws.gov/where for assistance.")
  r <- r[r$fws_region %in% region & r$prop_type %in% ptype, ]

  if (is.null(refuge)) {
    return(sort(r$orgname))
  } else {
    refs <- lapply(refuge, function(r_i) {
      ref <- r[grepl(r_i, r$orgname, ignore.case = TRUE), ]
      if (nrow(ref) == 0) {
        NULL
      } else sort(ref$orgname)
    })
    if (is.null(unlist(refs))) stop("No refuges matched your search criteria.")
    return(unlist(refs))
  }
}
