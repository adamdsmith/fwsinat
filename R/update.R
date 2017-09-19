#' Update previous retrieval of observations from an iNaturalist project
#'
#' Uses the query timestamp associated with the \code{fwsinat}
#'  \code{object} to perform a new iNaturalist retrieval for the associated
#'  USFWS properties (and possibly project) and updates records that have
#'  changed and adds records created since the previous query.
#'
#' @param object \code{fwsinat} object of iNaturalist observations
#'  produced by \code{\link{retrieve_inat}}
#' @param ... not used in this implementation
#'
#' @return \code{fwsinat} object of iNaturalist observations associated
#'  with one or more USFWS properties, and potentially an iNaturalist project.
#'  These observations are joined, when possible, with ITIS
#'  (\url{http://www.itis.gov}) information.
#'
#' @seealso \code{\link{retrieve_inat}} for details on making the initial
#'  retrieval of iNaturalist observations
#'
#' @export
#' @examples
#' \dontrun{
#' # Default is to retrieve observations from the USFWS National Wildlife
#' # Refuge System project on all available USFWS properties
#' fws <- retrieve_inat()
#'
#' # Wait a while... hours, days, months... for users to update and add new
#' # observations
#' fws <- update(fws)
#' }

update.fwsinat <- function(object, ...) {

  old_q_dt <- attr(object, "query_dt")
  since_date <- format(old_q_dt, format = "%Y-%m-%dT%H:%M:%SZ")
  inat_proj <- attr(object, "inat_proj")
  if (inat_proj == "all") inat_proj <- NULL
  refuge <- sort(unique(object$orgname))

  q_dt <- Sys.time()
  upd_dat <- lapply(refuge, function(i) {
    retrieve_inat(i, inat_proj, since_date = since_date, verbose = FALSE)
  })
  upd_dat <- bind_rows(upd_dat)
  attr(upd_dat, "inat_proj") <- ifelse(is.null(inat_proj), "all", inat_proj)
  attr(upd_dat, "query_dt") <- q_dt

  stopifnot(identical(names(object), names(upd_dat)))

  # Identify and remove original version of newly updated records
  upd_urls <- upd_dat$url
  old_recs <- match(upd_urls, object$url)
  upd_rows <- old_recs[!is.na(old_recs)]

  # Replace updated records
  message("Updated ", length(upd_rows), " existing records.")
  object[upd_rows, ] <- upd_dat[!is.na(old_recs), ]

  # Add new records
  message("Added ", sum(is.na(old_recs)), " new records.")
  object <- bind_rows(object, upd_dat[is.na(old_recs), ]) %>%
    arrange(orgname, iconic_taxon, sci_name, -as.numeric(date))
  attr(object, "query_dt") <- attr(upd_dat, "query_dt")

  object

}
