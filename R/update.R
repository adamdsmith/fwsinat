#' Update previous retrieval of observations from an iNaturalist project
#'
#' This function uses the query timestamp associated with the \code{fwsinat}
#'  \code{object} to perform a new iNaturalist retrieval for the associated
#'  projects and update/add records that have changed or are new since the
#'  previous query.
#'
#' @param object \code{fwsinat} \code{data.frame} of iNaturalist observations
#'  produced by running \code{\link{retrieve_inat}}, and (optionally) subsequently through
#'  \code{\link{assign_inat}}
#' @param ... not used in this implementation
#'
#' @return \code{fwsinat} \code{data.frame} of iNaturalist observations associated with the
#'  \code{inat_proj} joined, when possible, with ITIS (\url{http://www.itis.gov})
#'  information
#'
#' @export
#' @examples
#' \dontrun{
#' # Default is to retrieve records for USFWS National Wildlife Refuge System project
#' fws <- retrieve_inat()
#'
#' # Wait a while... hours, days, months...whatever
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
