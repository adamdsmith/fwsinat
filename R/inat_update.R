#' Update previous retrieval of observations from an iNaturalist project
#'
#' Uses the query timestamp associated with the \code{fwsinat}
#'  object to perform a new iNaturalist retrieval for the associated
#'  USFWS properties (and possibly project) and updates records that have
#'  changed and adds records created since the previous query.
#'
#' @param fwsinat \code{fwsinat} \code{data.frame} of iNaturalist observations
#'  produced by running \code{\link{inat_retrieve}}
#'
#' @return \code{fwsinat} object of iNaturalist observations associated
#'  with one or more USFWS properties, and potentially an iNaturalist project.
#'  These observations are joined, when possible, with ITIS
#'  (\url{http://www.itis.gov}) information.
#'
#' @seealso \code{\link{inat_retrieve}} for details on making the initial
#'  retrieval of iNaturalist observations
#'
#' @export
#' @examples
#' \dontrun{
#' # Default is to retrieve observations from the USFWS National Wildlife
#' # Refuge System project on all available USFWS properties
#' fws <- inat_retrieve()
#'
#' # Wait a while... hours, days, months... for users to update and add new
#' # observations
#' fws <- inat_update(fws)
#' }

inat_update <- function(fwsinat) {

  old_q_dt <- attr(fwsinat, "query_dt")
  since_date <- format(old_q_dt, format = "%Y-%m-%dT%H:%M:%SZ")
  inat_proj <- attr(fwsinat, "inat_proj")
  if (inat_proj == "all") inat_proj <- NULL
  refuge <- attr(fwsinat, "fws_props")

  q_dt <- Sys.time()

  message("Checking ", length(refuge), " USFWS properties for iNaturalist observation updates.")
  upd_dat <- inat_retrieve(refuge, inat_proj, since_date = since_date, verbose = FALSE)

  if (identical(upd_dat, tibble())) {
    attr(fwsinat, "inat_proj") <- ifelse(is.null(inat_proj), "all", inat_proj)
    attr(fwsinat, "query_dt") <- q_dt
    message("No updates available.")
    return(fwsinat)
  }

  # Check compatability before proceeding
  stopifnot(identical(names(fwsinat), names(upd_dat)))

  attr(upd_dat, "inat_proj") <- ifelse(is.null(inat_proj), "all", inat_proj)
  attr(upd_dat, "query_dt") <- q_dt

  # Identify and remove original version of newly updated records
  upd_urls <- upd_dat$url
  old_recs <- match(upd_urls, fwsinat$url)
  upd_rows <- old_recs[!is.na(old_recs)]

  # Replace updated records
  message("Updated ", length(upd_rows), " existing records.")
  fwsinat[upd_rows, ] <- upd_dat[!is.na(old_recs), ]

  # Add new records
  message("Added ", sum(is.na(old_recs)), " new records.")
  fwsinat <- bind_rows(fwsinat, upd_dat[is.na(old_recs), ]) %>%
    arrange(.data$orgname, .data$iconic_taxon,
            .data$sci_name, -as.numeric(.data$date))
  attr(fwsinat, "query_dt") <- attr(upd_dat, "query_dt")

  fwsinat

}
