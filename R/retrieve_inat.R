#' Retrieve observations from an iNaturalist project and link with ITIS
#'
#' An iNaturalist slug is usually the project name as single lower case
#'  string with words seperated by hyphens. For instance, the project "USFWS
#'  National Wildlife Refuge System" has a slug of
#'  "usfws-national-wildlife-refuge-system". The slug can also be extracted
#'  from the URL for the project. The USFWS NWRS project, for example, resides at
#'  \url{http://www.inaturalist.org/projects/usfws-national-wildlife-refuge-system}
#'
#' @param inat_proj character vector of name(s) of the iNaturalist project(s) as an
#'  iNaturalist slug or group ID. Default is to retrieve records for the USFWS National
#'  Wildlife Refuge projects
#'  (\url{http://www.inaturalist.org/projects/usfws-national-wildlife-refuge-system} and
#'  \url{http://www.inaturalist.org/projects/usfws-national-wildlife-refuge-system-bees-wasps}).
#' @param d1 First date of a date range as string in the form yyyy-mm-dd (e.g. "2017-07-01").
#'  Specifying only \code{d1} returns all records on and after \code{d1}.
#' @param d2 Last date of a date range as string in the form yyyy-mm-dd (e.g. "2017-07-31").
#'  Specifying only \code{d2} returns all records on and before \code{d2}.
#' @param since_date string with ISO 8601 datetime (e.g. "2017-07-04" or
#'  "2017-07-04T13:40:13-05:00"). Only records added or updated on iNaturalist after
#'  this date will be returned. Primarily intended to be used internally from
#'  \code{\link{update.fwsinat}}.
#' @param multipart logical (default \code{FALSE}) indicating whether the current retrieval
#'  is part of multiple retrievals to obtain > 10000 records.  If \code{TRUE}, the current
#'  system time is recorded as the the \code{query_dt} attribute of the resulting
#'  \code{fwsinat} object rather than \code{d2}, if specified.
#'
#' @return \code{fwsinat} \code{data.frame} of iNaturalist observations associated with the
#'  \code{inat_proj} joined, when possible, with ITIS (\url{http://www.itis.gov})
#'  information
#'
#' @seealso \code{\link{bind_rows}} for joining multiple iNaturalist data retrievals while
#'  preserving attributes
#' @seealso \code{\link{assign_inat}} for assigning iNaturalist observations to FWS
#'  properties
#' @seealso \code{\link{update.fwsinat}} for updating a previous iNaturalist data retrieval
#' @seealso \code{\link{export_inat}} for exporting a previous iNaturalist data retrieval
#'
#' @export
#' @examples
#' \dontrun{
#' # Default is to retrieve records for USFWS National Wildlife Refuge System projects
#' fws <- retrieve_inat()
#' fws_july17 <- retrieve_inat(d1 = "2017-07-01", d2 = "2017-07-31")
#'
#' # But it works with other projects too
#' bs <- retrieve_inat("bon-secour-national-wildlife-refuge-bioblitz")
#' }

retrieve_inat <- function(inat_proj = c("usfws-national-wildlife-refuge-system",
                                        "usfws-national-wildlife-refuge-system-bees-wasps"),
                          d1 = NULL, d2 = NULL, since_date = NULL,
                          multipart = FALSE) {

  q_dt <- Sys.time()
  if (!is.null(d2) && !multipart) q_dt <- min(q_dt, as.POSIXct(as.Date(d2)))

  obs <- lapply(inat_proj, function(i) {

    message("Processing iNaturalist project: ", i)

    # Retrieve observations for this request and inform of problem if too many
    n_recs <- GET_inat(i, d1, d2, since_date, TRUE)

    # Now get observations
    if (n_recs > 10000) {
      message("Only first 10,000 records will be returned.\n",
              "To retrieve all records split up your request with an informed\n",
              "use of the `d1` and `d2` arguments.\n\n",
              "Do you wish to continue? Enter 'y' or 'n'.")
      ans <- tolower(substr(readline(), 1L, 1L))
      if (!(ans == "n"|ans == "y")) stop("Unrecognized response. Try again.")
      if (ans == "n") invisible(return(NULL))
    }

    obs <- GET_inat(i, d1, d2, since_date)

    if (is.null(obs)) return(obs)

    obs <- obs %>%
      filter(!itistools::is_missing(Scientific.name),
             !itistools::is_missing(Latitude),
             !itistools::is_missing(Longitude)) %>%
      mutate(sci_name = clean_sci_name(Scientific.name),
             date = as.Date(Observed.on, format = "%Y-%m-%d"),
             last_inat_update = as.Date(Updated.at, format = "%Y-%m-%d"),
             com_name = ifelse(itistools::is_missing(Common.name),
                               NA_character_, itistools::Cap(Common.name)),
             loc_obscured = as.logical(toupper(Coordinates.obscured)),
             notes = ifelse(itistools::is_missing(Description), NA_character_, Description))

  })

  if (all(sapply(obs, is.null))) return(NULL)

  obs <- bind_rows(obs)

  itis <- itistools::get_itis(obs$sci_name)

  # Now join ITIS info to occurrence records
  obs <- left_join(ungroup(obs), itis, by = "sci_name") %>%
    mutate(sci_name = ifelse(is.na(valid_sci_name), sci_name, valid_sci_name),
           iconic_taxon = layman_iconic(Iconic.taxon.name)) %>%
    select(sci_name,
           com_name,
           itis_com_name,
           iconic_taxon,
           itis_taxon_rank,
           date,
           last_inat_update,
           lat = Latitude, lon = Longitude, loc_obscured,
           notes, grade = Quality.grade, url = Url,
           user = User.login) %>%
    arrange(iconic_taxon, sci_name, -as.numeric(date))

  message("Retained ", nrow(obs), " georeferenced iNaturalist records.")

  class(obs) <- c("fwsinat", class(obs))
  attr(obs, "inat_proj") <- inat_proj
  attr(obs, "query_dt") <- q_dt

  obs
}
