#' Retrieve iNaturalist observations on USFWS properties
#'
#' The observations can also be restricted to a single iNaturalist project,
#'  which is associated with a "slug", usually in the form of the project name
#'  as a single lower case string with words seperated by hyphens. For instance,
#'  the default "USFWS National Wildlife Refuge System" project has a slug of
#'  "usfws-national-wildlife-refuge-system". The slug can be extracted from the
#'  for the project. The USFWS NWRS project, for example, resides at
#'  \url{http://www.inaturalist.org/projects/usfws-national-wildlife-refuge-system}
#'
#' @param refuge character scalar or vector indicating USFWS properties
#'  on which to retrieve iNaturalist observations.  It is strongly advised that
#'  the input for this parameter result from running \code{\link{find_refuges}} to
#'  avoid potential mismatches.  See examples. By default, all 500+ USFWS
#'  properties with boundaries in iNaturalist are used.
#' @param inat_proj character scalar of an iNaturalist project as an iNaturalist
#'  slug or group ID. Default is to retrieve all records on the property associated
#'  with the USFWS National Wildlife Refuge System project
#'  ("usfws-national-wildlife-refuge-system";
#'  \url{http://www.inaturalist.org/projects/usfws-national-wildlife-refuge-system}).
#'  Another potentially useful option is to retrieve all available iNaturalist records
#'  on the property using (\code{inat_proj = NULL})
#' @param taxon_name character scalar. Retrieve only observations associated with
#'  \code{taxon_name}. This also retrieves descendant taxa. Note if \code{taxon_name} is
#'  ambiguous and matches multiple taxa, no observations may be retrieved.
#' @param d1 First date of a date range as string in the form yyyy-mm-dd (e.g. "2017-07-01").
#'  Specifying only \code{d1} returns all records on and after \code{d1}.
#' @param d2 Last date of a date range as string in the form yyyy-mm-dd (e.g. "2017-07-31").
#'  Specifying only \code{d2} returns all records on and before \code{d2}.
#' @param since_date string with ISO 8601 datetime (e.g. "2017-07-04" or
#'  "2017-07-04T13:40:13-05:00"). Only records added or updated on iNaturalist after
#'  this date will be returned. Primarily intended for internals use from
#'  \code{\link{inat_update}}.
#' @param multipart logical (default \code{FALSE}) indicating whether the current retrieval
#'  is part of multiple retrievals to obtain > 10000 records.  If \code{TRUE}, the current
#'  system time is recorded as the the \code{query_dt} attribute of the resulting
#'  \code{fwsinat} object rather than \code{d2}, if specified.
#' @param verbose logical (default \code{TRUE}); print informative messages?
#'
#' @return \code{fwsinat} object (essentially a \code{data.frame} with a few additional
#'  attributes) of iNaturalist observations associated with the supplied \code{refuge}(s)
#'  and \code{inat_proj}, if any, and joined, when possible, with ITIS
#'  (\url{http://www.itis.gov}) information
#'
#' @seealso \code{\link{inat_update}} for updating a previous iNaturalist data retrieval
#' @seealso \code{\link{inat_export}} for exporting an iNaturalist data retrieval
#'
#' @export
#' @examples
#' \dontrun{
#' # Default is to retrieve records for all available USFWS properties from the USFWS
#' # National Wildlife Refuge System projects
#' fws <- inat_retrieve()
#' fws_july17 <- inat_retrieve(d1 = "2017-07-01", d2 = "2017-07-31")
#'
#' # But it works with other projects too
#' bon <- find_refuges("bon secour")
#' bs <- inat_retrieve(bon, "bon-secour-national-wildlife-refuge-bioblitz")
#'
#' # Or get all available iNaturalist observations on the property
#' bon <- find_refuges("bon secour")
#' bs_all <- inat_retrieve(bon, inat_proj = NULL)
#' }

inat_retrieve <- function(refuge = NULL,
                          inat_proj = "usfws-national-wildlife-refuge-system",
                          taxon_name = NULL, d1 = NULL, d2 = NULL, since_date = NULL,
                          multipart = FALSE, verbose = TRUE) {

  # Check refuge input
  if (is.null(refuge)) refuge <- find_refuges()
  else {
    if (!is.character(refuge))
      stop("Function is expecting an input string of valid refuge names. ",
           "See `?find_refuges`.")
    if (!any(refuge %in% find_refuges()))
      stop("At least one refuge is not available for retrieval. See `?find_refuges`.")
  }
  if (length(inat_proj) > 1) stop("Only a single iNaturalist project may be specified")

  q_dt <- Sys.time()
  if (!is.null(d2) && !multipart) q_dt <- min(q_dt, as.POSIXct(as.Date(d2)))

  obs <- lapply(refuge, function(i) {
    r <- utils::read.csv(system.file("extdata", "fws_place_ids.csv", package = "fwsinat"),
                         stringsAsFactors = FALSE)
    ref_name <- r[r$orgname == i, "name"]
    place_id <- r[r$orgname == i, "inat_place_id"]

    if (is.null(inat_proj))
      proj_status <- " across all iNaturalist projects."
    else
      proj_status <- paste(" within the", inat_proj, "project.")

    if (verbose) message(wrap_text("Retrieving",
                                   ifelse(is.null(taxon_name), "", taxon_name),
                                   " observations on ", ref_name, proj_status))

    # Retrieve observations for this request and inform of problem if too many
    n_recs <- GET_inat(place_id, inat_proj, taxon_name, d1, d2, since_date, TRUE, verbose)

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

    obs <- GET_inat(place_id, inat_proj, taxon_name, d1, d2, since_date, FALSE, verbose)

    if (is.null(obs)) return(obs)

    obs <- obs %>%
      filter(!itistools::is_missing(.data$latitude),
             !itistools::is_missing(.data$longitude)) %>%
      mutate(orgname = i,
             sci_name = clean_sci_name(.data$scientific_name),
             date = as.Date(.data$observed_on, format = "%Y-%m-%d"),
             last_inat_update = as.Date(.data$updated_at, format = "%Y-%m-%d"),
             com_name = ifelse(itistools::is_missing(.data$common_name),
                               NA_character_, itistools::Cap(.data$common_name)),
             loc_obscured = as.logical(toupper(.data$coordinates_obscured)),
             notes = ifelse(itistools::is_missing(.data$description), NA_character_,
                            .data$description))

  })

  if (all(sapply(obs, is.null))) return(NULL)

  obs <- bind_rows(obs)
  n_dl <- nrow(obs)

  itis <- itistools::get_itis(obs$sci_name)

  if (identical(itis, tibble()))
    itis <- empty_itis()

  # Now join ITIS info to occurrence records
  obs <- left_join(ungroup(obs), itis, by = "sci_name") %>%
    mutate(sci_name = ifelse(is.na(.data$valid_sci_name), .data$sci_name, .data$valid_sci_name),
           iconic_taxon = layman_iconic(.data$iconic_taxon_name)) %>%
    select(.data$orgname,
           .data$sci_name,
           .data$com_name,
           .data$itis_com_name,
           .data$iconic_taxon,
           .data$itis_taxon_rank,
           .data$date,
           .data$last_inat_update,
           lat = .data$latitude, lon = .data$longitude, .data$loc_obscured,
           .data$notes, grade = .data$quality_grade, .data$url,
           user = .data$user_login) %>%
    arrange(.data$iconic_taxon, .data$sci_name, -as.numeric(.data$date))

  if (verbose)
    if (n_dl - nrow(obs) > 0)
      message(
        wrap_text("Discarded", n_dl - nrow(obs), "iNaturalist observations",
                "missing coordinate information.")
      )

  class(obs) <- c("fwsinat", class(obs))
  attr(obs, "inat_proj") <- ifelse(is.null(inat_proj), "all", inat_proj)
  attr(obs, "query_dt") <- q_dt

  obs
}
