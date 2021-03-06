#' Harvest observations outside the USFWS National Wildlife Refuge System
#' iNaturalist project for a given property
#'
#' @param refuge character string or vector indicating USFWS properties
#'  on which to harvest observations not associated with the USFWS
#'  National Wildlife Refuge System project ("usfws-national-wildlife-refuge-system";
#'  \url{http://www.inaturalist.org/projects/usfws-national-wildlife-refuge-system}).
#'  It is strongly advised that this parameter result from running
#'  \code{\link{find_refuges}} to avoid potential mismatches.  See Examples. By
#'  default (\code{refuge = NULL}), all USFWS properties with boundaries
#'  in iNaturalist are processed sequentially.
#' @param inat_proj character scalar of an iNaturalist project to which observations
#'  will be added (harvested). This can be a character slug or group ID. Default is
#'  to harvest observations on the \code{refuge} properties to the the USFWS National
#'  Wildlife Refuge System project ("usfws-national-wildlife-refuge-system";
#'  \url{http://www.inaturalist.org/projects/usfws-national-wildlife-refuge-system}).
#' @param taxon_name character scalar. Retrieve only observations associated with
#'  \code{taxon_name}. This also retrieves descendant taxa. This can greatly reduce
#'  harvesting time, and may be most useful harvest to projects for specific taxa
#'  (e.g., harvesting only 'Apoidea' observations to a bee and wasp project). Note
#'  if \code{taxon_name} is ambiguous and matches multiple taxa, no observations may
#'  be retrieved.
#' @param user character scalar of iNaturalist username or associated e-mail address
#' @param pw character scalar of iNaturalist password
#' @param interactive if \code{TRUE}, the default, the user is prompted to proceed
#'  prior to completing the harvest.  Setting to \code{FALSE} proceeds with the
#'  harvest without prompting
#'
#' @return This function performs a series of POST operations to assign observations
#'  to the USFWS NWRS project via the iNaturalist API (\url{???}). As a side effect,
#'  it returns a \code{data.frame} of observations that failed to be assigned, if any.
#'
#' @export
#' @examples
#' \dontrun{
#' # Harvest to the default USFWS NWRS project
#' hn <- find_refuges("harris neck")
#' hn_harvest <- inat_harvest(hn, user = "YOUR_USER_NAME", pw = "YOUR_PASSWORD")
#'
#' # Occasionally observations cannot be harvested because the user restricts their observations.
#' # If this happens, you will receive a message indicating how many records could not be harvested.
#' # If some records cannot be harvested, you can explore the reasons for these failures by
#' # "tabling" the error message column of the object you just created.
#' # Note that this only makes sense if you get a message about records not harvested
#' table(hn_harvest$error_msg)
#'
#' # Harvest to the USFWS NWRS Bee and Wasp project
#' hn <- find_refuges("harris neck")
#' hn_harvest <- inat_harvest(hn, inat_proj = "usfws-national-wildlife-refuge-system-bees-wasps",
#'                            taxon_name = "Apoidea", user = "YOUR_USER_NAME", pw = "YOUR_PASSWORD")
#' }

inat_harvest <- function(refuge = NULL, inat_proj = "usfws-national-wildlife-refuge-system",
                         taxon_name = NULL, user = NULL, pw = NULL, interactive = TRUE) {

  reqs <- list(user, pw)
  if (any(sapply(reqs, function(i) is.null(i) | !is.character(i))))
    stop("You must supply an iNaturalist username and password.")

  # Check project input
  proj_url <- paste0("http://www.inaturalist.org/projects/", inat_proj, ".json")
  try_GET <- try_verb_n(httr::GET)
  proj_req <- try_GET(proj_url)
  if (httr::http_error(proj_req))
    stop("Problem identifying project information.\n   ",
         paste(unlist(httr::content(proj_req)$error), collapse = "; "))
  proj_id <- jsonlite::fromJSON(httr::content(proj_req, as = "text"))$id

  # Check refuge input
  if (is.null(refuge)) refuge <- find_refuges()
  else {
    if (!is.character(refuge))
      stop("Function is expecting an input string of valid refuge names. ",
           "See `?find_refuges`.")
    if (!any(refuge %in% find_refuges()))
      stop("At least one refuge is not recognized. See `?find_refuges`.")
  }

  out <- lapply(refuge, function(i) {
    r <- utils::read.csv(system.file("extdata", "fws_place_ids.csv", package = "fwsinat"),
                         stringsAsFactors = FALSE)
    ref_name <- r[r$orgname == i, "name"]
    place_id <- r[r$orgname == i, "inat_place_id"]

    # Check necessity of harvest before pulling observations
    try_GET_inat <- try_verb_n(GET_inat)
    proj_obs <- try_GET_inat(place_id, proj = inat_proj, taxon_name = taxon_name,
                             nrecs_only = TRUE, verbose = FALSE)
    all_obs <- try_GET_inat(place_id, proj = NULL, taxon_name = taxon_name,
                            nrecs_only = TRUE, verbose = FALSE)

    if (all_obs > proj_obs) {
      try_inat_retrieve <- try_verb_n(inat_retrieve)
      proj_obs <- try_inat_retrieve(i, inat_proj = inat_proj, taxon_name = taxon_name, verbose = FALSE)

      if (!is.null(proj_obs))
        proj_obs <- proj_obs %>%
          mutate(obs_id = as.integer(sub(".*observations/", "", .data$url))) %>%
          pull(.data$obs_id)
      else
        proj_obs <- integer()

      add_obs <- try_inat_retrieve(i, inat_proj = NULL, taxon_name = taxon_name, verbose = FALSE) %>%
          mutate(obs_id = as.integer(sub(".*observations/", "", .data$url)))

      user_obs <- add_obs %>%
        select(.data$obs_id, .data$user)

      add_obs <- add_obs %>% pull(.data$obs_id) %>% setdiff(proj_obs)

    } else add_obs <- integer(0)

    message(length(add_obs), ifelse(is.null(taxon_name), "", paste0(" ", taxon_name)),
            " observations available for harvest on ", ref_name, ".")
    if (length(add_obs)) {
      if (interactive) {
        message("Do you want to proceed?")
        utils::menu(c("Yes", "No")) -> resp
        if (resp != 1) return(invisible(NULL))
      }

      # Set up authentication
      app_id <- "75994a8a45886aa6019654e83fad5833e53b29a426b3a5d5fcd9d00dfd55bf7a"
      app_secret <- "3cf3144a278cfd6f2637f942a91dc78560a852f6c2043eb9bfdbd4c3d5d485f9"
      base_url <- "https://www.inaturalist.org"
      payload <- list(client_id = app_id,
                      client_secret = app_secret,
                      grant_type = "password",
                      username = user,
                      password = pw)
      try_POST <- try_verb_n(httr::POST)
      p_token <- try_POST(paste0(base_url, "/oauth/token"),
                          body = payload)
      token <- httr::content(p_token)$access_token

      if (!requireNamespace("pbapply", quietly = TRUE)) {
        message("The pbapply package is needed and will be installed.")
        utils::install.packages("pbapply", quiet = TRUE, verbose = FALSE)
      }

      ref_out <- pbapply::pblapply(add_obs, function(obs_id) {
        post <- paste0("project_observation[observation_id]=", obs_id,
                       "&project_observation[project_id]=", proj_id)
        harvest <- try_POST(paste0(base_url, "/project_observations.json?", post),
                              httr::add_headers(Authorization = paste("Bearer", token)))
        has_error <- httr::http_error(harvest)
        data.frame(orgname = i,
                   observation_id = obs_id,
                   http_error = has_error,
                   error_msg = ifelse(has_error,
                                      paste(unlist(httr::content(harvest)$error), collapse = "; "),
                                      NA_character_),
                   stringsAsFactors = FALSE)
      })

      bind_rows(ref_out) %>%
        filter(.data$http_error) %>%
        left_join(user_obs, by = c("observation_id" = "obs_id"))

    } else NULL

  })

  out <- bind_rows(out)

  if (nrow(out) > 0) {
    message("Failed to harvest ", nrow(out), " observations due to errors.")
    short <- grep("Didn't pass rules", out$error_msg)
    if (!identical(integer(0), short)) out[short, "error_msg"] <- "Didn't pass project rules"
  }
  out

}
