#' Harvest observations outside the USFWS National Wildlife Refuge System
#' iNaturalist project for a given property
#'
#' @param refuge character string or vector indicating USFWS properties
#'  on which to harvest observations not associated with the USFWS
#'  National Wildlife Refuge System project ("usfws-national-wildlife-refuge-system";
#'  \url{http://www.inaturalist.org/projects/usfws-national-wildlife-refuge-system}).
#'  It is strongly advised that this parameter result from running
#'  \code{\link{find_refuges}} avoid potential mismatches.  See Examples. By
#'  default (\code{refuge = NULL}), all USFWS properties with boundaries
#'  in iNaturalist are processed sequentially.
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
#' ### CREATE A MEANINGFUL EXAMPLE
#' }

inat_harvest <- function(refuge = NULL, user = NULL, pw = NULL, interactive = TRUE) {

  reqs <- list(user, pw)
  if (any(sapply(reqs, is.null),
          sapply(reqs, function(i) !is.character(i))))
    stop(
      wrap_text("You must supply an iNaturalist username and password.")
    )

  if (is.null(refuge)) refuge <- fwsinat::find_refuges()

  out <- lapply(refuge, function(i) {
    r <- utils::read.csv(system.file("extdata", "fws_place_ids.csv", package = "fwsinat"),
                         stringsAsFactors = FALSE)
    ref_name <- r[r$orgname == i, "name"]
    place_id <- r[r$orgname == i, "inat_place_id"]

    # Check necessity of harvest before pulling observations
    nwrs_obs <- GET_inat(place_id, nrecs_only = TRUE, verbose = FALSE)
    all_obs <- GET_inat(place_id, proj = NULL, nrecs_only = TRUE, verbose = FALSE)

    if (all_obs > nwrs_obs) {
      nwrs_obs <- inat_retrieve(i, verbose = FALSE)

      if (!is.null(nwrs_obs))
        nwrs_obs <- nwrs_obs %>%
          mutate(obs_id = as.integer(sub(".*observations/", "", .data$url))) %>%
          pull(.data$obs_id)
      else
        nwrs_obs <- integer()

      add_obs <- inat_retrieve(i, inat_proj = NULL, verbose = FALSE) %>%
          mutate(obs_id = as.integer(sub(".*observations/", "", .data$url)))

      user_obs <- add_obs %>%
        select(.data$obs_id, .data$user)

      add_obs <- add_obs %>% pull(.data$obs_id) %>% setdiff(nwrs_obs)

    } else add_obs <- integer(0)

    message(length(add_obs), " observations available for harvest on ", ref_name, ".")
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
      p_token <- httr::POST(paste0(base_url, "/oauth/token"),
                            body = payload)
      token <- httr::content(p_token)$access_token

      if (!requireNamespace("pbapply", quietly = TRUE)) {
        message("The pbapply package is needed and will be installed.")
        utils::install.packages("pbapply", quiet = TRUE, verbose = FALSE)
      }

      ref_out <- pbapply::pblapply(add_obs, function(obs_id) {
        post <- paste0("project_observation[observation_id]=", obs_id,
                       "&project_observation[project_id]=11904")
        harvest <- httr::POST(paste0(base_url, "/project_observations.json?", post),
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
    short <-  grep("Didn't pass rules", out$error_msg)
    if (!identical(integer(0), short)) out[short, "error_msg"] <- "Didn't pass project rules"
  }
  out

}
