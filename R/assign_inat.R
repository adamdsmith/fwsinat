#' Assign iNaturalist observations to FWS property
#'
#' Assignment occurs in a 2-stage process.  First, the locational coordinates of the
#'  observations are compared directly with USFWS property boundaries.  Second, points
#'  not falling within a boundary *and* identified as having obscured locations are assigned
#'  to the nearest property within ~ 50 km, if any.
#'
#' @param fwsinat \code{data.frame} of iNaturalist observations produced by running
#'  \code{\link{retrieve_inat}}
#' @param progress logical (default = TRUE) should progress bar be displayed when
#'  assigning records to the nearest USFWS property
#'
#' @return \code{fwsinat} \code{data.frame} of iNaturalist observations with the USFWS
#'  property (i.e., \code{orgname}) appended as the first column
#'
#' @import sf
#' @export
#' @examples
#' \dontrun{
#' fws <- retrieve_inat()
#' fws <- assign_inat(fws)
#' }

assign_inat <- function(fwsinat, progress = TRUE) {

  inat_proj <- attr(fwsinat, "inat_proj")
  q_dt <- attr(fwsinat, "query_dt")

  # Check for existence of USFWS Cadastral geodatabase
  gdb <- file.exists(system.file("extdata", "FWSCadastral.gdb", package = "nwrspp"))
  if (!gdb) {
    message(paste(strwrap(
      paste("\nPrior to using `assign_inat` you must install the current USFWS National",
            "Wildlife Refuge cadastral information. To do so, please run",
            "`nwrspp::install_fws_cadastral()`.  This will take several minutes.",
            "\n\nWould you like to install now?")),
      collapse = "\n"))

    utils::menu(c("Yes", "No")) -> resp
    if (resp == 1) {
      nwrspp::install_fws_cadastral()
    } else {
      message("Very well. Run `nwrspp::install_fws_cadastral()` when you're ready to proceed.")
      return(invisible(NULL))
    }
  }

  gdb <- system.file("extdata", "FWSCadastral.gdb", package = "nwrspp")
  r <- st_read(gdb, layer = "FWSInterest", stringsAsFactors = FALSE, quiet = TRUE) %>%
    # Accommondate inconsistency with D'Arbonne in Approved vs Interest
    mutate(ORGNAME = gsub(" '", "'", ORGNAME)) %>%
    st_cast("MULTIPOLYGON") %>%
    st_transform(., 4269)

  inat_sf <- st_as_sf(fwsinat, coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(., 4269)

  suppressMessages(
    prop_int <- sapply(st_intersects(inat_sf, r), function(i) {ifelse(length(i) == 0, NA_integer_, i)})
  )
  in_prop <- which(!is.na(prop_int))
  no_prop <- which(is.na(prop_int)); n_no_prop <- length(no_prop)

  prop <- rep(NA_character_, length(prop_int))

  message(length(in_prop), " observations successfully assigned to an USFWS property.")
  prop[in_prop] <- as.data.frame(r)[prop_int[in_prop], "ORGNAME"]

  # Retrieve nearest property for obscured observations...
  no_prop <- no_prop[inat_sf$loc_obscured[no_prop]]
  message("Retrieving nearest USFWS property for ", length(no_prop),
          " observations with obscured locational coordinates.")
  message(n_no_prop - length(no_prop), " observations have been discarded.")
  near_prop <- nearest_prop(r, inat_sf, no_prop, progress = progress)
  prop[no_prop] <- near_prop

  fwsinat <- fwsinat %>% tibble::add_column(., orgname = prop, .before = 1) %>%
    # Drop records apperently outside USFWS property boundary
    filter(!is.na(orgname)) %>%
    mutate(orgname = clean_orgnames(orgname)) %>%
    arrange(orgname, iconic_taxon, sci_name, -as.numeric(date))

  class(fwsinat) <- c("fwsinat", "data.frame")
  attr(fwsinat, "inat_proj") <- inat_proj
  attr(fwsinat, "query_dt") <- q_dt

  fwsinat

}
