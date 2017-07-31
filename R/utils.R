nearest_prop <- function(r, inat_sf, no_prop, buff_deg = 0.2, progress = TRUE) {

  zones <- nwrspp:::get_UTM_zone(st_coordinates(inat_sf[no_prop, ])[, 1])
  zones <- ifelse(nchar(zones) == 1, paste0(0, zones), zones)
  l_zones <- unique(zones)

  out <- character(length(no_prop))

  if (progress) {
    pb <- utils::txtProgressBar(max = length(l_zones), style = 3, width = 50)
    i <- 1
  }
  for (z in l_zones) {
    z_no_prop <- no_prop[zones == z]
    inds <- match(z_no_prop, no_prop)
    recs <- inat_sf[z_no_prop, ]
    buff_extent <- raster::extent(
      c(st_bbox(recs)[1] - buff_deg, st_bbox(recs)[3] + buff_deg,
        st_bbox(recs)[2] - buff_deg, st_bbox(recs)[4] + buff_deg))
    suppressWarnings(
      r_crop <- st_intersection(r, st_set_crs(st_as_sf(methods::as(buff_extent, "SpatialPolygons")), st_crs(r)))
    )

    if (nrow(r_crop) == 0)
      tmp <- rep(NA_character_, length(z_no_prop))
    else {
      epsg <- as.integer(paste0(326, z))
      recs <- recs %>% st_transform(epsg)
      r_crop <- r_crop %>% st_transform(epsg)
      near_prop <- apply(st_distance(recs, r_crop), 1, which.min)
      tmp <- as.data.frame(r_crop)[near_prop, "ORGNAME"]
    }
    out[inds] <- tmp
    if (progress) {
      utils::setTxtProgressBar(pb, i); i <- i + 1
    }
  }

  if (progress) close(pb)

  out

}

layman_iconic <- function(iconics) {
  case_when(
    iconics == "Plantae" ~ "Plants",
    iconics == "Animalia" ~ "Animals",
    iconics == "Mollusca" ~ "Mollusks",
    iconics == "Actinopterygii" ~ "Ray-finned Fishes",
    iconics == "Mammalia" ~ "Mammals",
    iconics == "Aves" ~ "Birds",
    iconics == "Reptilia" ~ "Reptiles",
    iconics == "Amphibia" ~ "Amphibians",
    iconics == "Arachnida" ~ "Arachnids",
    iconics == "Insecta" ~ "Insects",
    iconics == "Fungi" ~ "Fungi",
    iconics == "Protozoa" ~ "Protozoans",
    iconics == "Chromista" ~ "Chromists",
    TRUE ~ NA_character_
  )
}

clean_orgnames <- function(orgnames) {
  old <- c("NATIONAL WILDLIFE REFUGE", "NATIONAL WILDLIFE RANGE",
           "WATERFOWL PRODUCTION AREA", "WILDLIFE MANAGEMENT AREA",
           "NATIONAL FISH AND WILDLIFE REFUGE", "WILDLIFE REFUGE",
           "NATIONAL FISH HATCHERY", "FARM SERVICE AGENCY")
  new <- c("NWR", "NWR", "WPA", "WMA", "NFWR", "WR", "NFH", "FSA")
  for(i in seq_along(old))
    orgnames <- gsub(old[i], new[i], orgnames, fixed = TRUE)
  orgnames
}

clean_sci_name <- function(sn_string) {
  # Trim any leading/trailing blank spaces
  sn_string <- gsub("^\\s+|\\s+$", "", sn_string) %>%
    # Replace UTF-8 'hybrid' indicator with 'x'
    gsub("×", "x", .) %>%
    # Drop any other improper characters
    iconv(., "UTF-8", "ascii", sub = "") %>%
    # Clean up any resulting double spaces...
    gsub("\\s+", " ", .)
  sn_string
}

construct_fn <- function(orgname) {
  orgname %>% gsub(" ", "_", .) %>%
    gsub("\\.", "", .) %>%
    paste0(".xlsx")
}
