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
    gsub("\u00D7|\u2715", "x", .) %>%
    # Drop any other improper characters
    iconv(., "UTF-8", "ascii", sub = "") %>%
    # Clean up any resulting double spaces...
    gsub("\\s+", " ", .)
  sn_string
}

construct_fn <- function(x) {
  x %>% gsub(" ", "_", .) %>%
    gsub("\\.|,|;", "", .) %>%
    paste0(".xlsx")
}

as_fwsinat <- function(x) {
  if (inherits(x, "fwsinat")) return(x)
  class(x) <- c("fwsinat", class(x))
  return(x)
}

wrap_text <- function(...) {
  text <- list(...)
  text <- paste(text, collapse = " ")
  paste(strwrap(text), collapse = "\n")
}

empty_itis <- function() {
  tibble(sci_name = NA_character_,
         valid_sci_name = NA_character_,
         itis_com_name = NA_character_,
         itis_taxon_rank = NA_character_)
}
