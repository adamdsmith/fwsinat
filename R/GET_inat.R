GET_inat <- function (place_id, proj = "usfws-national-wildlife-refuge-system",
                      taxon_name = NULL, d1 = NULL, d2 = NULL, since_date = NULL,
                      nrecs_only = FALSE, verbose) {

  # Set up filters
  place_id <- paste0("place_id=", place_id)
  if (!is.null(proj)) proj <- paste0("projects%5B%5D=", proj)
  if (!is.null(d1)) d1 <- paste0("d1=", d1)
  if (!is.null(d2)) d2 <- paste0("d2=", d2)
  if (!is.null(taxon_name)) taxon_name <- paste0("taxon_name=",
                                                 gsub(" ", "+", taxon_name))
  if (!is.null(since_date)) since_date <- paste0("updated_since=", since_date)
  search <- paste(c("", place_id, proj, d1, d2, taxon_name, since_date), collapse = "&")

  # Ping records
  base_url <- "http://www.inaturalist.org/observations"
  ping_path <- paste0(".json?verifiable=true", "&page=1&per_page=1", search)
  p_url <- paste0(base_url, ping_path)
  ping <- httr::GET(p_url)
  n_recs <- as.integer(ping$headers$`x-total-entries`)
  if (nrecs_only) return(n_recs)

  # Retrieve observations
  dat <- NULL
  if (n_recs == 0 && verbose) {
    message("Your search returned zero results. Either the property (and/or project)\n",
            "of interest has no records or you entered a too restrictive or invalid\n",
            "search.")
    return(dat)
  }

  n_loops <- min(ceiling(n_recs / 200), 50)
  dl_recs <- ifelse(n_recs > 10000, "first 10,000", n_recs)
  if (verbose) {
    message("Retrieving ", dl_recs, " records.")
    cat("Records retrieved: \n  0")
  }

  for (i in seq_len(n_loops)) {
    query_path <- paste0(".csv?verifiable=true&page=", i, "&per_page=200", search)
    q_url <- paste0(base_url, query_path)
    q_dat <- utils::read.csv(q_url, stringsAsFactors = FALSE, encoding = "UTF-8") %>%
      # Get rid of some unnecessary and occasionally problem-causing columns
      select(-.data$tag_list)
    dat <- bind_rows(dat, q_dat)
    if (verbose) {
      if (i %% 16 == 0) cat("\n ", nrow(dat)) else cat(paste0("-", nrow(dat)))
      if (i == n_loops) cat("\n")
    }
  }

  dat

}
