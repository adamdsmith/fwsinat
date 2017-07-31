GET_inat <- function (id, d1, d2, since_date, nrecs_only = FALSE) {

  # Set up filters
  if (!is.null(d1)) d1 <- paste0("d1=", d1)
  if (!is.null(d2)) d2 <- paste0("d2=", d2)
  if (!is.null(since_date)) since_date <- paste0("updated_since=", since_date)
  search <- c(d1, d2, since_date)
  if (!is.null(search)) search <- paste0("&", paste(search, collapse = "&"))

  base_url <- "http://www.inaturalist.org/observations/project/"
  ping_path <- paste0(id, ".json", "?page=1&per_page=1", search)
  p_url <- paste0(base_url, ping_path)
  ping <- httr::GET(p_url)
  n_recs <- as.integer(ping$headers$`x-total-entries`)

  if (identical(n_recs, integer(0))) stop("That iNaturalist project was not found.")

  if (nrecs_only) return(n_recs)

  dat <- NULL

  if (n_recs == 0) {
    message("Your search returned zero results. Either the project of interest\n",
            "has no records or you entered a too restrictive or invalid search.")
    return(dat)
  }

  n_loops <- min(ceiling(n_recs / 200), 50)
  dl_recs <- ifelse(n_recs > 10000, "first 10,000", n_recs)
  message("Retrieving ", dl_recs, " records.")
  cat("Records retrieved: \n  0")

  for (i in seq_len(n_loops)) {
    query_path <- paste0(id, ".csv?page=", i, "&per_page=200", search)
    q_url <- paste0(base_url, query_path)
    qdat <- utils::read.csv(q_url, stringsAsFactors = FALSE, encoding = "UTF-8")
    dat <- bind_rows(dat, qdat)
    if (i %% 16 == 0) cat("\n  ") else cat("-"); cat(nrow(dat))
  }
  cat("\n")

  dat

}
