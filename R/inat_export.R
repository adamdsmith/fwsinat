#' Export iNaturalist observations to Excel file(s) based on for assigned to FWS properties  refuges
#'
#' @param fwsinat \code{fwsinat} \code{data.frame} of iNaturalist observations
#'  produced by running \code{\link{inat_retrieve}}
#' @param dir a non-empty character scalar giving the directory within which to store
#'  exported spreadsheets
#' @param overwrite logical (default \code{TRUE}) indicating whether to overwrite an existing file
#' @param verbose logical (default \code{TRUE}) indicating whether to provide messaging during
#'  export process
#'
#' @return \code{NULL}; Exports Excel file(s) to \code{dir}
#'
#' @import openxlsx
#' @export
#' @examples
#' \dontrun{
#' fws <- inat_retrieve()
#' inat_export(fws, "./fws.xlsx")
#' }

inat_export <- function (fwsinat, dir = NULL, overwrite = TRUE, verbose = TRUE) {

  if (!inherits(fwsinat, "fwsinat")) stop("This function is intended for use ",
                                          "only with `fwsinat` objects. See `?inat_retrieve`.")

  if (is.null(dir)) stop("You must specify and output directory.\n",
                         "If it does not exist it will be created.")
  if (!dir.exists(dir)) dir.create(dir)

  orgs <- sort(unique(fwsinat$orgname))

  oldOpt <- options("openxlsx.dateFormat" = "yyyy-mm-dd")
  on.exit(options(oldOpt))

  r <- utils::read.csv(system.file("extdata", "fws_place_ids.csv", package = "fwsinat"),
                       stringsAsFactors = FALSE)
  fwsinat <- left_join(fwsinat, r[, c("orgname", "name")], by = "orgname")

  # Identify iNaturalist hyperlink column and round lat/lon
  class(fwsinat$url) <- "hyperlink"
  fwsinat <- mutate(fwsinat,
                    lat = round(.data$lat, 4),
                    lon = round(.data$lon, 4))

  lapply(orgs, function(org) {
    org_dat <- filter(fwsinat, .data$orgname == org)
    nm <- unique(org_dat$name)
    org_dat <- select(org_dat, -.data$orgname, -.data$name)
    wb <- createWorkbook()
    urls <- which(names(org_dat) == "url")
    addWorksheet(wb, "fwsinat Output")

    col_widths <- c(rep(25, 3), rep(16, 2), rep(13, 2), rep(9, 2), 13, 16, 10, rep(16, 2))
    setColWidths(wb, 1, cols = seq_along(org_dat), widths = col_widths)
    freezePane(wb, 1, firstRow = TRUE)

    # Write and save it
    writeData(wb, 1, org_dat, withFilter = TRUE)
    # Change hyperlink display text
    writeData(wb, sheet = 1, x = rep("iNaturalist record", nrow(org_dat)),
              startRow = 2, startCol = urls)

    fn <- construct_fn(nm)
    if (verbose) cat("Processing", paste0(nm, "...  "))
    saveWorkbook(wb, file.path(dir, fn), overwrite = overwrite)
    if (verbose) cat("Export successful.\n")
  })

  invisible()

}
