#' Export iNaturalist observations to Excel file(s) based on for assigned to FWS properties  refuges
#'
#' @param fwsinat \code{fwsinat} \code{data.frame} of iNaturalist observations
#'  produced by running \code{\link{retrieve_inat}}, and (optionally) subsequently through
#'  \code{\link{assign_inat}}
#' @param dir a non-empty character scalar giving the directory within which to store
#'  exported spreadsheets
#' @param xl_out character string indicating the output file name (without extension);
#'  note that periods and commas will be removed and spaces will be replaced with underscores
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
#' fws <- retrieve_inat()
#' export_inat(fws, "./fws.xlsx")
#' }

export_inat <- function (fwsinat, dir = NULL, xl_out, overwrite = TRUE, verbose = TRUE) {

  if (!inherits(fwsinat, "fwsinat")) stop("This function is intended for use ",
                                          "only with `fwsinat` objects. See `?retrieve_inat`.")

  if (is.null(dir)) stop("You must specify and output directory.\n",
                         "If it does not exist it will be created.")
  if (!dir.exists(dir)) dir.create(dir)

  if (verbose) cat("Processing", paste0(xl_out, "...  "))

  # Drop orgname from output
  is_assign <- "orgname" %in% names(fwsinat)
  if (is_assign) fwsinat <- select(fwsinat, -orgname)

  oldOpt <- options("openxlsx.dateFormat" = "yyyy-mm-dd")
  on.exit(options(oldOpt))

  # Identify iNaturalist hyperlink column and round lat/lon
  class(fwsinat$url) <- "hyperlink"
  fwsinat <- mutate(fwsinat,
                    lat = round(lat, 4),
                    lon = round(lon, 4))

  wb <- createWorkbook()
  urls <- which(names(fwsinat) == "url")
  addWorksheet(wb, "fwsinat Output")

  col_widths <- c(rep(25, 3), rep(16, 2), rep(13, 2), rep(9, 2), 13, 16, 10, rep(16, 2))
  setColWidths(wb, 1, cols = seq_along(fwsinat), widths = col_widths)
  freezePane(wb, 1, firstRow = TRUE)

  # Write and save it
  writeData(wb, 1, fwsinat, withFilter = TRUE)
  # Change hyperlink display text
  writeData(wb, sheet = 1, x = rep("iNaturalist record", nrow(fwsinat)),
            startRow = 2, startCol = urls)

  fn <- construct_fn(xl_out)
  saveWorkbook(wb, file.path(dir, fn), overwrite = overwrite)
  if (verbose) cat(fn, "successfully created.\n")
  invisible()
}
