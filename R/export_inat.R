#' Export iNaturalist observations to Excel file(s) based on for assigned to FWS properties  refuges
#'
#' @param fwsinat \code{fwsinat} \code{data.frame} of iNaturalist observations
#'  produced by running \code{\link{retrieve_inat}}, and (optionally) subsequently through
#'  \code{\link{assign_inat}}
#' @param dir a non-empty character scalar giving the directory within which to store
#'  exported spreadsheets
#'
#' @return \code{NULL}; Exports Excel file(s) to \code{dir}
#'
#' @import xlsx
#' @export
#' @examples
#' \dontrun{
#' fws <- retrieve_inat()
#' export_inat(fws, "./fws.xlsx")
#' }

export_inat <- function (fwsinat, dir = NULL, orgname, verbose = TRUE) {

  if (is.null(dir)) stop("You must specify and output directory.\n",
                         "If it does not exist it will be created.")
  if (!dir.exists(dir)) dir.create(dir)

  if (verbose) cat("Processing", paste0(orgname, "...  "))

  # Drop orgname from output
  fwsinat <- select(fwsinat, -orgname)

  oldOpt <- options(xlsx.date.format="yyyy-mm-dd")
  on.exit(options(oldOpt))
  wb <- createWorkbook(type = "xlsx")
  sheet <- createSheet(wb, "fwsinat Output")
  noRows <- nrow(fwsinat) + 1 # add room for header row
  noCols <- ncol(fwsinat)
  rows <- createRow(sheet, 1)
  cells <- createCell(rows, colIndex = 1:noCols)
  mapply(setCellValue, cells[1, 1:noCols], colnames(fwsinat))
  colIndex <- seq_len(ncol(fwsinat))
  rowIndex <- seq_len(nrow(fwsinat)) + 1 # skip header row
  createFreezePane(sheet, 2, 1)

  url_col <- which(names(fwsinat) == "url")
  rows <- createRow(sheet, rowIndex)
  cells <- createCell(rows, colIndex)

  for (ic in seq_along(fwsinat)) {
    if (ic != url_col) {
      mapply(setCellValue, cells[seq_len(nrow(cells)), colIndex[ic]], fwsinat[, ic],
             showNA = FALSE)
    } else {
      mapply(setCellValue, cells[seq_len(nrow(cells)), colIndex[ic]], "iNaturalist record",
             showNA = FALSE)
      mapply(addHyperlink, cells[seq_len(nrow(cells)), colIndex[ic]], fwsinat[, ic])
    }
  }

  indDT <- which(sapply(fwsinat, function(i) inherits(i, "Date")))
  if (length(indDT) > 0) {
    dateFormat <- CellStyle(wb) + DataFormat(getOption("xlsx.date.format"))
    for (ic in indDT) lapply(cells[seq_len(nrow(cells)), colIndex[ic]], setCellStyle, dateFormat)
  }

  addAutoFilter(getSheets(wb)[["fwsinat Output"]],
                paste(LETTERS[c(1, ncol(fwsinat))], collapse = ":"))
  autoSizeColumn(getSheets(wb)[["fwsinat Output"]], which(!names(fwsinat) == "notes"))

  fn <- construct_fn(orgname)
  saveWorkbook(wb, file.path(dir, fn))
  if (verbose) cat(fn, "successfully created.\n")
  invisible()
}
