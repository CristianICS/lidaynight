#' Compress the las files inside a folder
#'
#' @note
#' One important limitation: function requires to open the LAS file. With lidR,
#' the file is read into R and then written back as LAZ. For very large LAS
#' files, laszip from lastools (rapidLasso) would be a more efficient solution.
#'
#' @param ctg_folder Path pointing to a folder filled with las files.
#' @param overwrite If set to `TRUE`, overwrite the existing laz files.
#' @param remove_original If `TRUE`, remove the original las files.
#'
#' @export
catalogCompression <- function(
  ctg_folder,
  overwrite = FALSE,
  remove_original = FALSE
) {

  files <- list.files(
    ctg_folder,
    pattern = "\\.las$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  for (f in files) {
    laz_file <- paste0(tools::file_path_sans_ext(f), ".laz")

    if (file.exists(laz_file) & !isTRUE(overwrite)) {
      message("Skipping, already exists: ", laz_file)
      next
    }

    las <- lidR::readLAS(f)

    if (is.null(las)) {
      warning("Could not read file: ", f)
      next
    }

    ok <- lidR::writeLAS(las, laz_file, index=TRUE)

    if (isTRUE(ok) || file.exists(laz_file)) {
      if (remove_original) {
        file.remove(f)
      }
    } else {
      warning("Failed to write: ", laz_file)
    }

    rm(las)
    gc()
  }

  invisible(TRUE)
}
