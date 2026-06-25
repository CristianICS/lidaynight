#' Compress the LAS files inside a folder
#'
#' Converts all `.las` files in `ctg_folder` to `.laz` files using
#' [lidR::readLAS()] and [lidR::writeLAS()]. By default, existing `.laz`
#' files are not overwritten.
#'
#' @note
#' This function reads each LAS file into R before writing it back as LAZ.
#' For very large LAS files, command-line tools such as `laszip` from
#' LAStools/rapidlasso may be more memory-efficient.
#'
#' @param ctg_folder Path to a folder containing `.las` files.
#' @param overwrite If `TRUE`, overwrite existing `.laz` files.
#' @param create_lax If `TRUE`, create `.lax` spatial index files.
#'   For newly written `.laz` files, this is handled by [lidR::writeLAS()].
#'   For existing `.laz` files skipped because `overwrite = FALSE`, a missing
#'   `.lax` file is created with [rlas::writelax()].
#' @param remove_original If `TRUE`, remove the original `.las` file after the
#'   corresponding `.laz` file has been succesfully written.
#'
#' @return Invisibly returns `TRUE`.
#'
#' @export
compressLasFolder <- function(
  ctg_folder,
  overwrite = FALSE,
  create_lax = TRUE,
  remove_original = FALSE
) {

  stopifnot(
    is.character(ctg_folder),
    length(ctg_folder) == 1,
    dir.exists(ctg_folder),
    is.logical(overwrite),
    length(overwrite) == 1,
    is.logical(create_lax),
    length(create_lax) == 1,
    is.logical(remove_original),
    length(remove_original) == 1
  )

  files <- list.files(
    ctg_folder,
    pattern = "\\.las$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  for (f in files) {
    laz_file <- paste0(tools::file_path_sans_ext(f), ".laz")
    lax_file <- paste0(tools::file_path_sans_ext(f), ".lax")

    if (file.exists(laz_file) && !isTRUE(overwrite)) {
      if (isTRUE(create_lax) && !file.exists(lax_file)) {
        tryCatch(
          rlas::writelax(laz_file),
          error = function(e) {
            warning("Could not write LAX file for: ", laz_file,
                    "\nReason: ", conditionMessage(e))
          }
        )
      }

      next
    }

    las <- tryCatch(
      lidR::readLAS(f, check=FALSE),
      error = function(e) {
        warning("Could not read file: ", f,
                "\nReason: ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(las)) {
      warning("Could not read file: ", f)
      next
    }

    written <- tryCatch({
      lidR::writeLAS(las, laz_file, index = isTRUE(create_lax))
      file.exists(laz_file)
    }, error = function(e) {
      warning("Failed to write: ", laz_file,
              "\nReason: ", conditionMessage(e))
      FALSE
    })

    if (isTRUE(written) && isTRUE(remove_original)) {
      removed <- file.remove(f)

      if (!isTRUE(removed)) {
        warning("Could not remove original file: ", f)
      }
    }

    rm(las)
    gc()
  }

  invisible(TRUE)
}
