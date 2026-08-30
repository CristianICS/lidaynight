#' @title Check LAS/LAZ scale factor consistency
#' @description
#' Reads the header of every LAS/LAZ file in a folder and checks whether
#' they all share the same X/Y/Z scale factor. Useful before deduplicating
#' points merged from multiple flightlines/tiles (e.g. via
#' [lidR::filter_duplicates()] or `duplicated()`), since files quantized
#' with different scale factors can decode the same physical point to
#' different floating-point coordinates, causing missed duplicates
#' (false negatives).
#'
#' @param folder Path to the folder containing the LAS/LAZ files.
#' @param pattern Regex used to match LAS/LAZ file names. Defaults to
#'   `"\\.(laz|las)$"`.
#'
#' @return A named list with:
#' * `info`: a `data.frame` with one row per file, listing its X/Y/Z
#'   scale factors and offsets.
#' * `all_same`: `TRUE` if every file shares the same X/Y/Z scale factor,
#'   `FALSE` otherwise.
#' @export
check_scale_factors <- function(folder, pattern = "\\.(laz|las)$") {
  files <- list.files(folder, pattern = pattern, full.names = TRUE, ignore.case = TRUE)

  if (length(files) == 0L) {
    stop("No LAS/LAZ files found in: ", folder)
  }

  info <- do.call(rbind, lapply(files, function(f) {
    header <- lidR::readLASheader(f)
    data.frame(
      file = basename(f),
      x_scale = header[["X scale factor"]],
      y_scale = header[["Y scale factor"]],
      z_scale = header[["Z scale factor"]],
      x_offset = header[["X offset"]],
      y_offset = header[["Y offset"]],
      z_offset = header[["Z offset"]]
    )
  }))

  unique_scales <- unique(info[c("x_scale", "y_scale", "z_scale")])
  all_same <- nrow(unique_scales) == 1L

  if (all_same) {
    message(
      "All ", length(files), " files share the same scale factor: ",
      unique_scales$x_scale, ", ", unique_scales$y_scale, ", ", unique_scales$z_scale
    )
  } else {
    message(
      nrow(unique_scales), " distinct scale factor combinations found across ",
      length(files), " files:"
    )
    print(unique_scales)
  }

  list(info = info, all_same = all_same)
}
