#' Perform a ground classification
#'
#' Classify ground points using Progressive TIN Densification (PTD) algorithm.
#'
#' @param ctg_folder Folder to open as `lasCatalog`.
#' @param overwrite If the output folder already exists, skip the computation
#'   by default. Otherwise, set this to `TRUE`.
#' @param n_workers When parallel computation is activated, the number of worker
#'   or CPU cores to use.
#' @param parallel If `TRUE`, compute the new tiles in asynchronous way.
#' @param outlier_filter If `TRUE`, remove elevation outliers (flyers) from
#'   each chunk before ground classification, based on the 1st and 99th
#'   percentiles of Z.
#' @export
groundClassification <- function(
  ctg_folder,
  overwrite=FALSE,
  n_workers=NULL,
  parallel=FALSE,
  outlier_filter=FALSE
) {

  stopifnot(dir.exists(ctg_folder))

  output_folder <- file.path(ctg_folder, "cls")

  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

    if (!dir.exists(output_folder)) {
      stop("Could not create output folder.")
    }
  }

  # Check if there are classified files inside the directory
  cls_files <- list.files(
    output_folder,
    pattern = "\\.la[sz]$",
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )

  if (length(cls_files) > 0 && !overwrite) {
    warning(
      "Classified files exist inside the output folder. Set `overwrite=TRUE` ",
      "to recompute. Returning the existing output folder."
    )
    return(invisible(output_folder))
  }

  if (length(cls_files) > 0 && overwrite) {
    # Try to remove the files
    failed <- unlink(cls_files, force = TRUE)
    if (any(failed != 0)) {
      stop("Some existing files could not be removed.", call. = FALSE)
    }
  }

  message("Opening lidar files as LASCatalog...")

  ctg <- openCatalog(ctg_folder)

  lidR::opt_output_files(ctg) <- file.path(output_folder, "{*}_ground")
  lidR::opt_chunk_size(ctg) <- 0
  lidR::opt_chunk_buffer(ctg) <- 10
  lidR::opt_laz_compression(ctg) <- TRUE
  lidR::opt_progress(ctg) <- TRUE

  message("Starting ground classification...")

  # Conduct the operation in parallel
  if (isTRUE(parallel)) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)

    if (is.null(n_workers)) {
      future::plan(future::multisession)
    } else {
      future::plan(future::multisession, workers = n_workers)
    }
  }

  if (isTRUE(outlier_filter)) {
    classify_chunk <- function(chunk) {
      las <- lidR::readLAS(chunk)
      if (lidR::is.empty(las)) return(NULL)

      # Compute 1st and 99th percentile of Z to identify flyers
      qq <- stats::quantile(
        las$Z,
        probs = c(0.01, 0.99),
        na.rm = TRUE,
        names = FALSE,
        type = 7
      )

      # Keep only points within the quantile range
      las_clean <- lidR::filter_poi(las, Z >= qq[1] & Z <= qq[2])

      las_classified <- lidR::classify_ground(
        las_clean,
        algorithm = lidR::ptd(res=20)
      )

      lidR::remove_buffer(chunk, las_classified)
    }

    lidR::catalog_apply(
      ctg,
      classify_chunk,
      .options = list(need_buffer = TRUE, automerge = TRUE)
    )
  } else {
    lidR::classify_ground(
      ctg,
      algorithm = lidR::ptd(res=20)
    )
  }

  gc()
  invisible(output_folder)
}
