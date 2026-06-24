#' @title ROI/QA LiDAR metrics (extended)
#' @description
#' Computes per-tile extended QA metrics (adds pct 1-return, pct last,
#' NA heights, percentile outliers, negatives) and writes a CSV of column means.
#'
#' @param ctg_folder Path pointing to the folder containing the laz files.
#' @param roi_path Vector layer to search the stats within.
#' @param out_path Folder where  `out_name` will be written.
#' @param out_name Output CSV file name.
#'
#' @return Invisibly, a list with the summarized statistics.
#' @export
globalStats <- function(
    ctg_folder,
    roi_path,
    out_path,
    out_name = "roi_stats.csv"
) {
  ctg <- openCatalog(ctg_folder)

  # Prevent to store the metrics in separate files
  lidR::opt_output_files(ctg) <- ""

  roi <- openSpatial(roi_path, ctg)

  stats <- lidR::plot_metrics(
    ctg,
    ~lidaynight::roi_global_metrics(
      Z = Z,
      X = X,
      Y = Y,
      Intensity = Intensity,
      ReturnNumber = ReturnNumber,
      NumberOfReturns = NumberOfReturns,
      compute_duplicates = TRUE
    ),
    roi
  )

  stats <- add_pct_metrics(stats)

  # Remove temporal data
  gc()

  if (!dir.exists(out_path)) {
    dir.create(out_path, recursive = TRUE)
  }

  sf::st_write(
    stats,
    file.path(out_path, sub("\\.csv$", ".gpkg", out_name)),
    delete_dsn = TRUE,
    quiet = TRUE
  )

  utils::write.csv(
    sf::st_drop_geometry(stats),
    file.path(out_path, out_name),
    row.names = FALSE
  )

  invisible(stats)
}

#' @title Compute global LAS tile metrics
#'
#' @description Compute descriptive acquisition/structure indicators. These
#' set of statistics could be computed without the need of a ground
#' classification. All of them are absolute values instead percentages because
#' these metrics are normally computed across different tiles. The percentages
#' are computed in a later stage.
#'
#' The current function has been designed to include it inside
#' [lidR::plot_metrics()]. The function clips each ROI,
#' computes cloud metrics, and binds the metrics back to the ROI geometry.
#' It is documented as a wrapper around [lidR::clip_roi()],
#' [lidR::cloud_metrics()], and `cbind`, and its func argument is a
#' formula/expression, not a `LAScluster` worker.
#'
#' @details
#' The derived metrics are (see `.global_stats_ptype` prototype):
#' * n_pnts = Total number of points inside the tile file.
#' * dup_pnts = The number of duplicated points.
#' * intensity_avg = The mean intensity between all the valid points.
#' * ret_\* = Number of points with 1, 2 and 3 returns. Currently
#'   only 1-3 returns are computed.
#' * Single-return points (pulses that generated exactly one return)
#' * Points with more than one return
#' * Points without Z data
#' * Points with negative height
#' * Thresholds for the 1st and 99th quantile of heights
#' * Points from 0 to 1st quantile
#' * Points from 99th to last quantile
#'
#' @note
#' The parameters of the function are the LAS point properties required to
#' compute the statistics.
#'
#' @param Z Point height
#' @param Intensity Intensity value
#' @param ReturnNumber Represent the return number associated with the point.
#' @param NumberOfReturns Represent the amount of returns of the current point.
#' @param compute_duplicates If `TRUE`, compute duplicated points by check the
#'   amount of data with equal X,Y,Z coordinates.
#' @param X Coordinate X of the point. Required only to compute duplicate points
#' @param Y Coordinate X of the point. Required only to compute duplicate points
#' @return A named list with the computed metrics.
#' @export
roi_global_metrics <- function(
    Z,
    Intensity,
    ReturnNumber,
    NumberOfReturns,
    compute_duplicates = FALSE,
    X = NULL,
    Y = NULL
) {
  n <- length(Z)

  if (n == 0L) {
    stats <- list(
      n_pnts = 0L,
      dup_pnts = NA_integer_,
      intensity_avg = NA_real_,
      ret_1 = 0L,
      ret_2 = 0L,
      ret_3 = 0L,
      ret_single = 0L,
      ret_abvone = 0L,
      pnts_na_height = 0L,
      pnts_negative_height = 0L,
      height_q01 = NA_real_,
      height_q99 = NA_real_,
      pnts_height_lt_q01 = NA_integer_,
      pnts_height_gt_q99 = NA_integer_
    )
    # Check if all the columns are present with the correct type, if FALSE an
    # error is returned
    vctrs::vec_cast(stats, .global_stats_ptype)

    return(stats)
  }


  ret_1 <- sum(ReturnNumber == 1L, na.rm = TRUE)
  ret_2 <- sum(ReturnNumber == 2L, na.rm = TRUE)
  ret_3 <- sum(ReturnNumber == 3L, na.rm = TRUE)

  ret_single <- sum(NumberOfReturns == 1L, na.rm = TRUE)

  # Points belonging to pulses with more than one return.
  # If you instead want second/third/etc. returns, use:
  # ret_abvone <- sum(ReturnNumber > 1L, na.rm = TRUE)
  ret_abvone <- sum(NumberOfReturns > 1L, na.rm = TRUE)

  pnts_na_height <- sum(is.na(Z))
  pnts_negative_height <- sum(Z < 0, na.rm = TRUE)

  if (pnts_na_height < n) {
    qq <- stats::quantile(
      Z,
      probs = c(0.01, 0.99),
      na.rm = TRUE,
      names = FALSE,
      type = 7
    )

    height_q01 <- qq[1]
    height_q99 <- qq[2]

    pnts_height_lt_q01 <- sum(Z < height_q01, na.rm = TRUE)
    pnts_height_gt_q99 <- sum(Z > height_q99, na.rm = TRUE)
  } else {
    height_q01 <- NA_real_
    height_q99 <- NA_real_
    pnts_height_lt_q01 <- NA_integer_
    pnts_height_gt_q99 <- NA_integer_
  }

  dup_pnts <- NA_integer_

  if (
    isTRUE(compute_duplicates) &&
    !is.null(X) &&
    !is.null(Y) &&
    length(X) == n &&
    length(Y) == n
  ) {
    dup_pnts <- sum(duplicated(data.frame(X = X, Y = Y, Z = Z)))
  }

  stats <- list(
    n_pnts = as.integer(n),
    dup_pnts = as.integer(dup_pnts),
    intensity_avg = if (all(is.na(Intensity))) NA_real_ else mean(Intensity, na.rm = TRUE),
    ret_1 = as.integer(ret_1),
    ret_2 = as.integer(ret_2),
    ret_3 = as.integer(ret_3),
    ret_single = as.integer(ret_single),
    ret_abvone = as.integer(ret_abvone),
    pnts_na_height = as.integer(pnts_na_height),
    pnts_negative_height = as.integer(pnts_negative_height),
    height_q01 = as.numeric(height_q01),
    height_q99 = as.numeric(height_q99),
    pnts_height_lt_q01 = as.integer(pnts_height_lt_q01),
    pnts_height_gt_q99 = as.integer(pnts_height_gt_q99)
  )

  vctrs::vec_cast(stats, .global_stats_ptype)
  return(stats)
}

#' Compute the percentage from the statistics related to the number of points
#' @param stas The object returned from [lidR::plot_metrics()] function.
#' @param total_col Name of the column containing the total number of points.
#' @noRd
add_pct_metrics <- function(stats, total_col = "n_pnts") {
  n <- stats[[total_col]]

  pct_cols <- c(
    "dup_pnts",
    "ret_1",
    "ret_2",
    "ret_3",
    "ret_single",
    "ret_first",
    "ret_abvone",
    "pnts_na_height",
    "pnts_negative_height",
    "pnts_height_lt_q01",
    "pnts_height_gt_q99"
  )

  pct_cols <- intersect(pct_cols, names(stats))

  for (col in pct_cols) {
    out_col <- paste0(col, "_pct")

    stats[[out_col]] <- ifelse(
      is.na(n) | n <= 0 | is.na(stats[[col]]),
      NA_real_,
      (stats[[col]] / n) * 100
    )
  }

  stats
}

#' Prototypes for all the files containing the computed stats
.global_stats_ptype <- list(
  n_pnts = integer(),
  dup_pnts = integer(),
  intensity_avg = double(),
  ret_1 = integer(),
  ret_2 = integer(),
  ret_3 = integer(),
  ret_single = integer(),
  ret_abvone = integer(),
  pnts_na_height = integer(),
  pnts_negative_height = integer(),
  height_q01 = double(),
  height_q99 = double(),
  pnts_height_lt_q01 = integer(),
  pnts_height_gt_q99 = integer()
)
