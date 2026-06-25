#' Compute the RMSE and associated statistics from ground reference targets
#'
#' The stats are computed based on the target class, which could be one of the
#' followings:
#'
#' * `road` - Points located in roads or good paths with no vegetation cover.
#' * `shrub` - Area covered with bushes and shrubs with no or sparse trees.
#' * `open_treed` - Areas with sparse trees but with understory.
#' * `dense_treed` - High dense vegetated areas.
#'
#' @param ctg_path Folder containing the files to create a `lidR::LAScatalog`.
#' @param pnts_path Path to the spatial file containing the target points.
#' @param truth_col Name of numeric column holding ground-truth height.
#' @param class_col The name of the column holding the available target classes.
#' @param layer_name Layer name; if `NULL`, the first layer is used.
#' @param buffer_m Buffer radius in meters to apply to each target.
#' @param incoh_thresh Absolute residual threshold for incoherency.
#' @param out_folder An optional folder to save the files with the stats.
#' @param id_col Optional ID column; default uses row index.
#' @param ground_filter If `TRUE`, keep only points classified as ground (2).
#' @param convert_heights If `TRUE`, convert target heights between ellipsoidal
#'   and orthometric heights.
#' @param is_ellip Are input target heights in ellipsoidal system?
#' @param geoid_height_col Column name inside the `sf` object with the height
#'   of the geoid, used to compute ellipsoidal heights.
#'
#' @return Invisibly returns `NULL`. Writes one GPKG and one CSV file per target class.
#' @export
targetStats <- function(
    ctg_path,
    pnts_path,
    truth_col,
    class_col,
    layer_name = NULL,
    buffer_m = 0.5,
    incoh_thresh = 0.2,
    out_folder = "",
    id_col = NULL,
    ground_filter = TRUE,
    convert_heights = FALSE,
    is_ellip = FALSE,
    geoid_height_col = NULL
) {

  target_classes <- c(
    "road",
    "open_treed",
    "dense_treed"
  )

  if (!dir.exists(out_folder)) {
    dir.create(out_folder, recursive = TRUE)
  }

  if (!is.numeric(buffer_m) || length(buffer_m) != 1L || buffer_m <= 0) {
    stop("`buffer_m` must be a single positive numeric value.")
  }

  if (!is.numeric(incoh_thresh) || length(incoh_thresh) != 1L || incoh_thresh < 0) {
    stop("`incoh_thresh` must be a single non-negative numeric value.")
  }

  ctg <- openCatalog(ctg_path)

  # Prevent to store the metrics in separate files
  lidR::opt_output_files(ctg) <- ""
  lidR::opt_chunk_size(ctg) <- 0
  lidR::opt_chunk_buffer(ctg) <- 0

  if (ground_filter) {
    lidR::opt_filter(ctg) <- "-keep_class 2"
  }

  pnts <- openSpatial(pnts_path, ctg, layer_name)

  if (!truth_col %in% names(pnts)) {
    stop("Column '", truth_col, "' was not found in pnts layer.")
  }

  if (!is.numeric(pnts[[truth_col]])) {
    stop("Column '", truth_col, "' must be numeric.")
  }

  if (!class_col %in% names(pnts)) {
    stop("Column '", class_col, "' was not found in pnts layer.")
  }

  # Select only the pnts where their class_col value is inside target_classes
  pnts <- pnts[pnts[[class_col]] %in% target_classes, ]

  if (nrow(pnts) == 0L) {
    stop("No target points matched the expected target classes.")
  }

  if (is.null(id_col)) {
    pnts$.tid <- seq_len(nrow(pnts))
  } else if (!id_col %in% names(pnts)) {
    warning(
      sprintf(
        "ID column '%s' not found. Creating '.tid' using row indexes.",
        id_col
      )
    )

    pnts$.tid <- seq_len(nrow(pnts))
  } else {
    pnts$.tid <- pnts[[id_col]]
  }

  if (convert_heights) {
    pnts <- convertHeights(pnts, truth_col, is_ellip, geoid_height_col)
  }

  pnt_buffers <- sf::st_buffer(pnts, dist = buffer_m)

  # Store class-level summaries here.
  class_stats <- list()

  # To avoid including a lot of data into memory, perform computations by class
  for (cls_name in target_classes) {

    i_pnt_buffers <- pnt_buffers[pnt_buffers[[class_col]] == cls_name, ]

    if (nrow(i_pnt_buffers) == 0L) {
      message(sprintf("There are no points for class %s", cls_name))
      next
    }

    message(sprintf("Compute points for class %s", cls_name))
    # Select only the points inside the buffers.
    # Returns a list of LAS objects, one per buffer
    las_list <- lidR::clip_roi(ctg, i_pnt_buffers)

    ref_table <- sf::st_drop_geometry(i_pnt_buffers) |>
      dplyr::select(.tid, dplyr::all_of(truth_col))

    if (length(las_list) != nrow(ref_table)) {
      stop(
        "The number of clipped LAS objects does not match the number of ",
        "reference targets for class '", cls_name, "'."
      )
    }

    out <- data.table::rbindlist(
      Map(
        compute_target_metrics,
        las_list,
        ref_table$.tid,
        ref_table[[truth_col]],
        MoreArgs = list(incoh_thresh = incoh_thresh)
      )
    )

    # RMSE of LiDAR last returns within each buffer relative to that
    # target's reference height.
    out$rmse_last <- ifelse(
      out$n_pnts_last > 0L,
      sqrt(out$sum_errs_last_sq / out$n_pnts_last),
      NA_real_
    )

    out$bias_last <- ifelse(
      out$n_pnts_last > 0L,
      out$sum_errs_last / out$n_pnts_last,
      NA_real_
    )

    # Aggregate by class.
    class_stats[[cls_name]] <- aggregateByClass(out, cls_name)

    out_name <- if (ground_filter) {
      paste("bytarget", "gf", cls_name, sep = "_")
    } else {
      paste("bytarget", cls_name, sep = "_")
    }

    out_sf <- dplyr::left_join(
      i_pnt_buffers,
      out,
      by = c(".tid" = "pnt_id")
    )

    sf::st_write(
      out_sf,
      file.path(out_folder, paste0(out_name, ".gpkg")),
      delete_dsn = TRUE,
      quiet = TRUE
    )

    utils::write.csv(
      out,
      file.path(out_folder, paste0(out_name, ".csv")),
      row.names = FALSE
    )

  }

  if (length(class_stats) > 0L) {
    class_stats <- data.table::rbindlist(class_stats, use.names = TRUE)

    out_name <- if (ground_filter) {
      "byclass_gf"
    } else {
      "byclass"
    }

    utils::write.csv(
      class_stats,
      file.path(out_folder, paste0(out_name, ".csv")),
      row.names = FALSE
    )
  }

  invisible(NULL)
}

#' Height conversion
#'
#' Convert ellipsoidal heights into geoidal ones or viceversa.
#'
#' @param pnts An `sf` object with the target points.
#' @param truth_col Name of numeric column holding ground-truth height.
#' @param is_ellip Are input target heights in ellipsoidal system?
#' @param geoid_height_col Column name inside the `sf` object with the height
#'   of the geoid, used to compute ellipsoidal heights.
#' @noRd
convertHeights <- function(
  pnts,
  truth_col,
  is_ellip,
  geoid_height_col = NULL
) {

  if (is.null(geoid_height_col)) {
    stop(paste0(
      "`geoid_height_col` must be provided when ",
      "`convert_heights = TRUE`."
    ))
  }

  if (!(geoid_height_col %in% names(pnts))) {
    stop(sprintf(
      "Column '%s' not found inside the spatial layer.",
      geoid_height_col
    ))
  }

  geoid_height <- pnts[[geoid_height_col]]

  pnts[[truth_col]] <- if (!is_ellip) {
    # Input is orthometric; convert to ellipsoidal
    pnts[[truth_col]] + geoid_height
  } else {
    # Input is ellipsoidal; convert to orthometric
    pnts[[truth_col]] - geoid_height
  }

  pnts
}

#' Compute metrics from one point cloud within one ground reference point
#'
#' This function is called after [lidR::clip_roi()], obtaining a list of point
#' clouds for each ground reference point. Call this function inside [Map()].
#'
#' @details
#' The derived metrics are (see `.global_stats_ptype` prototype):
#' * n_pnts = Total number of points inside the buffer area.
#' * n_pnts_last = Number of points being last returns.
#' * n_pnts_last_incoherent = Last returns where the error between the point
#'     height and the ground reference value of the target is > `incoh_thresh`.
#' * intensity_avg = The mean intensity between all the valid points.
#' * thickness = Difference between max and min height among last returns.
#' * sum_errs_last = The sum of the height errors from last return points.
#' * sum_errs_last_sq = The sum of squared height errors from last return points.
#'
#' @param las A clipped LAS object.
#' @param pnt_id The ID of the current point to compute the stats.
#' @param pnt_height Ground reference height.
#' @param incoh_thresh Heigh value acting as a threshold to determine invalid
#'   heigh values.
#' @noRd
compute_target_metrics <- function(
    las,
    pnt_id,
    pnt_height,
    incoh_thresh
) {

  if (lidR::is.empty(las)) {
    out <- data.table::data.table(
      pnt_id = pnt_id,
      n_pnts = 0L,
      n_pnts_last = 0L,
      n_pnts_last_incoherent = 0L,
      intensity_avg = NA_real_,
      thickness = NA_real_,
      sum_errs_last = 0,
      sum_errs_last_sq = 0
    )

    return(out)
  }

  d <- las@data

  # Compute the last returns only and prevent NA values
  last_only <- !is.na(d$ReturnNumber) &
    !is.na(d$NumberOfReturns) &
    d$ReturnNumber == d$NumberOfReturns

  h_last <- d$Z[last_only]

  n_last <- length(h_last)

  intensity_avg <- mean(d$Intensity, na.rm = TRUE)

  if (n_last == 0L) {

    out <- data.table::data.table(
      pnt_id = pnt_id,
      n_pnts = nrow(d),
      n_pnts_last = 0L,
      n_pnts_last_incoherent = 0L,
      intensity_avg = intensity_avg,
      thickness = NA_real_,
      sum_errs_last = 0,
      sum_errs_last_sq = 0
    )
    return(out)
  }

  thickness <- if (sum(!is.na(h_last)) >= 2L) {
    diff(range(h_last, na.rm = TRUE))
  } else {
    NA_real_
  }

  # If ground reference height is NA, the error metrics will return 0 (wrong)
  if (is.na(pnt_height)) {
    return(data.table::data.table(
      pnt_id = pnt_id,
      n_pnts = nrow(d),
      n_pnts_last = n_last,
      n_pnts_last_incoherent = NA_integer_,
      intensity_avg = intensity_avg,
      thickness = thickness,
      sum_errs_last = NA_real_,
      sum_errs_last_sq = NA_real_
    ))
  }

  errs_last <- h_last - pnt_height
  valid_errs <- !is.na(errs_last)

  n_incoherent <- sum(abs(errs_last[valid_errs]) > incoh_thresh)

  out <- data.table::data.table(
    pnt_id = pnt_id,
    n_pnts = nrow(d),
    n_pnts_last = n_last,
    n_pnts_last_incoherent = n_incoherent,
    intensity_avg = intensity_avg,
    thickness = thickness,
    sum_errs_last = sum(errs_last, na.rm = TRUE),
    sum_errs_last_sq = sum(errs_last^2, na.rm = TRUE)
  )

  return(out)
}

#' Group the stats for each class
#' @details
#' Perform an aggregation of all the target metrics for one class.
#'
#' @note
#' Do not average per-target RMSE values because each buffer can contain a
#' different number of last returns.
#'
#' @param df Table with stats per target.
#' @param cls_name The class name being aggregated.
#' @noRd
aggregateByClass <- function(df, cls_name) {
  n_last_total <- sum(df$n_pnts_last, na.rm = TRUE)

  aggr <- data.frame(
    class = cls_name,
    n_targets = nrow(df),
    n_targets_with_last = sum(df$n_pnts_last > 0L, na.rm = TRUE),
    n_pnts_last = n_last_total,
    sum_errs_last = sum(df$sum_errs_last, na.rm = TRUE),
    sum_errs_last_sq = sum(df$sum_errs_last_sq, na.rm = TRUE),
    rmse_last = ifelse(
      n_last_total > 0L,
      sqrt(sum(df$sum_errs_last_sq, na.rm = TRUE) / n_last_total),
      NA_real_
    ),
    bias_last = ifelse(
      n_last_total > 0L,
      sum(df$sum_errs_last, na.rm = TRUE) / n_last_total,
      NA_real_
    )
  )
}


.target_stats_ptype <- data.table::data.table(
  pnt_id = integer(),
  n_pnts = integer(),
  n_pnts_last = integer(),
  n_pnts_last_incoherent = integer(),
  intensity_avg = double(),
  thickness = double(),
  # Helpers to perform later RMSE computation of common targets
  sum_errs_last = double(),
  sum_errs_last_sq = double()
)

