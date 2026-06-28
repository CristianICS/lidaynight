#' Plotting functions
#'
#' The following functions are able to produce the plots to construct the
#' reports for each site and the papers.

#' Parse acquisition metadata from a statistics folder name
#'
#' Extracts acquisition time and flight height from a folder name following the
#' expected pattern `<time>_<height>`, for example `"day_40"` or `"night_100"`.
#'
#' @param folder_path Character scalar. Path to a statistics subfolder.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{time}{Character. Acquisition time, currently `"day"` or `"night"`.}
#'   \item{height}{Numeric. Flight height extracted from the folder name.}
#' }
#'
#' @details
#' The function validates the folder name against the regular expression
#' `"^(day|night)_(\\d+)$"`. If the folder name does not match this pattern, an
#' error is raised.
parse_folder_metadata <- function(folder_path) {
  folder_name <- fs::path_file(folder_path)

  metadata <- stringr::str_match(folder_name, "^(day|night)_(\\d+)$")

  if (any(is.na(metadata))) {
    stop(
      "Folder name does not match expected pattern 'day_40', 'night_100', etc.: ",
      folder_name,
      call. = FALSE
    )
  }

  tibble::tibble(
    time = metadata[, 2],
    height = as.numeric(metadata[, 3])
  )
}


#' Read a statistics CSV from a folder
#'
#' Reads the required CSV stats file contained inside a statistics subfolder.
#'
#' @param folder_path Character scalar. Path to a folder expected to contain
#'   exactly one CSV file.
#' @param stat_prefix The character corresponding with the required stat file
#'   to find.
#'
#' @return A tibble containing the statistics read from the CSV file.
#'
#' @details
#' The function expects exactly one `.csv` file in `folder_path`. If no CSV file
#' or more than one CSV file is found, an error is raised.
read_stats_csv <- function(folder_path, stat_prefix) {
  pattern <- paste0(stat_prefix, "*.csv")
  csv_files <- list.files(folder_path, pattern = pattern, full.names = TRUE)

  if (length(csv_files) != 1) {
    stop(
      "Expected exactly one CSV file in folder: ",
      folder_path,
      ". Found: ",
      length(csv_files),
      call. = FALSE
    )
  }

  readr::read_csv(csv_files, show_col_types = FALSE)
}


#' Read statistics and metadata from one statistics folder
#'
#' Reads the statistics CSV from a single folder and appends metadata extracted
#' from the folder name.
#'
#' @param folder_path Character scalar. Path to a statistics subfolder named
#'   using the pattern `<time>_<height>`, for example `"day_75"`.
#' @param stat_prefix The character corresponding with the required stat file
#'   to find.
#'
#' @return A tibble containing the statistics from the CSV file plus two metadata
#' columns:
#' \describe{
#'   \item{time}{Character. Acquisition time extracted from the folder name.}
#'   \item{height}{Numeric. Flight height extracted from the folder name.}
#' }
#'
#' @details
#' This function combines [parse_folder_metadata()] and [read_stats_csv()].
#' It is intended for internal use by [load_all_statistics()], but can also be
#' useful for testing individual folders.
read_one_stats_folder <- function(folder_path, stat_prefix) {
  metadata <- parse_folder_metadata(folder_path)
  stats <- read_stats_csv(folder_path, stat_prefix)

  dplyr::mutate(
    stats,
    time = metadata$time,
    height = metadata$height
  )
}

#' Select custom columns for specific statistics files
#'
#' Project statistics files may contain many columns. For final reports and
#' publications, only a subset of columns is usually required. This function
#' selects the relevant columns according to the suffix used to identify the
#' statistics file type. Optionally, the selected columns can be renamed using
#' their display names. Mandatory ordering columns, such as `time` and
#' `height`, are always included.
#'
#' @param stats A data frame containing merged statistics.
#' @param suffix A character string identifying the statistics file type, such
#'   as `"roi_stats"`.
#' @param rename Logical. If `TRUE`, selected columns are renamed using their
#'   display names. Mandatory columns keep their original names. Defaults
#'   to `TRUE`.
#'
#' @return A data frame containing the mandatory ordering columns and the
#'   selected statistics columns.
select_stats_columns <- function(stats, suffix, rename = TRUE) {

  stats_columns <- switch(
    suffix,
    roi_stats = .roi_stats,
    bytarget_gf_dense_treed = .target_stats,
    bytarget_gf_open_treed = .target_stats,
    bytarget_gf_shrub = .target_stats,
    bytarget_gf_road = .target_stats,
    stop("Unsupported statistics suffix: ", suffix, call. = FALSE)
  )

  selected_columns <- c(.required_stats_columns, stats_columns)

  missing_columns <- setdiff(unname(selected_columns), names(stats))

  if (length(missing_columns) > 0) {
    stop(
      "The following required columns are missing from `stats`: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  selected_stats <- stats[, unname(selected_columns), drop = FALSE]

  if (rename) {
    names(selected_stats) <- names(selected_columns)
  }

  selected_stats
}

#' Load statistics from all acquisition folders
#'
#' Reads statistics from all subfolders inside a main statistics directory and
#' combines them into a single wide-format table.
#'
#' @param stats_dir Character scalar. Path to the main statistics directory.
#'   This directory should contain one subfolder per acquisition condition,
#'   named using the pattern `<time>_<height>`, for example `"day_40"`.
#' @param stat_prefix The character corresponding with the required stat file
#'   to find.
#' @param height_levels Numeric vector. Optional ordering of flight heights.
#'   Defaults to `c(40, 75, 100)`.
#' @param time_levels Character vector. Optional ordering of acquisition times.
#'   Defaults to `c("day", "night")`.
#'
#' @return A tibble in wide format. Each row corresponds to one acquisition
#' condition, with statistic columns plus:
#' \describe{
#'   \item{time}{Factor. Acquisition time.}
#'   \item{height}{Factor. Flight height.}
#' }
#'
#' @details
#' Each immediate subfolder of `stats_dir` is expected to contain exactly one
#' CSV file per `stat_prefix`. Folder names are used to infer acquisition
#' metadata.
#'
#' This function does not reshape the statistic columns. Use
#' [reshape_statistics_long()] to convert the output to long format for
#' plotting.
#'
#' @export
load_all_statistics <- function(
    stats_dir,
    stat_prefix,
    height_levels = c(40, 75, 100),
    time_levels = c("day", "night")
) {
  folders <- fs::dir_ls(stats_dir, type = "directory")

  if (length(folders) == 0) {
    stop("No subfolders found inside: ", stats_dir, call. = FALSE)
  }

  stats <- lapply(folders, read_one_stats_folder, stat_prefix = stat_prefix)

  stats <- do.call(vctrs::vec_rbind, stats)
  stats <- select_stats_columns(stats, stat_prefix)

  dplyr::mutate(
    stats,
    time = factor(time, levels = time_levels),
    height = factor(height, levels = height_levels)
  )
}


#' Reshape statistics from wide to long format
#'
#' Converts a wide-format statistics table into long format, with one row per
#' statistic and acquisition condition.
#'
#' @param stats_wide A data frame or tibble containing statistic columns and the
#'   metadata columns `time` and `height`.
#'
#' @return A tibble in long format with the columns:
#' \describe{
#'   \item{time}{Acquisition time.}
#'   \item{height}{Flight height.}
#'   \item{statistic}{Character. Name of the statistic.}
#'   \item{value}{Numeric. Statistic value.}
#' }
#'
#' @details
#' All columns except `time` and `height` are treated as statistic columns and
#' are gathered into the `statistic` and `value` columns.
#'
#' @examples
#' \dontrun{
#' stats_wide <- load_all_statistics("stats", "roi_stats")
#' stats_long <- reshape_statistics_long(stats_wide)
#' }
#' @export
reshape_statistics_long <- function(stats_wide) {
  tidyr::pivot_longer(
    stats_wide,
    cols = -c(time, height),
    names_to = "statistic",
    values_to = "value"
  )
}


#' Create dodged bar plots of statistics by height and acquisition time
#'
#' Creates a faceted dodged bar plot where each facet represents one statistic,
#' the x-axis represents flight height, and day/night values are shown as
#' side-by-side bars.
#'
#' @param stats_long A long-format data frame or tibble containing at least the
#'   columns `time`, `height`, `statistic`, and `value`.
#'
#' @return A `ggplot` object.
#'
#' @details
#' Values for `"day"` and `"night"` are plotted as positive bars and separated
#' using a dodged bar layout. This makes direct day-vs-night comparison possible
#' within each flight-height category.
#'
#' The input should usually be produced by [reshape_statistics_long()].
#'
#' @examples
#' \dontrun{
#' stats_wide <- load_all_statistics("stats", "roi_stats")
#' stats_long <- reshape_statistics_long(stats_wide)
#' make_dodged_barplot(stats_long)
#' }
#' @export
make_dodged_barplot <- function(stats_long) {
  stats_plot <- dplyr::mutate(
    stats_long,
    time = factor(time, levels = c("day", "night"))
  )

  ggplot2::ggplot(
    stats_plot,
    ggplot2::aes(x = factor(height), y = value, fill = time)
  ) +
    ggplot2::geom_col(
      width = 0.7,
      position = ggplot2::position_dodge(width = 0.75),
      color = "black",
      linewidth = 0.3
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        day = "#f9f0a1",
        night = "#867493"
      )
    ) +
    ggplot2::facet_wrap(
      ~ statistic,
      scales = "free_y"
    ) +
    ggplot2::labs(
      x = "Flight altitude (m)",
      y = "Statistic value",
      fill = "Time",
      title = "Statistics by flight altitude and acquisition time"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
}

#' Create violin plots of statistics by height and acquisition time
#'
#' Creates a faceted violin plot where each facet represents one statistic,
#' the x-axis represents flight height, and day/night values are shown as
#' side-by-side bars.
#'
#' @param stats_long A long-format data frame or tibble containing at least the
#'   columns `time`, `height`, `statistic`, and `value`.
#'
#' @return A `ggplot` object.
#'
#' @details
#' Values for `"day"` and `"night"` are plotted as positive bars and separated
#' using a violion layout. This makes direct day-vs-night comparison possible
#' within each flight-height category.
#'
#' The input should usually be produced by [reshape_statistics_long()].
#'
#' @examples
#' \dontrun{
#' stats_wide <- load_all_statistics("stats", "roi_stats")
#' stats_long <- reshape_statistics_long(stats_wide)
#' make_violin_plot(stats_long)
#' }
#' @export
make_violin_plot <- function(stats_long) {
  stats_plot <- dplyr::mutate(
    stats_long,
    time = factor(time, levels = c("day", "night"))
  )

  ggplot2::ggplot(
    stats_plot,
    ggplot2::aes(x = factor(height), y = value, fill = time)
  ) +
    ggplot2::geom_violin(
      trim = FALSE,
      alpha = 0.75,
      color = "black",
      linewidth = 0.3
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        day = "#f9f0a1",
        night = "#867493"
      )
    ) +
    ggplot2::facet_wrap(
      ~ statistic,
      scales = "free_y"
    ) +
    ggplot2::labs(
      x = "Flight altitude (m)",
      y = "Statistic value",
      fill = "Time",
      title = "Statistics by flight altitude and acquisition time"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
}


.required_stats_columns <- c(
  "time" = "time",
  "height" = "height"
)

.roi_stats <- c(
  "Total number of points" = "n_pnts",
  "Duplicated points (%)" = "dup_pnts_pct",
  "First returns (%)" = "ret_1_pct",
  "Second returns (%)" = "ret_2_pct",
  "Third returns (%)" = "ret_3_pct",
  "Points with only single returns (%)" = "ret_single_pct",
  "Points with more than one return (%)" = "ret_abvone_pct",
  "Points with negative heights (%)" = "pnts_negative_height_pct"
)

.target_stats <- c(
  "RMSE" = "rmse_last",
  "Intensity" = "intensity_avg"
)
