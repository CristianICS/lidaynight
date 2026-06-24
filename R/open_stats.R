#' Create the folder to store the stats from the current mission
#'
#' @param project_dir The directory containing the project files.
#' @param area_name The name of the flown area.
#' @param time The moment in which the flight was conducted (`night` or `day`).
#' @param height The flight height used to collect the point cloud.
#' @export
openStatsFolder <- function(
    project_dir,
    area_name,
    time,
    height
) {
  stats_fname <- paste0(time, "_", height)
  out_path <- file.path(project_dir, "results", "stats", area_name, stats_fname)

  if (!dir.exists(out_path)) {
    dir.create(out_path, recursive=TRUE)

    if (!dir.exists(out_path)) {
      stop("The stats path could not be created.")
    }
  }

  return(out_path)
}
