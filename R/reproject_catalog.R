#' @title Reproject the point cloud files of a catalog
#'
#' @description
#' Transforms every LAS/LAZ file of the catalog into the coordinate reference
#' system `epsg`, replacing the original files. Use it when a mission is
#' stored in a CRS where map units are not ground meters (e.g. a UTM zone that
#' does not contain the site), so buffers, grid resolutions and areas computed
#' later in map units match the real ground distances. Only the horizontal
#' coordinates change; Z is kept as is.
#'
#' Downstream functions ([globalStats()], [targetStats()]) do not need any
#' change: [openSpatial()] already transforms the vector layers into the CRS
#' of the catalog.
#'
#' @note
#' The files are reprojected in place, keeping the folder structure. They are
#' first written into a temporary subfolder and only then moved over the
#' original files, so a failed run does not leave a catalog with mixed CRSs.
#' If the catalog is already in `epsg`, nothing is done, so calling it again
#' on an already reprojected catalog is safe. Existing `.lax` index files are
#' rebuilt, since they no longer match the reprojected coordinates.
#'
#' @param ctg_path Folder containing the las/laz files to create the catalog.
#' @param epsg EPSG code of the target coordinate reference system.
#' @param n_workers When parallel computation is activated, the number of worker
#'   or CPU cores to use.
#' @param parallel If `TRUE`, reproject the files in asynchronous way.
#'
#' @return The catalog directory, `ctg_path`.
#' @export
reprojectCatalog <- function(
    ctg_path,
    epsg,
    n_workers = NULL,
    parallel = TRUE
) {

  if (!is.numeric(epsg) || length(epsg) != 1L || is.na(epsg)) {
    stop("`epsg` must be a single numeric EPSG code.")
  }

  target_crs <- sf::st_crs(epsg)

  if (is.na(target_crs)) {
    stop("Unknown EPSG code: ", epsg)
  }

  ctg <- openCatalog(ctg_path)

  if (is.na(sf::st_crs(ctg))) {
    stop("The catalog has no CRS, it cannot be reprojected: ", ctg_path)
  }

  if (sf::st_crs(ctg) == target_crs) {
    message("The catalog is already in EPSG:", epsg, ". Nothing to reproject.")
    return(ctg_path)
  }

  # Temporary output folder inside the catalog, so files can be moved back
  # without copying across drives
  tmp_dir <- file.path(ctg_path, ".reproject_tmp")
  unlink(tmp_dir, recursive = TRUE)
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Keep every attribute: the files are rewritten, not only analysed
  lidR::opt_select(ctg) <- "*"

  # One chunk per file, so each output file matches one input file
  lidR::opt_chunk_size(ctg) <- 0
  lidR::opt_chunk_buffer(ctg) <- 0
  lidR::opt_laz_compression(ctg) <- TRUE
  lidR::opt_output_files(ctg) <- file.path(tmp_dir, "{ORIGINALFILENAME}")

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

  reproject_chunk <- function(chunk, crs) {
    las <- lidR::readLAS(chunk)
    if (lidR::is.empty(las)) return(NULL)

    # lidR's method recomputes the offsets from the new coordinates, needed
    # when the coordinates move far away from the original offsets
    sf::st_transform(las, crs)
  }

  lidR::catalog_apply(
    ctg,
    reproject_chunk,
    crs = target_crs,
    .options = list(automerge = TRUE)
  )

  src_files <- ctg@data$filename
  new_files <- file.path(
    tmp_dir,
    paste0(tools::file_path_sans_ext(basename(src_files)), ".laz")
  )

  if (!all(file.exists(new_files))) {
    stop(
      "Some files were not reprojected, the original catalog is kept: ",
      paste(basename(src_files[!file.exists(new_files)]), collapse = ", ")
    )
  }

  # Replace the original files (and their stale .lax indexes)
  for (i in seq_along(src_files)) {
    src_stem <- tools::file_path_sans_ext(src_files[i])
    dst_file <- paste0(src_stem, ".laz")
    lax_file <- paste0(src_stem, ".lax")

    if (file.exists(lax_file)) {
      file.remove(lax_file)
    }

    if (src_files[i] != dst_file) {
      file.remove(src_files[i])
    }

    if (!file.rename(new_files[i], dst_file)) {
      stop("Could not replace file: ", dst_file)
    }

    rlas::writelax(dst_file)
  }

  return(ctg_path)
}
