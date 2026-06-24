#' @title Retile the original point cloud files
#'
#' @description
#' In this project, the point clouds are either highly overlapped or in a unique
#' file. The retailing process is mandatory to efficiently process the data.
#'
#' @note
#' The output directory is set automatically inside the catalog folder.
#'
#' @param ctg_path Folder containing the las/laz files to create the catalog.
#' @param chunk_opts A list with the required parameters to retile the
#'   `LAScatalog`. It comes from the function `checkTileGrid`.
#' @param overwrite If `TRUE`, overwrite the a prior retiled catalog,
#' @param n_workers When parallel computation is activated, the number of worker
#'   or CPU cores to use.
#' @param parallel If `TRUE`, compute the new tiles in asynchronous way.
#'
#' @return The output directory where the new tiles are stored.
#' @export
retileCatalog <- function(
    ctg_path,
    chunk_opts,
    overwrite = FALSE,
    n_workers = NULL,
    parallel = TRUE
) {

  ctg <- openCatalog(ctg_path)

  # Do not let lidR create artificial processing chunks here.
  lidR::opt_chunk_size(ctg) <- chunk_opts[["size"]]
  lidR::opt_chunk_buffer(ctg) <- chunk_opts[["buffer"]]
  lidR::opt_chunk_alignment(ctg) <- chunk_opts[["alignment"]]
  lidR::opt_laz_compression(ctg) <- TRUE

  out_dir <- file.path(ctg_path, "retiled")

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  } else if (!isTRUE(overwrite)) {
    message(paste0(
      "A prior retiled catalog exists. To modify it, set `overwrite=TRUE`. ",
      "The existing retiled directory is returned."
    ))
    return(out_dir)
  }

  # Remove existing retiled LAS/LAZ files when overwrite = TRUE
  if (isTRUE(overwrite)) {
    old_tiles <- list.files(
      out_dir,
      pattern = "\\.(las|laz)$",
      full.names = TRUE,
      ignore.case = TRUE
    )

    if (length(old_tiles) > 0) {
      file.remove(old_tiles)
    }
  }

  lidR::opt_output_files(ctg) <- file.path(out_dir, "retile_{XLEFT}_{YBOTTOM}")

  # Conduct the retile operation in parallel
  if (isTRUE(parallel)) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)

    if (is.null(n_workers)) {
      future::plan(future::multisession)
    } else {
      future::plan(future::multisession, workers = n_workers)
    }
  }

  lidR::catalog_retile(ctg)
  return(out_dir)
}

#' Inspect and decide the best chunk parameters to retile the catalog
#'
#' @param ctg_folder Directory where the point clouds are stored.
#' @param chunk_size The desired `opt_chunk_size`.
#' @param chunk_buffer The desired `opt_buffer_size`.
#' @param alignment If `TRUE`, align the grid with the catalog bounds.
#' @param check If `TRUE`, produce a fast check of the LAScatalog.
#' @export
checkTileGrid <- function(
    ctg_folder,
    chunk_size=250,
    chunk_buffer=0,
    alignment=TRUE,
    check=FALSE
) {

  ctg <- openCatalog(ctg_folder, check)

  lidR::opt_chunk_size(ctg) <- chunk_size
  lidR::opt_chunk_buffer(ctg) <- chunk_buffer

  if (alignment) {
    bbox <- sf::st_bbox(ctg)

    x0 <- floor(bbox["xmin"] / chunk_size) * chunk_size
    y0 <- floor(bbox["ymin"] / chunk_size) * chunk_size

    alignment <- c(x0, y0)
    lidR::opt_chunk_alignment(ctg) <- alignment
  } else {
    alignment <- c(0, 0)
  }

  lidR::plot(ctg, chunk_pattern=TRUE)

  return(
    list(
      "size" = chunk_size,
      "buffer" = chunk_buffer,
      "alignment" = alignment
    )
  )
}
