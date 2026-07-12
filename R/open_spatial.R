#' @title Read a spatial vector layer and align its CRS with a LAS catalog
#'
#' @description
#' Reads a vector dataset that could contain
#'
#' * The area of interest to perform regional statistics
#' * Points with ground reference data to compute height RMSE.
#'
#' After reading the file, the function transforms it to match the CRS of the
#' provided `LAScatalog` when necessary.
#'
#' @param vector_path Path pointing to a file readable by [sf::st_read()].
#' @param ctg Optional. A `lidR::LAScatalog` to match the CRS.
#' @param layer_name Optional. If it is provided, the function will try to load
#'   the layer with this name.
#'
#' @return An `sf` object (AOI) in the catalog CRS (if both CRSs are defined).
#' @export
openSpatial <- function(vector_path, ctg, layer_name = NULL) {

  # Open file and handle optional layer name
  if (is.null(layer_name)) {
    sf_obj <- sf::st_read(vector_path, quiet = TRUE)
  } else {
    layers <- sf::st_layers(vector_path)$name

    if (!layer_name %in% layers) {
      layer_name <- layers[1]
      warning(
        "Layer '", layer_name, "' not found inside ", basename(vector_path),
        ". Using first layer: '", layers[1], "'."
      )
    }

    sf_obj <- sf::st_read(vector_path, layer=layer_name, quiet = TRUE)
  }

  # Match the CRS with the provided catalog's CRS
  if (!is.null(ctg)) {
    crs_ctg <- sf::st_crs(ctg)
    crs_sf <- sf::st_crs(sf_obj)

    if (!is.na(crs_ctg) && !is.na(crs_sf) && crs_sf != crs_ctg) {
      sf_obj <- sf::st_transform(sf_obj, crs = crs_ctg)
    }
  }

  sf_obj
}
