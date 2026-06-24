#' Open a LAScatalog with project common options
#'
#' @param ctg_path Folder containing all the laz/las files.
#' @param check If `TRUE`, display a [lidR::las_check()] message.
openCatalog <- function(
    ctg_path,
    check = FALSE
) {
  stopifnot(dir.exists(ctg_path))

  ctg <- lidR::readLAScatalog(ctg_path)

  # Basic catalog checks
  if (check) {
    lidR::las_check(ctg)
  }

  # Select only needed attributes to reduce memory use
  required_attrs <- c(
    "X",
    "Y",
    "Z",
    "Intensity",
    "ReturnNumber",
    "NumberOfReturns",
    "Classification"
  )

  lidR::opt_select(ctg) <- paste(.attr_codes[required_attrs], collapse="")
  # Keep all returns by default
  lidR::opt_filter(ctg) <- ""

  return(ctg)
}

# Available attributes to filter the point clouds
.attr_codes <- c(
  X               = "x",
  Y               = "y",
  Z               = "z",
  Intensity       = "i",
  ReturnNumber    = "rn",
  NumberOfReturns = "nr",
  GPSTime         = "t",
  Classification  = "c",
  ScanAngle       = "a",
  UserData        = "u",
  PointSourceID   = "p",
  Red             = "R",
  Green           = "G",
  Blue            = "B"
)
