#' A range layer for testing the functions
#'
#' @description
#' An example testing layer in [`SpatRaster`] format describing the range of a species.
#' The example data is formatted in Lamberts equal area projection, at European extent and
#' at 10km resolution.
#'
#' @note
#' This layer is an example Species distribution modelling estimate of *Spermophilus citellus*
#'
#' @format A \code{SpatRaster} object in binary format providing a range.
#' @author Martin Jung
#' @keywords internal
#' @noRd
load_exampledata <- function(){
  range <- terra::rast(system.file('extdata/example_range.tif', package='insights',mustWork = TRUE))
  return(range)
}
