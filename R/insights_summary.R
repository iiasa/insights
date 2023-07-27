#' Summarize inSiGHTS into an index
#'
#' @description
#' This function handily summarizes the suitable habitat for a given species or
#' biodiversity feature into an index. If a single timestep (or object with a single layer)
#' is provided, this function simply summarizes the suitable area.
#'
#' @param obj A [`SpatRaster`] or temporal [`stars`] object describing with the
#' applied InSiGHTS outputs from \code{insights_fraction}. If the number of layers is greater
#' than 1, the parameter \code{"relative"} mgiht be applied.
#' @param toArea A [`logical`] flag whether the suitable habitat should be summarized to area (Default: \code{TRUE})?
#' Note: If this parameter is set to \code{FALSE}, the suitable habitat is summarized through a \code{"mean"}.
#' @param relative A [`logical`] flag whether a relative index is to be constructed (Default: \code{TRUE}).
#' @returns A [`data.frame`] with area estimates or the respective indicator.
#' @author Martin Jung
#' @references
#' * Baisero, Daniele, Piero Visconti, Michela Pacifici, Marta Cimatti, and Carlo Rondinini. "Projected global loss of mammal habitat due to land-use and climate change." One Earth 2, no. 6 (2020): 578-585.
#' * Powers, Ryan P., and Walter Jetz. "Global habitat loss and extinction risk of terrestrial vertebrates under future land-use-change scenarios." Nature Climate Change 9, no. 4 (2019): 323-329.
#' @name insights_summary
#' @export
NULL

#' @name insights_summary
#' @rdname insights_summary
#' @exportMethod insights_summary
#' @export
methods::setGeneric(
  "insights_summary",
  signature = methods::signature("obj"),
  function(obj, toArea = TRUE, relative = TRUE) standardGeneric("insights_summary"))

#' @name insights_summary
#' @rdname insights_summary
#' @usage \S4method{insights_summary}{SpatRaster,logical,logical}(obj,toArea,relative)
methods::setMethod(
  "insights_summary",
  methods::signature(obj = "SpatRaster"),
  function(obj, toArea = TRUE, relative = TRUE) {
    assertthat::assert_that(
      ibis.iSDM::is.Raster(obj),
      is.logical(toArea),
      is.logical(relative)
    )

    # Some basic checks
    rr <- terra::global(obj,"range",na.rm=TRUE)
    assertthat::assert_that(all(rr[["min"]]>=0 ),
                            all(rr[["max"]]<=1 ),
                            msg = "Not a properly refined range!"
    )
    rm(rr)

    # Apply area correction if set
    if(toArea){
      # Calculate the area size in km2
      ar <- terra::cellSize(obj, unit = "km")
      obj <- obj * ar
      fun <- "sum"
    } else {
      fun <- "mean"
    }

    # --- #
    # Summarize
    results <- data.frame(
      time = terra::time(obj),
      suitability = terra::global(obj, fun, na.rm = TRUE)[,1]
    )
    if(fun=="sum") results$unit <- "km2" else results$unit <- "fraction"

    # Relative conversion if set
    if(relative && nrow(results)>1){
      relChange <- function(v, fac = 100) (((v-v[1]) / v[1]) * fac)
      results$relative_change_perc <- relChange(results$suitability)
      results$suitability <- results$suitability - results$suitability[1]
    }

    assertthat::assert_that(is.data.frame(results),
                            nrow(results)>=1)
    return(results)
  }
)
