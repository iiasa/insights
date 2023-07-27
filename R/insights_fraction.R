#' Apply InSiGHTS with fractional land use data
#'
#' @description
#' This function applies an area-of-habitat (AOH) correction to a provided single time step or multiple step range estimate
#' (binary format). This thus assumes that species-habitat relations remain stable also in future conditions within a
#' provided climatic niche.
#'
#' It is assumed that the land-use layers come in fractional units, so are ranging from \code{0} to \code{1}.
#' Optionally also a elevation (\code{elev}) layer and habitat condition (\code{condition}) can be provided to support refinements
#' by relevational range or habitat condition.
#'
#' @param range A [`SpatRaster`] or temporal [`stars`] object describing the estimated distribution of a
#' biodiversity feature (e.g. species). **Has to be in binary format!**
#' @param lu A [`SpatRaster`] or temporal [`stars`] object of the future land-use fractions to be applied to the range.
#' **Each layer has to be in units of fractions, e.g. between 0 and 1.**
#' @param elev A [`SpatRaster`] or temporal [`stars`] object that provides an elevational layer. Can be missing.
#' @param condition A [`SpatRaster`] or temporal [`stars`] object that provides a habitat condition layer. Can be missing.
#' @param outfile A writeable [`character`] of where the output should be written to. If missing, the the function will return
#' a [`SpatRaster`] or [`stars`] object respectively.
#' @author Martin Jung
#' @keywords fraction
#' @name insights_fraction
#' @export
NULL

#' @name insights_fraction
#' @rdname insights_fraction
#' @exportMethod insights_fraction
#' @export
methods::setGeneric(
  "insights_fraction",
  signature = methods::signature("range", "lu"),
  function(range, lu, elev, condition, outfile = NULL) standardGeneric("insights_fraction"))

#' @name insights_fraction
#' @rdname insights_fraction
#' @usage \S4method{insights_fraction}{SpatRaster,SpatRaster,SpatRaster,SpatRaster,character}(range,lu,elev,condition,outfile)
methods::setMethod(
  "insights_fraction",
  methods::signature(range = "SpatRaster", lu = "SpatRaster"),
  function(range, lu, elev, condition, outfile = NULL) {
    assertthat::assert_that(any( class(obj) %in% getOption('ibis.engines') ),
                            is.character(method),
                            is.null(value) || is.numeric(value),
                            is.character(format)
    )
  }
)
