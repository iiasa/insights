#' Relative change function
#'
#' @description
#' This function calculates the relative change of a vector, taking the first value as
#' a reference value.
#' @param v A [`vector`] with numeric values greater than 2.
#' @param fac A [`numeric`] constant multiplier on the resulting metric.
#' @returns A [`numeric`] vector.
#' @examples
#' # Example vector
#' x <- c(20,6,2,1,15,25)
#' relChange(x)
#'
#' @author Martin Jung
#' @keywords utils
#' @export
relChange <- function(v, fac = 100){
  assertthat::assert_that(
    length(v)>1,
    is.numeric(fac)
  )

  return(
    (((v-v[1]) / v[1]) * fac)
  )
}
