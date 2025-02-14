


#' @title Assign a Value to `NaN` and/or `Inf` in \link[spatstat.explore]{fv.object}
#' 
#' @description
#' ..
#' 
#' 
#' @param x an \link[spatstat.explore]{fv.object}
#' 
#' @param value \link[base]{numeric} scalar, to replace
#' the `NaN` and `Inf` elements. Default `0`.
#' 
#' @examples
#' library(spatstat.data)
#' library(spatstat.explore)
#' # ?markcorr
#' r1 = seq(from = 0, to = 25, by = 1)
#' r2 = seq(from = 0, to = 250, by = 10)
#' markcorr(spruces, r = r1) |> key1val.fv()
#' markcorr(spruces, r = r2) |> key1val.fv()
#' 
#' \dontrun{
#' debug(sewsmod); markcorr(spruces, r = r2)} # reason found
#' 
#' a = markcorr(spruces, r = r2)
#' key1val.fv(a)
#' key1nonfinite(a) = 0
#' key1val.fv(a)
#' 
#' # equivalent
#' markcorr(spruces, r = r2) |>
#'  `key1nonfinite<-`(value = 0) |>
#'  key1val.fv()
#' @export
`key1nonfinite<-` <- function(x, value = 0) {
  key1. <- key1.fv(x)
  x[[key1.]][!is.finite(x[[key1.]])] <- value
  return(x)
}

