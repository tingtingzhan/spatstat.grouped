
#' @title as.groupedHyperframe
#' 
#' @description
#' as.groupedHyperframe
#' 
#' @param x see Usage
#' 
#' @param ... additional parameters
#' 
#' @name as.groupedHyperframe
#' @export
as.groupedHyperframe <- function(x, ...) UseMethod(generic = 'as.groupedHyperframe')

#' @rdname as.groupedHyperframe
#' 
#' @param group \link[stats]{formula}
#' 
#' @examples
#' library(spatstat.data)
#' 
#' # ?spatstat.data::osteo
#' # `brick`: serial number (1 to 10) of sampling volume within this bone sample (`id`)
#' head(as.groupedHyperframe(osteo, group = ~ id/brick))
#' 
#' # ?spatstat.data::flu
#' # `frameid`: not unique across different values of `virustype` and `stain`.
#' # currently not supporting
#' # group = ~ virustype*stain/flu
#' head(tmp <- with(unclass(flu)$df, expr = data.frame(
#'   virus.stain = interaction(virustype, stain, drop = TRUE, sep = '_', lex.order = TRUE)
#' )))
#' head(flu2 <- spatstat.geom::cbind.hyperframe(flu, tmp))
#' head(as.groupedHyperframe(flu2, group = ~ virus.stain/frameid))
#' @importFrom spatstat.geom names.hyperframe
#' @export as.groupedHyperframe.hyperframe
#' @export
as.groupedHyperframe.hyperframe <- function(x, group, ...) {
  
  if (!is.language(group) || group[[1L]] != '~') stop('`group` must be a formula')
  
  if (length(group) != 2L) stop('`group` must be one-sided formula')
  
  if (!all(all.vars(group) %in% names.hyperframe(x))) stop('`group` contains unknown variable')
  
  attr(x, which = 'group') <- group
  class(x) <- unique.default(c('groupedHyperframe', class(x)))
  return(x)
  
}