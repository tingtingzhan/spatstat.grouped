

#' @title log.ppp
#' 
#' @description
#' ...
#' 
#' @param x a \link[spatstat.geom]{ppp.object}
#' 
#' @param base \link[base]{numeric} scalar
#' 
#' @details
#' Function [log.ppp] takes a \link[base]{log} of continuous marks 
#' of a \link[spatstat.geom]{ppp.object}.
#' 
#' @return 
#' Function [log.ppp] returns a \link[spatstat.geom]{ppp.object}.
#' 
#' @examples
#' data(longleaf, package = 'spatstat.data')
#' plot(longleaf)
#' plot(log.ppp(longleaf))
#' @importFrom spatstat.geom markformat.ppp
#' @export log.ppp
#' @export
log.ppp <- function(x, base = exp(1)) {
  
  switch(markformat.ppp(x), dataframe = {
    id <- vapply(x$marks, FUN = is.numeric, FUN.VALUE = NA)
    x$marks[id] <- lapply(x$marks[id], FUN = log, base = base)
  }, vector = {
    if (is.numeric(x$marks)) x$marks <- log(x$marks, base = base)
    # else do nothing
  }, none = {
    # do nothing
  })
  
  return(x)
}



# nobs.ppp <- function(object, ...) .Defunct(new = 'spatstat.geom::npoints.ppp')






#' @title Handle Missing \link[spatstat.geom]{marks} in \link[spatstat.geom]{ppp.object}.
#' 
#' @param object a \link[spatstat.geom]{ppp.object}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' Function [na.omit.ppp] omits missing \link[spatstat.geom]{marks} in a \link[spatstat.geom]{ppp.object}.
#' 
#' @returns
#' Function [na.omit.ppp] returns a \link[spatstat.geom]{ppp.object}.
#' 
#' @note
#' tzh suppose missing `$x` and `$y` are 
#' forbidden in \link[spatstat.geom]{ppp.object} anyway.
#' 
#' @examples
#' library(spatstat.data)
#' library(spatstat.geom)
#' 
#' npoints(nbfires)
#' npoints(na.omit.ppp(nbfires))
#' 
#' npoints(amacrine)
#' npoints(na.omit.ppp(amacrine)) # no missing marks to be removed
#' 
#' nbfires_julian = unstack.ppp(nbfires)$out.julian
#' suppressWarnings(print.ppp(nbfires_julian))
#' suppressWarnings(plot.ppp(nbfires_julian))
#' na.omit.ppp(nbfires_julian)
#' 
#' @importFrom stats na.omit
#' @importFrom spatstat.geom subset.ppp markformat.ppp
#' @method na.omit ppp
#' @export na.omit.ppp
#' @export
na.omit.ppp <- function(object, ...) {

  switch(markformat.ppp(object), none = {
    return(object) # exception handling
    
  }, {
    
    tmp <- na.omit(object$marks)
    # ?stats:::na.omit.data.frame; if (markformat.ppp(object) == 'dataframe')
    # ?stats:::na.omit.default; if (markformat.ppp(object) == 'vector')
    
    id <- attr(tmp, which = 'na.action', exact = TRUE)
    
    if (!length(id)) return(object) # nothing to omit
    
    ret <- subset.ppp(object, subset = -id)
    attr(ret, which = 'na.action') <- id
    return(ret)
    
  })
  
}

