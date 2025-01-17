
#' @title Aggregate \link[base]{numeric} \link[spatstat.geom:hyperframe]{hypercolumns} and/or \link[spatstat.geom]{marks}, by Cluster
#' 
#' @description
#' Aggregate \link[base]{numeric} \link[spatstat.geom:hyperframe]{hypercolumns}
#' and/or \link[spatstat.geom]{marks}
#' by sample clustering.
#' 
#' @param X a \link[spatstat.geom]{hyperframe}, 
#' containing either or all of
#' \itemize{
#' \item {one or more \link[base]{numeric} 
#' \link[spatstat.geom:hyperframe]{hypercolumns}}
#' \item {one-and-only-one \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumns}
#' with one or more \link[base]{numeric} \link[spatstat.geom]{marks}}
#' }
#'  
#' 
#' @param by one-sided \link[stats]{formula}, 
#' one-level hierarchy clustering, e.g., `~patient` or `~image`.
#' Do **not** use multi-level hierarchy, e.g., `~patient/image`
#' 
#' @param FUN \link[base]{function} to extract information, currently supports 
#' functions \link[stats]{quantile} and [kerndens]
#' 
#' @param f_aggr_ \link[base]{character} scalar, method to aggregate
#' within cluster, currently supports
#' `'mean'`, `'median'`, `'max'`, and `'min'`.
#' 
#' @param ... additional parameters of function `FUN`
#' 
#' @returns
#' Function [aggregate_num] returns a \link[base]{data.frame}, with
#' aggregated information stored in \link[base]{matrix}-columns.
#' 
#' @name aggregate_num
#' @importFrom spatstat.geom is.ppp marks.ppp
#' @importFrom matrixStats colMedians colMaxs colMins
#' @export
aggregate_num <- function(
    X, 
    by = stop('must specify `by`'),
    FUN,
    f_aggr_ = c('mean', 'median', 'max', 'min'), 
    ...
) {
  
  X0 <- unclass(X) 
  x <- X0$df # to be returned later
  
  hc <- X0$hypercolumns
  
  # 'numeric'-`hypercolumns`
  hc_num <- vapply(hc, FUN = function(x) {
    all(vapply(x, FUN = is.numeric, FUN.VALUE = NA))
  }, FUN.VALUE = NA)
  hyper_num_ <- if (any(hc_num)) hc[names(which(hc_num))] # else NULL
  
  # 'numeric' 'marks' in 'ppp'-`hypercolumns`
  hc_ppp <- vapply(hc, FUN = function(x) {
    all(vapply(x, FUN = is.ppp, FUN.VALUE = NA))
  }, FUN.VALUE = NA)
  if (sum(hc_ppp) > 1L) stop('does not allow more than 1 ppp-hypercolumn')
  if (sum(hc_ppp) == 1L) {
    mk_ <- lapply(hc[[names(which(hc_ppp))]], FUN = marks.ppp, drop = FALSE)
    mk <- .mapply(FUN = list, dots = mk_, MoreArgs = NULL)
    names(mk) <- names(mk_[[1L]])
    mk_num <- vapply(mk, FUN = function(x) {
      all(vapply(x, FUN = is.numeric, FUN.VALUE = NA))
    }, FUN.VALUE = NA)
    mark_num_ <- if (any(mk_num)) mk[mk_num] # else NULL
  } else mark_num_ <- NULL
  
  ret0 <- lapply(c(hyper_num_, mark_num_), FUN = function(x) {
    do.call(what = rbind, args = lapply(x, FUN = FUN, ...))
  })
  names(ret0) <- paste(names(ret0), deparse1(substitute(FUN)), sep = '.')
  
  if (any(names(ret0) %in% names(x))) {
    warning('Existing column(s) overwritten')
    #warning(sprintf(fmt = 'Existing `%s` column overwritten', nm_iso))
  }
  
  f <- x[[by[[2L]]]]
  ids <- split.default(seq_along(f), f = f)
  
  if (all(lengths(ids) == 1L)) {
    # no need to aggregate
    x[names(ret0)] <- ret0 # done!
    
  } else {
    
    x <- .data_unique(data = x, f = f)
    fn <- switch(match.arg(f_aggr_), mean = colMeans, median = colMedians, max = colMaxs, min = colMins)
    x[names(ret0)] <- lapply(ret0, FUN = function(m) {
      do.call(what = rbind, args = lapply(ids, FUN = function(i) fn(m[i,,drop = FALSE])))
    })
    
  }
  
  return(x)
  
}


#aggregate_





#' @rdname aggregate_num
#' 
#' @details
#' Function [aggregate_quantile] is a wrapper of 
#' workhorse function [aggregate_num] with `FUN = quantile`.
#' 
#' @importFrom stats quantile
#' @export
aggregate_quantile <- function(X, ...) aggregate_num(X, FUN = quantile, ...)



#' @rdname aggregate_num
#' 
#' @details
#' Function [aggregate_kerndens] is a wrapper of 
#' workhorse function [aggregate_num] with `FUN = kerndens`.
#' 
#' @export
aggregate_kerndens <- function(X, ...) aggregate_num(X, FUN = kerndens, ...)





