
#' @title Aggregate \link[spatstat.explore]{fv.object}s by Cluster
#' 
#' @description
#' Aggregate information in \link[spatstat.explore]{fv.object}s
#' by sample clustering.
#' 
#' @param X a \link[spatstat.geom]{hyperframe}, 
#' containing one or more \link[spatstat.explore]{fv.object} column(s)
#' 
#' @param by one-sided \link[stats]{formula}, sample clustering.
#' Use only one-level hierarchy (e.g., `~patient` or `~image`).
#' Do not use multi-level hierarchy (e.g., `~patient/image`)
#' 
#' @param f_aggr_ \link[base]{character} scalar, method to aggregate
#' within cluster, currently supports
#' `'mean'`, `'median'`, `'max'`, and `'min'`.
#' 
#' @param ... additional parameters, currently not in use
#' 
# @note 
# Tingting hesitates to create a function of `aggregate.hyperframe`,
# which could be claimed by \CRANpkg{spatstat} authors in future.
# -- Inna is correct: we do not aggregate-hyperframe, we aggregate-fv-columns-inside-hyperframe.
#' 
#' @returns
#' Function [aggregate_fv] returns a \link[base]{data.frame}, with
#' aggregated information stored in \link[base]{matrix}-columns.
#' 
#' Note that \link[spatstat.geom]{hyperframe} does not support
#' \link[base]{matrix}-column (for good reasons!).
#' Therefore, function [aggregate_fv] must return \link[base]{data.frame}, 
#' instead of \link[spatstat.geom]{hyperframe}.
#' 
#' @importFrom matrixStats colMedians colMaxs colMins
#' @export
aggregate_fv <- function(
    X, 
    by = stop('must specify `by`'),
    f_aggr_ = c('mean', 'median', 'max', 'min'), 
    ...
) {
  
  X0 <- unclass(X) 
  x <- X0$df # to be returned later

  if (!any(id <- (X0$vclass == 'fv'))) stop('input `X` must contain at least one `fv` column')
  fv <- as.list.hyperframe(X)[names(X)[id]] # one or more 'fv' column(s)
  
  ret0 <- lapply(fv, FUN = function(x) return(list(
    value = y.fvlist(x), 
    cumarea = cumtrapz.fvlist(x)
  )))
  
  ret1 <- unlist(ret0, recursive = FALSE, use.names = TRUE) # smart!!
  
  if (any(names(ret1) %in% names(x))) {
    warning('Existing column(s) overwritten')
    #warning(sprintf(fmt = 'Existing `%s` column overwritten', nm_iso))
  }
  
  aggregate_by_(dots = ret1, X = X, by = by, f_aggr_ = f_aggr_)

}

