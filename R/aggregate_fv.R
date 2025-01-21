
#' @title Aggregate \link[spatstat.explore]{fv.object}s by Cluster
#' 
#' @description
#' Aggregate information in \link[spatstat.explore]{fv.object}s
#' by sample clustering.
#' 
#' @param X a [groupedHyperframe], 
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
#' Therefore, function [aggregate_fv] must return a \link[base]{data.frame}, 
#' instead of a \link[spatstat.geom]{hyperframe}.
#' 
#' @examples
#' library(spatstat.data)
#' library(spatstat.geom)
#' r = seq.int(from = 0, to = 100, by = 5)
#' flu_spat = flu |>
#'  subset(stain == 'M2-M1') |>
#'  as.groupedHyperframe(group = ~ virustype/frameid) |>
#'  Gcross_(i = 'M1', j = 'M2', r = r, correction = 'best') |>
#'  aggregate_fv(by = ~ virustype)
#' names(flu_spat)
#' dim(flu_spat$pattern.G.value)
#' dim(flu_spat$pattern.G.cumarea)
#' @importFrom spatstat.geom names.hyperframe
#' @export
aggregate_fv <- function(
    X, 
    by = stop('must specify `by`'),
    f_aggr_ = c('mean', 'median', 'max', 'min'), 
    ...
) {
  
  if (!any(id <- (unclass(X)$vclass == 'fv'))) stop('input `X` must contain at least one `fv` column')
  
  fv <- as.list.hyperframe(X)[names.hyperframe(X)[id]] # one or more 'fv' column(s)
  
  ret0 <- fv |> 
    lapply(FUN = function(x) return(list(
      value = y.fvlist(x), 
      cumarea = cumtrapz.fvlist(x)
    ))) |>
    unlist(recursive = FALSE, use.names = TRUE) # smart!!
  
  aggregate_by_(dots = ret0, X = X, by = by, f_aggr_ = f_aggr_)

}

