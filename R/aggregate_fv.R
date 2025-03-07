
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
# tzh hesitates to create a function of `aggregate.hyperframe`,
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
#' m = flu |>
#'  subset(stain == 'M2-M1') |>
#'  Gcross_(i = 'M1', j = 'M2', r = r, correction = 'best') |>
#'  as.groupedHyperframe(group = ~ virustype/frameid) |>
#'  aggregate_fv(by = ~ virustype)
#' names(m)
#' dim(m$pattern.G.value)
#' dim(m$pattern.G.cumarea)
#' @importFrom spatstat.geom names.hyperframe
#' @importFrom stats setNames
#' @export
aggregate_fv <- function(
    X, 
    by = stop('must specify `by`'),
    f_aggr_ = c('mean', 'median', 'max', 'min'), 
    ...
) {
  
  if (!inherits(X, what = 'hyperframe')) stop('input must be hyperframe')
  
  if (!any(id <- (unclass(X)$vclass == 'fv'))) stop('input `X` must contain at least one `fv` column')
  
  fv <- as.list.hyperframe(X)[names.hyperframe(X)[id]] # one or more 'fv' column(s)
  
  ret0 <- lapply(setNames(nm = names(fv)), FUN = function(nm) {
    x <- fv[[nm]]
    check_fvlist(x)
    cumtrapz. <- cumtrapz.fvlist(x, check = FALSE)
    if (anyNA(cumtrapz.)) {
      id <- min(rowSums(!is.na(cumtrapz.)))
      message(col_cyan(nm), ': please limit ', col_magenta('r'), ' from ', x[[1L]]$r[1L], ' to ', x[[1L]]$r[id])
    }
    return(list(
      value = key1val.fvlist(x, check = FALSE), 
      cumarea = cumtrapz.
    ))
  })

  ret1 <- ret0 |>
    unlist(recursive = FALSE, use.names = TRUE) # smart!!
  
  aggregate_by_(dots = ret1, X = X, by = by, f_aggr_ = f_aggr_)

}

