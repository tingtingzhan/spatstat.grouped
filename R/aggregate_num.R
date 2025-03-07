
#' @title Aggregate \link[base]{numeric} \link[spatstat.geom:hyperframe]{hypercolumns} and/or \link[spatstat.geom]{marks}, by Cluster
#' 
#' @description
#' Aggregate \link[base]{numeric} \link[spatstat.geom:hyperframe]{hypercolumns}
#' and/or \link[spatstat.geom]{marks}
#' by sample clustering.
#' 
#' @param X a [groupedHyperframe], 
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
#' @param FUN.name (optional) \link[base]{character} scalar,
#' user-friendly name of `FUN`
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
#' @importFrom spatstat.geom is.ppplist marks.ppp
#' @export
aggregate_num <- function(
    X, 
    by = stop('must specify `by`'),
    FUN,
    FUN.name = deparse1(substitute(FUN)),
    f_aggr_ = c('mean', 'median', 'max', 'min'), 
    ...
) {
  
  if (!inherits(X, what = 'hyperframe')) stop('input must be hyperframe')
  
  # Step 1: Find numeric information

  hc <- unclass(X)$hypercolumns
  
  # 'numeric'-`hypercolumns`
  hc_num <- vapply(hc, FUN = function(x) {
    all(vapply(x, FUN = is.numeric, FUN.VALUE = NA))
  }, FUN.VALUE = NA)
  hyper_num_ <- if (any(hc_num)) hc[names(which(hc_num))] # else NULL
  
  # 'numeric' 'marks' in 'ppp'-`hypercolumns`
  X_ppp <- vapply(X, FUN = is.ppplist, FUN.VALUE = NA)
  if (sum(X_ppp) > 1L) stop('does not allow more than 1 ppp-hypercolumn')
  if (sum(X_ppp) == 1L) {
    mk_ <- lapply(X[[names(which(X_ppp))]], FUN = marks.ppp, drop = FALSE)
    mk <- .mapply(FUN = list, dots = mk_, MoreArgs = NULL)
    names(mk) <- names(mk_[[1L]])
    mk_num <- vapply(mk, FUN = function(x) {
      all(vapply(x, FUN = is.numeric, FUN.VALUE = NA))
    }, FUN.VALUE = NA)
    mark_num_ <- if (any(mk_num)) mk[mk_num] # else NULL
  } else mark_num_ <- NULL
  
  x <- c(hyper_num_, mark_num_)
  names(x) <- paste(names(x), FUN.name, sep = '.')
  ret0 <- lapply(x, FUN = function(x) {
    do.call(what = rbind, args = lapply(x, FUN = FUN, ...))
  })
  
  # Step 2: aggregation
  
  aggregate_by_(dots = ret0, X = X, by = by, f_aggr_ = f_aggr_)
  
}



#' @title Aggregate-By, for [groupedHyperframe]
#' 
#' @param dots a \link[base]{list} of \link[base]{numeric} \link[base]{matrix}es
#' 
#' @param X a [groupedHyperframe]
#' 
#' @param by,f_aggr_ see function [aggregate_num]
#' 
#' @details
#' Function [aggregate_by_] checks `by` against `attr(X,'group')`.
#' 
#' @returns 
#' Function [aggregate_by_] returns 
#' a \link[base]{list} of \link[base]{numeric} \link[base]{matrix}es.
#'  
#' @keywords internal
#' @importFrom cli col_cyan col_magenta
#' @importFrom matrixStats colMedians colMaxs colMins
#' @export
aggregate_by_ <- function(
    dots, # 
    X, # 
    by, # 'formula'
    f_aggr_ = c('mean', 'median', 'max', 'min')
) {
  
  x <- unclass(X)$df
  if (any(names(dots) %in% names(x))) warning('Existing column(s) overwritten')
  
  group <- attr(X, which = 'group', exact = TRUE)
  
  if (!is.call(by) || by[[1L]] != '~' || length(by) != 2L) stop('`by` must be one-sided formula')
  if (!is.symbol(by. <- by[[2L]])) {
    new_by <- vapply(all.vars(by.), FUN = function(x) deparse1(call(name = '~', as.symbol(x))), FUN.VALUE = '')
    message('grouped structure ', col_cyan(paste('by =', deparse1(by))), ' is not allowed')
    new_by_txt <- col_magenta(paste('by =', new_by))
    message('please use either one of ', paste(new_by_txt, collapse = ', '), '.')
    stop('`by` must be a formula and right-hand-side must be a symbol')
  }
  # `group` 'up-to' `by.`
  # how to do it beautifully?
  # below is an ugly bandage fix
  g <- all.vars(group)
  id <- match(as.character(by.), table = g)
  if (is.na(id)) stop('`by` must match one of the hierarchy in groupedHyperframe')
  # end of ugly bandage fix
  
  f <- interaction(x[g[seq_len(id)]], drop = TRUE, sep = '.', lex.order = TRUE)
  ids <- split.default(seq_along(f), f = f)
  
  if (all(lengths(ids) == 1L)) {
    # no need to aggregate
    
    # passing of `f_aggr_` is hard coded, because I need `...` in [aggregate_num]
    # if (!missing(f_aggr_)) warning('aggregation on lowest cluster; parameter `f_aggr_` ignored')
    
    x[names(dots)] <- dots # done!
    
  } else {
    
    x <- mcaggregate_unique(data = x, f = f)
    fn <- switch(match.arg(f_aggr_), mean = colMeans, median = colMedians, max = colMaxs, min = colMins)
    x[names(dots)] <- lapply(dots, FUN = function(m) {
      do.call(what = rbind, args = lapply(ids, FUN = function(i) fn(m[i,,drop = FALSE])))
    })
    
  }
  
  if (id > 1L) {
    warning('tzh\'s next game: make this output an nlme::groupedData')
    #group_ret <- str2lang(paste(g[seq_len(id)], collapse = '/'))
    #fom <- eval(call('~', quote(.), call('|', quote(.), group_ret)))
    #nlme::groupedData(formula = fom, data = x) # um, I need to know more about ?nlme::groupedData
  } # else: aggregated by highest cluster, returns 'data.frame'
  
  return(x)
  
}




# @section Step 1. Cluster-Specific Sample Quantiles [clusterQp] 
# (superseded by grouped_ppp() |> aggregate_quantile):
# Function [clusterQp] calculates user-selected sample quantiles in each cluster of observations.
#' @rdname aggregate_num
#' 
#' @details
#' Function [aggregate_quantile] is a wrapper of 
#' workhorse function [aggregate_num] with `FUN = quantile`.
#' 
#' @importFrom stats quantile
#' @export
aggregate_quantile <- function(X, ...) aggregate_num(X, FUN = .quantile_num_name, FUN.name = 'quantile', ...)



#' @importFrom stats quantile
.quantile_num_name <- function(x, probs, ...) {
  qs <- quantile(x, probs, ...)
  # see last few rows of ?stats:::quantile.default
  names(qs) <- probs # numerical-name
  return(qs)
}




#' @rdname aggregate_num
#' 
#' @details
#' Function [aggregate_kerndens] is a wrapper of 
#' workhorse function [aggregate_num] with `FUN = kerndens`.
#' 
#' @export
aggregate_kerndens <- function(X, ...) aggregate_num(X, FUN = kerndens, ...)





