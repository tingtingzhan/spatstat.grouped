

#' @title Operations on a \link[base]{list} of \link[spatstat.explore]{fv.object}s
#' 
#' @param X a \link[base]{list} of \link[spatstat.explore]{fv.object}s
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' Function [y.fvlist] gathers the primary outcome (via [ynm.fv])
#' of the \link[spatstat.explore]{fv.object}s.
#' 
#' @returns
#' All functions return a \link[base]{double} \link[base]{matrix}.
#' 
#' @name fvlist
#' @export
y.fvlist <- function(X, ...) {
  id_error <- vapply(X, FUN = inherits, what = 'error', FUN.VALUE = NA)
  if (all(id_error)) stop('do not allow')
  X0 <- X[!id_error]
  
  x <- lapply(X0, FUN = `[[`, 1L)
  if (!all(duplicated.default(x)[-1L])) stop('x-axis of all fv.objects are not the same')
  fname <- lapply(X, FUN = attr, which = 'fname', exact = TRUE)
  if (!all(duplicated.default(fname)[-1L])) stop('fname of all fv.objects are not the same')
  
  # if (any(!is.finite.fv(X[[1L]])))
  # needs to check infinite status being same accross all `X`.
  # then, should we
  # .. do X[[i]][is.finite.fv(X[[i]])]
  # .. simply let `ret[is.infinite(ret)] <- NA_real_`
  # ????
  
  tmp <- replicate(n = length(X), expr = NA_real_, simplify = FALSE)
  tmp[!id_error] <- lapply(X0, FUN = `[[`, ynm.fv(X[[1L]]))
  ret <- do.call(rbind, args = tmp)
  colnames(ret) <- x[[1L]]
  return(ret)
}



#' @rdname fvlist
#' 
#' @details 
#' Function [cumtrapz.fvlist] is a batch process of function [cumtrapz.fv].
#' 
#' @importFrom parallel mclapply detectCores
#' @export
cumtrapz.fvlist <- function(X, ...) {
  id_error <- vapply(X, FUN = inherits, what = 'error', FUN.VALUE = NA)
  if (all(id_error)) stop('do not allow')
  X0 <- X[!id_error]
  x <- lapply(X0, FUN = `[[`, 1L)
  if (!all(duplicated.default(x)[-1L])) stop('x-axis of all fv.objects are not the same')
  
  # same question:
  # how to deal with infinite.fv ?
  # here we have to `X[[i]][is.finite.fv(X[[i]])]`
  # because NA_real_ will not help with pracma::trapz
  
  tmp <- replicate(n = length(X), expr = NA_real_, simplify = FALSE)
  tmp[!id_error] <- mclapply(X0, mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores()), FUN = cumtrapz.fv, ...)
  ret <- do.call(rbind, args = tmp)
  colnames(ret) <- x[[1L]][-1L]
  return(ret)
}





# @title Area Under Curve of `'fvlist'` Object
# 
# @description
# Defunct!  Do this manually
# 
# @param X an `'fvlist'` object
# 
# @param ... additional parameters, currently not in use
# 
# @returns 
# Function [trapz.fvlist] returns a \link[base]{numeric} \link[base]{vector}.
# 
# @keywords internal
# @export trapz.fvlist
# @export
#trapz.fvlist <- function(X, ...) vapply(X, FUN = trapz.fv, ..., FUN.VALUE = NA_real_)


# do NOT define `[.fvlist`; Tingting does not want to override `[` for 'list'
# @export
#subset.fvlist <- function(x, subset, ...) {
# the batch process of ?spatstat.explore::`[.fv`
#}



