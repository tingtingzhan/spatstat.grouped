

check_fvlist <- function(X, ...) {
  
  x <- lapply(X, FUN = `[[`, 1L)
  if (!all(duplicated.default(x)[-1L])) stop('x-axis of all fv.objects are not the same')
  
  fname <- lapply(X, FUN = attr, which = 'fname', exact = TRUE)
  if (!all(duplicated.default(fname)[-1L])) stop('fname of all fv.objects are not the same')
  
  # I do not require [key1.fv] to be the same!!!!
  
}


key1_fvlist <- function(X, ...) {
  vapply(X, FUN = key1.fv, FUN.VALUE = NA_character_)
}



#' @title Operations on a \link[base]{list} of \link[spatstat.explore]{fv.object}s
#' 
#' @param X a \link[base]{list} of \link[spatstat.explore]{fv.object}s
#' 
#' @param check \link[base]{logical} scalar
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' Function [y.fvlist] gathers the primary outcome (via [key1.fv])
#' of the \link[spatstat.explore]{fv.object}s.
#' 
#' @returns
#' All functions return a \link[base]{double} \link[base]{matrix}.
#' 
#' @name fvlist
#' @export
y.fvlist <- function(X, check = TRUE, ...) {
  
  if (check) check_fvlist(X, ...)

  # if (any(!is.finite.fv(X[[1L]])))
  # needs to check infinite status being same accross all `X`.
  # then, should we
  # .. do X[[i]][is.finite.fv(X[[i]])]
  # .. simply let `ret[is.infinite(ret)] <- NA_real_`
  # ????
  
  r <- X[[1L]][[1L]]
  
  ret <- lapply(X, FUN = function(x) x[[key1.fv(x)]]) |>
    unlist(use.names = FALSE)
  dim(ret) <- c(length(r), length(X))
  dimnames(ret) <- list(r, NULL)
  return(t.default(ret))
  
}



#' @rdname fvlist
#' 
#' @details 
#' Function [cumtrapz.fvlist] is a batch process of function [cumtrapz.fv].
#' 
#' @importFrom parallel mclapply detectCores
#' @export
cumtrapz.fvlist <- function(X, check = TRUE, ...) {
  
  if (check) check_fvlist(X, ...)
  
  # same question:
  # how to deal with infinite.fv ?
  # here we have to `X[[i]][is.finite.fv(X[[i]])]`
  # because NA_real_ will not help with pracma::trapz
  
  r <- X[[1L]][[1L]][-1L]
  
  ret <- mclapply(X = X, mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores()), FUN = cumtrapz.fv, ...) |>
      unlist(use.names = FALSE)
  dim(ret) <- c(length(r), length(X))
  dimnames(ret) <- list(r, NULL)
  return(t.default(ret))

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



