

#' @title check_fvlist
#' 
#' @param X a \link[base]{list} of \link[spatstat.explore]{fv.object}s
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' To check
#' \itemize{
#' \item {if \eqn{x}-axis of all \link[spatstat.explore]{fv.object}s are all same}
#' \item {`attr(,'fname')` of all \link[spatstat.explore]{fv.object}s are all same}
#' }
#' 
#' Note that
#' \itemize{
#' \item {function [key1.fv] returns of all \link[spatstat.explore]{fv.object}s are not required to be all same}
#' }
#' 
#' @returns 
#' Function [check_fvlist] does not have a returned value.
#' 
#' @keywords internal
#' @export
check_fvlist <- function(X, ...) {
  
  x <- lapply(X, FUN = `[[`, 1L)
  if (!all(duplicated.default(x)[-1L])) stop('x-axis of all fv.objects are not the same')
  
  fname <- lapply(X, FUN = attr, which = 'fname', exact = TRUE)
  if (!all(duplicated.default(fname)[-1L])) stop('fname of all fv.objects are not the same')
  
  # I do not require [key1.fv] to be the same!!!!
  
}


key1_fvlist <- function(X, ...) {
  .Defunct(msg = 'currently not using')
  vapply(X, FUN = key1.fv, FUN.VALUE = NA_character_)
}



#' @title Operations on a \link[base]{list} of \link[spatstat.explore]{fv.object}s
#' 
#' @param X a \link[base]{list} of \link[spatstat.explore]{fv.object}s
#' 
#' @param check \link[base]{logical} scalar, an option to suppress 
#' function [check_fvlist] in a batch process.
#' Default `TRUE`
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' Function [key1val.fvlist] gathers the primary outcome (via [key1.fv])
#' of the \link[spatstat.explore]{fv.object}s.
#' 
#' @returns
#' All functions return a \link[base]{double} \link[base]{matrix}.
#' 
#' @name fvlist
#' @export
key1val.fvlist <- function(X, check = TRUE, ...) {
  
  if (check) check_fvlist(X, ...)

  r <- X[[1L]][[1L]]
  
  ret <- lapply(X, FUN = function(x) {
    #key1nonfinite(x) <- 0 # do I want to do this here?
    x[[key1.fv(x)]]
  }) |>
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
  # because NA_real_ does not help with pracma::trapz
  
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


# do NOT define `[.fvlist`
# tzh does not want to override `[` for 'list'
# @export
#subset.fvlist <- function(x, subset, ...) {
#}



