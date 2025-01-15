

#' @title Finite Indices of \link[spatstat.explore]{fv.object}
#' 
#' @description
#' Finite indices of the *black solid curve* 
#' as shown in \link[spatstat.explore]{plot.fv}
#' of an \link[spatstat.explore]{fv.object}.
#' 
#' @param x an \link[spatstat.explore]{fv.object}
#' 
#' @details
#' Function [is.finite.fv] is an S3 method dispatch of the generic function 
#' \link[base]{is.finite}.
#' 
#' Function [is.finite.fv] finds the \link[base]{finite},
#' i.e., non-`NA`, non-`NaN` and non-`Inf`,
#' indices of the *black solid curve* of an \link[spatstat.explore]{fv.object}.
#' 
#' @returns 
#' Function [is.finite.fv] returns a \link[base]{logical} \link[base]{vector}.
#' 
#' @method is.finite fv
#' @export is.finite.fv
#' @export
is.finite.fv <- function(x) {
  is.finite(x[[ynm.fv(x)]])
}




#' @title Operations on \link[spatstat.explore]{fv.object}
#' 
#' @description
#' Various operations on a function value \link[spatstat.explore]{fv.object}.
#' 
#' @param X an \link[spatstat.explore]{fv.object}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @name op_fv
NULL


#' @rdname op_fv
#' 
#' @details
#' Function [ynm.fv] finds the name of primary outcome,
#' i.e., the *black solid curve* as shown in \link[spatstat.explore]{plot.fv},
#' of an \link[spatstat.explore]{fv.object}.
#' 
#' @returns
#' Function [ynm.fv] returns a \link[base]{character} scalar.
#' Specifically,
#' \describe{
#' \item{`'iso'`}{for \link[spatstat.explore]{fv.object} returned from functions 
#' \link[spatstat.explore]{Emark},
#' \link[spatstat.explore]{Vmark},
#' \link[spatstat.explore]{markcorr},
#' \link[spatstat.explore]{markvario},
#' \link[spatstat.explore]{Kcross},
#' \link[spatstat.explore]{Kest},
#' \link[spatstat.explore]{Lcross}
#' }
#' \item{`'km'`}{for \link[spatstat.explore]{fv.object} returned from functions 
#' \link[spatstat.explore]{Gcross},
#' \link[spatstat.explore]{Jcross}
#' }
#' \item{`'fobs'`}{for \link[spatstat.explore]{fv.object} returned from function \link[spatstat.explore]{roc.ppp}}
#' }
#' 
#' @export
ynm.fv <- function(X, ...) {
  fname1 <- attr(X, which = 'fname', exact = TRUE)[1L]
  switch(EXPR = fname1, roc = { # from ?spatstat.explore::roc.ppp
    'fobs'
  }, 
  E =, # from ?spatstat.explore::Emark
  g =, # from ?spatstat.explore::pcfcross
  gamma =, # from ?spatstat.explore::markvario
  k =, # from ?spatstat.explore::markcorr
  K =, # from ?spatstat.explore::Kcross and ?spatstat.explore::Kest
  L =, # from ?spatstat.explore::Lcross
  p =, # from ?spatstat.explore::markconnect
  V = { # from ?spatstat.explore::Vmark
    'iso'
  }, 
  G =, # from ?spatstat.explore::Gcross
  J = { # from ?spatstat.explore::Jcross
    'km'
  }) 
}








#' @rdname op_fv
#' 
#' @details
#' Functions [trapz.fv] and [cumtrapz.fv] 
#' obtain the (cumulative) 
#' \link[pracma]{trapz}oidal integration of 
#' the area under the ***black solid curve*** (see function [ynm.fv]) 
#' of a function value \link[spatstat.explore]{fv.object}.
#' 
#' @returns
#' Function [trapz.fv] returns a \link[base]{numeric} scalar.
#' 
#' @examples
#' library(spatstat.data)
#' library(spatstat.explore)
#' library(spatstat.geom) # ?spatstat.geom::subset.ppp
#' 
#' # numeric mark
#' plot(x <- Emark(spruces))
#' ynm.fv(x)
#' trapz.fv(x)
#' tail(cumtrapz.fv(x))
#' 
#' plot(x <- Vmark(spruces))
#' trapz.fv(x)
#' tail(cumtrapz.fv(x))
#' 
#' plot(x <- markcorr(spruces))
#' trapz.fv(x)
#' tail(cumtrapz.fv(x))
#' 
#' plot(x <- markvario(spruces))
#' trapz.fv(x)
#' tail(cumtrapz.fv(x))
#' 
#' # multitype
#' (btc = subset.ppp(betacells, select = 'type'))
#' plot(x <- Gcross(btc, i = 'off', j = 'on'))
#' ynm.fv(x)
#' trapz.fv(x)
#' tail(cumtrapz.fv(x))
#' 
#' plot(x <- Kcross(btc, i = 'off', j = 'on'))
#' trapz.fv(x)
#' tail(cumtrapz.fv(x))
#' 
#' plot(x <- Lcross(btc, i = 'off', j = 'on'))
#' trapz.fv(x)
#' tail(cumtrapz.fv(x))
#' 
#' plot(x <- Jcross(btc, i = 'off', j = 'on'))
#' x$km # um.. bug?
#' #trapz.fv(x)
#' #tail(cumtrapz.fv(x))
#' 
#' plot(x <- pcfcross(btc, i = 'off', j = 'on'))
#' head(x$iso)
#' head(is.finite(x))
#' trapz.fv(x)
#' tail(cumtrapz.fv(x))
#' 
#' plot(x <- markconnect(btc, i = 'off', j = 'on'))
#' trapz.fv(x)
#' tail(cumtrapz.fv(x))
#' 
#' # roc
#' plot(x <- roc.ppp(swedishpines, covariate = 'x'))
#' trapz.fv(x)
#' tail(cumtrapz.fv(x))
#' @keywords internal
#' @importFrom pracma trapz
#' @export
trapz.fv <- function(X, ...) {
  X <- X[is.finite.fv(X)] # to invoke ?spatstat.explore::`[.fv`
  #fname1 <- attr(X, which = 'fname', exact = TRUE)[1L]
  #xnm <- switch(fname1, roc = 'p', 'r')
  # `X[[1L]]` is (presumably) x-axis variable
  #return(trapz(x = X[[xnm]], y = X[[ynm.fv(X)]]))
  return(trapz(x = X[[1L]], y = X[[ynm.fv(X)]]))
}





#' @rdname op_fv
#' 
#' @returns 
#' Function [cumtrapz.fv] returns a \link[base]{numeric} \link[base]{vector}.
#' 
#' @importFrom pracma cumtrapz
#' @export 
cumtrapz.fv <- function(X, ...) {
  
  X <- X[is.finite.fv(X)] # to invoke ?spatstat.explore::`[.fv`
  
  # 'fv' inherits from 'data.frame', as of 2024-10-22 # packageDate('spatstat.explore')
  nr <- .row_names_info(X, type = 2L)
  if (nr == 1L) return(invisible()) # exception handling
  # needed! Otherwise ?pracma::cumtrapz errs

  # a trapz needs two points
  # therefore `X[[1L]][-1L]`
  ret0 <- cumtrapz(x = X[[1L]], y = X[[ynm.fv(X)]])
  ret <- c(ret0[-1L])
  names(ret) <- X[[1L]][-1L]
  return(ret)
  
  # ?spatstat.explore::`[.fv` too slow!
  #fv. <- lapply(X[[1L]][-1L], FUN = function(i) { # (i = X[[1L]][-1L][1L])
  #  # to invoke ?spatstat.explore::`[.fv`
  #  eval(call(name = '[', quote(X), call(name = '<=', quote(X[[1L]]), i)))
  #}) # a 'list' of 'fv'
  #ret <- vapply(fv., FUN = trapz.fv, FUN.VALUE = NA_real_)
  #names(ret) <- X[[1L]][-1L]
  #return(ret)
  
}






