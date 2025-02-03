

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
  is.finite(x[[key1.fv(x)]])
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
#' Function [key1.fv] finds the name of primary outcome,
#' i.e., the *black solid curve* as shown in \link[spatstat.explore]{plot.fv},
#' of an \link[spatstat.explore]{fv.object}.
#' 
#' @returns
#' Function [key1.fv] returns a \link[base]{character} scalar.
#' 
#' @importFrom grDevices dev.off png
#' @importFrom spatstat.explore plot.fv
#' @export
key1.fv <- function(X, ...) {
  
  # unlike ?graphics::barplot.default, ?graphics::boxplot.default, or ?graphics::hist.default
  # ?spatstat.explore::plot.fv 
  # a). does *not* have a parameter `plot` to suppress the plot
  # b). passes `...` to ?graphics::plot.default, which does *not* have an option to suppress plot
  
  f. <- tempfile(fileext = '.png')
  png(filename = f.)
  d <- plot.fv(X) # 'data.frame'
  dev.off()
  file.remove(f.)
  
  # opening/closing a device, then creating a plot, in batch process is extremely slow
  # tzh is writing to Dr. Baddeley
  
  key <- with(d, expr = key[lty == 1L])
  return(key)
  
}








#' @rdname op_fv
#' 
#' @details
#' Functions [trapz.fv] and [cumtrapz.fv] 
#' obtain the (cumulative) 
#' \link[pracma]{trapz}oidal integration of 
#' the area under the ***black solid curve*** (see function [key1.fv]) 
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
#' key1.fv(x)
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
#' key1.fv(x)
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
  return(trapz(x = X[[1L]], y = X[[key1.fv(X)]]))
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
  # therefore `[-1L]`
  ret <- c(cumtrapz(x = X[[1L]], y = X[[key1.fv(X)]])[-1L])
  names(ret) <- X[[1L]][-1L]
  return(ret)
  
}






