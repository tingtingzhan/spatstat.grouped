
#' @title Operations on \link[spatstat.geom]{ppp.object}
#' 
#' @description
#' Create
#' \link[spatstat.explore]{fv.object}s
#' or 
#' distances
#' from a \link[spatstat.geom]{ppp.object}.
#' 
#' @param x a \link[spatstat.geom]{ppp.object}
#' 
#' @param fn a distance \link[base]{function}, 
#' or a \link[base]{function} that returns an \link[spatstat.explore]{fv.object}, 
#' see **Details**
#' 
#' @param mark_nm (optional) \link[base]{character} scalar,
#' name of \link[spatstat.geom]{marks},
#' when `markformat.ppp(x) == 'vector'`
#' 
#' @param ... additional parameters of \link[base]{function} `fn`
#' 
#' @details
#' First, the input \link[spatstat.geom]{ppp.object} is \link[spatstat.geom]{unstack.ppp}-ed.
#' 
#' Each of the \link[base]{numeric} \link[spatstat.geom]{marks} could be analyzed by   
#' following functions that return an \link[spatstat.explore]{fv.object},
#' \itemize{
#' \item {\link[spatstat.explore]{Emark}}
#' \item {\link[spatstat.explore]{Vmark}}
#' \item {\link[spatstat.explore]{markcorr}} 
#' \item {\link[spatstat.explore]{markvario}}
#' }
#' If one of the functions above are provided 
#' but there is no \link[base]{numeric} \link[spatstat.geom]{marks} in the input,
#' a `NULL` value will be returned.
#' 
#' Each of the \link[spatstat.geom]{marks} that \link[spatstat.geom]{is.multitype}
#' could be analyzed 
#' by following functions that return an \link[spatstat.explore]{fv.object},
#' \itemize{
#' \item {\link[spatstat.explore]{Gcross}}
#' \item {\link[spatstat.explore]{Jcross}}
#' \item {\link[spatstat.explore]{Kcross}}
#' \item {\link[spatstat.explore]{Lcross}}
#' \item {\link[spatstat.explore]{markconnect}}
#' }
#' or by following functions that return a distance,
#' \itemize{
#' \item {[nncross_.ppp]}
#' }
#' If one of the functions above are provided 
#' but there is no \link[spatstat.geom]{marks} \link[spatstat.geom]{is.multitype} in the input,
#' a `NULL` value will be returned.
#' 
#' @returns 
#' Function [fv_ppp] returns a \link[base]{list} of 
#' \link[spatstat.explore]{fv.object}s, 
#' one for each eligible \link[spatstat.geom]{marks}.
#' 
#' Function [dist_ppp] returns a \link[base]{list} of 
#' \link[base]{double} \link[base]{vector}s,
#' one for each eligible \link[spatstat.geom]{marks}.
#' 
#' @examples
#' library(spatstat.data)
#' library(spatstat.explore)
#' 
#' fv_ppp(betacells, fn = Emark) # applicable to numeric mark
#' fv_ppp(betacells, fn = Gcross, i = 'off', j = 'on') # applicable to multitype mark
#' 
#' dist_ppp(betacells, fn = nncross_.ppp, i = 'off', j = 'on')
#' dist_ppp(gorillas, fn = nncross_.ppp, i = 'major', j = 'minor')
#' dist_ppp(gorillas, fn = nncross_.ppp, i = 'rainy', j = 'dry')
#' @keywords internal
#' @name op_ppp
#' @importFrom spatstat.geom unstack.ppp
#' @export
dist_ppp <- function(x, fn, mark_nm, ...) {
  
  x. <- unstack.ppp(x)
  if (length(x.) == 1L && !length(names(x.))) names(x.) <- mark_nm
  
  mtp <- vapply(x., FUN = is.multitype.ppp, FUN.VALUE = NA)
  if (!any(mtp)) return(invisible())
  
  fn_mtp_ <- c(
    nncross = 'nncross_.ppp'
  )
  fn_mtp <- lapply(fn_mtp_, FUN = get)
  id_mtp <- vapply(fn_mtp, FUN = identical, x = fn, FUN.VALUE = NA)
  
  if (!any(id_mtp)) stop('fn not supported')

  ret <- lapply(x.[mtp], FUN = fn, ...)
  names(ret) <- paste(names(ret), names(fn_mtp_)[id_mtp], sep = '.')
  
  return(ret[lengths(ret) > 0L])
  
}





#' @rdname op_ppp
#' @importFrom spatstat.explore Emark Vmark markcorr markvario
#' @importFrom spatstat.explore Gcross Jcross Kcross Lcross markconnect
#' @importFrom spatstat.geom unstack.ppp
#' @export
fv_ppp <- function(x, fn, mark_nm, ...) {
  
  x. <- unstack.ppp(x)
  if (length(x.) == 1L && !length(names(x.))) names(x.) <- mark_nm
  
  mtp <- vapply(x., FUN = is.multitype.ppp, FUN.VALUE = NA)
  num <- vapply(x., FUN = function(i) is.numeric(i$marks), FUN.VALUE = NA)
  # stopifnot(is.double(POSIXct), !is.numeric(POSIXct))
  
  fn_numeric <- any(vapply(list(
    Emark, Vmark, markcorr, markvario
  ), FUN = identical, x = fn, FUN.VALUE = NA))
  
  fn_mtp <- any(vapply(list(
    Gcross, Jcross, Kcross, Lcross, markconnect
  ), FUN = identical, x = fn, FUN.VALUE = NA))
  
  # functions like ?spatstat.explore::Kest
  # applicable to none-mark \link[spatstat.geom]{ppp.object}
  # how to deal?
  
  if (!xor(fn_numeric, fn_mtp)) stop('unknown fv-function to tzh?')
  
  x.. <- if (fn_numeric) {
    if (!any(num)) return(invisible())
    x.[num]
  } else if (fn_mtp) {
    if (!any(mtp)) return(invisible())
    x.[mtp]
  }
  
  ret <- lapply(x.., FUN = fn, ...)
  
  # restore names of `fv`-hypercolumns from the result
  # attr(,'fname') is determined by `fn`
  fname1 <- attr(ret[[1L]], which = 'fname', exact = TRUE)[1L]
  names(ret) <- paste(names(ret), fname1, sep = '.')
  
  return(ret)
  
}



