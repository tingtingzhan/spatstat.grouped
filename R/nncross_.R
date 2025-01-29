
#' @title Alternative Interface of \link[spatstat.geom]{nncross}
#' 
#' @description
#' An alternative interface of function \link[spatstat.geom]{nncross}.
#' 
#' @param X see **Details**
#' 
#' @param i,j \link[base]{character} or \link[base]{integer} scalars. 
#' See functions \link[spatstat.explore]{Gcross},
#' and \link[spatstat.explore]{Kcross} for more details
#' 
#' @param ... additional parameters, 
#' including `i` and `j` for function [nncross_.hyperframe]
#' 
#' @details
#' Function [nncross_.ppp] creates an interface similar to 
#' functions \link[spatstat.explore]{Gcross},
#' \link[spatstat.explore]{Jcross},
#' and \link[spatstat.explore]{Kcross}, etc.,
#' which takes an \link[spatstat.geom]{is.multitype} \link[spatstat.geom]{ppp.object}
#' and two mark values `i` and `j`, 
#' then calls the workhorse function 
#' \link[spatstat.geom]{nncross} with parameter `what = 'dist'`.
#' If mark values `i` and `j` does not exist in the \link[spatstat.geom]{ppp.object},
#' a `NULL` value will be returned.
#' 
#' @returns
#' Function [nncross_.ppp] returns
#' a \link[base]{numeric} \link[base]{vector} 
#' if `i` and `j` are valid mark values of \link[spatstat.geom]{ppp.object} `X`;
#' otherwise returns a `NULL` value.
#' 
#' @examples
#' library(spatstat.data)
#' library(spatstat.geom)
#' 
#' (xs = split.ppp(amacrine))
#' (a1 = nncross(X = xs$off, Y = xs$on, what = 'dist'))
#' a2 = nncross_.ppp(amacrine, i = 'off', j = 'on')
#' a3 = nncross_.ppp(amacrine, i = 1L, j = 2L)
#' stopifnot(identical(a1, a2), identical(a1, a3))
#' 
#' nncross_.ppp(amacrine, i = 'a', j = 'b') # exception handling
#' @name nncross_
#' @export
nncross_ <- function(X, ...) UseMethod(generic = 'nncross_')

#' @rdname nncross_
#' @importFrom spatstat.geom nncross.ppp is.marked.ppp is.multitype.ppp marks.ppp split.ppp
#' @export nncross_.ppp
#' @export
nncross_.ppp <- function(X, i, j, ...) {
  
  # see ?spatstat.explore::Gcross carefully
  if (!is.marked.ppp(X, dfok = FALSE)) stop(paste('point pattern has no', sQuote('marks')))
  if (!is.multitype.ppp(X)) stop('nncross_ requires multitype') 
  
  x_ <- split.ppp(X) # using default `f`
  
  # end user named non-existing levels?
  if (!length(i_ <- x_[[i]])) return(invisible())
  if (!length(j_ <- x_[[j]])) return(invisible())
  
  return(nncross.ppp(X = i_, Y = j_, what = 'dist', ...))
  
}


