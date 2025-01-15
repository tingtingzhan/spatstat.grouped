

# badly written functions...
# ?spatstat.explore::markcorr does not use other fv-functions (??)
# ?spatstat.explore::Emark uses ?spatstat.explore::markcorr
# ?spatstat.explore::markvario uses ?spatstat.explore::markcorr
# ?spatstat.explore::Vmark uses ?spatstat.explore::Emark and ?spatstat.explore::markcorr
# Tingting does not want to re-write these functions though..
# .. would be a headache to catch up the authors updates

#' @title User Interface of Operations on \link[spatstat.geom]{hyperframe} with One-and-Only-One \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}
#' 
#' @description
#' See workhorse functions [fv_hyperframe] and [dist_hyperframe].
#' 
#' @param X a \link[spatstat.geom]{hyperframe}
#' 
#' @param ... additional parameters of user operation
#' 
#' @details
#' See explanations in workhorse functions [fv_hyperframe] and [dist_hyperframe].
#' 
#' @returns 
#' See explanations in workhorse functions [fv_hyperframe] and [dist_hyperframe].
#' 
#' @examples
#' library(spatstat.data)
#' 
#' # no good example for [Emark_.hyperframe]
#' # no hyperframe with ppp-hypercolumn with numeric marks
#' 
#' (flu0 = spatstat.geom::subset.hyperframe(flu, stain == 'M2-M1'))
#' Gcross_(flu0, i = 'M1', j = 'M2') # Kcross_; Jcross_; Lcross_
#' nncross_(flu0, i = 'M1', j = 'M2')
#' @name user_hyperframe
#' @export
Emark_ <- function(X, ...) UseMethod(generic = 'Emark_')
#' @rdname user_hyperframe
#' @importFrom spatstat.explore Emark
#' @export Emark_.hyperframe
#' @export
Emark_.hyperframe <- function(X, ...) fv_hyperframe(X, fn = Emark, ...)

#' @rdname user_hyperframe
#' @export
Vmark_ <- function(X, ...) UseMethod(generic = 'Vmark_')
# \link[spatstat.explore]{Vmark} gives warning with >1 warning; we don't know why
#' @rdname user_hyperframe
#' @importFrom spatstat.explore Vmark
#' @export Vmark_.hyperframe
#' @export
Vmark_.hyperframe <- function(X, ...) fv_hyperframe(X, fn = Vmark, ...)

#' @rdname user_hyperframe
#' @export
markcorr_ <- function(X, ...) UseMethod(generic = 'markcorr_')
#' @rdname user_hyperframe
#' @importFrom spatstat.explore markcorr
#' @export markcorr_.hyperframe
#' @export
markcorr_.hyperframe <- function(X, ...) fv_hyperframe(X, fn = markcorr, ...)

#' @rdname user_hyperframe
#' @export
markvario_ <- function(X, ...) UseMethod(generic = 'markvario_')
#' @rdname user_hyperframe
#' @importFrom spatstat.explore markvario
#' @export markvario_.hyperframe
#' @export
markvario_.hyperframe <- function(X, ...) fv_hyperframe(X, fn = markvario, ...)

#' @rdname user_hyperframe
#' @export
Gcross_ <- function(X, ...) UseMethod(generic = 'Gcross_')
#' @rdname user_hyperframe
#' @importFrom spatstat.explore Gcross
#' @export Gcross_.hyperframe
#' @export
Gcross_.hyperframe <- function(X, ...) fv_hyperframe(X, fn = Gcross, ...)

#' @rdname user_hyperframe
#' @export
Jcross_ <- function(X, ...) UseMethod(generic = 'Jcross_')
#' @rdname user_hyperframe
#' @importFrom spatstat.explore Jcross
#' @export Jcross_.hyperframe
#' @export
Jcross_.hyperframe <- function(X, ...) fv_hyperframe(X, fn = Jcross, ...)

#' @rdname user_hyperframe
#' @export
Kcross_ <- function(X, ...) UseMethod(generic = 'Kcross_')
#' @rdname user_hyperframe
#' @importFrom spatstat.explore Kcross
#' @export Kcross_.hyperframe
#' @export
Kcross_.hyperframe <- function(X, ...) fv_hyperframe(X, fn = Kcross, ...)

#' @rdname user_hyperframe
#' @export
Lcross_ <- function(X, ...) UseMethod(generic = 'Lcross_')
#' @rdname user_hyperframe
#' @importFrom spatstat.explore Lcross
#' @export Lcross_.hyperframe
#' @export
Lcross_.hyperframe <- function(X, ...) fv_hyperframe(X, fn = Lcross, ...)

# Inside \link[spatstat.explore]{Gcross} and \link[spatstat.explore]{Kcross}
# @param i type of the points *from* which distances are measured,
# i.e., `X` (or \emph{of}) in \link[spatstat.geom]{nncross}.
# @param j type of the points *to* which distances are measured,
# i.e., `Y` (or \emph{in}) in \link[spatstat.geom]{nncross}.


#' @rdname user_hyperframe
#' @export nncross_.hyperframe
#' @export
nncross_.hyperframe <- function(X, ...) dist_hyperframe(X, fn = nncross_.ppp, ...)



