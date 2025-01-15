

#' @title Batch Operations of `'ppplist'` Object
#' 
#' @description
#' Batch operations of function [fv_ppp] or [dist_ppp], for a `'ppplist'` input.
#' 
#' @param x a `'ppplist'` object
#' 
#' @param ... additional parameters of workhorse functions 
#' [fv_ppp] or [dist_ppp]
#' 
#' @details
#' Function [fv_ppplist] is a batch process of the workhorse function [fv_ppp],
#' with the help of R-core package \pkg{parallel}.
#' 
#' Function [dist_ppplist] is a batch process of the workhorse function [dist_ppp],
#' with the help of R-core package \pkg{parallel}.
#' 
#' @returns 
#' Function [fv_ppplist] returns a \link[base]{list} of function [fv_ppp] returns.
#' 
#' Function [dist_ppplist] returns a \link[base]{list} of function [dist_ppp] returns.
#' 
#' @examples
#' library(spatstat.data)
#' library(spatstat.geom) # for ?spatstat.geom::split.ppp
#' library(spatstat.explore) # for ?spatstat.explore::Emark, etc.
#' 
#' # create a 'ppplist' object
#' Vc = with(shapley$marks, expr = {
#'  cut.default(V, breaks = quantile(V, probs = c(0, 1/3, 2/3, 1)), labels = c('L', 'M', 'H'))
#' })
#' x0 = shapley; x0$marks$V = NULL
#' plot(x <- split.ppp(x0, f = Vc))
#' 
#' xE = fv_ppplist(x, fn = Emark)
#' names(xE)
#' names(xE$L)
#' stopifnot(inherits(xE$L$Mag.E, what = 'fv'))
#' 
#' # create another 'ppplist' object
#' nbf = nbfires |> 
#'   subset.ppp(select = c('fire.type', 'cause', 'ign.src')) |>
#'   na.omit.ppp()
#' x2 = split.ppp(nbf, f = nbf$marks$fire.type)
#' dist_ppplist(x2, fn = nncross_.ppp, i = 'rrds', j = 'ltning')
#' dist_ppplist(x2, fn = nncross_.ppp, i = 'unknown', j = 'burn.no.perm')
#' @keywords internal
#' @name op_ppplist
#' @export
fv_ppplist <- function(x, ...) op_ppplist(x, op = fv_ppp, ...)
  
#' @rdname op_ppplist
#' @export
dist_ppplist <- function(x, ...) op_ppplist(x, op = dist_ppp, ...)
  


#' @importFrom parallel mclapply detectCores
op_ppplist <- function(x, op, ...) {
  
  n <- length(x)
  
  ret <- mclapply(X = seq_len(n), mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores()), FUN = function(i) {
  #ret <- lapply(X = seq_len(n), FUN = function(i) { # to debug inside
    # echo-command does not work with '\r' (carriage return)
    if (identical(Sys.getenv('RSTUDIO'), '1')) on.exit(system(command = sprintf(fmt = 'printf \'\r%d/%d done!    \'', i, n)))
    return(op(x = x[[i]], ...))
  })
  on.exit(message())
  
  names(ret) <- names(x)
  return(ret)
  
}


