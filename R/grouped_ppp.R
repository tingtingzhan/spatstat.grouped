


#' @title [groupedHyperframe] with One-and-Only-One \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}
#' 
#' @description
#' ..
#' 
#' @param formula \link[stats]{formula} in the format of 
#' `m1+m2 ~ y+x1+x2 | g1/g2`,
#' where \eqn{m_i}'s are one or more \link[spatstat.geom]{marks},
#' \eqn{y} and \eqn{x_j}'s are the endpoint and predictor(s) for downstream analysis,
#' and \eqn{g_k} are one or more hierarchical grouping structure
#' (in the fashion of parameter `random` of function \link[nlme]{lme})
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param coords \link[stats]{formula}, variable names
#' of \eqn{x}- and \eqn{y}-coordinates in `data`.
#' Default `~x+y`.
#' 
#' @param window an observation window \link[spatstat.geom]{owin}, 
#' default is the \eqn{x}- and \eqn{y}-span of `coords` in `data`.
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' ...
#' 
#' 
#' 
#' @returns
#' Function [grouped_ppp] returns a [groupedHyperframe]
#' with ***one-and-only-one*** 
#' \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}.
#' 
#' @examples
#' library(spatstat.grouped.data)
#' library(survival) # to help ?spatstat.geom::hyperframe understand ?survival::Surv
#' grouped_ppp(hladr + phenotype ~ OS + gender + age | patient_id/image_id, data = wrobel_lung)
#' @importFrom parallel mclapply detectCores
#' @importFrom spatstat.geom owin ppp hyperframe
#' @export
grouped_ppp <- function(
    formula, 
    data, 
    coords = ~ x + y, 
    window = owin(xrange = range(.x), yrange = range(.y)),
    ...
) {	
  
  mk <- all.vars(formula[[2L]]) # name clash ?spatstat.geom::marks
  
  x <- formula[[3L]][[2L]] # patient-level covariates
  group <- formula[[3L]][[3L]]
  
  xy_ <- as.list.default(coords[[2L]])
  if ((xy_[[1L]] != '+') || (length(xy_) != 3L)) stop('Specify x and y coordinates names as ~x+y')
  if (!is.symbol(x <- xy_[[2L]])) stop('x-coordinates must be a symbol, for now')
  if (!is.symbol(y <- xy_[[3L]])) stop('y-coordinates must be a symbol, for now')
  if (anyNA(.x <- data[[x]])) stop('Do not allow missingness in x-coordinates')
  if (anyNA(.y <- data[[y]])) stop('Do not allow missingness in y-coordinates')
  
  g <- all.vars(group)
  data[g] <- lapply(data[g], FUN = function(i) {
    if (is.factor(i)) return(factor(i)) # drop empty levels!!
    factor(i, levels = unique(i))
  }) 
  
  f_ppp <- interaction(data[g], drop = TRUE, sep = '.', lex.order = TRUE) # one or more hierarchy

  force(window)
  ppp. <- ppp(x = .x, y = .y, window = window, marks = data[mk], checkdup = FALSE, drop = FALSE) |> # `drop = FALSE` important!!!
    #log.ppp() |> # if needed
    split_ppp_dataframe(f = f_ppp) # only 'list'
  
  # ?spatstat.geom::split.ppp (which is also slow) 
  # does not respect col-1 dataframe; need to change
  # `marks(x)` -> `marks(x, drop = FALSE)`
  # `marks<-.ppp` -> `marks(x, drop = FALSE) <- ...` # probably
  # parameter `drop` of ?spatstat.geom::split.ppp has a different meaning
  # I don't want to email Dr. Baddeley, yet
  
  dat0 <- data[all.vars(formula[[3L]])] |>
    mcaggregate_unique(f = f_ppp)
  
  hf <- do.call(what = hyperframe, args = c(
    list(ppp. = ppp.),
    as.list.data.frame(dat0)
  ))
  
  # additional attributes to mimic ?nlme::groupedData
  # also see example 'groupedData's from package datasets
  attr(hf, which = 'group') <- call('~', group) # not carrying `f_ppp`, for now
  # for ?nlme::getGroupsFormula
  
  class(hf) <- unique.default(c('groupedHyperframe', class(hf)))
  return(hf)
  
}



# Tingting is not ready to suggest changing [split.ppp] to Dr. Baddeley, yet..
# [split_ppp_dataframe] is a bandage-fix which respects col-1 dataframe
#' @importFrom spatstat.geom markformat.ppp
split_ppp_dataframe <- function(x, f) {
  # `f` must be 'factor'
  mapply(FUN = function(...) {
    ret <- list(...)
    class(ret) <- 'ppp'
    return(ret)
  }, 
  x = split.default(x$x, f = f),
  y = split.default(x$y, f = f),
  marks = split.data.frame(x$marks, f = f),
  n = lengths(split.default(seq_along(f), f = f), use.names = FALSE),
  MoreArgs = list(
    window = x$window,
    markformat = markformat.ppp(x)
  ), SIMPLIFY = FALSE)
}






# ?stats::aggregate.data.frame is not parallel computing
# ?collapse::collap does not support 'Surv' column
mcaggregate_unique <- function(data, f) {
  
  nr <- .row_names_info(data, type = 2L)
  if (nr != length(f)) stop('`data` and `f` different length')
  
  ids <- split.default(seq_len(nr), f = f)
  
  .ident <- vapply(data, FUN = function(d) { # (d = data[[1L]])
    tmp <- mclapply(ids, mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores()), FUN = function(id) {
    #tmp <- lapply(ids, FUN = function(id) {
      all(duplicated(unclass(d[id]))[-1L])
    })
    return(all(unlist(tmp)))
  }, FUN.VALUE = NA)
  
  if (any(!.ident)) {
    nm <- names(data)[!.ident]
    message('Column(s) ', sQuote(nm), ' not identical per aggregation-cluster; thus removed')
    data[nm] <- NULL
  }

  ret <- data[vapply(ids, FUN = `[`, 1L, FUN.VALUE = NA_integer_),]
  # do.call(rbind.data.frame, args = .) # ?base::rbind.data.frame does not respect 'Surv', etc.
  .rowNamesDF(ret) <- NULL
  return(ret)

}






clusterppp <- function(...) .Defunct(new = 'grouped_ppp')
