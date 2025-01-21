
#' @title Grouped Hyperframe
#' 
#' @description
#' A class [groupedHyperframe] to represent \link[spatstat.geom]{hyperframe} 
#' with a (multilevel) hierarchical structure.
#' 
#' @details
#' The class [groupedHyperframe] inherits from class \link[spatstat.geom]{hyperframe}.
#' This class has additional \link[base]{attributes} 
#' \describe{
#' \item{`attr(,'group')`}{\link[stats]{formula}}
#' }
#' 
#' @returns 
#' A [groupedHyperframe]
#' 
#' @name groupedHyperframe
NULL



#' @title Print [groupedHyperframe]
#' 
#' @param x a [groupedHyperframe]
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [print.groupedHyperframe] does not have a returned value.
#' 
#' @seealso `?nlme:::print.groupedData`
#' 
#' @importFrom spatstat.geom as.data.frame.hyperframe
#' @export print.groupedHyperframe
#' @export
print.groupedHyperframe <- function(x, ...) {
  cat('\nGrouped Hyperframe: ')
  grp <- attr(x, which = 'group', exact = TRUE)
  #if (identical(emptyenv(), environment(grp))) {
  #  environment(grp) <- globalenv()
  #} # not sure how this is useful in ?nlme:::print.groupedData
  print(grp, ...)
  cat('\n')
  print(as.data.frame.hyperframe(x, discard = FALSE), ...) # see inside ?spatstat.geom::print.hyperframe
}








#' @title Extract Subset of [groupedHyperframe]
#' 
#' @param x a [groupedHyperframe]
#' 
#' @param ... additional parameters of \link[spatstat.geom]{[.hyperframe}
#' 
#' @returns
#' Function \link{[.groupedHyperframe} returns a [groupedHyperframe] or a \link[spatstat.geom]{hyperframe}.
#' 
#' @importFrom spatstat.geom [.hyperframe
#' @export [.groupedHyperframe
#' @export
`[.groupedHyperframe` <- function(x, ...) {
  ret <- `[.hyperframe`(x, ...)
  # a bandage fix hahaha
  group <- attr(x, which = 'group', exact = TRUE)
  if (!all(all.vars(group) %in% names(ret))) return(ret) # just 'hyperframe'
  attr(ret, which = 'group') <- group
  class(ret) <- unique.default(c('groupedHyperframe', class(ret)))
  return(ret)
}


#' @importFrom spatstat.geom subset.hyperframe
#' @export
subset.groupedHyperframe <- function(x, ...) {
  ret <- subset.hyperframe(x, ...)
  # a bandage fix hahaha
  group <- attr(x, which = 'group', exact = TRUE)
  if (!all(all.vars(group) %in% names(ret))) return(ret) # just 'hyperframe'
  attr(ret, which = 'group') <- group
  class(ret) <- unique.default(c('groupedHyperframe', class(ret)))
  return(ret)
}



#' @title Extract Grouping Formula from [groupedHyperframe]
#' @description
#' ..
#' 
#' @param object a [groupedHyperframe]
#' @param asList,sep place holders for S3 generic \link[nlme]{getGroupsFormula}
#' @returns 
#' Function [getGroupsFormula.groupedHyperframe] returns a one-sided \link[stats]{formula}
#' @keywords internal
#' @importFrom nlme getGroupsFormula
#' @export getGroupsFormula.groupedHyperframe
#' @export
getGroupsFormula.groupedHyperframe <- function(object, asList, sep) {
  attr(object, which = 'group', exact = TRUE)
}

