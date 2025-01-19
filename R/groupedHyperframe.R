

#' @title Print `groupedHyperframe`
#' 
#' @param x a `groupedHyperframe`
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
  cat('Grouped Hyperframe: ')
  grp <- attr(x, which = 'group', exact = TRUE)
  #if (identical(emptyenv(), environment(grp))) {
  #  environment(grp) <- globalenv()
  #} # not sure how this is useful in ?nlme:::print.groupedData
  print(grp, ...)
  print(as.data.frame.hyperframe(x, discard = FALSE), ...) # see inside ?spatstat.geom::print.hyperframe
}

# ?spatstat.geom::head.hyperframe
# need to wrint head.groupedHyperframe -- NOOOOO!!!!

# @title Extract Subset of `groupedHyperframe`
# 
# @param name description
#spatstat.geom::`[.hyperframe`





#' @title Extract Grouping Formula from `groupedHyperframe`
#' @description
#' ..
#' 
#' @param object a `groupedHyperframe`
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

