

# may consider writing
# print.grouped_hyperframe <- function(x, ...)

# ?spatstat.geom::head.hyperframe
# need to wrint head.grouped_hyperframe


#' @title Extract Grouping Formula from `grouped_hyperframe`
#' @description
#' ..
#' 
#' @param object a `grouped_hyperframe`
#' @param asList,sep place holders for S3 generic \link[nlme]{getGroupsFormula}
#' @returns 
#' Function [getGroupsFormula.grouped_hyperframe] returns a one-sided \link[stats]{formula}
#' @keywords internal
#' @importFrom nlme getGroupsFormula
#' @export getGroupsFormula.grouped_hyperframe
#' @export
getGroupsFormula.grouped_hyperframe <- function(object, asList, sep) {
  attr(object, which = 'group', exact = TRUE)
}

