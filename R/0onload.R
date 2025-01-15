

.onLoad <- function(libname, pkgname = 'spatstat.grouped') {
  
  Sys.setenv('_R_CHECK_LIMIT_CORES_' = 'false') 
  # otherwise ?parallel:::.check_ncores causes error when ?devtools::check
  
}
