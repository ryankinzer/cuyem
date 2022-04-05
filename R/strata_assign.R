#' @title strata_assign
#' @description Create strata groups that have an equal number of days, except for
#' the last group.  The last group is truncated to be the same length as the
#' dataset.
#' @param x vector of consecutive days for splitting into equal lengths
#' @param h_n number of days contained within each strata
#' @return
#' @export
#' @author Ryan N. Kinzer
#' @examples
#' strata_assign(1:75,h_n = 7)
strata_assign <- function(x, h_n){
  grp = rep(1:1000,each=h_n)[1:length(x)]
return(grp)
}


