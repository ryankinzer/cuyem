#------------------------------------------------------------------------------
# Abundance Partioning
#------------------------------------------------------------------------------
# Partions an estimated escapement or abundacne into different groups by
# proportions.  The function requires escapement and SE estimates, and numbers
# of individuals in the different groups of interest. Standard errors are
# provided using the product variance esimate, and confidence intervals are
# provided with three methods;
#------------------------------------------------------------------------------
# Ryan N. Kinzer
# Created 06/01/2016
#------------------------------------------------------------------------------

#' Title
#'
#' @param N
#' @param SE.N
#' @param X
#' @param groups
#' @param alpha
#' @param print
#'
#' @return
#' @export
#'
#' @examples
abundance_partioning <- function(N,SE.N,X,groups=NULL,
                                  alpha=.05,print=TRUE){

  # N = total escapement or abundance
  # SE.N = standard error of N
  # X = vector of summed individuals in each group
  #     (e.g. 100 NOR, 50 Integrated, 75 Segregated carcasses; c(100, 50, 75))
  # groups = group names, matches X in length
  # method = variance and ci's calculation method
  # alpha = precision level; e.g. 0.1, 0.05,... - DEFAULT = 0.05

  p_l <- estimate_proportion(X,sum(X),print=FALSE)

  N_l <- N*p_l$phat
  SE.N_l <- sqrt((N^2*p_l$Std.error^2) + (p_l$phat^2*(SE.N^2)) + (p_l$Std.error^2 * SE.N^2))
  CI.lower.N <- N_l - qnorm(1-alpha/2)*SE.N_l
  CI.upper.N <- N_l + qnorm(1-alpha/2)*SE.N_l


  grp.name <- 1:length(X)

  if(!is.null(groups)){
    grp.name <- groups
    } # end is.null

   output <- list(proportion = data.frame(p.hat = p_l$phat,
                                           SE.p = p_l$Std.error,
                                           CI.lower = p_l$CI.lower,
                                           CI.upper = p_l$CI.upper,
                                  row.names=grp.name),
                   abundance = data.frame(N.hat = N_l, SE.N = SE.N_l,
                                          CI.lower = CI.lower.N,
                                          CI.upper = CI.upper.N,
                                          row.names=grp.name)
                    )
   if(print){
     cat("Confidence intervals are", (1-alpha)*100,"% \n")
     print(output)
   }

   return(output)

} # end function
