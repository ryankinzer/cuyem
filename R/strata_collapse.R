#' @title strata_collapse
#' @description Collapses an initial strata scheme (e.g., 7 days) into strata containing at least the designated number of recaps. Seven day strata may become 14 days if recaps in the first strata are less than designated number.
#' @param x vector of recaps
#' @param recaps number of designated recaps
#' @author Ryan N. Kinzer
#' @return
#' @export
#' @examples
#' x <- c(1,1,1,4,1,1,2,2,2,1,6,7)
#' strata_collapse(x, 7)
strata_collapse <- function(x, recaps){
  strata = NULL
  recapSum = NULL
  tmp_cnt <- 0
  s = 1

    for(i in 1:length(x)){

      if(tmp_cnt < recaps){
          tmp_cnt <- tmp_cnt + x[i]
          strata[i] <- s
          recapSum[i] = tmp_cnt
      }

      if(recapSum[i] >= recaps){
        s = s+1
        tmp_cnt = 0
      }

    }


  if(recapSum[i] < recaps){
    strata[strata==s] <- s - 1
  }

  #strata_df <- data.frame(Strata = strata)
  #strata_df <- as.integer(Strata = strata)
  #return(strata_df)
  return(strata)
  #return(strata[!is.na(strata)])
}
