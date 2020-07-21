#' @title add_Totals
#'
#' @description Adds a column and Row with Total values to the provided df..
#'
#' @param data A prepared dataframe ready to receive totals
#' @author Tyler Stright
#'
#' @examples
#'
#' @import tidyverse
#' @export
#' @return NULL


add_Totals <- function(data) {

# remove spaces
# names(data) <- gsub(' ','_', names(data))

# Calculate Column Total
data <- data %>%
  mutate(Total = apply(data[,c(2:ncol(data))], 1, sum, na.rm = TRUE))

# Calculate Row Total
totals_row <- c('Total', apply(data[,c(2:ncol(data))], 2, sum, na.rm = TRUE))

data <- data %>%
  rbind(totals_row)

# add spaces back in
# names(data) <- gsub('_', ' ', names(data))

  return(data)
}
