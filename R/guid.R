#' @title Generate GUID
#'
#' @description Originally shared by Christopher Bare on R-bloggers;
#'   \url{https://www.r-bloggers.com/generate-uuids-in-r/}. The function was
#'   adapted to provide a 36 character GUID.
#'
#' @param uppercase
#'
#' @return A 36 character long version 4.0 globally unique identifier.
#'
#' @examples
#' guid(uppercase = TRUE)
#' guid(uppercase = FALSE)

guid <- function(uppercase=FALSE) {

  hex_digits <- c(as.character(0:9), letters[1:6])
  hex_digits <- if (uppercase) toupper(hex_digits) else hex_digits

  y_digits <- hex_digits[9:12]

  paste(
    paste0(sample(hex_digits, 8), collapse=''),
    paste0(sample(hex_digits, 4), collapse=''),
    paste0(c('4', sample(hex_digits, 3)), collapse=''),
    paste0(c(sample(y_digits,1),
           sample(hex_digits, 3)),
           collapse=''),
    paste0(sample(hex_digits, 12), collapse=''),
    sep='-')
}
