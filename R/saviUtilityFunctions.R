#' Return a random sample of size 1
#'
#' The default sample function is not compatible with vectors of
#' length 1.  If the code says "sample(3:3)," sample will return a
#' random integer between 1 and 3.  The way our code is written, we
#' would like it to return just 3.  We implement the following
#' function for that purpose.
#'
#' @param x A vector of integers
#' @return A random integer from x.  If x is c(5:5), return 5.
#' @examples
#'     sampleOne(c(4:10))
#'     sampleOne(c(4:4))
#'
sampleOne <- function(x) {
  if (length(x) <= 1) {
    return(x)
  } else {
    return(sample(x,1))
  }
}
