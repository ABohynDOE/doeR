#' @title num2gen
#'
#' @description `num2gen` converts a number into its corresponding generator.
#'
#' @param n A numeric or numeric vector.
#' @return A string or character vector holding the generator corresponding to
#' `n`
#' @examples
#' num2gen(7)
#' num2gen(n = c(13,31,28))
#' @export
num2gen <- function(n){
  out <- c()
  for (x in n) {
    i <- 1
    index <- 1
    s <- ""
    while(i <= x){
      if(bitwAnd(i, x) != 0){
        s <- paste0(s, letters[index])
      }
      i <- bitwShiftL(i,1)
      index <- index + 1
    }
    out <- append(out,s)
  }
  return(out)
}
