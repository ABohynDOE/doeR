#' @title gen2num
#'
#' @description `gen2num` converts a generator into its corresponding column
#' number.
#'
#' @param g A string or string vector.
#' @return A numeric or numeric vector holding the numbers corresponding to the
#' generators in `g`
#' @examples
#' gen2num("abc")
#' gen2num(g = c("acd", "e", "cdf"))
#' @export
gen2num <- function(g) {
  out <- c()
  for (gen in g) {
    # Check that g only contains lowercase letters
    g_ascii <- utf8ToInt(gen)
    if (any(g_ascii > 122 | g_ascii < 97)) {
      stop("Generators can only contain lowercase letters")
    }
    # Compute the corresponding number
    n <- sum(2**(g_ascii - 97))
    out <- append(out, n)
  }
  return(out)
}
