utils::globalVariables(c("csw", "xu"))
#' @title design
#'
#' @description `design` generates the design matrix of a design from the
#' catalog of Chen, Sun and Wu (1993) or Xu (2009) using the run size and the
#' index of the design as identifiers.
#'
#'     The index of a design can be found in the design tables of the
#'corresponding paper.
#'
#' @param run_size An integer specifying the run size of the design. It must be
#' a power of 2.
#' @param index A string representing the index of the design, from the paper.
#' All strings are of the from "n-p.i" where `n` is the number of factors in the
#' design, `p` is the number of added factors in the design, and `i` is the rank
#' of the design in the table.
#' @return A numeric matrix corresponding to the design matrix.
#' @export
design <- function(run_size, index) {
  # Check run size (16 to 4096)
  if (match(log2(run_size), c(4:12), 0) == 0) {
    stop(
      sprintf(
        "Runsize must be a power of 2 between 16 and 4096.",
        run_size
      )
    )
  }
  # Define data set based on the run size
  if (run_size < 128) {
    data <- csw
  } else {
    data <- xu
  }
  correct_row <- (data["n.runs"] == run_size & data["index"] == index)
  # Error if index doesn't match the run size
  if (!any(correct_row)) {
    stop(
      sprintf(
        "Design with runsize %i and index \"%s\" does not exist.",
        run_size,
        index
      )
    )
  }
  # TODO: cover case with no columns in Xu 2009
  # Extract columns and map to vector
  cols_str <- data[correct_row, ]$cols
  cols <- as.numeric(unlist(strsplit(cols_str, ",")))
  # Generate the columns of the basic factors
  k <- as.integer(log2(run_size))
  base_cols <- 2**(0:k-1)
  # Build the matrix
  full_cols <- c(base_cols, cols)
  mat <- design_model_matrix(k, full_cols)
  bf_mat <- basic_factor_matrix(k)
  design_mat <- (bf_mat %*% mat) %% 2
  return(design_mat)
}
