#' @title design
#'
#' @description `design` generates the design matrix of a design from the
#' catalog of Chen, Sun and Wu (1993) or Xu (2009) using the run size and the
#' index of the design as identifiers.
#'
#'     The index of a design can be found in the design tables of the
#' corresponding paper.
#'
#' @param run_size An integer specifying the run size of the design. It must be
#' a power of 2.
#' @param index A string representing the index of the design, from the paper.
#' All strings are of the from "n-p.i" where `n` is the number of factors in the
#' design, `p` is the number of added factors in the design, and `i` is the rank
#' of the design in the table.
#' @return A numeric matrix corresponding to the design matrix.
#' @examples
#' design(32, "8-3.1")
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
  # Extract columns and map to vector
  cols_str <- data[correct_row, ]$cols
  cols <- as.numeric(unlist(strsplit(cols_str, ",")))
  # Case with no columns in Xu 2009
  if (length(cols) == 0) {
    stop("Sorry, no columns given for that design. Impossible to generate !")
  }
  # Generate the columns of the basic factors
  k <- as.integer(log2(run_size))
  base_cols <- 2**(0:(k - 1))
  # Build the matrix
  full_cols <- c(base_cols, cols)
  mat <- design_model_matrix(k, full_cols)
  bf_mat <- basic_factor_matrix(k)
  design_mat <- (bf_mat %*% mat) %% 2
  return(design_mat)
}

#' @title Generate a custom design
#'
#' @description Given a run size and a set of column numbers, generate the
#' corresponding design matrix.
#'
#' @param run_size Integer representing the number of runs in the design.
#' @param added_cols Numeric vector holding the numbers of the columns of the
#' design. It should only contain the added columns, as all the basic factors
#' are added to the set of columns before generating the design.
#' @param add_bf Logical. Add the numbers corresponding to the basic factors to
#' the column numbers already provided. Default is TRUE.
#'
#' @return A numeric matrix representing the design matrix.
#' @examples
#' custom_design(32, c(7,15,31))
#' custom_design(16, c(1,2,8,11), add_bf = FALSE)
#' @export
custom_design <- function(run_size, added_cols, add_bf = TRUE) {
  # Check run size is a power of 2
  if (2**round(log2(run_size)) != 2**log2(run_size)) {
    stop(
      sprintf("Runsize %i must be a power of 2.", run_size)
    )
  }
  if (any(added_cols >= run_size)) {
    stop('Column numbers cannot be larger than the runsize')
  }
  # Generate the columns of the basic factors
  k <- as.integer(log2(run_size))
  # Check that there are no duplicate columns
  if (add_bf) {
    # Generate basic factors
    base_cols <- 2**(0:(k - 1))
    # Join the columns together
    full_cols <- c(base_cols, added_cols)
    new_cols <- unique(full_cols)
    if (length(new_cols) != length(full_cols)) {
      warning(
        sprintf(
          "Duplicates found in the column numbers. Columns used: (%s)",
          paste(new_cols, collapse = ", ")
        )
      )
    }
    full_cols <- new_cols
  } else{
    full_cols <- added_cols
  }
  mat <- design_model_matrix(k, full_cols)
  bf_mat <- basic_factor_matrix(k)
  design_mat <- (bf_mat %*% mat) %% 2
  return(design_mat)
}
