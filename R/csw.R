#' Catalog of designs from CSW (1993)
#'
#' Catalog of all the regular designs presented in the 1993 paper "A Catalogue
#' of Two-Level and Three-Level Fractional Factorial Designs with Small Runs"
#' of Chen, Sun and Wu.
#'
#' @docType data
#'
#' @usage data(csw)
#'
#' @format An object of the class \code{"data.frame"}
#' \describe{
#' \item{n.runs}{Run size of the design, can be either 16, 32 or 64}
#' \item{index}{Index of the design used to uniquely identify it}
#' \item{n.cols}{Total number of columns `n` of the design}
#' \item{n.added}{Number of added columns `p` in the design (with `p<n`)}
#' \item{design.rank}{Rank, in term of aberration, of the design among all
#' designs with similar run size and `n`}
#' \item{cols}{String representing the columns used to create the design}
#' \item{wlp}{String representing the word length pattern of the design,
#' starting with words of length 3 or length 4 for run sizes of 64}
#' \item{clear.2fi}{Number of clear two-factor interactions (2FI) of the design}
#' }
#'
#' @references Chen, J., Sun, D. X., & Wu, C. F. J. (1993).
#' (\href{https://doi.org/10.2307/1403599}{JSTOR})
#'
#' @examples
#' data(csw)
#' head(csw)
"csw"
