#' Catalog of designs from CSW (1993)
#'
#' Catalog of all the regular designs presented in the 2009 paper "Algorithmic
#' Construction of Efficient Fractional Factorial Designs With Large Run Sizes"
#' of Hongquan Xu.
#'
#' @docType data
#'
#' @usage data(xu)
#'
#' @format An object of the class \code{"data.frame"}
#' \describe{
#' \item{n.runs}{Run size of the design, from 128 to 4096}
#' \item{index}{Index of the design used to uniquely identify it}
#' \item{n.cols}{Total number of columns `n` of the design}
#' \item{n.added}{Number of added columns `p` in the design (with `p<n`)}
#' \item{design.rank}{Rank, in term of aberration, of the design among all
#' designs with similar run size and `n`}
#' \item{cols}{String representing the columns used to create the design}
#' \item{wlp}{String representing the word length pattern of the design}
#' }
#'
#' @references Hongquan Xu (2009),Technometrics, 51:3, 262-277,
#' \href{http://dx.doi.org/10.1198/tech.2009.07120}{doi}.
#'
#' @examples
#' data(xu)
#' runsizes <- unique(xu$n.runs)
#' print(runsizes)
"xu"
