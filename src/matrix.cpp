#include <Rcpp.h>
using namespace Rcpp;

//' @title basic_factor_matrix
//'
//' @description Generate the matrix containing `k` basic factors.
//'
//' @param k Number of basic factors.
//' @return A \eqn{2^{k} \times k} matrix containing the basic factors.
//' @examples
//' \dontrun{
//' basic_factor_matrix(4)
//' }
// [[Rcpp::export]]
IntegerMatrix basic_factor_matrix(int k){
  int N = pow(2,k);
  IntegerMatrix mat = IntegerMatrix(N, k);
  IntegerVector base = {0,1};
  for(int i = 0; i<k; i++){
    int a = pow(2,(k-(i+1)));
    int b = pow(2,i);
    IntegerVector v = rep_each(base, a);
    IntegerVector column = rep(v, b);
    // Assign vector to the column
    mat(_, i) = column;
  }
  return mat;
}


//' Generate the model matrix for the design with `k` factors and the columns
//' specified in the vector ´cols´.
//'
//' @param k Number of basic factors.
//' @param cols Columns used in the design.
//' @return A numeric matrix corresponding to the model matrix of the design.
//' @importFrom Rcpp evalCpp
//' @examples
//' \dontrun{
//' design_model_matrix(4, c(1,2,3,8))
//' }
// [[Rcpp::export]]
IntegerMatrix design_model_matrix(int k, IntegerVector cols)
{
  // Check that k is a positive integer
  if (k <= 0) {
    stop("'k' must be a positive value.");
  }
  int nCols = cols.length();
  // Model matrix for the columns
  IntegerMatrix m = IntegerMatrix(k, nCols);
  for (int j = 0; j < nCols; j++)
  {
    int colNumber = cols(j);
    for (int i = 0; i < k; i++)
    {
      int iVal = pow(2,i);
      // If the shared bits are equal to the bits of the basic factor, then the
      // basic factor is part of the number
      if ((colNumber & iVal) == iVal)
      {
        m(i, j) = 1;
      }
    }
  }
  return m;
}
