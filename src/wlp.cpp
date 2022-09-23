#include <Rcpp.h>
using namespace Rcpp;

//' @title Hamming distance between two vectors
//'
//' @description Computes the hamming distance between two vectors \code{a} and
//' \code{b}. The hamming distance is the number of places where they differ.
//' Therefore, for two vectors of size n, the distance is an integer between 0
//' and n.
//'
//' @param a first vector
//' @param b second vector
//' @return An integer
// [[Rcpp::export]]
int hamming_distance(NumericVector a, NumericVector b){
  // Check that a and b have same size
  if (a.size() != b.size()){
    stop("Both vectors must have the same size");
  }
  int dist =0;
  for (int i = 0; i < a.size(); i++){
    dist += (a[i] != b[i]);
  }
  return dist;
}


//' Distance distribution of a matrix
//'
//' @description Computes the distribution of the hamming distances between
//' all pairs of rows (even identical) in a matrix. For a matrix with N rows,
//' there are \eqn{\binom{N}{2}+N} such combinations.
//'
//' @details
//' For a matrix \eqn{\mathbf{X}} of size \eqn{(n \times m)}, the distance
//' distribution is a row-vector \eqn{B} of length `n`, where \eqn{B_i} is
//' the number of pairs of rows with hamming distance \eqn{i} divided by `n`.
//' Since pairs of identical rows are considered, \eqn{B_0} will always be `n`.
//'
//' @param x input matrix
//' @return A vector of the distance distribution
// [[Rcpp::export]]
NumericVector distance_distribution(NumericMatrix x) {
  int nrow = x.nrow();
  int ncol = x.ncol();
  NumericVector distVec(ncol+1);
  for (int i = 0; i < nrow; i++){
    for (int j = i; j < nrow; j++){
      double h = hamming_distance(x(i,_),x(j,_));
      distVec[h]++;
    }
  }
  return distVec/nrow;
}
