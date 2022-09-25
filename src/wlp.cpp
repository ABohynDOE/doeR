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


//' @title Distance distribution of a matrix
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
    for (int j = 0; j < i; j++){
      int h = hamming_distance(x(i,_),x(j,_));
      distVec[h] += 2;
    }
  }
  // Along diagonal
  distVec[0] += nrow;
  // Normalize
  for (int i = 0; i <= ncol; i++) {
    distVec[i] /= nrow;
  }
  return distVec;
}

//' @title Factorial of a number
//'
//' @description Compute the factorial of a number.
//' @param x A positive integer
//' @return A double
// [[Rcpp::export]]
double factorial(double x)
{
  double sol = R::gammafn(x + 1.0);
  return sol;
}


//' @title Binomial coefficient
//'
//' @description Compute the number of ways of choosing `k` elements out of a
//' set of size `n`. Chosen behavior for negative `k` is to return 0. Chosen
//' behavior for `k` equals to zero is to return 1.
//' @param n An integer, the size of the set of all elements.
//' @param k An integer, the number of elements to choose.
//' @return An integer
//[[Rcpp::export]]
int nchoosek(int n, int k)
{
  if ((k < 0) || (n < k))
  {
    return 0;
  }
  if (k == 0 || n == k)
  {
    return 1;
  }
  int sol = factorial(n) / (factorial(k) * factorial(n - k));
  return sol;
}

//' @title Krawtchouk polynomials
//'
//' @description Compute the Krawtchouk polynomial P_j(x,n,s) for s=2, since it
//' is for two-level designs.
//' @param j An integer
//' @param x An integer
//' @param n An integer
//' @return An integer giving the value of the polynomial evaluated at j, x and
//' n.
//[[Rcpp::export]]
int krawtchouk(int j, int x, int n)
{
  // P_0(x,n) is always equal to 1
  if (j == 0)
  {
    return 1;
  }
  int sol = 0;
  for (int i = 0; i <= j; i++)
  {
    sol += pow(-1, i) * nchoosek(x, i) * nchoosek(n - x, j - i);
  }
  return sol;
}

//' @title MacWilliams Transform
//'
//' @description Compute the MacWiliams transform of the distance distribution
//' of a design.
//' @param B A numeric vector containing the distance distribution of a design.
//' @param N An integer representing the run size of the design.
//' @return A numeric vector of the same size as B.
//[[Rcpp::export]]
NumericVector mac_williams_transform(NumericVector B, int N){
  int n = B.length()-1;
  NumericVector B_prime(n+1);
  for(int j=0; j<=n; j++){
    int val = 0;
    for(int i=0; i<=n; i++){
      val += B[i]*krawtchouk(j,i,n);
    }
    B_prime[j] = val/N;
  }
  return B_prime;
}

//' @title Word length pattern
//'
//' @description Compute the Word Length Pattern (WLP) of a design. For this
//' vector B, B_i represents the number of words of length i.
//'
//' @param D A numeric matrix corresponding to the design matrix.
//' @return A numeric vector with size equal to the number of columns of the
//' original design D, containing the word length pattern.
//[[Rcpp::export]]
NumericVector wlp(NumericMatrix D){
  int N = D.nrow();
  NumericVector d = distance_distribution(D);
  return mac_williams_transform(d, N);
}
