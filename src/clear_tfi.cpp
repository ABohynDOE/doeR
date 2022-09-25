#include <RcppArmadillo.h>
#include "wlp.h"

//' @title Number of clear 2FI
//'
//' @description Compute the number of clear two-factor interactions (2FI) of a
//' design. A 2FI that is not aliased with any main effect or other 2FI's is
//' called clear.
//'
//' @param X A numeric matrix representing the design matrix.
//' @return An integer representing the number of clear 2FI in the design.
// [[Rcpp::export]]
double clear_2fi(arma::mat X){
  arma::mat Xcoded = X*2-1;
  arma::uword n = X.n_cols;
  int N = X.n_rows;
  double dim = nchoosek(n, 2);
  arma::mat T(N, dim);
  arma::uword index = 0;
  for(arma::uword i=0; i<n; i++){
    for(arma::uword j=(i+1); j<n; j++){
      T.col(index) = Xcoded.col(i) % Xcoded.col(j);
      index ++;
    }
  }
  arma::mat XT = arma::join_rows(Xcoded, T);
  arma::mat A = abs(XT.t() * T)/N;
  arma::rowvec col_sum = sum(A, 0) -1 ;
  arma::uvec clear_2fi_idx = arma::find(col_sum == 0);
  return clear_2fi_idx.size();
}
