#ifndef WLP_H
#define WLP_H

#include <Rcpp.h>
using namespace Rcpp;

int hamming_distance(NumericVector a, NumericVector b);
NumericVector distance_distribution(NumericMatrix x);
double factorial(double x);
int nchoosek(int n, int k);
int krawtchouk(int j, int x, int n);
NumericVector mac_williams_transform(NumericVector B, int N);
NumericVector wlp(NumericMatrix D);

#endif
