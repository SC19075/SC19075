#include <Rcpp.h>
using namespace Rcpp;

//' @title Random walk Metropolis using Rcpp
//' @description Implement the random walk version of the Metropolis using Rcpp
//' @param sigma the variance
//' @param x0 the inial location
//' @param m the total steps
//' @return a list conclude x and k \code{n}
//' @export
// [[Rcpp::export]]
List rw_MetropolisL(double sigma, int x0, int m){
  NumericVector x(m);
  NumericVector u(m);
  as<DoubleVector>(x)[0] = x0;
  u = as<DoubleVector>(runif(m));
  int k = 0;
  int i;
  for(i=1;i<m;++i){
    double y = as<double>(rnorm(1,x[i-1],sigma));
    if(u[i] <= exp(-fabs(y)+abs(x[i-1]))){
      x[i] = y;
    }
    else{
      x[i] = x[i-1];
      k = k+1;
    }
  }
  List L=List::create(Named("x")=x, Named("k")=k);
  return L;       
}
