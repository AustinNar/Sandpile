// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
using namespace Rcpp;
using namespace RcppParallel;

struct Toppler : public Worker
{
  // source matrix
  const RMatrix<double> input;
  
  // destination matrix
  RMatrix<double> output;
  
  // initialize with source and destination
  Toppler(const NumericMatrix input, NumericMatrix output) 
    : input(input), output(output) {};
  
  // calculate value of cell after toppling
  void operator()(std::size_t begin, std::size_t end) {
    for(int k = begin; k < end; k++){
      int i = k % input.nrow();
      int j = k / input.nrow();
      int n = 0;
      int s = 0;
      int e = 0;
      int w = 0;
      int topple = 0;
      
      if(i > 0){n = input(i-1,j) > 3;}
      if(i < input.nrow() - 1){s= input(i+1,j) > 3;}
      if(j > 0){w = input(i,j-1) > 3;}
      if(j < input.ncol() - 1){e = input(i,j+1) > 3;}
      if(input(i,j) > 3){topple = -4;}
      
      output[k] = input[k] + n + s + e + w + topple;
    }
  }
};



// [[Rcpp::export]]
NumericMatrix parallelTopple(NumericMatrix x) {
  
  // allocate the output matrix
  NumericMatrix output(x.nrow(), x.ncol());
  
  // Toppler functor (pass input and output matrixes)
  Toppler toppler(x, output);
  
  // call parallelFor to do the work
  parallelFor(0, x.length(), toppler);
  
  // return the output matrix
  return output;
}


// [[Rcpp::export]]
NumericMatrix topple(NumericMatrix x) {
  double max = 0;
  NumericMatrix a(x.nrow(), x.ncol());
  do{
    max = 0;
    for(int i = 0; i < x.nrow(); i++){
      for(int j = 0; j < x.ncol(); j++){
        int north = 0;
        int south = 0;
        int east = 0;
        int west = 0;
        int topple = 0;
        if(i > 0){north = x(i-1,j) > 3;}
        if(i < x.nrow() - 1){south = x(i+1,j) > 3;}
        if(j > 0){west = x(i,j-1) > 3;}
        if(j < x.ncol() - 1){east = x(i,j+1) > 3;}
        if(x(i,j) > 3){topple = -4;}
        
        a(i,j) = x(i,j) + north + south + east + west + topple;
        if(a(i,j) > max){max = a(i,j);}
      }
    }
    x = clone(a);
  }while(max > 3);
  return a;
}