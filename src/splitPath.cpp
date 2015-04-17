#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
IntegerVector splitPath(IntegerVector time)
{
  int len = time.size();
  IntegerVector res(len);
  int flag = 1;
  res[0] = flag;
  if(len > 1)
  {
    for(int i=1; i<len; i++)
    {
      res[i] = time[i] - time[i-1] < 600 ? flag:(++flag);
    }    
  }
  return res;
}