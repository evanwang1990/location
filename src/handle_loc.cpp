#include <Rcpp.h>
using namespace Rcpp;
//the location information is uploaded every 5 sec, 
//so it is too much if the user did't move at all
//the function is to reduce the data and calculate
//how long the user stayed at one place in working time and non-working time

// [[Rcpp::export]]
DataFrame handle_loc(IntegerVector id, NumericVector lon, NumericVector lat, IntegerVector time, IntegerVector worktime)
{
  
  int len = lon.size();
  
  NumericVector dur(len);
  NumericVector wdur(len);
  NumericVector idx(len);
  
  int start = 0;
  int wstart = worktime[0] == 1 ? 0:-1;
  int wend = worktime[0] == 1 ? 0:-1;
  int j = 0;
  
  for(int i = 1; i < len; i ++)
  {
    if((lon[i] != lon[i-1]) || (lat[i] != lat[i-1]) || ((time[i]-time[i-1]) > 1800) || (id[i] != id[i-1]))
    {
      dur[j] = time[i-1] - time[start];
      wdur[j] = wstart >=0 ? (time[wend] - time[wstart]):0;
      idx[j] = i;
      //重新初始化各种指针
      j ++;
      start = i;
      wstart = worktime[i] == 1 ? i:-1;
      wend = worktime[i] == 1 ? i:-1;
    }
    else
    {
      if(worktime[i] == 1)
      {
        if(wstart<0) wstart = i;
        wend = i;
      }
    }
  }
  
  return DataFrame::create(_["idx"] = idx[idx > 0], _["dur"] = dur[idx > 0], _["wdur"] = wdur[idx > 0]);
}