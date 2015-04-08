#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
DataFrame handle_loc(DataFrame data)
{
  IntegerVector id = data["V1"];
  CharacterVector date = data["V2"];
  NumericVector time = data["time"];
  NumericVector lon = data["lon"];
  NumericVector lat = data["lat"];
  IntegerVector worktime = data["worktime"];
  
  int len = data.size();
  
  IntegerVector id1(len);
  CharacterVector date1(len);
  NumericVector time1(len);
  NumericVector lon1(len);
  NumericVector lat1(len);
  NumericVector dur(len);
  NumericVector wdur(len);
  
  int start = 0;
  int wstart = worktime[0] == 1 ? 0:-1;
  int wend = worktime[0] == 1 ? 0:-1;
  float during;
  float wduring;
  int j = 0;
  for(int i = 1; i < id.size(); i ++)
  {
    if(!((id[i] == id[i-1]) && (date[i] == date[i-1]) && (lon[i] == lon[i-1]) && (lat[i] == lat[i-1])) || ((time[i]-time[i-1]) > 1800))
    {
      id1[j] = id[i-1];
      date1[j] = date[i-1];
      lon1[j] = lon[i-1];
      lat1[j] = lat[i-1];
      during = time[i-1] - time[start];
      dur[j] = during;
      wduring =wstart >=0 ? (time[wend] - time[wstart]):0;
      wdur[j] = wduring;
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
  
  return DataFrame::create(_["id"] = id1[id1 > 0], _["date"] = date1[id1 > 0], _["lon"] = lon1[id1 > 0], _["lat"] = lat1[id1 > 0], _["dur"] = dur[id1 > 0], _["wdur"] = wdur[id1 > 0]);
}