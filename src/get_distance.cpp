#include <Rcpp.h>
using namespace Rcpp;

float getDistance(float lon1, float lat1, float lon2, float lat2)
{
  float pi = 3.141593;
  lon1 = lon1 * pi / 180;
  lat1 = lat1 * pi / 180;
  lon2 = lon2 * pi / 180;
  lat2 = lat2 * pi / 180;
  float a = lat1 - lat2;
  float b = lon1 - lon2;
  return 2 * asin(sqrt(sin(a / 2) * sin(a / 2) + cos(lat1) * cos(lat2) * sin(b / 2) * sin(b / 2))) * 6378.137;
}

//[[Rcpp::export]]
DataFrame findTwoPlaces(DataFrame loc, float dis_thres)
{
  IntegerVector id = loc["id"];
  NumericVector lon = loc["lon"];
  NumericVector lat = loc["lat"];
  NumericVector dur = loc["dur"];
  NumericVector wdur = loc["wdur"];
  
  IntegerVector id1;
  NumericVector lon1;
  NumericVector lat1;
  NumericVector dur1;
  NumericVector wdur1;
  
  int ptr = 0;
  float distance;
  int count = 0;
  id1.push_back(id[ptr]);
  lon1.push_back(lon[ptr]);
  lat1.push_back(lat[ptr]);
  dur1.push_back(dur[ptr]);
  wdur1.push_back(wdur[ptr]);
  for(int i = 1; i < id.size(); i ++)
  {
    if(id[i-1] != id[i])
    {
      ptr = i;
      count = 0;
      id1.push_back(id[ptr]);
      lon1.push_back(lon[ptr]);
      lat1.push_back(lat[ptr]);
      dur1.push_back(dur[ptr]);
      wdur1.push_back(wdur[ptr]); 
    }
    else
    {
      distance = getDistance(lon[ptr], lat[ptr], lon[i], lat[i]);
      if(distance >= dis_thres)
      {
        count ++;
        if(count < 2){
          id1.push_back(id[i]);
          lon1.push_back(lon[i]);
          lat1.push_back(lat[i]);
          dur1.push_back(dur[i]);
          wdur1.push_back(wdur[i]);
        }
      }
    }
  }
  DataFrame res = DataFrame::create(_["id"] = id1, _["lon"] = lon1, _["lat"] = lat1, _["dur"] = dur1, _["wdur"] = wdur1);
  return res;
}
