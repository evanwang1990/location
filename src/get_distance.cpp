#include <Rcpp.h>
using namespace Rcpp;
//get two places where the user most often appeared

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
LogicalVector find_two_places(IntegerVector id, NumericVector lon, NumericVector lat, float dis_thres)
{
  int len = id.size();
  LogicalVector idx(len);
  idx[0] = true;
  
  int ptr = 0;
  float distance;
  int count = 0;
  for(int i = 1; i < id.size(); i ++)
  {
    if(id[i-1] != id[i])
    {
      ptr = i;
      count = 0;
      idx[i] = true;
    }
    else
    {
      distance = getDistance(lon[ptr], lat[ptr], lon[i], lat[i]);
      if(distance >= dis_thres)
      {
        count ++;
        if(count < 2) idx[i] = true;
      }
    }
  }
  return idx;
}
