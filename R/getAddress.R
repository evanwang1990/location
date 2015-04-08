getAddress <- function(lon, lat, points)
{
  url <- getURL(paste0('http://api.map.baidu.com/geocoder/v2/?ak=uBEkuo0ZltIW7GdCoaVb3Ufs&location=',lat,',',lon,'&output=json&pois=',points), .encoding = 'UTF-8')#必须设置编码，否则乱码
  res <- fromJSON(url)
  res
}