#'@name getAddress
#'@title get address information via longitude and latitude
#'@param lon longitude
#'@param lat latitude
#'@details the output is a list which contains four elements:detailed address, province, district, street
#'@export
getAddress <- function(lon, lat)
{
  url <- getURL(paste0('http://api.map.baidu.com/geocoder/v2/?ak=uBEkuo0ZltIW7GdCoaVb3Ufs&location=',lat,',',lon,'&output=json&pois=0'), .encoding = 'UTF-8')
  res <- fromJSON(url)
  res <- list(detail_address = res$result$formatted_address, 
              province = res$result$addressComponent$province, 
              district = res$result$addressComponent$district,
              street = res$result$addressComponent$street)
  res
}