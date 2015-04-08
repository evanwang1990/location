convAdress <- function()
{
  url <- getURL(paste0('http://api.map.baidu.com/geoconv/v1/?coords=', coords, '&from=3&to=5&ak=uBEkuo0ZltIW7GdCoaVb3Ufs'))
  
}