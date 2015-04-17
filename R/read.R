readLoc <- function(filepath, header = T)
{
  require(lubridate, quietly = T)
  loc <- fread(filepath, header = header)
  if(header == T) setnames(loc, c('V1', 'V2', 'V3', 'V4'))
  
  loc <- loc[ ,
             `:=`(id = V1,
                  lon = round(V3, 0.002),
                  lat = round(V4, 0.002),
                  time = as.numeric(ymd_hms(V2)),
                  worktime = ymd_hms(V2),
                  date = substr(V2, 1, 10))
             ][ ,
               worktime := as.integer((wday(worktime) %in% 2:6) & (hour(worktime) %between% c(9,18)))
               ][order(id, time),
                 c('id', 'V3', 'V4', 'lon', 'lat', 'date', 'time', 'worktime'),
                 with = F]
  loc
}

readShop <- function(filepath, header = T)
{
  shops <- fread(filepath, header = header, select = c(4, 8:10))
  setnames(shops, c('shop_name', 'area', 'lat', 'lon'))
  shops
}