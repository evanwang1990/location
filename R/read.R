read.loc <- function(filepath)
{
  loc <- fread(filepath, header = F)
  
  loc <<- loc[ ,
             `:=`(id = V1,
                  lon = round(V3, 0.002),
                  lat = round(V4, 0.002),
                  time = as.numeric(ymd_hms(V2)),
                  worktime = ymd_hms(V2),
                  date = substr(V2, 1, 10))
             ][ ,
               worktime := as.integer((wday(worktime) %in% 2:6) & (hour(worktime) %between% c(9,18)))
               ][order(id, time),
                 c('id', 'lon', 'lat', 'date', 'time', 'worktime'),
                 with = F]
}