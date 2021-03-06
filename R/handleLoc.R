#'@name handleLoc
#'@title handle the location information
#'@description reduce the redundant location information and calculate the time a user stayed in one place
#'@param data the dataset read by read.loc
#'@export
handleLoc <- function(data)
{
  vars <- setdiff(c('id', 'lon', 'lat', 'time', 'worktime'), names(data))
  if(length(vars) > 0) stop(paste(vars, collapse = ' '), ' are not in ', substitute(data), '!\n')
  temp <- with(data, handle_loc(id, lon, lat, time, worktime))
  res <- cbind(data[temp$idx, list(id, V3, V4, lon, lat, time, date)], temp[c('dur', 'wdur')])
  res
}

