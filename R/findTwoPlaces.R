#'@name findTwoPlaces
#'@title find two places where the user appeared most frequently
#'@param data the dataset processed by function `handleLoc`
#'@param dis_thres minimum distance between the two places where the user appeared most frequently
#'@export
findTwoPlaces <- function(data, dis_thres)
{
  vars <- setdiff(c('dur', 'id', 'lon', 'lat', 'wdur'), names(data))
  if(length(vars) > 0) stop(paste(vars, collapse = ' '), ' are not in ', substitute(loc), '!\n')
  data <- data[dur > 0, 
               list(dur = sum(dur),
                    wdur = sum(wdur)),
               by = list(id, lon, lat)]
  temp <- with(data, find_two_places(id, lon, lat, dis_thres))
  res <- data[temp]
  res
}

#'@name detectOffice
#'@title detect office address and home address
#'@description distinguish between two places, detect which is office and which is home
#'@param data the dataset produced by `findTwoPlaces` function
#'@export
detectOffice <- function(data)
{
  vars <- setdiff(c('wdur', 'dur', 'lon', 'lat'), names(data))
  if(length(vars) > 0) stop(paste(vars, collapse = ' '), ' are not in ', substitute(data), '!\n')
  res <- data[, `:=`(wrate = wdur / dur)
              ][, `:=`(max_wrate = max(wrate),
                       min_wrate = min(wrate),
                       max_dur = max(dur),
                       max_wdur = max(wdur)),
                by = id
                ][, place := ifelse(max_wrate == 0, ifelse(dur == max_dur, 'home', NA),
                                    ifelse(min_wrate == 1, ifelse(wdur == max_wdur, 'office', NA),
                                           ifelse(wrate == max_wrate, 'office', 'home')))
                  ][, list(id, lon, lat, dur, wdur, place)]
  res
}