########################
#using R to create html
########################
#created by Evan
#created at 2015/04/08



create_bmap.points <- function(lon, lat){
  res <- paste0('new BMap.Point(', lon, ',', lat, ')', collapse = ',\n')
  res <- paste0('[', res, ']')
  res
}

create_bmap.poly <- function(lon, lat, flag, color){
  res <- create_bmap.points(lon, lat)
  color <- unique(color)
  res <- paste0('var line', flag, ' = new BMap.Polyline(', res, ', {strokeColor:"', color, '"});\n', 'map.addOverlay(line', flag, ');\n')
  res
}

create_individual_path <- function(id, loc){
  show.path.line <- loc[V1 == id][order(time), time_split := splitPath(time)]
  show.path.line <- show.path.line[, `:=`(lon = round(V3, 0.0001),
                                          lat = round(V4, 0.0001))
                                   ][-(which(diff(show.path.line$lon) == 0 & diff(show.path.line$lat) == 0) + 1)]
  
  path.nodes <- show.path.line[, .N, by = time_split][N > 10]
  show_num <- min(nrow(path.nodes), 5)
  
  res <- ''
  if(show_num > 0)
  {
    colors <- c('red', 'darkgreen', 'darkorange', 'darkblue', 'chartreuse')
    path.nodes <- cbind(head(path.nodes[order(-N)], show_num), colors[1:show_num])
    setnames(path.nodes, 'V2', 'color')
    res <- merge(show.path.line, path.nodes, by = 'time_split')[,
                                                                list(path = create_bmap.poly(lon, lat, time_split, color)),
                                                                by = time_split
                                                                ][, list(path = paste0(path, collapse = ''))]
    res <- res$path
  }
  res
}

create_pathPlot <- function(id, loc){
  path <- create_individual_path(id, loc)
  center <- loc[V1 == id, list(lon = mean(lon), lat = mean(lat))]
  res <- paste0('
<!DOCTYPE html>
<html>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <meta name="viewport" content="initial-scale=1.0, user-scalable=no" />
    <style type="text/css">
        body, html,#allmap {width: 100%;height: 100%;overflow: hidden;margin:0;font-family:"微软雅黑";}
    </style>
    <script type="text/javascript" src="http://api.map.baidu.com/api?v=2.0&ak=941e314f7aecfeea3c84b1f777208e7c"></script>
    <title>memberID_', id, 'pathPlot</title>
</head>
<body>
<div id="allmap"></div>
</body>
</html>
<script type="text/javascript">
    var map = new BMap.Map("allmap");
    var point = new BMap.Point(', center$lon, ',', center$lat, ');
    map.centerAndZoom(point, 15);
    map.enableScrollWheelZoom();\n
  ',path, '\n</script>')
  write(res, paste0('path_', id, '.html'))
}
