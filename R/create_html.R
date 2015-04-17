########################
#using R to create html
########################
#created by Evan
#created at 2015/04/08


create_bmap.marker <- function(lon, lat, label){
  res <- paste0('var point = new BMap.Point(', lon, ',', lat, ');
                var marker = new BMap.Marker(point);
                map.addOverlay(marker);',
                ifelse(label == '', '', paste0('var label = new BMap.Label(', label, ',{offset:new BMap.Size(20,-10)});
                marker.setLabel(label);')))
  res
}

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

create_bmap.heatmap <- function(loc){
  temp <- loc[, list(count = .N), by = list(lon, lat)]
  max_count <- min(max(temp$count), (4 * quantile(temp$count, 3/4) - 3 * quantile(temp$count, 1/4)))
  res <- c(with(temp, paste0(paste0('{"lng":', lon, ',"lat":', lat, ',"count":', count, '}'), collapse = ',\n')),
           max_count) 
  res
}

create_individual_path <- function(id1, loc){
  show.path.line <- loc[id == id1][order(time)][, time_split := splitPath(time)]
  show.path.line <- show.path.line[, `:=`(lon = round(V3, 0.0001),
                                          lat = round(V4, 0.0001))
                                   ][-(which(diff(show.path.line$lon) == 0 & diff(show.path.line$lat) == 0) + 1)]
  
  path.nodes <- show.path.line[, .N, by = time_split][N > 5]
  show_num <- min(nrow(path.nodes), 5)
  
  res <- ''
  if(show_num > 0)
  {
    colors <- c('red', 'darkgreen', 'darkorange', 'darkblue', 'chartreuse')
    path.nodes <- cbind(head(path.nodes[order(-N)], show_num), color = colors[1:show_num])
    res <- merge(show.path.line, path.nodes, by = 'time_split')[,
                                                                list(path = create_bmap.poly(lon, lat, time_split, color)),
                                                                by = time_split
                                                                ][, list(path = paste0(path, collapse = ''))]
    res <- res$path
  }
  res
}

create_pathPlot <- function(id1, loc){
  vars <- setdiff(c('id', 'time', 'lon', 'lat'), names(loc))
  if(length(vars) > 0) stop(paste(vars, collapse = ' '), ' are not in ', substitute(loc), '!\n')
  if(!id1 %in% loc$id) stop(id1, ' can not be found in ', substitute(loc), ' !\n')
  path <- create_individual_path(id1, loc)
  center <- loc[id == id1, list(lon = mean(lon), lat = mean(lat))]
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
    <title>memberID_', id1, 'pathPlot</title>
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
  write(res, paste0('path_', id1, '.html'))
}

create_pathPlots <- function(ids, loc, address, suffix = ''){
  vars <- setdiff(c('id', 'time', 'lon', 'lat'), names(loc))
  if(length(vars) > 0) stop(paste(vars, collapse = ' '), ' are not in ', substitute(loc), '!\n')
  vars <- setdiff(c('id', 'place'), names(address))
  if(length(vars) > 0) stop(paste(vars, collapse = ' '), ' are not in ', substitute(address), '!\n')
  
  if(any(!ids %in% loc$id)) stop(id1, ' can not be found in ', substitute(loc), ' !\n')
  
  center <- loc[id %in% ids, list(lon = mean(lon), lat = mean(lat))]
  paths <- sapply(ids, function(id_){
    res <- create_individual_path(id_, loc)
    if(nchar(res) > 1){
      work <- address[id == id_ & place == 'office']
      home <- address[id == id_ & place == 'home']
      res <- paste0(res,
                    ifelse(nrow(work) > 0, create_bmap.marker(work$lon, work$lat, '"office"'), ''),
                    ifelse(nrow(home) > 0, create_bmap.marker(home$lon, home$lat, '"home"'), ''))
    }
    res
    })
  idx <- which(nchar(paths) > 1)#delete the user who has no paths to plot
  paths <- paths[idx]
  ids <- ids[idx]
  if(length(ids) > 100) warning("There are too many path lines in the html!\n")
  if(length(ids) > 1000) stop("The number of users to be displayed are more than 1000, please reduce some users!")
  paths <- paste0('case "', 1:length(ids), '":\n',
                  'map.clearOverlays();\n',
                  paths,
                  'document.getElementById("demo").innerHTML="', ids, '";\nbreak;\n',
                  collapse = '\n')
  res <- paste0('
<!DOCTYPE html>
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
  <meta name="viewport" content="initial-scale=1.0, user-scalable=no" />
  <style type="text/css">
    ul,li{list-style: none;margin:0;padding:0;float:left;font-family:"微软雅黑";}
    html{height:100%}
    body{height:100%;margin:0px;padding:0px;}
    #allmap{height:93%;width:100%;}
    #r-result{width: 100%;}
  </style>
  <script type="text/javascript" src="http://api.map.baidu.com/api?v=2.0&ak=941e314f7aecfeea3c84b1f777208e7c"></script>
  <title>PathPlots</title>
</head>
<body>
<div id="allmap"></div>
<div id="r-result">
  <form name="form" method="post" action="#">','
    select path(1~', length(ids), '):
    <input type="text" name="member_id" id="member_id" />
    <input type="button" value="Submit" onclick="fun();" />
    member id:
    <textarea id="demo" rows="1" cols="15"></textarea>
  </form>
  <p>caution: Only the paths which contains more than 5 points can be displayed and at most 5 paths can be ploted for each user</p>
</div>
</body>
</html>
<script type="text/javascript">
  var map = new BMap.Map("allmap");
  var point = new BMap.Point(', center$lon, ',', center$lat, ');',
  'map.centerAndZoom(point, 14);
  map.enableScrollWheelZoom();\n',
'function fun(){
  var id = document.getElementById("member_id").value;
  switch (id)
  {', paths,
'\n
}
}
</script>')
write(res, paste0('pathPlots_', suffix, '.html'))
}

create_heatMap <- function(loc, shop){
  vars <- setdiff(c('place', 'lon', 'lat'), names(loc))
  if(length(vars) > 0) stop(paste(vars, collapse = ' '), ' are not in ', substitute(loc), '!\n')
  vars <- setdiff(c('shop_name', 'area', 'lon', 'lat'), names(shop))
  if(length(vars) > 0) stop(paste(vars, collapse = ' '), ' are not in ', substitute(shop), '!\n')
  
  home_points <- create_bmap.heatmap(loc[place == 'home'])
  office_points <- create_bmap.heatmap(loc[place == 'office'])
  shop_marker <- create_bmap.marker(shop$lon, shop$lat, label = '')
  center <- loc[, list(lon = mean(lon), lat = mean(lat))]
  res <- paste0('
<!DOCTYPE html>
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
  <meta name="viewport" content="initial-scale=1.0, user-scalable=no" />
  <script type="text/javascript" src="http://api.map.baidu.com/api?v=2.0&ak=941e314f7aecfeea3c84b1f777208e7c"></script>
  <script type="text/javascript" src="http://api.map.baidu.com/library/Heatmap/2.0/src/Heatmap_min.js"></script>
  <title>location</title>
  <style type="text/css">
    ul,li{list-style: none;margin:0;padding:0;float:left;}
    html{height:100%}
    body{height:100%;margin:0px;padding:0px;font-family:"微软雅黑";}
    #container{height:95%;width:100%;}
    #r-result{width: 100%;}
  </style>
</head>
<body>
  <div id="container"></div>
  <div id="r-result">
    <input type="button" onclick="openHeatmap_home();" value="HOME ON"/>
    <input type="button" onclick="closeHeatmap_home();" value="HOME OFF"/>
    <input type="button" onclick="openHeatmap_work();" value="OFFICE ON"/>
    <input type="button" onclick="closeHeatmap_work();" value="OFFICE OFF"/>
    <input type="button" onclick="addMarker();" value="SHOW SHOP ADDRESS"/>
    <input type="button" onclick="removeMarker();" value="CLOSE SHOP ADDRESS"/>
  </div>
</body>
</html>
<script type="text/javascript">
  var map = new BMap.Map("container");
  var point = new BMap.Point(', center$lon, ', ', center$lat, ');
  map.centerAndZoom(point, 13);
  map.enableScrollWheelZoom();
var home_points = [', home_points[1], '];
var office_points = [', office_points[1], '];
if(!isSupportCanvas()){
      alert("Your Browser does not support heatMap!")
  }

heatmapOverlay_home = new BMapLib.HeatmapOverlay({"radius":20,
    gradient:{.2:"rgb(0,255,255)",.5:"rgb(0,110,255)",.8:"rgb(100,0,255)"}});
map.addOverlay(heatmapOverlay_home);
heatmapOverlay_home.setDataSet({data:home_points,max:',home_points[2], '});

heatmapOverlay_office = new BMapLib.HeatmapOverlay({"radius":20,
    gradient:{.2:"rgb(244,164,96)",.5:"rgb(255,69,0)",.8:"rgb(255,0,0)"}});
map.addOverlay(heatmapOverlay_office);
heatmapOverlay_office.setDataSet({data:office_points,max:',office_points[2], '});

function openHeatmap_home(){
    heatmapOverlay_home.show();
}
function closeHeatmap_home(){
    heatmapOverlay_home.hide();
}
function openHeatmap_work(){
    heatmapOverlay_office.show();
}
function closeHeatmap_work(){
    heatmapOverlay_office.hide();
}
closeHeatmap_home();
closeHeatmap_work();

function addMarker()
  {\n', paste0(shop_marker, collapse = '\n'), '\n}
function removeMarker()
{
  map.clearOverlays();
}

function isSupportCanvas(){
      var elem = document.createElement("canvas");
      return !!(elem.getContext && elem.getContext("2d"));
  }
</script>')
  write(res, 'heatMap.html')
}
