createSpatialPointsDataFrame <- function(lon, lat, vals, crs) {
  sp_df = sp::SpatialPointsDataFrame(coords = cbind(lon, lat), 
                                         data = vals,
                                         proj4string = crs)
  return(sp_df)
}

createSpatialPolygonsDataFrame <- function(geojson, crs) {
  coords = geojson$features[[1]]$geometry$coordinates[[1]]
  lat = c()
  lon = c()
  for (coord in coords) {
    lat = append(lat, coord[[1]])
    lon = append(lon, coord[[2]])
  }
  
  lat = append(lat, geojson$features[[1]]$geometry$coordinates[[1]][[1]][[1]])
  lon = append(lon, geojson$features[[1]]$geometry$coordinates[[1]][[1]][[2]])
  polygon <- sp::Polygon(cbind(lat,lon))
  polygons <- sp::Polygons(list(polygon), ID = "A")
  spatial_polygon <- sp::SpatialPolygons(list(polygons), proj4string = crs)
  centroids = coordinates(spatial_polygon)
  x = centroids[,1]
  y = centroids[,2]
  spdf = sp::SpatialPolygonsDataFrame(spatial_polygon, 
                                      data = data.frame(x=x, y=y, row.names=row.names(spatial_polygon)))
  return(spdf)
}

runInterpolation <- function(points, values, interPolationPower){
  grd              <- as.data.frame(spsample(points, "regular", n=50000))
  names(grd)       <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd)     <- TRUE
  fullgrid(grd)    <- TRUE
  
  proj4string(points) <- proj4string(points)
  proj4string(grd) <- proj4string(points)
  return(gstat::idw(values ~ 1, points, newdata=grd, idp=interPolationPower))
}

