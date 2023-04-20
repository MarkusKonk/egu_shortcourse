plotStudyArea <- function(polygon, points){
  plot <- tm_shape(lake_spolydf) + tm_polygons() +
    tm_shape(points_spdf) +
    tm_dots(col="measurement", palette = rev(brewer.pal(7, "RdBu")),
            title="Sampled precipitation \n(in inches)", size=1) +
    tm_text("measurement", just="left", xmod=.5, size = 0.7) +
    tmap_options(check.and.fix = TRUE) +
    tm_legend(legend.outside=TRUE)
  return(plot)
}

plotInterpolationMap <- function(raster, points){
  plot <- tm_shape(raster) + 
    tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
              title="Predicted precipitation \n(in inches)") + 
    tm_shape(points) + tm_dots(size=0.2) +
    tm_legend(legend.outside=TRUE)
  return(plot)
}

