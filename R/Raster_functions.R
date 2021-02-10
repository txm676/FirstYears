
##takes the dataframe (e.g. preds), then extracts the coords columns
#and for each bio column in turn, turns it into a raster. Then
#builds and returns a raster stack

df2ras <- function(dd){

rasterDF <- vector("list", length(ncol(dd) - 2))
k <- 1
#first two cols are x and y coords
for (i in 3:ncol(dd)){

  spg <- dd[, c(1, 2, i)]
  coordinates(spg) <- ~ x + y

  # coerce to SpatialPixelsDataFrame
  gridded(spg) <- TRUE
  # coerce to raster
  rasterDF[[k]] <- raster(spg,)
  k <- k + 1
}

RS <- stack(unlist(rasterDF))
crs(RS) <- "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs"

return(RS)
}
