#' find_by_distances
#' 
#' find the two possible positions of one unknown point using the distances to two known points
#'
#' @param x1 numeric; x coordinate of the first known point.
#' @param y1 numeric; y coordinate of the first known point.
#' @param x2 numeric; x coordinate of the second known point.
#' @param y2 numeric; y coordinate of the second known point.
#' @param dist1 numeric; distance to the first known point.
#' @param dist2 numeric; distance to the second known point.
#'
#' @return coordinates of the two possible positions
#'
#'
#' @references http://gis.stackexchange.com/questions/112905/map-location-of-unknown-point-with-distances-to-two-known-points
#'
#' @author Marius Hauglin 2016 \email{marius.hauglin@@gnmail.com}
#' @export
#' @importFrom sp SpatialPoints SpatialLines Lines Line coordinates
#' @importFrom rgeos gBuffer gIntersection

find_by_distances<-function(x1,y1,x2,y2,dist1,dist2){


pointB <- SpatialPoints(cbind(x1,y1))
pointC <- SpatialPoints(cbind(x2,y2))
distanceA2B <- dist1
distanceA2C <- dist2

# create the circle polygons around the points with the distances
polyB <- gBuffer(pointB, width = distanceA2B)
polyC <- gBuffer(pointC, width = distanceA2C)

# extract the feature coordinates of the polygon - we need to intersect lines, not polygons
coordsB <- lapply(polyB@polygons, function(x) {x@Polygons[[1]]@coords})
coordsC <- lapply(polyC@polygons, function(x) {x@Polygons[[1]]@coords})

# create circles as lines
lineB <- SpatialLines(list(Lines(Line(coordsB[[1]]), "B")))
lineC <- SpatialLines(list(Lines(Line(coordsC[[1]]), "C")))

# find intersections
if (is.null(gIntersection(lineB, lineC))) {coords <- NULL} else {coords <- coordinates(gIntersection(lineB, lineC))}

return(coords)


}
