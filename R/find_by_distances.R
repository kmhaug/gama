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
#' @param return_circles logical; if true the two circles is also returned.
#'
#' @return 1) SpatialPoints with the coordinates of the two possible positions, 
#' or 2) a list with two objects: The coordinates of the two points, and
#' a SpatialPolygons object with the two circles.
#'
#'
#' @references http://gis.stackexchange.com/questions/112905/map-location-of-unknown-point-with-distances-to-two-known-points
#'
#' @author Marius Hauglin 2016 \email{marius.hauglin@@gnmail.com}
#' @export
#' @importFrom sp SpatialPoints SpatialLines Lines Line coordinates spChFIDs
#' @importFrom rgeos gBuffer gIntersection
#' @importFrom maptools spRbind

find_by_distances<-function(x1,y1,x2,y2,dist1,dist2,return_circles=FALSE){

	dat<-data.frame(cbind(c(1,2),c(1,2))) # for Spatial*dataframe

pointB <- SpatialPoints(cbind(x1,y1))
pointC <- SpatialPoints(cbind(x2,y2))
distanceA2B <- dist1
distanceA2C <- dist2

# create the circle polygons around the points with the distances
polyB <- gBuffer(pointB, width = distanceA2B,quadsegs=200)
polyC <- gBuffer(pointC, width = distanceA2C,quadsegs=200)


# extract the feature coordinates of the polygon - we need to intersect lines, not polygons
coordsB <- lapply(polyB@polygons, function(x) {x@Polygons[[1]]@coords})
coordsC <- lapply(polyC@polygons, function(x) {x@Polygons[[1]]@coords})

# create circles as lines
lineB <- SpatialLines(list(Lines(Line(coordsB[[1]]), "B")))
lineC <- SpatialLines(list(Lines(Line(coordsC[[1]]), "C")))

# find intersections
if (is.null(gIntersection(lineB, lineC))) {
	
	coords <- NULL

	} else {
		
	coords <- coordinates(gIntersection(lineB, lineC))
	row.names(coords)<-c(1,2)
	coords<-SpatialPointsDataFrame(coordinates(coords),data=dat)
}


if (return_circles) {
		
	circles<-spRbind(spChFIDs(polyB, as.character(1)),spChFIDs(polyC, as.character(2)))
	circles<-SpatialPolygonsDataFrame(circles,data=dat)
	
	return(list(coords=coords,circles=circles))
	
	} else {
		
	return(coords)
	
	}

}
