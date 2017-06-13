#' Plots cloroploth map of points
#'
#' \code{heatMap} Plots cloropleth map
#'
#' Plots chloropleth map
#'
#' @param data   Spatial Points Data Frame object of Data
#' @param shape   Spatial Polygon Data Frame
#' @param col   Color of heatMap
#' @param main  Title for Map
#' @return geospatial visualization
#' @examples \dontrun{
#' heatMap(df, shape)
#' }
#'
#' @export
heatMap <-function(data,shape=NULL,col="blue",main="Sample HeatMap"){
	# Plots a Heat Map of a Polygons Data Frame.  This will
	# demonstrate density within a finite set of polygons
	#
	# Args:
	#   data: 	Spatial Points dataframe
	#   shape: 	Polygons Data Frame
	#
	#
	#   Notes:  This function requires the sp and RColorBrewer
	# 			Packages
	#
	#   Beskow: 03/28/11
	#
	is.installed <- function(mypkg) is.element(mypkg,
				installed.packages()[,1])
	if (is.installed(mypkg="sp")==FALSE)  {
		stop("sp package is not installed")}
	if (is.installed(mypkg="RColorBrewer")==FALSE)  {
		stop("RColorBrewer package is not installed")}
	if (!class(data)=="SpatialPointsDataFrame")  {
		stop("data argument is not SpatialPointsDataFrame")}
require(sp)
require(RColorBrewer)
freq_table<-data.frame(tabulate(over(as(data,"SpatialPoints"),
	as(shape,"SpatialPolygons")),nbins=length(shape)))
names(freq_table)<-"counts"

shape1<-spChFIDs(shape,as.character(1:length(shape)))
row.names(as(shape1,"data.frame"))
spdf<-SpatialPolygonsDataFrame(shape1, freq_table, match.ID = TRUE)

rw.colors<-colorRampPalette(c("white",col))
spplot(spdf,scales = list(draw = TRUE),
		col.regions=rw.colors(max(freq_table)),	main=main)
}

