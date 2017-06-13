#' Create Grid Heat Map
#'
#' \code{gridDensity} Create Grid Heat Map
#'
#' Create Grid Heat Map
#'
#' @param data   Spatial Points Data Frame object of Data
#' @param shape   Spatial Polygon Data Frame
#' @param factor   Categorical Variable to Plot
#' @param time  Title for Map
#' @param main Title for Map
#' @param legend Boolean for whether to plot legend
#' @param size Size of subplots
#' @param cs Size of grid
#' @param legend.pos Position of legend
#' @return geospatial visualization
#' @examples \dontrun{
#' gridDensity(df, shape)
#' }
#'
#' @export
gridDensity <-		function(data,shape,cs=c(1,1),col="blue",scale=TRUE,
				heat=FALSE,main="Grid Density Heat Map"){
	# Plots a Gridded Density Heat Map user selected grid
	#
	# Args:
	#   data:			Spatial Points dataframe
	#   shape: 			Shapefile
	#   cs:				The dimensions of the grid in lat/lon
	#   col:      		If single color, this is the color.  See heat parameter
	#   scale: 	        TRUE/FALSE to place scale on side of plot
	#   heat:			TRUE/FALSE to use the heat.colors palette.
	#					This over rides the col parameter
	#	main: 			the title of the graph
	#
	#   Notes:  This function requires the sp package.
	#
	#   Beskow: 03/30/11
	#
	is.installed <- function(mypkg) is.element(mypkg,
				installed.packages()[,1])
	if (is.installed(mypkg="sp")==FALSE)  {
		stop("sp package is not installed")}
	if (!class(data)=="SpatialPointsDataFrame")  {
		stop("data argument is not SpatialPointsDataFrame")}
	require(sp)
	bb<-bbox(shape)
	cc<-bb[,1]+(cs/2)
	cd<-ceiling(diff(t(bb))/cs)
	my_grd<-GridTopology(cellcentre.offset=cc, cellsize=cs,cells.dim=cd)
	p4s<-CRS(proj4string(data))
	spatialGrid<-SpatialGrid(my_grd,proj4string=p4s)
	shape1<-as(as(spatialGrid, "SpatialPixels"), "SpatialPolygons")
	freq_table<-data.frame(tabulate(over(as(data,"SpatialPoints"),as(shape1,"SpatialPolygons")),nbins=length(shape1)))
	names(freq_table)<-"counts"

	spdf<-SpatialGridDataFrame(my_grd, freq_table)

	rw.colors<-colorRampPalette(c("white",col))
	test.shape<-SpatialPolygons(shape@polygons)
	stuff<-list("sp.polygons",test.shape,first=FALSE)
	z<-max(freq_table)
	if(heat){
		spplot(spdf,scales = list(draw = scale),
				col.regions=rev(heat.colors(z)),
				sp.layout=list(stuff),main=main)
	}
	else{
		spplot(spdf,scales = list(draw = scale),
				col.regions=rw.colors(z),
				sp.layout=list(stuff),main=main)
	}
}

