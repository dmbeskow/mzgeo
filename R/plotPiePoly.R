#' Plots Pie Charts over Polygons
#'
#' \code{plotPiePoly } Plots Pie Charts over Polygons
#'
#' Superimposes Pie Charts over polygons
#'
#' @param data   Spatial Points Data Frame object of Data
#' @param shape   Spatial Polygon Data Frame
#' @param factor   Categorical Variable to Plot
#' @param main Title for Map
#' @param legend Boolean for whether to plot legend
#' @param size Size of subplots
#' @param legend.pos Position of legend
#' @return geospatial visualization
#' @examples \dontrun{
#' plotPiePoly (df, shape, factor)
#' }
#'
#' @export
plotPiePoly <-function(data,shape,factor,main="Pie Plot",
		size=c(1,1),legend=TRUE,legend.pos="bottomright"){
	# Plots a Pie Plot over over Polygons in a SpatialPolygonDataFrame
	#
	# Args:
	#   data:			Spatial Points dataframe
	#   shape: 			Shapefile
	#   factor.column: 	The column in the SpatialPointsDataFrame
	#	main: 			the title of the graph
	#   size: 			The size of the pieplot in inches
	#
	#   Notes:  This function requires the sp package.
	#
	#   Beskow: 03/30/11
	#
	is.installed <- function(mypkg) is.element(mypkg,
				installed.packages()[,1])
	if (is.installed(mypkg="sp")==FALSE)  {
		stop("sp package is not installed")}
	if (is.installed(mypkg="TeachingDemos")==FALSE)  {
		stop("TeachingDemos package is not installed")}
	if (!class(data)=="SpatialPointsDataFrame")  {
		stop("data argument is not SpatialPointsDataFrame")}
	require(sp)
	require(TeachingDemos)
	names(data)[grep(factor,names(data))]<-"factor"
	data.table<-table(data$factor)
	data.table<-sort(data.table[data.table>0],decreasing=TRUE)
	my.names<-names(data.table)
	palette(rainbow(length(my.names)))
	plot(shape)
	if(legend){
	legend(legend.pos, my.names, col=c(2:(length(my.names)+1)),
			text.col = "green4", pch = c(15), bg = 'gray90',cex=0.8)}
	for (i in 1:length(shape)){
		x<-!is.na(over(as(data,"SpatialPoints"),as(shape[i,],
								"SpatialPolygons")))
		if(sum(x)>0){
			my.table<-table(as.factor(data$factor[x]))
			my.table<-my.table[my.table>0]
			table.Colors<-match(names(my.table),my.names)+1
			subplot(pie(my.table,col=table.Colors,labels=NA),
					coordinates(shape[i,])[1],
					coordinates(shape[i,])[2],size=size)
		}
	}
	palette("default")
	mtext(main,cex=2)
}

