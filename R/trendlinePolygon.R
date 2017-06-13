#' Plots Trendlines over Polygons
#'
#' \code{trendlinePolygon} Plots Trendlines over Polygons
#'
#' Superimposes trendline plots over polygons
#'
#' @param data   Spatial Points Data Frame object of Data
#' @param shape   Spatial Polygon Data Frame
#' @param breaks   Color of heatMap
#' @param time  Title for Map
#' @param main Title for Map
#' @param normal Boolean for whether to normalize points
#' @param size Size of subplots
#' @param Color palette
#' @return geospatial visualization
#' @examples \dontrun{
#' heatMap(df, shape)
#' }
#'
#' @export
trendlinePolygon <-function(data,shape,
		Breaks="months",time,
		main="Plot Title",normal=TRUE,
		size=c(1,1),Color=palette()) {
	# Plots Frequency Graphs over Polygons
	#
	# Args:
	#   data:		A list of at least one SpatialPointsDataframes
	#   shape: 		Shapefile
	#	Breaks		The size of the breaks for the frequncies ("months",
	#				"weeks","years","days")
	#   time: 		The column in the SpatialPointsDataFrame
	#	normal: 	TRUE/FALSE if all plots on same data normalized.
	#   size: 		The size of the subplots in inches
	#
	#   Notes:  This function requires the sp and
	#			TeachingDemos packages.
	#
	#   Beskow: 04/01/11
	#
	is.installed <- function(mypkg) is.element(mypkg,
				installed.packages()[,1])
	if (is.installed(mypkg="sp")==FALSE)  {
		stop("sp package is not installed")}
	if (is.installed(mypkg="TeachingDemos")==FALSE)  {
		stop("TeachingDemos package is not installed")}
	library(sp)
	library(TeachingDemos)
	for (g in 1:length(data)){
		names(data[[g]])[grep(time[g],names(data[[g]]))]<-"time"   }
	plot(shape,lwd=2)
	mtext(main,cex=2)
	myplot<-function(a.time,a) {
		if(length(a[[1]])>0){
			plot(a.time[[1]],a[[1]],type="l",main=NA,ylab=NA,xlab=NA,cex.axis=0.6,
					col=Color[1],lwd=2)
		}
		if(length(a)>1){
			for(h in 2:length(a)){
				if(length(a[[h]])>0){
					points(a.time[[h]],a[[h]],type="l",xlab=NA,col=Color[h],lwd=2)
				}
			}
		}
	}
	for (i in 1:length(shape)){
		a<-list(NULL); a.time<-list(NULL);plot<-FALSE
		for (j in 1:length(data)){
			#a[[j]]<-list(NULL); a.time[[j]]<-list(NULL)
			x<-!is.na(over(as(data[[j]],"SpatialPoints"),as(shape[i,],
									"SpatialPolygons")))
			if(sum(x)>0){
				temp_data<-data[[j]][x,]
				a[[j]]<-table(cut(temp_data$time,breaks=Breaks))
				if(normal & max(a[[j]])>0){
					a[[j]]<-a[[j]]/max(a[[j]])
				}
				a.time[[j]]<-as.POSIXlt(strptime(as.character(names(a[[j]])),
								"%Y-%m-%d"), "GMT")
			}
		}
		for(k in length(a)){
			if(length(a[[k]])>0) plot=TRUE
		}
		if(plot){
			subplot(myplot(a.time,a),coordinates(shape[i,])[1],
					coordinates(shape[i,])[2],size=size)
		}
	}
}

