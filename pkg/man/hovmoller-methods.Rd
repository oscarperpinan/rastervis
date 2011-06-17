\name{hovmoller-methods}
\docType{methods}
\alias{hovmoller-methods}
\alias{hovmoller,RasterStackBrick-method}
\title{ ~~ Methods for Function \code{hovmoller}  ~~}
\description{
 ~~ Methods for function \code{hovmoller}  ~~
}
\section{Methods}{
\describe{

\item{\code{signature(object = "RasterStackBrick")}}{
%%  ~~describe this method here~~
}
}}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
\examples{
\dontrun{
##Solar irradiation data from CMSAF
##Data available from http://www.box.net/shared/rl51y1t9sldxk54ogd44

old <- getwd()
##change to your folder...
setwd('/home/oscar/Datos/CMSAF')
listFich <- dir(pattern='2008')
listNC <- lapply(listFich, raster)
stackSIS <- do.call(stack, listNC)
stackSIS <- stackSIS*24 ##from irradiance (W/m2) to irradiation Wh/m2
setwd(old)

idx <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')

SISmm <- setZ(stackSIS, idx)
layerNames(SISmm) <- as.character(idx)

levelplot(SISmm)

##Do not close the last graphical window
##Interaction
##Use the left button of the mouse to identify points and the right button to finish
chosen <- identifyRaster(SISmm, layer=3, values=TRUE)
chosen
##Use the left button of the mouse to build a border with points, and the right button to finish.
##The points enclosed by the border will be highlighted and returned as a SpatialPoints object.
reg <- chooseRegion()
summary(reg)

##Density, etc.
densityplot(SISmm)
densityplot(SISmm, FUN=as.yearqtr)##FUN applies to z if not NULL

histogram(SISmm)
histogram(SISmm, FUN=as.yearqtr)

splom(SISmm, plot.loess=TRUE)##fast if plot.loess=FALSE

##Graphical tools for time series
hovmoller(SISmm, dirXY=y, xlab='Latitude')##latitude
hovmoller(SISmm, dirXY=sqrt(x^2+y^2))##a function of coordinates...

horizonplot(SISmm)

xyplot(SISmm)
}
}