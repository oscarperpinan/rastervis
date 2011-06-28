\name{xyplot-methods}
\docType{methods}
\alias{xyplot-methods}
\alias{xyplot,RasterStackBrick,ANY-method}
\title{xyplot for Raster objects}
\description{Scatter plots of Raster objects for directions defined by \code{xyLayer}
}
\usage{
\S4method{xyplot}{RasterStackBrick,ANY}(x, data=NULL, dirXY=y, xlab='Time',
ylab='', digits=0, par.settings=rasterTheme,...)
}

\arguments{
\item{x}{A \code{RasterStackBrick} object.}
\item{data}{Not used.}
\item{dirXY}{A direction as a function of the coordinates (see
\code{\link{xyLayer}}).}
\item{xlab, ylab}{Labels of the axis.}
\item{par.settings}{Customization of \code{lattice}. See \code{xyplot}
for details.}
\item{digits}{An integer, number of digits for \code{\link{zonal}}.}
\item{\dots}{Additional arguments for the \code{xyplot} function.}
}

\author{Oscar Perpiñán Lamigueiro}

\examples{
\dontrun{
##Solar irradiation data from CMSAF
##Data available from http://www.box.net/shared/rl51y1t9sldxk54ogd44

old <- getwd()
##change to your folder...
setwd('CMSAF')
listFich <- dir(pattern='2008')
listNC <- lapply(listFich, raster)
stackSIS <- do.call(stack, listNC)
stackSIS <- stackSIS*24 ##from irradiance (W/m2) to irradiation Wh/m2
setwd(old)

idx <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')

SISmm <- setZ(stackSIS, idx)
layerNames(SISmm) <- month.abb

xyplot(SISmm)
}
}
\keyword{methods}
