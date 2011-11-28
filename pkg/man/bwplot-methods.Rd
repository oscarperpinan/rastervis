\name{bwplot-methods}
\docType{methods}
\alias{bwplot}
\alias{bwplot,RasterStackBrick,missing-method}
\alias{bwplot,formula,Raster-method}
\title{Box and whisker plots of Raster objects.}
\description{
Methods for \code{bwplot} and \code{RasterStackBrick} objects using
a combination of \code{\link{panel.violin}} and
\code{\link{panel.bwplot}} to compose the graphic.
}
\usage{
\S4method{bwplot}{RasterStackBrick,missing}(x, data=NULL, layers, FUN,
            maxpixels = 1e+05,
            xlab='', ylab='', main='',
            violin=TRUE,
            par.settings=rasterTheme,
            ...)
\S4method{bwplot}{formula,Raster}(x, data, dirXY, 
            maxpixels = 1e+05,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            horizontal=FALSE,
            violin=TRUE,
            par.settings=rasterTheme,
            ...)
}

\arguments{
  \item{x}{A \code{RasterStackBrick} object or a \code{formula}.}
  \item{data}{\code{NULL} or a \code{Raster} object.}
  \item{layers}{A numeric or character which should indicate the layers
    to be displayed.}
  \item{dirXY}{A direction as a function of the coordinates (see
    \code{\link{xyLayer}}).}
  \item{FUN}{A function to applied to the \code{z} slot of a
            \code{RasterStackBrick} object. The result of this function
            is used as the grouping variable of the plot.}
  \item{maxpixels}{A numeric, for \code{\link{sampleRandom}}.}
  \item{xscale.components, yscale.components}{Graphical
	    parameters of \code{lattice}. See \code{\link{xyplot}} for
	    details.}
  \item{horizontal}{Defaults to \code{FALSE}, meaning that the right hand
  of the formula is a factor or shingle.}
  \item{xlab, ylab, main}{Labels for axis and title}.
  \item{violin}{Logical, if TRUE the panel is built with
    \code{panel.violin} and \code{panel.bwplot}.}
  \item{par.settings}{See \code{\link{xyplot}} for details.}
  \item{\dots}{Additional arguments for \code{\link{bwplot}}}

}
\author{Oscar Perpiñán Lamigueiro}
\seealso{\code{\link{bwplot}},
  \code{\link{panel.violin}},
  \code{\link[raster]{subset}}
}
\examples{
f <- system.file("external/test.grd", package="raster")
r <- raster(f)
s <- stack(r, r-500, r+500)
bwplot(s)

\dontrun{
##Solar irradiation data from CMSAF
##Data available from http://www.box.net/shared/rl51y1t9sldxk54ogd44

old <- getwd()
##change to your folder...
setwd('CMSAF')
listFich <- dir(pattern='2008')
stackSIS <- stack(listFich)
stackSIS <- stackSIS*24 ##from irradiance (W/m2) to irradiation Wh/m2
setwd(old)

idx <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')

SISmm <- setZ(stackSIS, idx)
layerNames(SISmm) <- month.abb

bwplot(SISmm)
bwplot(SISmm, FUN=as.yearqtr)##FUN applies to z if not NULL
}
\dontrun{
##http://neo.sci.gsfc.nasa.gov/Search.html?group=64
pop <- raster('875430rgb-167772161.0.FLOAT.TIFF')
pop[pop==99999] <- NA
levelplot(pop, zscaleLog=10, par.settings=BTCTheme,
          panel=panel.levelplot.raster, interpolate=TRUE)

##http://neo.sci.gsfc.nasa.gov/Search.html?group=20
landClass <- raster('241243rgb-167772161.0.TIFF')
landClass[landClass==254] <- NA


s <- stack(pop, landClass)
layerNames(s) <- c('pop', 'landClass')

bwplot(asinh(pop) ~ landClass|cut(y, 3), data=s,
       layout=c(3, 1), violin=FALSE)

bwplot(asinh(pop) ~ cut(y, 5)|landClass, data=s,
       scales=list(x=list(rot=45)), layout=c(4, 5),
       strip=strip.custom(strip.levels=TRUE))
}
}
\keyword{methods}
\keyword{spatial}
