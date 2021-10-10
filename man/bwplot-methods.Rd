\name{bwplot-methods}
\docType{methods}

\alias{bwplot}
\alias{bwplot,RasterStackBrick,missing-method}
\alias{bwplot,formula,Raster-method}
\alias{bwplot,SpatRaster,missing-method}
\alias{bwplot,formula,SpatRaster-method}

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
            violin = TRUE, draw.points = FALSE, do.out = FALSE,
            par.settings = bwTheme(),
            violin.ratio = 1, box.ratio = 0.5,
            scales=list(x=list(rot=45, cex=0.8)),
            ...)
\S4method{bwplot}{formula,Raster}(x, data, dirXY, 
            maxpixels = 1e+05,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            horizontal=FALSE,
            violin = TRUE, draw.points = FALSE, do.out = FALSE,
            violin.ratio = 1, box.ratio = 0.5,
            par.settings = bwTheme(),
            ...)
\S4method{bwplot}{SpatRaster,missing}(x, data=NULL, layers, FUN,
            maxpixels = 1e+05,
            xlab='', ylab='', main='',
            violin = TRUE, draw.points = FALSE, do.out = FALSE,
            par.settings = bwTheme(),
            violin.ratio = 1, box.ratio = 0.5,
            scales=list(x=list(rot=45, cex=0.8)),
            ...)
\S4method{bwplot}{formula,SpatRaster}(x, data, dirXY, 
            maxpixels = 1e+05,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            horizontal=FALSE,
            violin = TRUE, draw.points = FALSE, do.out = FALSE,
            violin.ratio = 1, box.ratio = 0.5,
            par.settings = bwTheme(),
            ...)
}

\arguments{
  \item{x}{A \code{RasterStackBrick} object, a \code{SpatRaster} object, or a \code{formula}.}
  \item{data}{\code{NULL}, a \code{Raster} object, or a \code{SpatRaster} object.}
  \item{layers}{A numeric or character which should indicate the layers
    to be displayed.}
  \item{dirXY}{A direction as a function of the coordinates (see
    \code{\link{xyLayer}}).}
  \item{FUN}{A function to applied to the \code{z} slot of a
            \code{RasterStackBrick} object, or the time index of a
  \code{SpatRaster} object. The result of this function
            is used as the grouping variable of the plot.}
  \item{maxpixels}{A numeric, for \code{\link{sampleRandom}} or \code{spatSample}.}
  \item{xscale.components, yscale.components}{Graphical
	    parameters of \code{lattice}. See \code{\link{xyplot}} for
	    details.}
  \item{horizontal}{Defaults to \code{FALSE}, meaning that the right hand
  of the formula is a factor or shingle.}
  \item{xlab, ylab, main}{Labels for axis and title}.
  \item{violin}{Logical, if \code{TRUE} the panel is built with
    \code{panel.violin} and \code{panel.bwplot}.}
  \item{draw.points}{Logical, if \code{TRUE} raw points are displayed in
    the background with \code{\link{panel.stripplot}}}
  \item{do.out}{Logical, if \code{TRUE} outliers are drawn.}
  \item{box.ratio}{ratio of box thickness to inter box space}
  \item{violin.ratio}{ratio of the thickness of each violin and inter violin space}
  \item{par.settings, scales}{See \code{\link{xyplot}} for details.}
  \item{\dots}{Additional arguments for \code{\link{bwplot}}}
}
\author{Oscar Perpiñán Lamigueiro}
\seealso{\code{\link{bwplot}},
  \code{\link{panel.violin}},
  \code{\link[raster]{subset}},
  \code{\link{bwTheme}}
}
\examples{
library(raster)
library(terra)

f <- system.file("external/test.grd", package="raster")
r <- raster(f)
s <- stack(r, r-500, r+500)
bwplot(s)

bwplot(s, violin = FALSE, box.ratio = 1)

## Modify colours
myTheme <- bwTheme(
    box.rectangle = list(col = 'green', fill = 'lightgreen'),
    plot.polygon = list(col = 'blue'),
    plot.symbol = list(col = 'gray', cex = 0.8, alpha = 0.1)
)
## Display raw points
bwplot(s, draw.points = TRUE,
       par.settings = myTheme)

\dontrun{

dataURL <- "https://raw.github.com/oscarperpinan/bookvis/master/data/"

##Solar irradiation data from CMSAF http://dx.doi.org/10.5676/EUM_SAF_CM/RAD_MVIRI/V001
old <- setwd(tempdir())
download.file(paste0(dataURL, "SISmm2008_CMSAF.zip"),
   "SISmm2008_CMSAF.zip", method='wget')
unzip("SISmm2008_CMSAF.zip")

listFich <- dir(pattern='\\\\.nc')
stackSIS <- stack(listFich)
stackSIS <- stackSIS*24 ##from irradiance (W/m2) to irradiation Wh/m2
setwd(old)

idx <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')

SISmm <- setZ(stackSIS, idx)
names(SISmm) <- month.abb

bwplot(SISmm)

##FUN applies to z if not NULL
library(zoo)

bwplot(SISmm, FUN=as.yearqtr)
}
\dontrun{
##http://neo.sci.gsfc.nasa.gov/Search.html?group=64
old <- setwd(tempdir())
download.file(paste0(dataURL, "875430rgb-167772161.0.FLOAT.TIFF"),
   "875430rgb-167772161.0.FLOAT.TIFF", method='wget')

pop <- raster("875430rgb-167772161.0.FLOAT.TIFF")
pop[pop==99999] <- NA
levelplot(pop, zscaleLog=10, par.settings=BTCTheme,
          panel=panel.levelplot.raster, interpolate=TRUE)

##http://neo.sci.gsfc.nasa.gov/Search.html?group=20
download.file(paste0(dataURL, "241243rgb-167772161.0.TIFF"),
   "241243rgb-167772161.0.TIFF", method='wget')
landClass <- raster("241243rgb-167772161.0.TIFF")
landClass[landClass==254] <- NA


s <- stack(pop, landClass)
names(s) <- c('pop', 'landClass')

bwplot(asinh(pop) ~ landClass|cut(y, 3), data = s,
       layout = c(3, 1))

bwplot(asinh(pop) ~ cut(y, 5)|landClass, data = s,
       scales = list(x=list(rot=45)), layout = c(4, 5),
       strip = strip.custom(strip.levels = TRUE))

## Modify colours
bwplot(asinh(pop) ~ cut(y, 5)|landClass, data = s,
       scales = list(x=list(rot=45)), layout = c(4, 5),
       strip = strip.custom(strip.levels = TRUE),
       par.settings = bwTheme(plot.polygon = list(col = 'lightgray'),
                              box.rectangle = list(fill = 'lightgreen')))
}
}
\keyword{methods}
\keyword{spatial}
