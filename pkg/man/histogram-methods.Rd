\name{histogram-methods}
\docType{methods}
\alias{histogram}
\alias{histogram,RasterLayer,missing-method}
\alias{histogram,RasterStackBrick,missing-method}
\title{
Histogram of Raster objects.
}
\description{Draw histograms (with \code{lattice}) of Raster objects.
}

\usage{
\S4method{histogram}{RasterLayer,missing}(x, data=NULL, maxpixels = 1e+05, breaks=100, 
            xlab='', ylab='', main='', col='gray', ...)

\S4method{histogram}{RasterStackBrick,missing}(x, data=NULL, layers, FUN,
            maxpixels = 1e+05, breaks=100,
            xlab='', ylab='', main='', col='gray',
            between=list(x=0.5, y=0.2),
            as.table=TRUE,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            par.settings=rasterTheme,
            ...)
}
\arguments{
  \item{x}{A \code{Raster*} object.}
  \item{data}{Not used.}
  \item{layers}{A numeric or character which should indicate the layers
    to be displayed.}
  \item{FUN}{A function to applied to the \code{z} slot of a
            \code{RasterStackBrick} object. The result of this function
            is used as the grouping variable of the plot.}
  \item{breaks}{Breaks for the histogram. See the documentation of
            \code{\link[lattice]{histogram}} at lattice for details.}
  \item{maxpixels}{A numeric, for \code{\link{sampleRandom}}.}
  \item{xlab, ylab, main, col}{Arguments for \code{\link{histogram}}.}
  \item{between, as.table, xscale.components, yscale.components,
            par.settings}{Graphical parameters of \code{lattice}. See \code{\link{xyplot}} for details.}
  \item{\dots}{Additional arguments for \code{\link{histogram}}}
}

\author{Oscar Perpiñán Lamigueiro}

\seealso{\code{\link[lattice]{histogram}},\code{\link{xscale.raster}},
            \code{\link{yscale.raster}}, \code{\link{rasterTheme}}
}
\examples{
f <- system.file("external/test.grd", package="raster")
r <- raster(f)
histogram(r)
s <- stack(r, r+500, r-500)
histogram(s)

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

histogram(SISmm)
histogram(SISmm, FUN=as.yearqtr)
}
}
\keyword{methods}
\keyword{spatial}

