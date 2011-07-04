\name{densityplot-methods}
\docType{methods}
\alias{densityplot-methods}
\alias{densityplot,RasterLayer,missing-method}
\alias{densityplot,RasterStackBrick,missing-method}
\title{Density plots for Raster objects.}
\description{Draw kernel density plots (with \code{lattice}) of Raster objects.
}

\usage{
\S4method{densityplot}{RasterLayer,missing}(x, data=NULL, maxpixels = 1e+05,
            xlab='', ylab='', main='', col='black', ...)

\S4method{densityplot}{RasterStackBrick,missing}(x, data=NULL, layer, FUN,
            maxpixels = 1e+05,
            xlab='', ylab='', main='',
            par.settings=rasterTheme,
            ...)
}
\arguments{
  \item{x}{A \code{Raster*} object.}
  \item{data}{Not used.}
  \item{layer}{A numeric or character which should indicate the layers
    to be displayed.}
  \item{FUN}{A function to applied to the \code{z} slot of a
            \code{RasterStackBrick} object. The result of this function
            is used as the grouping variable of the plot.}
  \item{maxpixels}{A numeric, for \code{\link{sampleRandom}}.}
  \item{xlab, ylab, main, col, par.settings}{Arguments for \code{\link{densityplot}}.}
  \item{\dots}{Additional arguments for \code{\link{densityplot}}}
}
\author{Oscar Perpiñán Lamigueiro}
\seealso{\code{\link[lattice]{densityplot}},\code{\link{xscale.raster}},
            \code{\link{yscale.raster}}, \code{\link{rasterTheme}}}

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

densityplot(SISmm)
densityplot(SISmm, FUN=as.yearqtr)##FUN applies to z if not NULL
}
}

\keyword{methods}
\keyword{spatial}


