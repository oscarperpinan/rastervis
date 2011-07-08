\name{bwplot-methods}
\docType{methods}
\alias{bwplot}
\alias{bwplot,RasterStackBrick,missing-method}
\title{Box and whisker plots of Raster objects.}
\description{
A method for \code{bwplot} and \code{RasterStackBrick} objects. It uses
a combination of \code{\link{panel.violin}} and
\code{\link{panel.bwplot}} to compose the graphic.
}
\usage{\S4method{bwplot}{RasterStackBrick,missing}(x, data=NULL, layers, FUN,
            maxpixels = 1e+05,
            xlab='', ylab='', main='',
            violin=TRUE,
            par.settings=rasterTheme,
            ...)
}
\arguments{
  \item{x}{A \code{RasterStackBrick} object.}
  \item{data}{Not used.}
  \item{layers}{A numeric or character which should indicate the layers
    to be displayed.}
  \item{FUN}{A function to applied to the \code{z} slot of a
            \code{RasterStackBrick} object. The result of this function
            is used as the grouping variable of the plot.}
  \item{maxpixels}{A numeric, for \code{\link{sampleRandom}}.}
  \item{xlab, ylab, main}{Characters, for \code{\link{bwplot}}.}
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
listNC <- lapply(listFich, raster)
stackSIS <- do.call(stack, listNC)
stackSIS <- stackSIS*24 ##from irradiance (W/m2) to irradiation Wh/m2
setwd(old)

idx <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')

SISmm <- setZ(stackSIS, idx)
layerNames(SISmm) <- month.abb

bwplot(SISmm)
bwplot(SISmm, FUN=as.yearqtr)##FUN applies to z if not NULL
}
}
\keyword{methods}
\keyword{spatial}
