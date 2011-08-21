\name{levelplot-methods}
\docType{methods}

\alias{levelplot}
\alias{contourplot}
\alias{levelplot,Raster,missing-method}
\alias{contourplot,Raster,missing-method}
\title{Level and contour plots of Raster objects.}
\description{Level and contour plots of Raster objects with \code{lattice} methods
  and marginal plots with \code{grid} objects.
}
\usage{
\S4method{levelplot}{Raster,missing}(x, data=NULL, layers,
            margin=TRUE, FUN.margin=mean,
            maxpixels=1e5,
            par.settings=rasterTheme,
            between=list(x=0.5, y=0.2),
            as.table=TRUE,
            xlab='', ylab='', main='',
            scales=list(draw=TRUE),
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            colorkey=list(space='right'),
            ...)

\S4method{contourplot}{Raster,missing}(x, data=NULL, layers, ...)
}

\arguments{
  \item{x}{A Raster object.}
  \item{data}{Not used.}
  \item{layers}{A numeric or character which should indicate the layers
    to be displayed.}
  \item{margin}{logical, if TRUE a marginal plot of the
    \code{FUN.margin} function to each coordinate will be displayed.}
  \item{FUN.margin}{A function to be applied to each coordinate.}
  \item{maxpixels}{A numeric, for \code{\link{sampleRandom}}.}
  \item{xlab, ylab, main}{Arguments for \code{\link{histogram}}.}
  \item{between, as.table, scales, xscale.components, yscale.components,
            par.settings, colorkey}{Graphical parameters of \code{lattice}. See \code{\link{xyplot}} for details.}
  \item{\dots}{Additional arguments for \code{\link{histogram}}}
}

\details{The result of the \code{levelplot} method is similar to the
    \code{spplot} method for \code{Raster} objects defined in the
    \code{raster} package. However, this method does not use the
    \code{spplot} of the \code{sp} package and, therefore, no conversion
    between classes is needed.

    The \code{contourplot} method is a wrapper for \code{levelplot} with
    the next additional default settings: \code{cuts=7}, \code{labels=TRUE},
    \code{contour}=TRUE, \code{pretty}=TRUE, \code{region=TRUE} and
    \code{colorkey=TRUE} (see the help of \code{contourplot} for details.)
  }
  
\author{Oscar Perpiñán Lamigueiro}
\seealso{\code{\link{spplot}}, \code{\link[lattice]{levelplot}}}
\examples{
f <- system.file("external/test.grd", package="raster")
r <- raster(f)
levelplot(r)
s <- stack(r, r+500, r-500)
levelplot(s, contour=TRUE)
contourplot(s, labels=list(cex=0.6), cuts=12)

##Add a layer of sampling points
##and change the theme
pts <- sampleRandom(r, size=20, sp=TRUE)
levelplot(r, par.settings=BTCTheme) + layer(sp.points(pts, col='red'))
contourplot(r, labels=FALSE) + layer(sp.points(pts, col='red'))

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

levelplot(SISmm)

levelplot(SISmm, layers=1, FUN.margin=median, contour=TRUE)
}}
\keyword{methods}
\keyword{spatial}
