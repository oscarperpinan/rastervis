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
            names.attr,
            scales=list(), scales.margin=NULL,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            zscaleLog=NULL,
            colorkey=list(space='right'),
            panel=panel.levelplot,
            contour=FALSE, region=TRUE, labels=FALSE,
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
  \item{scales.margin}{A list with components \code{x} and
  \code{y}. Each of these components is a numeric vector of length 2
  defining the range for each marginal plot. If
  \code{scales.margin=NULL} the range is internally computed.}
  \item{maxpixels}{A numeric, for \code{\link{sampleRegular}}.}
  \item{xlab, ylab, main}{Arguments for \code{\link{levelplot}}.}
  \item{names.attr}{Character, names to use in each panel. If missing its
  default value is the result of \code{layerNames(x)} (after subsetting
  the layers to be displayed).}
  \item{xscale.components, yscale.components}{See
  \code{\link{xscale.raster}} and \code{\link{yscale.raster}}.}
  \item{between, as.table, par.settings, scales, colorkey, panel, contour, region, labels}{Graphical
	    parameters of \code{lattice}. See \code{\link{xyplot}} for
	    details.}
  \item{zscaleLog}{Controls whether the Raster* will be log transformed before being passed to the
              panel function.  Defaults to ‘NULL’, in which case the
              Raster* is not transformed.  Other possible values are any
              number that works as a base for taking logarithm, ‘TRUE’
              (which is equivalent to 10), and ‘"e"’ (for the natural
              logarithm).  As a side effect, the colorkey is
              labeled differently.}
  \item{\dots}{Additional arguments for \code{\link{levelplot}}}
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

## With subticks
levelplot(r, xscale.components=xscale.raster.subticks,
             yscale.components=yscale.raster.subticks)

levelplot(r, xscale.components=xscale.raster.subticks,
             yscale.components=yscale.raster.subticks,
             scales=list(x=list(rot=30, cex=0.8)))

## defining the scales for the marginal plot
levelplot(r, scales.margin=list(x=c(100, 600), y=c(100, 1000)))
## if a component of the list is null, it is internally calculated
levelplot(r, scales.margin=list(x=c(100, 1000)))

## log-scale
levelplot(r^2, zscaleLog=TRUE, contour=TRUE)

s <- stack(r, r+500, r-500)
levelplot(s, contour=TRUE)
contourplot(s, labels=list(cex=0.4), cuts=12)

levelplot(s, names.attr=c('R', 'R + 500', 'R - 500'))

## Add a layer of sampling points
## and change the theme
pts <- sampleRandom(r, size=20, sp=TRUE)

## Using +.trellis and layer from latticeExtra
levelplot(r, par.settings = BTCTheme) + layer(sp.points(pts, col = 'red'))
contourplot(r, labels = FALSE) + layer(sp.points(pts, col = 'red'))

## or with a custom panel function
levelplot(r, par.settings=BTCTheme,
          panel=function(...){
            panel.levelplot(...)
            sp.points(pts, col='red')
            })


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

levelplot(SISmm)

levelplot(SISmm, layers=1, FUN.margin=median, contour=TRUE)
}}
\keyword{methods}
\keyword{spatial}
