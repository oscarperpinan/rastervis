\name{plot3D}

\docType{methods}

\alias{plot3D}
\alias{plot3D,RasterLayer-method}

\title{Interactive 3D plot of a RasterLayer}

\description{
Make an interactive 3D plot (map) of a RasterLayer. This is a wrapper around \code{surface3d} in the rgl package. You can use \code{decorate3d} to add axes.
}

\usage{
\S4method{plot3D}{RasterLayer}(x, maxpixels=1e5, 
   zfac=1, drape=NULL, col=terrain.colors,
   at=100, rev=FALSE,
   useLegend=TRUE, adjust=TRUE, ...)
}

\arguments{

 \item{x}{a RasterLayer object}

 \item{maxpixels}{Maximum number of pixels to use}

 \item{zfac}{Numeric, to set the elevation scale relative to x and y}

 \item{drape}{RasterLayer, to 'drape' colors representing the values of
 this layer on the 3D representation of layer \code{x}. In this
 case\code{x} typically has elevation data}

 \item{col}{A color palette generating function such as \code{rainbow},
 \code{heat.colors}, and \code{topo.colors}, or one or your own making}

 \item{at}{A numeric variable of breakpoints defining intervals along
          the range of \code{x} or a number definining the number of
          intervals the range of \code{x} will be divided into.}
	
 \item{rev}{Logical. If \code{TRUE}, the color palette values are
   reversed in order}

 \item{useLegend}{Logical. If \code{TRUE} (default) the content of the
   slot \code{x@legend@colortable} is used instead of \code{col} and
   \code{at}.}

 \item{adjust}{Logical. If \code{TRUE}, the x and y axes are scaled
 relative to the cell (z) values}

 \item{...}{Any argument that can be passed to
 \code{\link[rgl]{surface3d}}}

}

\note{
Previous versions opened a new device with each call of
\code{plot3D}. This behaviour has been modified, and now a new device is
opened only if none is active. Thus, you should use \code{rgl::open3d()}
if you need to display a new scene without modifying the current one.}

\author{ Robert J. Hijmans and Oscar Perpiñán}

\examples{ \dontrun{
library(raster)

## rgl is needed to use plot3D
library(rgl)

data(volcano)
r <- raster(volcano)
extent(r) <- c(0, 610, 0, 870)

## level plot as reference
levelplot(r, col.regions=terrain.colors)

plot3D(r)
## Use different colors with a predefined function
plot3D(r, col = rainbow)
## or with a custom function using colorRampPalette
myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, 'PuOr'))
plot3D(r, col = myPal)

## With at you can define an homogeneus color table for different Rasters

r2 <- r + 100
r3 <- r + 200
s <- stack(r, r2, r3)

maxVal <- max(maxValue(s))
minVal <- min(minValue(s))
N <- 40
breaks <- seq(minVal, maxVal, length=N)

plot3D(r, at=breaks)
plot3D(r2, at=breaks)
plot3D(r3, at=breaks)

## Default: x-axis and y-axis are adjusted with z-values. Therefore,
## labels with decorate3d() are useless
plot3D(r, adjust=TRUE)
decorate3d()
## Compare the graphic limits
par3d('bbox')
## with the extent of the Raster
extent(r)

## Set adjust=FALSE to fix it
plot3D(r, adjust=FALSE)
decorate3d()
## Once again, compare the graphic limits
par3d('bbox')
## with the extent of the Raster
extent(r)

## zfac controls the z values so z-axis will be distorted
plot3D(r, adjust=FALSE, zfac=2)
decorate3d()
par3d('bbox')


## With drape you can disconnect the z-axis from the colors
drape <- cut(r^4, 4)
plot3D(r, drape=drape)
## Compare with:
plot3D(r, at=4)
}
}

\keyword{methods}
\keyword{spatial}

