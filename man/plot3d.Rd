\name{plot3D}

\docType{methods}

\alias{plot3D}
\alias{plot3D,RasterLayer-method}

\title{Interactive 3D plot of a RasterLayer}

\description{
Make an interactive 3D plot (map) of a RasterLayer. This is a wrapper around \code{surface3d} in the rgl package. You can use \code{decorate3d} to add axes.
}

\usage{
\S4method{plot3D}{RasterLayer}(x, maxpixels=100000, 
   zfac=1, drape=NULL, col, rev=FALSE, adjust=TRUE, ...)
}

\arguments{
 \item{x}{a RasterLayer object}
% \item{background}{value used to replace NA values}
 \item{maxpixels}{Maximum number of pixels to use}
 \item{zfac}{Numeric, to set the elevation scale relative to x and y}
 \item{drape}{RasterLayer, to 'drape' colors representing the values of this layer on the 3D representation of layer \code{x}. In this case\code{x} typically has elevation data}
 \item{col}{A color palette generating function such as \code{rainbow}, \code{heat.colors}, and \code{topo.colors}, or one or your own making}
 \item{rev}{Logical. If \code{TRUE}, the color palette values are reversed in order}
 \item{adjust}{Logical. If \code{TRUE}, the x and y axes are scaled relative to the cell (z) values}
 \item{...}{Any argument that can be passed to \code{\link[rgl]{surface3d}}}
}


\author{ Robert J. Hijmans }

\examples{
if (require(rgl)) {

    data(volcano)
    r <- raster(volcano)
    extent(r) <- c(0, 610, 0, 870)

    ## level plot as reference
    levelplot(r, col.regions=terrain.colors)

    plot3D(r)

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

    ## Set drape to define the color breaks
    drape <- cut(r, 5)
    plot3D(r, drape=drape)
}
}

\keyword{methods}
\keyword{spatial}

