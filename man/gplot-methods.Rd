\name{gplot-methods}

\docType{methods}

\alias{gplot}
\alias{gplot,Raster-method}
\alias{gplot,SpatRaster-method}


\title{Use ggplot to plot a Raster* or a SpatRaster object.}

\description{This function displays \code{Raster*} and \code{SpatRaster}
  objects with the \code{ggplot2} system. Note that the function in the
  \code{rasterVis} package is called \code{gplot} with a single 'g'.

  \code{gplot} uses \code{\link[ggplot2]{geom_raster}} to display the
  data, sets the color map with
  \code{\link[ggplot2]{scale_fill_viridis_c}}, and sets the coordinate
  reference system with \code{\link[ggplot2]{coord_sf}}. For multilayer
  objects, it uses \code{\link[ggplot2]{facet_wrap}} to display a matrix
  of panels.

}

\usage{
\S4method{gplot}{Raster}(x, maxpixels=50000, palette = "magma", ...) 
\S4method{gplot}{SpatRaster}(x, maxpixels=50000, palette = "magma", ...) 
}

\arguments{
  \item{x}{A Raster* or SpatRaster object}
  \item{maxpixels}{Maximum number of pixels to use}
  \item{palette}{Name of the \code{viridis} color map to use. See
  \link[ggplot2]{scale_fill_viridis_c} for details.}
  \item{...}{Additional arguments for ggplot}
}

\seealso{
\code{ \link[raster]{plot}, \link[raster]{spplot}}
}

\author{ Robert J. Hijmans and Oscar Perpiñán; based on an example by Paul Hiemstra }


\examples{ 
\dontrun{
library(raster)
library(terra)
library(ggplot2)
library(sf)

theme_set(theme_bw())

r <- raster(system.file("external/test.grd", package="raster"))
r2 <- stack(r, r*2)
names(r2) <- c('meuse', 'meuse x 2')

## With raster
gplot(r)

gplot(r2)

## With terra
s <- rast(r)
s2 <- rast(r2)

gplot(s)

gplot(s2)
}
}

\keyword{methods}
\keyword{spatial}

