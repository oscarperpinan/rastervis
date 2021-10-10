\name{gplot-methods}

\docType{methods}

\alias{gplot}
\alias{gplot,Raster-method}
\alias{gplot,SpatRaster-method}


\title{Use ggplot to plot a Raster* or a SpatRaster object.}

\description{ A wrapper function around \link[ggplot2]{ggplot} (ggplot2
package). Note that the function in the raster package is called gplot
with a single 'g'.  }

\usage{
\S4method{gplot}{Raster}(x, maxpixels=50000,...) 
\S4method{gplot}{SpatRaster}(x, maxpixels=50000,...) 
}

\arguments{
  \item{x}{A Raster* or SpatRaster object}
  \item{maxpixels}{Maximum number of pixels to use}
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

r <- raster(system.file("external/test.grd", package="raster"))

s1 <- stack(r, r*2)
names(s1) <- c('meuse', 'meuse x 2')

library(ggplot2)

theme_set(theme_bw())

## With raster
gplot(s1) + geom_tile(aes(fill = value)) +
          facet_wrap(~ variable) +
          scale_fill_gradient(low = 'white', high = 'blue') +
          coord_equal()

## With terra
s2 <- rast(s1)
gplot(s2) + geom_tile(aes(fill = value)) +
          facet_wrap(~ variable) +
          scale_fill_gradient(low = 'white', high = 'blue') +
          coord_equal()
}
}

\keyword{methods}
\keyword{spatial}

