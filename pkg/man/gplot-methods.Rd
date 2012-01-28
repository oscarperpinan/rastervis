\name{gplot-methods}

\docType{methods}

\alias{gplot}
\alias{gplot,Raster-method}


\title{Use ggplot to plot a Raster* object}

\description{ A wrapper function around \link[ggplot2]{ggplot} (ggplot2
package). Note that the function in the raster package is called gplot
with a single 'g'.  }

\usage{
\S4method{gplot}{Raster}(x, maxpixels=50000,...) 
}

\arguments{
  \item{x}{A Raster* object}
  \item{maxpixels}{Maximum number of pixels to use}
  \item{...}{Additional arguments for ggplot}
}


\seealso{
\code{ \link[raster]{plot}, \link[raster]{spplot}}
}

\author{ Robert J. Hijmans; based on an example by Paul Hiemstra }


\examples{ 
\dontrun{
r <- raster(system.file("external/test.grd", package="raster"))
s <- stack(r, r*2)
layerNames(s) <- c('meuse', 'meuse x 2')

library(ggplot2)

theme_set(theme_bw())
gplot(s) + geom_tile(aes(fill = value)) +
          facet_wrap(~ variable) +
          scale_fill_gradient(low = 'white', high = 'blue') +
          coord_equal()
}
}

\keyword{methods}
\keyword{spatial}

