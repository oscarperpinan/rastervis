\name{xyLayer}
\alias{xyLayer}
\title{xyLayer
}
\description{
Create a numeric vector from a function of the coordinates.
}
\usage{
xyLayer(object, dirXY = y, vector = TRUE, maxpixels)
}
\arguments{
  \item{object}{A \code{Raster} or a \code{SpatRaster} object.
}
  \item{dirXY}{A expression indicating the function of \code{x} and
    \code{y} (coordinates of the \code{Raster} object) to be evaluated.
  }
  \item{vector}{Logical. If TRUE (default) a numeric vector is returned,
    else a \code{Raster} or a \code{SpatRaster} object is returned.
  }
  \item{maxpixels}{A positive integer giving the number of cells to
    display, for \code{\link{sampleRegular}} or
    \code{\link{spatSample}}.}
  }

\value{
A numeric vector or a \code{Raster}/\code{SpatRaster} object.
}
\author{
Oscar Perpiñán Lamigueiro.
}

\seealso{
\code{init}, \code{substitute}, \code{eval}
}
\examples{
f <- system.file("external/test.grd", package="raster")
r <- raster(f)
dirX <- xyLayer(r, x)
dirXY <-xyLayer(r, sqrt(x^2 + y^2))
}
\keyword{spatial}
