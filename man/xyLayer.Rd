\name{xyLayer}
\alias{xyLayer}
\title{xyLayer
}
\description{
Create a numeric vector from a function of the coordinates.
}
\usage{
xyLayer(object, dirXY = y)
}
\arguments{
  \item{object}{A \code{Raster} object.
}
  \item{dirXY}{A expression indicating the function of \code{x} and
    \code{y} (coordinates of the \code{Raster} object) to be evaluated.
}
}
\value{
A numeric vector.
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
