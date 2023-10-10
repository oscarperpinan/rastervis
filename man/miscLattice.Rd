\name{lattice}

\alias{llines.SpatVector}
\alias{lpoints.SpatVector}
\alias{lpolygon.SpatVector}

\title{Lines, points and polygons}

\description{
Represent lines, points and polygons
}

\usage{
\method{llines}{SpatVector}(x, ...)
\method{lpoints}{SpatVector}(x, ...)
\method{lpolygon}{SpatVector}(x, ...)
}

\arguments{

 \item{x}{a SpatVector object}

 \item{...}{Any argument that can be passed to
 \code{\link[lattice]{llines}}, \code{\link[lattice]{lpoints}}, or \code{\link[lattice]{lpolygon}}}

}

\author{ Alexandre Courtiol and Oscar Perpiñán}

\examples{
library(lattice)
library(latticeExtra)
library(terra)
library(rasterVis)

f <- system.file("ex/lux.shp", package="terra")
v <- vect(f)

r <- rast(v)
values(r) <- 1:ncell(r)

pts <- terra::as.points(v)
levelplot(r, margin = FALSE) +
    layer(lpoints(pts, col = "white"))

lns <- terra::as.lines(v)
levelplot(r, margin = FALSE) +
    layer(llines(lns, col = "white", lty = 3))

pols <- terra::as.polygons(v)
levelplot(r, margin = FALSE) +
    layer(lpolygon(pols, border = "white", col = "transparent", lty = 3))
}

\keyword{methods}
\keyword{spatial}

