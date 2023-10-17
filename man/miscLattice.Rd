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
\method{lpolygon}{SpatVector}(x, rule = "evenodd", ...)
}

\arguments{

 \item{x}{a SpatVector object}

 \item{rule}{character string specifying how ‘NA’ values are interpreted
          for polygons. The default rule (‘"evenodd"’) interprets
          ‘NA’-separated segments as subpaths, possibly representing
          holes, of a single path, and are rendered using
          \code{grid.path}. Note that this default value is different
          from the default value defined in
          \code{\link[lattice]{lpolygon.default}}.}

 \item{...}{Any argument that can be passed to
 \code{\link[lattice]{llines}}, \code{\link[lattice]{lpoints}}, or
 \code{\link[lattice]{lpolygon}}}
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
    layer(lpolygon(pols, border = "black", col = "white"))

## Example adapted from the help page of terra::vect

x <- rbind(c(-10,0), c(140,60), c(160,0), c(140,-55))
hole <- rbind(c(50,0), c(105,30), c(120,2), c(105,-30))
     
z <- rbind(cbind(object=1, part=1, x, hole=0), 
           cbind(object=1, part=1, hole, hole=1))
colnames(z)[3:4] <- c('x', 'y')

p <- vect(z, "polygons", atts=data.frame(id=1))
crs(p) <- "+proj=longlat"

r <- rast(p)
values(r) <- runif(ncell(r))
     
levelplot(r, margin = FALSE) + 
    layer(lpolygon(p, border = "black", col = "white"))
}

\keyword{methods}
\keyword{spatial}

