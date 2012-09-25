\name{vectorplot-methods}
\docType{methods}

\alias{vectorplot}
\alias{vectorplot,Raster-method}
\title{Vector plots of Raster objects.}
\description{Plots vector fields from Raster objects with \code{lattice} methods
}
\usage{
\S4method{vectorplot}{Raster}(object, layers,
    narrows=2e3, lwd.arrows=0.6, length=unit(5e-2, 'npc'),
    region=TRUE,
    isField=FALSE, unit='radians',...)
}

\arguments{
  \item{object}{A Raster object. If \code{isField=FALSE} the vector
  field is calculated internally from the result of
  \code{\link{terrain}}. }
  \item{layers}{A numeric or character which should indicate the layers
    to be displayed.}
  \item{narrows}{A numeric, for \code{\link{sampleRegular}}.}
  \item{lwd.arrows}{Numeric, width of the lines of the arrows}
  \item{length}{Unit, extent of the arrow head.}
  \item{region}{Logical, if \code{TRUE} the region is displayed with
    \code{panel.levelplot.raster}}
  \item{isField}{Logical, if \code{TRUE} (the object is a vector field),
  \code{object} must be a Raster*
  with two layers, slope and aspect (in this order), following the
  philosophy of \code{\link{terrain}}.}
\item{unit}{Character, angle units of the \code{aspect} layer if
  \code{isField=TRUE}: 'radians' or 'degrees'.}
  \item{\dots}{Additional arguments for \code{\link{levelplot}}}
}
  
\author{Oscar Perpiñán Lamigueiro}
\seealso{\code{\link{panel.arrows}}, \code{\link{levelplot}}, \code{\link{terrain}}}
\examples{
df <- expand.grid(x=seq(-2, 2, .1), y=seq(-2, 2, .1))
df$z <- with(df, (3*x^2 + y)*exp(-x^2-y^2))
r1 <- rasterFromXYZ(df)
df$z <- with(df, x*exp(-x^2-y^2))
r2 <- rasterFromXYZ(df)
df$z <- with(df, y*exp(-x^2-y^2))
r3 <- rasterFromXYZ(df)

projection(r1) <- projection(r2) <- projection(r3) <- CRS("+proj=longlat +datum=WGS84")

vectorplot(r1, par.settings=RdBuTheme)
vectorplot(r2, par.settings=RdBuTheme)
vectorplot(r3, par.settings=RdBuTheme)
}
\keyword{methods}
\keyword{spatial}
