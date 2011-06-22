\name{rasterTheme}
\alias{rasterTheme}
\alias{xscale.raster}
\alias{yscale.raster}

\title{
A theme for \code{raster} with \code{lattice}.
}
\description{
Auxiliary functions for the customization of trellis graphics with
\code{lattice}. \code{xscale.raster} and \code{yscale.raster} suppress
the right and top axis, respectively. \code{rasterTheme} is an
improvement of the \code{custom.theme.2} function of \code{latticeExtra}.
}

\usage{
yscale.raster(...)
xscale.raster(...)
rasterTheme(pch=19, cex=0.7, region=rev(brewer.pal(9, 'YlOrRd')), ...)
}

\arguments{
  \item{pch}{Symbol used for points.}
  \item{cex}{A numeric multiplier to control the size of the points.}
  \item{region}{A palette to fill in regions.}
  \item{...}{Additional arguments for \code{\link{custom.theme.2}},
  \code{\link{yscale.components.default}} and
  \code{\link{xscale.components.default}}
}
}
\author{Oscar Perpiñán Lamigueiro}
\seealso{\code{custom.theme}, \code{custom.theme.2}}