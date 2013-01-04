\name{rasterTheme}
\alias{rasterTheme}
\alias{BTCTheme}
\alias{RdBuTheme}
\alias{BuRdTheme}
\alias{PuOrTheme}
\alias{GrTheme}
\alias{streamTheme}
\alias{xscale.raster}
\alias{yscale.raster}
\alias{xscale.raster.subticks}
\alias{yscale.raster.subticks}


\title{
Themes for \code{raster} with \code{lattice}.
}
\description{
Auxiliary functions for the customization of trellis graphics with
\code{lattice}.

\code{xscale.raster} and \code{yscale.raster} suppress the right and top
axis, respectively. \code{xscale.raster.subticks} and
\code{yscale.raster.subticks} also suppress those axis and draw subticks.

\code{rasterTheme} is a
customization of the \code{custom.theme.2} function of
\code{latticeExtra} using a Yellow-Orange-Red palette from
\code{RColorBrewer}.

\code{RdBuTheme}, \code{GrTheme} and
\code{BTCTheme} are variations of \code{rasterTheme} with different
color defaults for the region argument.

\code{streamTheme} is a variation of \code{rasterTheme} using black for
the region, gray for the panel background and a sequential palette for points.
}

\usage{
yscale.raster(lim, ...)
xscale.raster(lim, ...)
yscale.raster.subticks(lim, ...)
xscale.raster.subticks(lim, ...)
rasterTheme(pch=19, cex=0.7, region=rev(brewer.pal(9, 'YlOrRd')), ...)
RdBuTheme(region=brewer.pal(9, 'RdBu'), ...)
BuRdTheme(region=rev(brewer.pal(9, 'RdBu')), ...)
PuOrTheme(region=brewer.pal(9, 'PuOr'), ...)
GrTheme(region=rev(brewer.pal(9, 'Greys')), ...) 
BTCTheme(region=BTC(n=9), ...)
streamTheme(region='black',
            symbol=brewer.pal(n=5, name='Blues'),
            alpha=0.6,
            panel.background=list(col='gray20'),
            ...)
}

\arguments{
  \item{lim}{Range of data.}
  \item{pch}{Symbol used for points.}
  \item{cex}{A numeric multiplier to control the size of the points.}
  \item{region}{A palette to fill in regions.}
  \item{symbol}{A palette to display symbols.}
  \item{panel.background}{Parameters of the panel background.}
  \item{alpha, ...}{Additional arguments for \code{\link{custom.theme.2}},
  \code{\link{yscale.components.default}} and
  \code{\link{xscale.components.default}}
}
}
\author{Oscar Perpiñán Lamigueiro}
\seealso{\code{custom.theme}, \code{custom.theme.2}, \code{BTC},
  \code{xscale.components.default}, \code{xscale.components.subticks}}