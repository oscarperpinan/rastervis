\name{rasterTheme}
\alias{rasterTheme}
\alias{BTCTheme}
\alias{YlOrRdTheme}
\alias{plasmaTheme}
\alias{infernoTheme}
\alias{viridisTheme}
\alias{magmaTheme}
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

\code{rasterTheme} is a customization of the \code{custom.theme.2} function of
\code{latticeExtra} using the magma palette of the \code{viridis}
package. The other palettes provided by this package are available
through the \code{viridisTheme}, \code{infernoTheme}, and \code{plasmaTheme}.

\code{YlOrRdTheme}, \code{BuRdTheme}, \code{RdBuTheme}, \code{GrTheme}
and \code{BTCTheme} are variations of \code{rasterTheme} using palettes
of the \code{RColorBrewer} and \code{hexbin} packages.

\code{streamTheme} is a variation of \code{rasterTheme} using black for
the region, gray for the panel background and a sequential palette for points.
}

\usage{
yscale.raster(lim, ...)
xscale.raster(lim, ...)
yscale.raster.subticks(lim, ...)
xscale.raster.subticks(lim, ...)

rasterTheme(region = magma(10),
            pch=19, cex=0.7, 
            strip.background = list(col = 'transparent'),
            strip.shingle = list(col = 'transparent'),
            strip.border = list(col = 'transparent'),
            add.lines = list(lwd = .4),
            ...)

magmaTheme(region = magma(10), ...)
infernoTheme(region = inferno(10), ...)
plasmaTheme(region = plasma(10), ...)
viridisTheme(region = viridis(10), ...)
 
YlOrRdTheme(region = brewer.pal(9, 'YlOrRd'), ...)
RdBuTheme(region = brewer.pal(9, 'RdBu'), ...)
BuRdTheme(region = rev(brewer.pal(9, 'RdBu')), ...)
PuOrTheme(region = brewer.pal(9, 'PuOr'), ...)
GrTheme(region = rev(brewer.pal(9, 'Greys')), ...) 
BTCTheme(region = BTC(n=9), ...)

streamTheme(region = 'black',
            symbol = brewer.pal(n=5, name='Blues'),
            alpha = 0.6,
            panel.background = list(col='gray20'),
            ...)
}

\arguments{
  \item{lim}{Range of data.}
  \item{pch}{Symbol used for points.}
  \item{cex}{A numeric multiplier to control the size of the points.}
  \item{region}{A vector of colors that is used to define a continuous
  color gradient using \code{colorRampPalette} to fill in
  regions. Note that the length of this gradient is set by
    \code{\link{custom.theme}} to exactly 100 colors.}
  \item{symbol}{A palette to display symbols.}
  \item{panel.background}{Parameters of the panel background.}
  \item{alpha, ...}{Additional arguments for \code{\link{custom.theme.2}},
    \code{\link{yscale.components.default}} and
    \code{\link{xscale.components.default}}
  }
  \item{strip.background, strip.shingle, strip.border}{List whose
  components define the configuration of the strip areas. Read the help page of \code{\link{trellis.par.get}} for details.}
  \item{add.lines}{List whose components define the lines superposed on
  the graphic. Read the help page of \code{\link{trellis.par.get}} for details.}
}
\author{Oscar Perpiñán Lamigueiro}
\seealso{\code{custom.theme}, \code{custom.theme.2}, \code{BTC},
  \code{xscale.components.default}, \code{xscale.components.subticks}}