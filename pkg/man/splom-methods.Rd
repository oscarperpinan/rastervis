\name{splom-methods}
\docType{methods}
\alias{splom}
\alias{splom,RasterStackBrick,missing-method}
\title{Scatter plot matrices of Raster objects.}
\description{
Draw conditional scatter plot matrices with hexagonally binning.}
\usage{\S4method{splom}{RasterStackBrick,missing}(x, data=NULL,maxpixels=1e5, plot.loess=FALSE, colramp=BTC, varname.cex=0.6,...)}
\arguments{
  \item{x}{A \code{RasterStackBrick} object.}
  \item{data}{Not used.}
  \item{maxpixels}{A numeric, for \code{\link{sampleRandom}}.}
  \item{plot.loess}{Logical, should a loess fit be drawn?.}
  \item{colramp}{A function accepting an integer \code{n} as argument
    and returning \code{n} colors (for \code{\link{hexbinplot}}).}
  \item{varname.cex}{A numerical multiplier to control the size of the
    variables names.}
  \item{...}{Additional arguments for splom.}
}
\author{Oscar Perpiñán Lamigueiro}

\note{While the hexagonal binning is quite fast for large datasets, the
  use of the \code{loess} fit will slow this function.}

\seealso{\code{\link{hexbinplot}}, \code{\link[lattice]{splom}}
  }

  \examples{
\dontrun{
##Solar irradiation data from CMSAF
##Data available from http://www.box.net/shared/rl51y1t9sldxk54ogd44

old <- getwd()
##change to your folder...
setwd('CMSAF')
listFich <- dir(pattern='2008')
stackSIS <- stack(listFich)
stackSIS <- stackSIS*24 ##from irradiance (W/m2) to irradiation Wh/m2
setwd(old)

idx <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')

SISmm <- setZ(stackSIS, idx)
names(SISmm) <- month.abb

splom(SISmm)
}}

\keyword{methods}
\keyword{spatial}
