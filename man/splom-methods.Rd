\name{splom-methods}
\docType{methods}
\alias{splom}
\alias{splom,Raster,missing-method}
\alias{splom,SpatRaster,missing-method}
\title{Scatter plot matrices of Raster objects.}
\description{
Draw conditional scatter plot matrices with hexagonally binning.}

\usage{
\S4method{splom}{Raster,missing}(x, data=NULL,maxpixels=1e5,
plot.loess=FALSE, colramp=BTC, varname.cex=0.6,...)
\S4method{splom}{SpatRaster,missing}(x, data=NULL,maxpixels=1e5, plot.loess=FALSE, colramp=BTC, varname.cex=0.6,...)}

\arguments{
  \item{x}{A \code{Raster} or a \code{SpatRaster} object.}
  \item{data}{Not used.}
  \item{maxpixels}{A numeric, for \code{\link{sampleRandom}} or \code{spatSample}.}
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
dataURL <- "https://raw.github.com/oscarperpinan/bookvis/master/data/"

##Solar irradiation data from CMSAF http://dx.doi.org/10.5676/EUM_SAF_CM/RAD_MVIRI/V001
old <- setwd(tempdir())
download.file(paste0(dataURL, "SISmm2008_CMSAF.zip"),
   "SISmm2008_CMSAF.zip", method='wget')
unzip("SISmm2008_CMSAF.zip")

listFich <- dir(pattern='\\\\.nc')
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
