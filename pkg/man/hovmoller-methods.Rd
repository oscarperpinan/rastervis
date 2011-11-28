\name{hovmoller-methods}
\docType{methods}
\alias{hovmoller}
%\alias{hovmoller-methods}
\alias{hovmoller,RasterStackBrick-method}
\title{Hovmoller plots}
\description{
Hovmoller plots of Raster objects.}
\usage{\S4method{hovmoller}{RasterStackBrick}(object, dirXY=y, FUN=mean, 
digits=2, xlab='Direction', ylab='Time', par.settings=rasterTheme,
add.contour=TRUE, labels=FALSE, region=TRUE, ...)
}
\arguments{
  \item{object}{A RasterStackBrick with a non-empty \code{z} slot.}
  \item{dirXY}{A direction as a function of the coordinates (see
    \code{\link{xyLayer}}).}
  \item{FUN}{A function to be applied to the zones calculated with
    \code{dirXY} and \code{zonal}.}
  \item{digits}{An integer, number of digits for \code{\link{zonal}}.}
  \item{xlab, ylab}{Labels of the axis.}
  \item{par.settings}{Customization of lattice. See \code{\link{levelplot}}
    for details.}
  \item{labels, region}{Customization of \code{contourplot} when \code{add.contour} is
  TRUE.}
  \item{add.contour}{Logical, if TRUE a contourplot with filled regions
    is drawn.}
  \item{\dots}{Additional arguments for the \code{\link{contourplot}}
    and \code{\link{levelplot}} functions.}
}

\details{Extracted from wikipedia: "A Hovmöller diagram is a
commonly used way of plotting meteorological data to highlight the role
of waves. The axes of a Hovmöller diagram are typically longitude or
latitude (abscissa or x-axis) and time (ordinate or y-axis) with the
value of some field represented through color or shading."
The direction defined by \code{dirXY} and the function \code{FUN} allows
for a variety of diagrams with this method.
}
\author{Oscar Perpiñán Lamigueiro}
\seealso{\code{\link[lattice]{levelplot}}, \code{\link{zonal}}
  }
\keyword{methods}
\references{\itemize{
    \item{Hovmoller, E. 1949. The trough and ridge diagram. Tellus 1, 62–66.}
  \item{\url{http://www.mmm.ucar.edu/episodes/Hovmoller/noJS/hovm200707.htm}}
  \item{\url{http://www.star.nesdis.noaa.gov/sod/sst/squam/L4/l4_delsst_hovmoller.htm}}
  \item{\url{http://www.esrl.noaa.gov/psd/map/clim/sst.shtml}}
}
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
layerNames(SISmm) <- month.abb

hovmoller(SISmm, dirXY=y, xlab='Latitude')##latitude
hovmoller(SISmm, dirXY=y, labels=FALSE, xlab='Latitude')##without labels
hovmoller(SISmm, dirXY=y, add.contour=FALSE, xlab='Latitude')##without contours

hovmoller(SISmm, dirXY=sqrt(x^2+y^2))##a function of coordinates...
}
\dontrun{
library(zoo)

url <- "ftp://ftp.wiley.com/public/sci_tech_med/spatio_temporal_data/"

sst.dat = read.table(paste(url, "SST011970_032003.dat", sep=''), header
	= FALSE) 

sst.ll = read.table(paste(url, "SSTlonlat.dat", sep=''),
	header = FALSE)

spDat <- SpatialPointsDataFrame(sst.ll, sst.dat)
gridded(spDat) <- TRUE
proj4string(spDat) = "+proj=longlat +datum=WGS84"

sDat <- stack(spDat)

idx <- seq(as.Date('1970-01-01'), as.Date('2003-03-01'), by='month')
idx <- as.yearmon(idx)
sDat <- setZ(sDat, idx)
layerNames(sDat) <- as.character(idx)

levelplot(sDat, layers=1:8)
hovmoller(sDat, xlab='Latitude',
          par.settings=BTCTheme, 
          panel=panel.levelplot.raster, interpolate=TRUE)
}
}
\keyword{spatial}
\keyword{methods}
