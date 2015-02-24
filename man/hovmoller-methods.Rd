\name{hovmoller-methods}
\docType{methods}
\alias{hovmoller}
\alias{hovmoller,RasterStackBrick-method}
\title{Hovmoller plots}
\description{
Hovmoller plots of Raster objects.}
\usage{\S4method{hovmoller}{RasterStackBrick}(object, dirXY=y, FUN=mean, 
digits=2, xlab='Direction', ylab='Time',
par.settings=rasterTheme(), xscale.components=xscale.raster,
add.contour=FALSE, labels=FALSE, region=TRUE, ...)
}
\arguments{
  \item{object}{A RasterStackBrick with a non-empty \code{z} slot.}
  \item{dirXY}{A direction as a function of the coordinates (see
    \code{\link{xyLayer}}).}
  \item{FUN}{A function to be applied to the zones calculated with
    \code{dirXY} and \code{zonal}.}
  \item{digits}{An integer, number of digits for \code{\link{zonal}}.}
  \item{xlab, ylab}{Labels of the axis.}
  \item{par.settings}{Customization of lattice. See
    \code{\link{levelplot}} and \code{\link{rasterTheme}} for details.}
  \item{xscale.components}{See \code{\link{xscale.raster}}.}
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
\seealso{\code{\link[lattice]{levelplot}},
  \code{\link{zonal}}, \code{\link{panel.2dsmoother}}
  }
\keyword{methods}
\references{\itemize{
    \item{Hovmoller, E. 1949. The trough and ridge diagram. Tellus 1, 62–66.}
    \item{\url{http://www2.mmm.ucar.edu/episodes/Hovmoller/noJS/hovm200707.htm}}
    \item{\url{http://www.esrl.noaa.gov/psd/map/clim/sst.shtml}}
}
}

\examples{
\dontrun{
##Solar irradiation data from CMSAF http://dx.doi.org/10.5676/EUM_SAF_CM/RAD_MVIRI/V001
old <- setwd(tempdir())
download.file('https://raw.github.com/oscarperpinan/spacetime-vis/master/data/SISmm2008_CMSAF.zip',
   'SISmm2008_CMSAF.zip', method='wget')
unzip('SISmm2008_CMSAF.zip')

listFich <- dir(pattern='\\.nc')
stackSIS <- stack(listFich)
stackSIS <- stackSIS*24 ##from irradiance (W/m2) to irradiation Wh/m2

idx <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')

SISmm <- setZ(stackSIS, idx)
names(SISmm) <- month.abb

## Latitude as default
hovmoller(SISmm, xlab='Latitude')

## With contour lines and labels
hovmoller(SISmm, labels=TRUE, add.contour=TRUE,
          xlab='Latitude')

## Smooth color regions with latticeExtra::panel.2dsmoother
hovmoller(SISmm, panel=panel.2dsmoother, n=1000,
          labels=FALSE, add.contour=TRUE,
          xlab='Latitude')

## Using a function of coordinates
hovmoller(SISmm, dirXY=sqrt(x^2+y^2))
}

\dontrun{
library(zoo)
library(rasterVis)

## DESCRIPTION: http://iridl.ldeo.columbia.edu/SOURCES/.CAC/.sst/
setwd(tempdir())
download.file('http://iridl.ldeo.columbia.edu/SOURCES/.CAC/.sst/data.nc', destfile = 'SST.nc')
SST <- stack('SST.nc')
idx <- seq(as.Date('1970-01-01'), as.Date('2003-03-01'), by='month')
tt <- as.yearmon(idx)
SST <- setZ(SST, tt)
names(SST) <- as.character(tt)

## Extract month value from a Date or yearmon object
month <- function(x)format(x, '%m')
## Compute anomaly using monthly grouping with ave  
anomaly <- function(x){
    ## Monthly means
    mm <- ave(x, month(tt), FUN = mean)
    ## Monthly standard deviation
    msd <- ave(x, month(tt), FUN = sd)
    ## anomaly
    (x - mm)/msd
}
## Use anomaly with calc
SSTanom <- calc(SST, anomaly)
SSTanom <- setZ(SSTanom, tt)

## Ok, let's see the result
hovmoller(SSTanom,
          at = seq(-3, 3, .25),
          panel = panel.levelplot.raster,
          interpolate = TRUE,
          yscale.components = yscale.raster.subticks,
          par.settings = BuRdTheme)
}
}

\keyword{spatial}
\keyword{methods}
