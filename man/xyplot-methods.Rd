\name{xyplot-methods}
\docType{methods}
\alias{xyplot}
\alias{xyplot,RasterStackBrick,missing-method}
\alias{xyplot,SpatRaster,missing-method}
\title{xyplot for Raster objects}
\description{Scatter plots of space-time Raster objects for directions defined by \code{xyLayer}
}
\usage{
\S4method{xyplot}{RasterStackBrick,missing}(x, data=NULL, dirXY=y,
       stat='mean', xlab='Time', ylab='',
       digits=0, par.settings=rasterTheme(),
       auto.key = FALSE, ...)
\S4method{xyplot}{SpatRaster,missing}(x, data=NULL, dirXY=y,
       stat='mean', xlab='Time', ylab='',
       digits=0, par.settings=rasterTheme(),
       auto.key = FALSE, ...)
}
\arguments{
\item{x}{A \code{RasterStackBrick} object or a \code{SpatRaster} object
  whose \code{z} slot or time index are not \code{NULL}.}
\item{data}{Not used.}
\item{dirXY}{A direction as a function of the coordinates (see
  \code{\link{xyLayer}}).}
\item{stat}{a function to be applied to summarize the values by
  zone. See \code{\link{zonal}} for details.}
\item{xlab, ylab}{Labels of the axis.}
\item{par.settings}{Customization of \code{lattice}. See \code{xyplot}
for details.}
\item{digits}{An integer, number of digits for \code{\link{zonal}}.}
\item{auto.key}{A logical or a list. If it is FALSE (default) a label
  with the value of the direction (as defined by \code{dirXY})will be
  printed next to each line. You should read the help page of
  \code{xyplot} for details about the usage of \code{auto.key} as a list.}
\item{\dots}{Additional arguments for the \code{xyplot} function.}
}

\seealso{zonal}

\author{Oscar Perpiñán Lamigueiro}

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

idx <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')

SISmm <- setZ(stackSIS, idx)
names(SISmm) <- month.abb

xyplot(SISmm)
}

\dontrun{
library(zoo)

url <- "ftp://ftp.wiley.com/public/sci_tech_med/spatio_temporal_data/"
sst.dat = read.table(paste(url, "SST011970_032003.dat", sep=''), header = FALSE) 
sst.ll = read.table(paste(url, "SSTlonlat.dat", sep=''), header = FALSE)

spSST <- SpatialPointsDataFrame(sst.ll, sst.dat)
gridded(spSST) <- TRUE
proj4string(spSST) = "+proj=longlat +datum=WGS84"
SST <- brick(spSST)

idx <- seq(as.Date('1970-01-01'), as.Date('2003-03-01'), by='month')
idx <- as.yearmon(idx)
SST <- setZ(SST, idx)
names(SST) <- as.character(idx)

xyplot(SST)
}

}
\keyword{methods}
\keyword{spatial}
