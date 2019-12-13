\name{horizonplot-methods}
\docType{methods}
\alias{horizonplot}
\alias{horizonplot,RasterStackBrick-method}
\alias{horizonplot,RasterStackBrick,missing-method}
\title{Horizon plots of Raster objects.}
\description{This method draws horizon graphs for each zone as
  calculated with \code{zonal} from the directions defined by
  \code{xyLayer}}

\usage{\S4method{horizonplot}{RasterStackBrick,missing}(x, data = NULL,
    dirXY = y, stat = 'mean', digits = 0,
    origin = mean, 
    xlab = 'Time', ylab = 'direction',
    colorkey = TRUE, colorkey.digits = 1,
    scales=list(y = list(relation = "same")),
    ...)
}

\arguments{
\item{x}{A \code{RasterStackBrick} object.}

\item{data}{Not used.}

\item{dirXY}{A direction as a function of the coordinates (see
\code{\link{xyLayer}}).}

\item{stat}{a function to be applied to summarize the values by
  zone. See \code{\link{zonal}} for details.}

\item{digits}{An integer, number of digits for \code{\link{zonal}}.}

\item{origin}{From the \code{latticeExtra} help page: "the baseline y
          value for the first (positive) segment (i.e. the value at
          which red changes to blue)." It can be: a number, used acrros
          all panels, or a function (or a character defining a
          function), evaluated with the values in each panel. The
          default is the \code{mean} function.}

\item{xlab, ylab}{Labels of the axis.}

\item{colorkey}{If \code{colorkey = TRUE} a suitable color scale bar is
          constructed using the values of \code{origin} and
          \code{horizonscale} (see below). For additional information
          see \code{\link{levelplot}}.}


\item{colorkey.digits}{Digits for rounding values in \code{colorkey}
  labels} 

\item{scales}{From the \code{lattice::xyplot} help page: "A list
          determining how the x- and y-axes (tick marks and labels) are
          drawn.  The list contains parameters in \code{name=value}
          form, and may also contain two other lists called \code{x} and
          \code{y} of the same form.  Components of \code{x} and
          \code{y} affect the respective axes only, while those in
          \code{scales} affect both.  When parameters are specified in
          both lists, the values in \code{x} or \code{y} are used." In
          \code{horizonplot} the most interesting component is
          \code{relation}, a character string that determines how axis
          limits are calculated for each panel.  Possible values are
          \code{"same"} (default), \code{"free"} and \code{"sliced"}.
          "For ‘relation="same"’, the same limits,
          usually large enough to encompass all the data, are used for
          all the panels.  For ‘relation="free"’, limits for each panel
          is determined by just the points in that panel.  Behavior for
          ‘relation="sliced"’ is similar, except that the length (max -
          min) of the scales are constrained to remain the same across
          panels."}
	  
\item{\dots}{Additional arguments for the \code{horizonplot}
  function. \code{horizonscale} is the most interesting, being (from
  the \code{latticeExtra} help page) "the scale of each color
  segment. There are 3 positive segments and 3 negative segments. If
  this is a given as a number then all panels will have comparable
  distances, though not necessarily the same actual values (similar in
  concept to ‘scales$relation = "sliced"’)". On the other hand,
  \code{col.regions} is used to choose the color scale.}

}

\references{
  \url{http://www.perceptualedge.com/articles/visual_business_intelligence/time_on_the_horizon.pdf}
}

\details{(Extracted from the reference): "The horizon graph allows to
  examine how a large number of items changed through time, to spot
  extraordinary behaviors and predominant patterns, view each of the
  items independently from the others when they wish, make comparisons
  between the items, and view changes that occurred with enough
  precision to determine if further examination is required."}


\seealso{ \code{\link[latticeExtra]{horizonplot}},
  \code{\link[lattice]{xyplot}}, \code{\link{levelplot}}.}

\examples{
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

horizonplot(SST)

horizonplot(SST, stat='sd')

## Different scales for each panel, with colors representing deviations
## from the origin in *that* panel
horizonplot(SST, scales=list('free'))

## origin may be a function...
horizonplot(SST, origin=mean)
## ...or a number
horizonplot(SST, origin=0)

## A different color palette
pal <- RColorBrewer::brewer.pal(n=6, 'PuOr')
horizonplot(SST, origin = 0, col.regions = pal)

## The width of each color segment can be defined with horizonscale
horizonplot(SST, horizonscale=1, origin=0)
}

\dontrun{


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

horizonplot(SISmm)
}


}
\keyword{methods}
\keyword{spatial}

