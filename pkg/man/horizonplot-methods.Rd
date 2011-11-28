\name{horizonplot-methods}
\docType{methods}
\alias{horizonplot}
\alias{horizonplot,RasterStackBrick-method}
\alias{horizonplot,RasterStackBrick,missing-method}
\title{Horizon plots of Raster objects.}
\description{This method draws horizon graphs for each zone as
  calculated with \code{zonal} from the directions defined by
  \code{xyLayer}}

\usage{\S4method{horizonplot}{RasterStackBrick,missing}(x, data=NULL, dirXY=y, xlab='Time', ylab='direction', digits=0, ...)
}
\arguments{
\item{x}{A \code{RasterStackBrick} object.}
\item{data}{Not used.}
\item{dirXY}{A direction as a function of the coordinates (see
\code{\link{xyLayer}}).}
\item{xlab, ylab}{Labels of the axis.}
\item{digits}{An integer, number of digits for \code{\link{zonal}}.}
\item{\dots}{Additional arguments for the \code{horizonplot} function.}
}

\references{
  \url{http://www.perceptualedge.com/articles/visual_business_intelligence/time_on_the_horizon.pdf}
}
\details{(Extracted from the reference): The horizon graph allows to examine how a large number of
  items changed through time, to spot extraordinary behaviors and
  predominant patterns, view each of the items independently from the
  others when they wish, make comparisons between the items, and
view changes that occurred with enough precision to determine if further
examination is required.}
\seealso{\code{\link[latticeExtra]{horizonplot}}
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

horizonplot(SISmm)
}
}
\keyword{methods}
\keyword{spatial}

