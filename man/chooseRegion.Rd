\name{Interaction}
\docType{methods}
\alias{identifyRaster}
\alias{chooseRegion}
\alias{identifyRaster}
\alias{identifyRaster,Raster-method}

\title{
Interaction with trellis objects.
}
\description{
\code{chooseRegion} provides a set of points (in the form of a
\code{SpatialPoints}) inside a region defined by several mouse clicks.
\code{identifyRaster} labels and returns points of a \code{trellis}
graphic according to mouse clicks.
}
\usage{
chooseRegion(sp = TRUE, proj = CRS("+proj=latlon +ellps=WGS84"))
\S4method{identifyRaster}{Raster}(object, layer=1, values=FALSE, pch=13, cex=0.6, col='black',...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp}{logical, if TRUE the result is a \code{SpatialPoints}
object, otherwise it is a logical vector as returned by \code{\link{in.out}}}
  \item{proj}{A character string for the \code{proj4string} of
    \code{SpatialPoints}.}
  \item{object}{A Raster object.}
  \item{layer}{A numeric or character which should indicate the layer
    to be chosen.}
  \item{values}{logical, if TRUE the values are returned.}
  \item{pch, cex, col}{Graphical parameters for \code{panel.identify}
    and \code{panel.link.splom}.}
  \item{\dots}{Additional arguments for
    \code{trellis.focus},\code{panel.link.splom} and
    \code{panel.identify}.}
  }
  
\details{When called, these functions wait for the user to identify points (in the panel being drawn) via
     mouse clicks.  Clicks other than left-clicks close the region
     (for \code{chooseRegion}) and the procedure (for
     \code{identifyRaster}).
   }
\note{\code{chooseRegion} needs the package \code{mgcv} to be installed.
}

\author{
Oscar Perpiñán Lamigueiro
}

\seealso{
\code{panel.identify},\code{panel.link.splom}, \code{trellis.focus}, \code{grid.locator}
}
\examples{
f <- system.file("external/test.grd", package="raster")
r <- raster(f)
levelplot(r)
##Do not close the last graphical window
##Use the left button of the mouse to identify points and the right button to finish
chosen_r <- identifyRaster(r, values=TRUE)
chosen_r
s <- stack(r, r-500, r+500)
levelplot(s)
chosen_s <- identifyRaster(s, values=TRUE)
chosen_s

\dontrun{
##The package mgcv is needed for the next example
##Use the left button of the mouse to build a border with points, and the right button to finish.
##The points enclosed by the border will be highlighted and returned as a SpatialPoints object.
levelplot(s)
reg <- chooseRegion()
summary(reg)
}

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

levelplot(SISmm)

##Do not close the last graphical window
##Interaction
##Use the left button of the mouse to identify points and the right button to finish
chosen <- identifyRaster(SISmm, layer=3, values=TRUE)
chosen
##Use the left button of the mouse to build a border with points, and the right button to finish.
##The points enclosed by the border will be highlighted and returned as a SpatialPoints object.
reg <- chooseRegion()
summary(reg)
}
}
\keyword{methods}
\keyword{spatial}
