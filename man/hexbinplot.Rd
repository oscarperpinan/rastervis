\name{Formula methods}
\alias{hexbinplot}
\alias{hexbinplot,formula,Raster-method}
\alias{xyplot,formula,Raster-method}
\title{Formula methods}
\description{Formula methods
}
\usage{
\S4method{xyplot}{formula,Raster}(x, data, dirXY, maxpixels=1e5,
            alpha=0.05,
            xscale.components=xscale.raster, yscale.components=yscale.raster,
            par.settings=rasterTheme(),...)
\S4method{hexbinplot}{formula,Raster}(x, data, dirXY,
            xscale.components=xscale.raster, yscale.components=yscale.raster,
            par.settings=rasterTheme(),...)

}
\arguments{
\item{x}{A \code{formula} describing the variables to be related. It may
  include the layer names (which are internally converted to valid ones
  with \code{\link{make.names}}) and the \code{x}, \code{y} variables
  representing the coordinates of the Raster object. Besides, if dirXY
  is not missing, the variable \code{dirXY} can also be included in the
  \code{formula}.
  }
\item{data}{A \code{Raster} object.}
\item{dirXY}{A direction as a function of the coordinates (see
  \code{\link{xyLayer}}).}
\item{maxpixels}{A numeric, for \code{\link{sampleRegular}}.}
\item{alpha}{A numeric, transparency of the points.}
\item{xscale.components, yscale.components,par.settings}{Customization of \code{lattice}. See \code{xyplot}
for details.}
\item{\dots}{Additional arguments for the \code{xyplot} and
  \code{\link{hexbinplot}} functions.}
}

\author{Oscar Perpiñán Lamigueiro}

\examples{
f <- system.file("external/test.grd", package="raster")
r <- raster(f)
names(r)

xyplot(test~y, data=r, alpha=0.5)

\dontrun{
##Solar irradiation data from CMSAF
##Data available from http://www.box.net/shared/rl51y1t9sldxk54ogd44

old <- getwd()
##change to your folder...
setwd('CMSAF')
listFich <- dir(pattern='2008')
stackSIS <- stack(listFich)SISmm <- SISmm*24 ##from irradiance (W/m2) to irradiation Wh/m2
setwd(old)

names(SISmm) <- month.abb

##Relation between the January & February versus July radiation for four
##differents longitude regions.
xyplot(Jan+Feb~Jul|cut(x, 4), data=SISmm, auto.key=list(space='right'))
##Faster with hexbinplot
hexbinplot(Jan~Jul|cut(x, 6), data=SISmm)
}
}
\keyword{methods}
\keyword{spatial}
