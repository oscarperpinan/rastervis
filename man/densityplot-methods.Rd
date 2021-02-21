\name{densityplot-methods}
\docType{methods}

\alias{densityplot}
\alias{densityplot,Raster,missing-method}
\alias{densityplot,SpatRaster,missing-method}
\alias{densityplot,formula,Raster-method}
\alias{densityplot,formula,SpatRaster-method}

\title{Density plots for Raster objects.}

\description{Draw kernel density plots (with \code{lattice}) of Raster objects.
}

\usage{
\S4method{densityplot}{Raster,missing}(x, data=NULL, layers, FUN,
            maxpixels = 1e+05,
            xlab='', ylab='', main='',
            par.settings=rasterTheme(),
            draw.labels = TRUE,
            auto.key = list(space = "right"),
            att = 1,
            ...)

\S4method{densityplot}{formula,Raster}(x, data, dirXY,
            maxpixels = 1e+05,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            auto.key = list(space = 'right'), 
            par.settings=rasterTheme(),...)


\S4method{densityplot}{SpatRaster,missing}(x, data=NULL, layers, FUN,
            maxpixels = 1e+05,
            xlab='', ylab='', main='',
            par.settings=rasterTheme(),
            draw.labels = TRUE,
            auto.key = list(space = "right"),
            att = 1,
            ...)

\S4method{densityplot}{formula,SpatRaster}(x, data, dirXY,
            maxpixels = 1e+05,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            auto.key = list(space = 'right'), 
            par.settings=rasterTheme(),...)

}
\arguments{
  \item{x}{A \code{Raster*} object, a \code{SpatRaster} object, or a \code{formula}.}
  \item{data}{\code{NULL}, a \code{Raster} object, or a
    \code{SpatRaster} object.}
  \item{layers}{A numeric or character which should indicate the layers
    to be displayed.}
  \item{dirXY}{A direction as a function of the coordinates (see
    \code{\link{xyLayer}}).}
  \item{FUN}{A function to applied to the \code{z} slot of a
            \code{RasterStackBrick} object or to the time index of a
	    \code{SpatRaster} object. The result of this function
            is used as the grouping variable of the plot.}
  \item{maxpixels}{A numeric, for \code{\link{sampleRandom}} or \code{spatSample}.}
  \item{draw.labels}{Logical. If TRUE (default), labels are displayed
    over the maximum value of each layer. If FALSE, a key is displayed
    according to the \code{auto.key} argument.}
  \item{xlab, ylab, main, xscale.components, yscale.components,
    par.settings, auto.key}{Arguments for \code{\link{densityplot}}.}
  \item{att}{Integer or character to choose which variable (column) in
    the RAT table should be used.}
  \item{\dots}{Additional arguments for \code{\link{densityplot}}}
}
\author{Oscar Perpiñán Lamigueiro}
\seealso{\code{\link[lattice:histogram]{lattice:densityplot()}},\code{\link{xscale.raster}},
            \code{\link{yscale.raster}}, \code{\link{rasterTheme}}}

\examples{
f <- system.file("external/test.grd", package="raster")
r <- raster(f)
densityplot(r)
s <- stack(r, r+500, r-500)
## With labels
densityplot(s)
## With a key
densityplot(s, draw.labels = FALSE)

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

densityplot(SISmm)
##FUN applies to z if not NULL
densityplot(SISmm, FUN=as.yearqtr)
}
\dontrun{
##http://neo.sci.gsfc.nasa.gov/Search.html?group=64
pop <- raster('875430rgb-167772161.0.FLOAT.TIFF')
pop[pop==99999] <- NA
levelplot(pop, zscaleLog=10, par.settings=BTCTheme,
          panel=panel.levelplot.raster, interpolate=TRUE)

##http://neo.sci.gsfc.nasa.gov/Search.html?group=20
landClass <- raster('241243rgb-167772161.0.TIFF')
landClass[landClass==254] <- NA


s <- stack(pop, landClass)
names(s) <- c('pop', 'landClass')

densityplot(~asinh(pop)|landClass, data=s,
            scales=list(relation='free'),
            strip=strip.custom(strip.levels=TRUE))
}
}

\keyword{methods}
\keyword{spatial}


