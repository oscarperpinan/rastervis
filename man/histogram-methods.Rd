\name{histogram-methods}
\docType{methods}
\alias{histogram}
\alias{histogram,RasterLayer,missing-method}
\alias{histogram,RasterStackBrick,missing-method}
\alias{histogram,formula,Raster-method}

\title{
Histogram of Raster objects.
}
\description{Draw histograms (with \code{lattice}) of Raster objects.
}

\usage{
\S4method{histogram}{RasterLayer,missing}(x, data=NULL, maxpixels = 1e+05, nint=100, 
            xlab='', ylab='', main='', col='gray', ...)

\S4method{histogram}{RasterStackBrick,missing}(x, data=NULL, layers, FUN,
            maxpixels = 1e+05, nint=100,
            xlab='', ylab='', main='', col='gray',
            between=list(x=0.5, y=0.2),
            as.table=TRUE,
            scales=list(x=list(relation='free'),
              y=list(relation='free',
                draw=FALSE)),
            par.settings=rasterTheme(),
            ...)

\S4method{histogram}{formula,Raster}(x, data, dirXY,
            maxpixels = 1e+05,
            strip=TRUE,
            par.settings=rasterTheme(),...)

}
\arguments{
  \item{x}{A \code{Raster*} object or a \code{formula}.}
  \item{data}{\code{NULL} or a \code{Raster} object.}
  \item{layers}{A numeric or character which should indicate the layers
    to be displayed.}
  \item{dirXY}{A direction as a function of the coordinates (see
    \code{\link{xyLayer}}).}
  \item{FUN}{A function to applied to the \code{z} slot of a
            \code{RasterStackBrick} object. The result of this function
            is used as the grouping variable of the plot.}
  \item{nint}{Number of breaks for the histogram. See the documentation of
            \code{lattice::\link[lattice]{histogram}} at lattice for details.}
  \item{maxpixels}{A numeric, for \code{\link{sampleRandom}}.}
  \item{xlab, ylab, main, col}{Arguments for \code{\link{histogram}}.}
  \item{between, as.table, scales,
    strip, par.settings}{Graphical parameters of \code{lattice}. See \code{lattice::\link[lattice]{xyplot}} for details.}
  \item{\dots}{Additional arguments for \code{lattice::\link[lattice]{histogram}}}
}

\note{If you need different breakpoints in each panel, set \code{breaks}
  explicitely with \code{NULL}, a numeric or a character (for example,
  'Sturges'; see \code{\link{hist}} for details)}


\author{Oscar Perpiñán Lamigueiro}

\seealso{\code{\link[lattice]{histogram}},\code{\link{xscale.raster}},
            \code{\link{yscale.raster}}, \code{\link{rasterTheme}}
}
\examples{
f <- system.file("external/test.grd", package="raster")
r <- raster(f)
histogram(r)
s <- stack(r, r+500, r-500)
## Same breakpoints across panels
histogram(s)
## Each panel with different breakpoints
histogram(s, breaks=NULL)
histogram(s, breaks='Sturges')
histogram(s, breaks=30)


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

histogram(SISmm)
histogram(SISmm, FUN=as.yearqtr)

## With the formula interface you can create histograms for a set of variables
histogram(~ Jan + Dec, data=SISmm)
## Or use the coordinates for generating zonal histograms.
## For example, five histograms for each latitude zone  
histogram(~Jan|cut(y, 5), data=SISmm)
## More sophisticated bands can be defined using the dirXY argument 
histogram(~Jan|cut(dirXY, 5), dirXY = x^2 + y^2, data=SISmm)
 
setwd(old)
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

histogram(~asinh(pop)|landClass, data=s,
            scales=list(relation='free'),
            strip=strip.custom(strip.levels=TRUE))
}

}
\keyword{methods}
\keyword{spatial}

