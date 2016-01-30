\name{vectorplot-methods}
\docType{methods}

\alias{vectorplot}
\alias{vectorplot,Raster-method}
\alias{vectorplot,RasterStack-method}
\alias{streamplot}
\alias{streamplot,Raster-method}
\alias{streamplot,RasterStack-method}

\title{Vector plots of Raster objects.}
\description{

  \code{vectorplot} displays vector fields from Raster
  objects using arrows.

  \code{streamplot} displays streamlines with a procedure inspired by
  the FROLIC algorithm (see references): for each point (\emph{droplet})
  of a jittered regular grid, a short streamline portion
  (\emph{streamlet}) is calculated by integrating the underlying vector
  field at that point. The main color of each streamlet indicates local
  vector magnitude (\emph{slope}). Streamlets are composed of points
  whose sizes, positions and color degradation encode the local vector
  direction (\emph{aspect}).  }

\usage{
\S4method{vectorplot}{Raster}(object, layers,
            narrows=2e3, lwd.arrows=0.6, col.arrows='black',
            length=unit(5e-2, 'npc'),
            maxpixels=1e5, region=TRUE, margin=FALSE,  
            isField=FALSE, reverse=FALSE,
            unit='radians', scaleSlope=TRUE,
            aspX=0.08, aspY=aspX,
            key.arrow = NULL,
            ...)

\S4method{vectorplot}{RasterStack}(object, layers,
            narrows=2e3, lwd.arrows=0.6, col.arrows='black',
            length=unit(5e-2, 'npc'),
            maxpixels=1e5, region=TRUE, margin=FALSE,
            isField=FALSE, reverse=FALSE,
            unit='radians', scaleSlope=TRUE,
            aspX=0.08, aspY=aspX,
            key.arrow = NULL,
            uLayers, vLayers, ...)

\S4method{streamplot}{Raster}(object, layers,
            droplet = list(), streamlet = list(),
            par.settings=streamTheme(),
            isField = FALSE, reverse=FALSE, 
            parallel=TRUE, mc.cores=detectCores(), cl=NULL,
            ...)

\S4method{streamplot}{RasterStack}(object, layers,
            droplet = list(), streamlet = list(),
            par.settings=streamTheme(),
            isField = FALSE, reverse=FALSE, 
            parallel=TRUE, mc.cores=detectCores(), cl=NULL,
            ...)
}

\arguments{
  \item{object}{A Raster object. If \code{isField=FALSE} the vector
  field is calculated internally from the result of
  \code{\link{terrain}}. }
  \item{layers}{A numeric or character which should indicate the layers
    to be displayed.}

  \item{maxpixels}{A numeric, number of cells to be shown if
  \code{region=TRUE} or if \code{region} is a \code{Raster*} object.}

\item{narrows}{A numeric, number of arrows.}

\item{lwd.arrows}{Numeric, width of the lines of the arrows}
\item{col.arrows}{character, color of the arrows}

\item{length}{Unit, extent of the arrow head.}


\item{margin}{Logical, if TRUE two marginal graphics show the summaries
  of the object.}

\item{scaleSlope}{Logical or numeric. If TRUE the slope (vector
  magnitude) is scaled (but not centered) with its standard
  deviation. If it is a numeric, the slope is scaled with this value.
  It is not used if \code{isField='dXY'}.}

\item{aspX, aspY}{Numeric. Multipliers to convert the slope values into
  horizontal (\code{aspX}) and vertical (\code{aspY}) displacements.}


\item{key.arrow}{A reference (or legend) vector. If is not NULL, it is a list with two named
  components, \code{size}, a numeric to define the length of the arrow
  (default 1), and \code{label}, a character to define the label
  (default \code{''}).} 

\item{uLayers, vLayers}{Numeric, indexes of layers with horizontal and
  vertical components, respectively, when \code{isField='dXY'} and the
  \code{RasterStack} has more than 2 layers.  If missing, the horizontal
  components are the layers of the first half of the object, and the
  vertical components are the layers of the second half.}

\item{droplet}{A list whose elements define the droplet
  configuration:\itemize{ \item{cropExtent: Percentage of the object
  extent to be cropped (default .97) to avoid droplets at boundaries}

\item{pc: A numeric. It is the percentage of cells used to compute
droplets. Its default value is 0.5. Therefore, only the 0.5\% of the
cells are used. For example, if you use a Raster with 180 rows and 360
columns (64800 cells), with this default value \code{streamplot} will
produce 324 droplets.}}}

\item{streamlet}{A list whose elements define the streamlet
  configuration:\itemize{ \item{L: length of the streamlet (number of
      points, default 10)}
    \item{h: streamlet calculation step (default \code{mean(res(object))}).}}}

\item{par.settings}{A list to define the graphical parameters. For
  \code{streamplot} there is an specific theme, \code{streamTheme}.}

\item{parallel}{Logical, TRUE (default) to use \code{\link{parallel}}
  package.}
\item{cl}{a cluster object. Read the help page of
  \code{\link{parLapply}} for details.}

\item{mc.cores}{The number of cores to use if \code{parallel=TRUE} and
  no \code{cl} is provided. Read the help page of \code{\link{mclapply}}
  for details.}

\item{region}{Logical, if \code{TRUE} the region is displayed as the
  background  using \code{levelplot}. It can be a \code{Raster*} with
  the same extent and resolution as \code{object}.}

\item{reverse}{Logical, TRUE if arrows or streamlets go against the direction
  of the gradient.}

\item{isField}{If \code{TRUE} the object is a vector field. Thus,
  \code{object} must be a Raster* with two layers, slope (local vector
  magnitude) and aspect (local vector direction), in
  this order, following the philosophy of \code{\link{terrain}}. The
  slope layer will be used as the background if \code{region} is
  \code{TRUE}. 

  If \code{isField='dXY'} \code{object} must be a Raster* with two
  layers representing the horizontal and the vertical components,
  respectively. The slope is computed and used as the background if
  \code{region} is TRUE.
}

\item{unit}{Character, angle units of the \code{aspect} layer if
  \code{isField=TRUE}: 'radians' or 'degrees'.}

\item{\dots}{Additional arguments for \code{\link{levelplot}}}
}

\references{
    R. Wegenkittl and E. Gröller, Fast Oriented Line Integral Convolution for Vector Field
     Visualization via the Internet, Proceedings IEEE Visualization ’97, 1997, \url{http://www.cg.tuwien.ac.at/research/vis-dyn-syst/frolic/frolic_crc.pdf}
  }
  
\author{Oscar Perpiñán Lamigueiro}

\seealso{\code{\link{panel.arrows}},
  \code{\link{levelplot}},
  \code{\link{terrain}},
  \code{\link{mclapply}},
  \code{\link{parLapply}}
}

\examples{
\dontrun{
proj <- CRS('+proj=longlat +datum=WGS84')

df <- expand.grid(x=seq(-2, 2, .01), y=seq(-2, 2, .01))
df$z <- with(df, (3*x^2 + y)*exp(-x^2-y^2))
r1 <- rasterFromXYZ(df, crs=proj)
df$z <- with(df, x*exp(-x^2-y^2))
r2 <- rasterFromXYZ(df, crs=proj)
df$z <- with(df, y*exp(-x^2-y^2))
r3 <- rasterFromXYZ(df, crs=proj)
s <- stack(r1, r2, r3)
names(s) <- c('R1', 'R2', 'R3')

vectorplot(r1)
vectorplot(r2, par.settings=RdBuTheme())
vectorplot(r3, par.settings=PuOrTheme())

## scaleSlope, aspX and aspY
vectorplot(r1, scaleSlope=FALSE)
vectorplot(r1, scaleSlope=1e-5)
vectorplot(r1, scaleSlope=5e-6, alpha=0.6)
vectorplot(r1, scaleSlope=TRUE, aspX=0.1, alpha=0.6)
vectorplot(r1, scaleSlope=TRUE, aspX=0.3, alpha=0.3)


## Reference vector
# Default size (1)
vectorplot(r1, region = FALSE,
           key.arrow = list(label = 'm/s'))
vectorplot(r1, region = FALSE,
           key.arrow = list(size = 2, label = 'm/s'))

## A vector field defined with horizontal and vertical components
u <- v <- raster(xmn=0, xmx=2, ymn=0, ymx=2, ncol=1e3, nrow=1e3)
x <- init(u, v='x')
y <- init(u, v='y')
u <- y * cos(x)
v <- y * sin(x) 
field <- stack(u, v)
names(field) <- c('u', 'v')

vectorplot(field, isField='dXY', narrows=5e2)

## We can display both components as the background
vectorplot(field, isField='dXY', narrows=5e2, region=field)

## It is also possible to use a RasterStack
## with more than 2 layers when isField='dXY'
u1 <- cos(y) * cos(x)
v1 <- cos(y) * sin(x)
u2 <- sin(y) * sin(x)
v2 <- sin(y) * cos(x)
field <- stack(u, u1, u2, v, v1, v2)
names(field) <- c('u', 'u1', 'u2', 'v', 'v1', 'v2')

vectorplot(field, isField='dXY',
           narrows=300, lwd.arrows=.4,
           par.settings=BTCTheme(),
           layout=c(3, 1))

## uLayer and vLayer define which layers contain
## horizontal and vertical components, respectively
vectorplot(field, isField='dXY',
           narrows=300,
           uLayer=1:3,
           vLayer=6:4)

##################################################################
## Streamplot
##################################################################
## If no cluster is provided, streamplot uses parallel::mclapply except
## with Windows. Therefore, next code could spend a long time under
## Windows.
streamplot(r1)

## With a cluster
hosts <- rep('localhost', 4)
cl <- parallel::makeCluster(hosts)
streamplot(r2, cl=cl,
           par.settings=streamTheme(symbol=brewer.pal(n=5,
                                                      name='Reds')))
parallel::stopCluster(cl)

## Without parallel
streamplot(r3, parallel=FALSE,
           par.settings=streamTheme(symbol=brewer.pal(n=5,
                                                      name='Greens')))

## Configuration of droplets and streamlets
streamplot(s, layout=c(1, 3), droplet=list(pc=.2), streamlet=list(L=20),
           par.settings=streamTheme(cex=.6))
}

}

\keyword{methods}
\keyword{spatial}
