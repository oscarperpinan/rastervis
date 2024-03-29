\name{levelplot-methods}
\docType{methods}

\alias{levelplot}
\alias{contourplot}
\alias{levelplot,Raster,missing-method}
\alias{levelplot,SpatRaster,missing-method}
\alias{contourplot,Raster,missing-method}
\alias{contourplot,SpatRaster,missing-method}

\title{Level and contour plots of Raster objects.}
\description{Level and contour plots of Raster objects with \code{lattice} methods
  and marginal plots with \code{grid} objects.
}
\usage{
\S4method{levelplot}{Raster,missing}(x, data = NULL, layers,
            margin = list(), 
            maxpixels = 1e5, 
            par.settings = rasterTheme(),
            between = list(x=0.5, y=0.2),
            as.table = TRUE,
            xlab=if(isLonLat(x)) 'Longitude' else NULL,
            ylab=if(isLonLat(x)) 'Latitude' else NULL,
            main=NULL,
            names.attr,
            scales  =list(),
            xscale.components = xscale.raster,
            yscale.components = yscale.raster,
            zscaleLog = NULL,
            colorkey = list(space='right'),
            panel = panel.levelplot,
            pretty = FALSE, 
            contour = FALSE, region = TRUE, labels = FALSE,
            FUN.margin = NULL, 
            scales.margin = NULL, axis.margin = NULL,
            ..., att=1L)

\S4method{levelplot}{SpatRaster,missing}(x, data = NULL, layers,
            margin = list(), 
            maxpixels = 1e5, 
            par.settings = rasterTheme(),
            between = list(x=0.5, y=0.2),
            as.table = TRUE,
            xlab=if(is.lonlat(x)) 'Longitude' else NULL,
            ylab=if(is.lonlat(x)) 'Latitude' else NULL,
            main=NULL,
            names.attr,
            scales  =list(),
            xscale.components = xscale.raster,
            yscale.components = yscale.raster,
            zscaleLog = NULL,
            colorkey = list(space='right'),
            panel = panel.levelplot,
            pretty = FALSE, 
            contour = FALSE, region = TRUE, labels = FALSE,
            FUN.margin = NULL, 
            scales.margin = NULL, axis.margin = NULL,
            ..., att=1L)

\S4method{contourplot}{Raster,missing}(x, data=NULL, layers, ...)
}

\arguments{
  
  \item{x}{A Raster or SpatRaster object.}

  \item{data}{Not used.}

  \item{layers}{A numeric or character which should indicate the layers
    to be displayed.}

  \item{maxpixels}{A positive integer giving the number of cells to
    display, for \code{\link{sampleRegular}}.
  }

  \item{margin}{A list or a logical. If it is TRUE, two marginal
    graphics show the column (x) and row (y) summaries of the
    \code{Raster*} object. The summary is computed with the function
    \code{mean}. If it is a list, it contains parameters in \code{name=value}
    form that define both margins, and may contain two other lists
    called \code{x} and \code{y} whose components affect the respective margins
    only: \itemize{

      \item draw: A logical. If TRUE (default) the marginal graphics are
      drawn.

      \item FUN: A function to summarise the \code{Raster*} or
	\code{SpatRaster} by rows and columns (default: \code{mean}).

      \item scales: A list with components \code{x} (columns) and
	\code{y} (rows). Each of these components must be a numeric
	vector of length 2 defining the range for each marginal plot.
	If \code{scales = NULL} (default) the range is internally
	computed.  If any of the elements of the vectors is \code{NA},
	the corresponding limit of the range will be calculated
	internally.  If any of the vectors is of length 1, it is assumed
	that it defines the lower limit of the range, and the upper
	limit is calculated internally. 

      \item axis: Logical or a list. Its default value is
	\code{FALSE}. If it is \code{TRUE} or a list, a simple axis is
	drawn with the marginal graphic.  If it is a list, its
	components define the graphical parameters of the axis using
	\code{grid::\link{gpar}}.  The default value is \code{gpar(col =
	  'darkgrey', fontsize = 7)}

    }
  }
  
  \item{FUN.margin, scales.margin, axis.margin}{Deprecated
    arguments. Use \code{margin} as a list instead}
  
  \item{att}{Integer or character to choose which variable (column) in
    the RAT table should be used.}
  
  \item{xlab, ylab, main}{A character string or expression describing
    the axis and title labels. These arguments are used by the
    underlying \code{lattice::\link[lattice]{xyplot}} function, which
    provides this information in its help page:
    
    \dQuote{\code{main}, \code{xlab} and \code{ylab} are usually a
      character string or an expression that gets used as the label, but
      can also be a list that controls further details.  Expressions are
      treated as specification of LaTeX-like markup as described in
      \code{\link{plotmath}}.  The label can be a vector, in which case
      the components will be spaced out horizontally (or vertically for
      \code{ylab}).  This feature can be used to provide column or row
      labels rather than a single axis label.
      
      When \code{main} (etc.) is a list, the actual label should
      be specified as the \code{xlab} component (which may be
      unnamed if it is the first component).  The label can be
      missing, in which case the default will be used.  Further
      named arguments are passed on to \code{\link{textGrob}}; this can
      include arguments controlling positioning like \code{just} and
      \code{rot} as well as graphical parameters such as \code{col} and
      \code{font} (see \code{\link{gpar}} for a full list).
      
      \code{main}, \code{xlab} and \code{ylab} can also be
      arbitrary "grobs" (grid graphical objects).}
  }
  
  
  \item{names.attr}{Character or expression, names to use in each
    panel. If missing its default value is the result of \code{names(x)}
    (after subsetting the layers to be displayed).}
  
  \item{xscale.components, yscale.components}{See
    \code{\link{xscale.raster}} and \code{\link{yscale.raster}}.}

  \item{colorkey}{logical specifying whether a color key is to be drawn
    alongside the plot (default is \code{TRUE}), or a list describing
    the color key (see the \code{colorkey} section of the help page of
    \code{lattice::levelplot} for details).

    It accepts a \code{title} argument to include the title of the color
    key. It can be a \code{character} vector or expression providing the
    text, or a list with the text and graphical parameters. In this
    case, the text must be the \code{label} component of the list (or
    unnamed if it is the first component). The other components of the
    list are the graphical parameters and are passed to the
    \code{grid::\link{gpar}} function (for example, \code{col},
    \code{fontfamily}, \code{fontface}, etc.). Besides, it may include a
    component named \code{title.control}: (extracted from
    \code{lattice::\link[lattice]{levelplot}}): A list providing control
    over the placement of a title, if specified. Currently two
    components are honoured: \code{side} can take values "top",
    "bottom", "left", and "right", and specifies the side of the
    colorkey on which the title is to be placed. Defaults to the value
    of the "space" component; \code{padding} is a multiplier for the
    default amount of padding between the title and the colorkey.

    The color key will always be drawn at the bottom if the marginal
    graphics are also displayed. If you need to change the location of
    the color key, disable the marginal graphics with \code{margin =
    FALSE}.}
  
  \item{between, as.table, par.settings, scales, panel}{Graphical
    parameters used by \code{lattice::\link[lattice]{xyplot}}. Adapted
    from the help page of this function: \itemize{
      
      \item between: A list with components \code{x} and
        \code{y} (both usually 0 by default), numeric vectors
        specifying the space between the panels (units are character
        heights). \code{x} and \code{y} are repeated to account for all
        panels in a page and any extra components are ignored.
      
      \item as.table: A logical flag that controls the order in which
        panels should be displayed: if \code{TRUE} (the default), left
        to right, top to bottom (as in a table).  If \code{FALSE}
        panels are drawn left to right, bottom to top.

      \item par.settings: A list to choose some display settings
        temporarily. This list is supplied to
        \code{\link{trellis.par.set}}. When the resulting object
        is plotted, these options are applied temporarily for the
        duration of the plotting, after which the settings revert
        back to what they were before.  This enables the user to
        attach some display settings to the trellis object itself
        rather than change the settings globally.
	
	\code{rasterVis} includes some functions with predefined
	themes that can be directly supplied to \code{par.settings}:
	\code{\link{rasterTheme}} (default), \code{\link{RdBuTheme}} and
	\code{\link{BuRdTheme}}, \code{\link{GrTheme}},
	\code{\link{BTCTheme}}, \code{\link{PuOrTheme}} and
	\code{\link{streamTheme}} (for \code{\link{streamplot}}).  These
	themes are defined using \code{\link{custom.theme}}. You can use
	\code{\link{rasterTheme}} or \code{\link{custom.theme}} to
	define your own theme (see examples for details).
      

      \item scales: A list determining how the x- and y-axes (tick
          marks and labels) are drawn.  The list contains parameters in
          \code{name=value} form, and may also contain two other lists
          called \code{x} and \code{y} of the same form. Components of
          \code{x} and \code{y} affect the respective axes only, while
          those in \code{scales} affect both.  When parameters are
          specified in both lists, the values in \code{x} or \code{y}
          are used. For example, use \code{scales=list(draw=FALSE)} to
          suppress ticks and labels in both axis. Read the help page of
          \code{lattice::\link[lattice]{xyplot}} to know about the
          possible components of \code{scales}.

      \item panel: A function object or a character string giving the
          name of a predefined function. The default is
          \code{\link{panel.levelplot}}. Another useful option is
          \code{\link{panel.levelplot.raster}}.

    }}
    
         
      \item{pretty, contour, region, labels}{Graphical parameters
	supplied to \code{lattice::\link[lattice]{levelplot}}.  Adapted
	from its help page: \itemize{

	\item pretty: A logical flag, indicating whether to use pretty
          cut locations and labels. It is \code{FALSE} for
          \code{levelplot} and \code{TRUE} for \code{contourplot}.

	\item contour: A logical flag, indicating whether to draw
	contour lines. It is \code{TRUE} for \code{contourplot} and
	\code{FALSE} for \code{levelplot}.
	
	\item region: A logical flag, indicating whether regions
          between contour lines should be filled as in a level plot. It
          is \code{FALSE} for \code{contourplot} and \code{TRUE} for
          \code{levelplot}.
	
	\item labels: Typically a logical indicating whether the
          labels are to be drawn (default is \code{TRUE} for
          \code{contourplot}), a character or expression vector giving
          the labels associated with the \code{at} values, or a list
          whose components define the labels and their graphical
          parameters. Read the help page of
          \code{\link{panel.levelplot}} for details.}
      }
    
  \item{zscaleLog}{Controls whether the Raster* will be log transformed
    before being passed to the panel function.  Defaults to \code{NULL},
    in which case the \code{Raster*} is not transformed.  Other possible
    values are any number that works as a base for taking logarithm,
    \code{TRUE} (which is equivalent to 10), \code{"e"} (for the natural
    logarithm), and \code{FALSE} (that is equivalent to \code{NULL}).
    As a side effect, the colorkey is labeled differently.  }

  \item{\dots}{Additional arguments for
    \code{lattice::\link[lattice]{levelplot}},
    \code{lattice::\link[lattice]{xyplot}},
    \code{\link{panel.levelplot}} and
    \code{\link{panel.levelplot.raster}}. The most important ones are:
    \itemize{

      \item layout: From the help page of
        \code{lattice::\link[lattice]{xyplot}}: \code{layout} is a
        numeric vector of length 2 or 3 giving the number of
        columns, rows, and pages (optional) in a multipanel
        display.  The number of pages is by default set to as many
        as is required to plot all the panels, and so rarely needs
        to be specified.
	
	For example, with \code{layout=c(1, 1)} each panel
        (corresponding to a layer of a \code{RasterStackBrick})
        object will be printed in a separate page (which could be
        useful to generate a series of output files to build an
        animation.)

      \item xlim, ylim: From the help page of
        \code{lattice::\link[lattice]{xyplot}}: A numeric vector
        of length 2 giving left and right limits for x-axis, and
        lower and upper limits for the y-axis.
      
      \item shrink: From the help page of
        \code{\link{panel.levelplot}}: Either a numeric vector of
        length 2 (meant to work as both x and y components), or a list
        with components x and y which are numeric vectors of length 2.
        This allows the rectangles to be scaled proportional to the
        z-value. The specification can be made separately for widths
        (x) and heights (y).  The elements of the length 2 numeric
        vector gives the minimum and maximum proportion of shrinkage
        (corresponding to min and max of z).
      
      \item border, border.lty, border.lwd: Graphical parameters
	(color, type of line, width of line, respectively) of each
	rectangle borders. See the help page of
	\code{\link{panel.levelplot}} for details.
    }
  }
}

\details{

  The result of the \code{levelplot} method is similar to the
  \code{spplot} method for \code{Raster} objects defined in the
  \code{raster} package. However, this method does not use the
  \code{spplot} of the \code{sp} package and, therefore, no conversion
  between classes is needed.
  
  The \code{contourplot} method is a wrapper for \code{levelplot} with
  the next additional default settings: \code{cuts=7},
  \code{labels=TRUE}, \code{contour}=TRUE, \code{pretty}=TRUE,
  \code{region=TRUE} and \code{colorkey=TRUE} (see the help of
  \code{\link{contourplot}} for details.)
  
  \code{levelplot} displays categorical data with a convenient
  legend. You should use \code{\link{ratify}} to define a layer as a
  categorical variable. It is able to display multilayer categorical
  rasters \bold{only if} they share the same RAT (Raster Attribute
  Table). \code{levelplot} is not able to display multilayer rasters
  with factor \bold{and} numeric layers. See \code{\link{ratify}} and the
  examples below for details.
}
  
\author{Oscar Perpiñán Lamigueiro}

\seealso{
  \code{\link[sp]{spplot}},
  \code{lattice::\link[lattice]{levelplot}}
}

\examples{
library(raster)
library(terra)

f <- system.file("external/test.grd", package="raster")

## raster
r <- raster(f)
levelplot(r)
## terra
rt <- rast(f)
levelplot(rt)

## Change the color theme
levelplot(r, par.settings=GrTheme())

\dontrun{
myPal <- RColorBrewer::brewer.pal('Blues', n=9)
myTheme <- rasterTheme(region = myPal)
levelplot(r, par.settings = myTheme)
}
## Define the legend breaks
my.at <- seq(100, 1850, 500)
levelplot(rt, at=my.at)

myColorkey <- list(at=my.at, ## where the colors change
                   labels=list(
                     at=my.at ## where to print labels
                     ))
levelplot(r, at=my.at, colorkey=myColorkey)

## Define the units of the color key
levelplot(r, margin = FALSE,
             colorkey = list(title = list("[m]",
                                          cex = 1,
                                          fontface = "bold",
                                          col = "red")
                             ))


levelplot(r, margin = FALSE,
          colorkey = list(title = "[m]",
                          space = "left",
                          title.control = list(side = "bottom")))

## shrink and border color
## raster
rCenter <- (maxValue(r) + minValue(r)) / 2
levelplot(r - rCenter, par.settings=RdBuTheme(), shrink=c(.8, 15), border='black')
\dontrun{
## terra
tCenter <- mean(minmax(rt))
levelplot(rt - tCenter, par.settings=RdBuTheme(), shrink=c(.8, 15), border='black')
}

## With subticks
levelplot(r, xscale.components=xscale.raster.subticks,
             yscale.components=yscale.raster.subticks)

\dontrun{
levelplot(rt, xscale.components=xscale.raster.subticks,
             yscale.components=yscale.raster.subticks,
             scales=list(x=list(rot=30, cex=0.8)))
}
## log-scale
levelplot(r^2, zscaleLog=TRUE, contour=TRUE)

## Customizing axis and title labels
levelplot(rt, margin=FALSE,
          main=list('My plot', col='red'),
          xlab=c('This is the', 'X-Axis'),
          ylab=list('Y-Axis', rot=30, fontface='bold')
          )

## xlim and ylim to display a smaller region
levelplot(r, xlim=c(179000, 181000), ylim=c(329500, 334000))

## RasterStacks
s <- stack(r, r+500, r-500)
levelplot(s, contour=TRUE)
contourplot(s, labels=list(cex=0.4), cuts=12)

\dontrun{
## Use of layout
levelplot(s, layout=c(1, 3))

## SpatRaster with several layers
st <- c(rt, rt + 500, rt - 500)

## c() assign the same name for all the layers,
## but each layer needs a unique name for levelplot
set.names(st, c("r0", "rp500", "rm500"))

levelplot(st, contour=TRUE)
contourplot(st, labels=list(cex=0.4), cuts=12)

levelplot(st, layout=c(1, 1)) # useful for animations
}

## names.attr to change the labels of each panel
levelplot(s, names.attr=c('R', 'R + 500', 'R - 500'))

## Defining the scales for the marginal plot
levelplot(r, margin = list(axis = TRUE,
                           scales = list(x=c(100, 600),
                                         y=c(100, 1000))))
## if a component of the list is null, it is internally calculated
levelplot(rt, margin=list(axis = TRUE, scales = list(x=c(100, 1000))))

## Add a layer of sampling points
## and change the theme
pts <- sampleRandom(r, size=20, sp=TRUE)



## Using +.trellis and layer from latticeExtra
library(latticeExtra)

levelplot(r, par.settings = BTCTheme) +
   layer(sp.points(pts, col = 'red'))
contourplot(r, labels = FALSE) +
   layer(sp.points(pts, col = 'red'))

## or with a custom panel function
levelplot(r, par.settings=BTCTheme,
          panel=function(...){
            panel.levelplot(...)
            sp.points(pts, col='red')
            })


## Categorical data
r <- raster(nrow=10, ncol=10)
r[] = 1
r[51:100] = 3
r[3:6, 1:5] = 5
r <- ratify(r)
     
rat <- levels(r)[[1]]
rat$landcover <- c('Pine', 'Oak', 'Meadow')
rat$class <- c('A1', 'B2', 'C3')
levels(r) <- rat
r

levelplot(r, col.regions=c('palegreen', 'midnightblue', 'indianred1'))

## with 'att' you can choose another variable from the RAT
levelplot(r, att=2, col.regions=c('palegreen', 'midnightblue', 'indianred1'))
levelplot(r, att='class', col.regions=c('palegreen', 'midnightblue', 'indianred1'))

r2 <- raster(r)
r2[] = 3
r2[51:100] = 1
r2[3:6, 1:5] = 5

r3 <- raster::init(r, function(n)sample(c(1, 3, 5), n, replace=TRUE))

## Multilayer categorical Raster* are displayed only if their RATs are the same
levels(r2) <- levels(r3) <- levels(r)

s <- stack(r, r2, r3)
names(s) <- c('A', 'B', 'C')

levelplot(s)
levelplot(s, att=2)

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

levelplot(SISmm)

levelplot(SISmm, layers=1, margin = list(FUN = 'median'), contour=TRUE)

setwd(old)
}}
\keyword{methods}
\keyword{spatial}
