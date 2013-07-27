\name{levelplot-methods}
\docType{methods}

\alias{levelplot}
\alias{contourplot}
\alias{levelplot,Raster,missing-method}
\alias{contourplot,Raster,missing-method}
\title{Level and contour plots of Raster objects.}
\description{Level and contour plots of Raster objects with \code{lattice} methods
  and marginal plots with \code{grid} objects.
}
\usage{
\S4method{levelplot}{Raster,missing}(x, data=NULL, layers,
            margin=!(any(is.factor(x))), FUN.margin=mean, 
            maxpixels=1e5, 
            par.settings=rasterTheme(),
            between=list(x=0.5, y=0.2),
            as.table=TRUE,
            xlab=if(isLonLat(x)) 'Longitude' else NULL,
            ylab=if(isLonLat(x)) 'Latitude' else NULL,
            main=NULL,
            names.attr,
            scales=list(), scales.margin=NULL,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            zscaleLog=NULL,
            colorkey=list(space='right'),
            panel=panel.levelplot,
            contour=FALSE, region=TRUE, labels=FALSE,
            ..., att=1L)

\S4method{contourplot}{Raster,missing}(x, data=NULL, layers, ...)
}

\arguments{
  
  \item{x}{A Raster object.}

  \item{data}{Not used.}

  \item{layers}{A numeric or character which should indicate the layers
    to be displayed.}

  \item{margin}{logical, if TRUE a marginal plot of the
    \code{FUN.margin} function to each coordinate will be displayed.}

  \item{FUN.margin}{A function to be applied to each coordinate.}

  \item{scales.margin}{A list with components \code{x} and
    \code{y}. Each of these components is a numeric vector of length 2
    defining the range for each marginal plot. If
    \code{scales.margin=NULL} the range is internally computed.}

  \item{maxpixels}{A positive integer giving the number of cells to
    display, for \code{\link{sampleRegular}}.
  }

  \item{att}{An integer o character to choose which variable (column) in the RAT table
    should be used.}

  \item{xlab, ylab, main}{A character string or expression describing
  the axis and title labels. These arguments are used by the underlying
  \code{lattice::\link[lattice]{xyplot}} function, which provides this
  information in its help page:

  \dQuote{\code{main}, \code{xlab} and \code{ylab} are usually a
  character string or an expression that gets used as the
  label, but can also be a list that controls further
  details.  Expressions are treated as specification of
  LaTeX-like markup as described in \code{\link{plotmath}}.  The label
  can be a vector, in which case the components will be
  spaced out horizontally (or vertically for \code{ylab}).  This
  feature can be used to provide column or row labels rather
  than a single axis label.

  When \code{main} (etc.) is a list, the actual label should
  be specified as the \code{xlab} component (which may be
  unnamed if it is the first component).  The label can be
  missing, in which case the default will be used.  Further
  named arguments are passed on to \code{\link{textGrob}}; this can
  include arguments controlling positioning like \code{just} and
  \code{rot} as well as graphical parameters such as \code{col} and
  \code{font} (see \code{\link{gpar}} for a full list).
  
  \code{main}, \code{xlab} and \code{ylab} can also be
  arbitrary ‘"grob"’s (grid graphical objects).}
  }


  \item{names.attr}{Character, names to use in each panel. If missing its
    default value is the result of \code{names(x)} (after subsetting
    the layers to be displayed).}

  \item{xscale.components, yscale.components}{See
    \code{\link{xscale.raster}} and \code{\link{yscale.raster}}.}

  \item{between, as.table, par.settings, scales, panel}{Graphical parameters used by
    \code{lattice::\link[lattice]{xyplot}}. Adapted from the help page
    of this function: \itemize{

      \item{between: }{A list with components \code{code} and \code{code} (both usually 0 by
        default), numeric vectors specifying the space between the
        panels (units are character heights). \code{x} and \code{y} are
        repeated to account for all panels in a page and any extra
        components are ignored.}
      
      \item{as.table: }{A logical flag that controls the order in which
        panels should be displayed: if \code{TRUE} (the default), left
        to right, top to bottom (as in a table).  If \code{FALSE}
        panels are drawn left to right, bottom to top.}

      \item{par.settings: }{A list to choose some display settings
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
      }

      \item{scales: }{A list determining how the x- and y-axes
          (tick marks and labels) are drawn.  The list contains
          parameters in \code{name=value} form, and may also contain two
          other lists called \code{x} and \code{y} of the same
          form. Components of \code{x} and \code{y} affect the
          respective axes only, while those in \code{scales} affect both.
          When parameters are specified in both lists, the values in \code{x}
          or \code{y} are used. For example, use
          \code{scales=list(draw=FALSE)} to suppress ticks and labels in
          both axis. Read the help page of
          \code{lattice::\link[lattice]{xyplot}} to know about the
          possible components of \code{scales}.}

      \item{panel: }{A function object or a character string giving the
          name of a predefined function. The default is
          \code{\link{panel.levelplot}}. Another useful option is
          \code{\link{panel.levelplot.raster}}.}

      }}

      \item{colorkey, contour, region, labels}{Graphical parameters
	supplied to \code{lattice::\link[lattice]{levelplot}}. Adapted
      from its help page:
      \itemize{
	
	\item{colorkey: }{logical specifying whether a color key is to
          be drawn alongside the plot (default is \code{TRUE}), or a
          list describing the color key.}

	\item{contour: }{A logical flag, indicating whether to draw
	contour lines. It is \code{TRUE} for \code{contourplot} and
	\code{FALSE} for \code{levelplot}.}
	
	\item{region: }{A logical flag, indicating whether regions between contour
          lines should be filled as in a level plot. It is \code{FALSE} for \code{contourplot} and
	\code{TRUE} for \code{levelplot}.}
	
	\item{labels: }{Tipically a logical indicating whether the
          labels are to be drawn (default is \code{TRUE} for
          \code{contourplot}), a character or expression vector giving
          the labels associated with the \code{at} values, or a list
          whose components define the labels and their graphical
          parameters. Read the help page of
          \code{\link{panel.levelplot}} for details.}
      }}
    
  \item{zscaleLog}{Controls whether the Raster* will be log
    transformed before being passed to the
    panel function.  Defaults to ‘NULL’, in which case the
    Raster* is not transformed.  Other possible values are any
    number that works as a base for taking logarithm, ‘TRUE’
    (which is equivalent to 10), and ‘"e"’ (for the natural
    logarithm).  As a side effect, the colorkey is
    labeled differently.
  }

  \item{\dots}{Additional arguments for
    \code{lattice::\link[lattice]{levelplot}},
    \code{lattice::\link[lattice]{xyplot}},
    \code{\link{panel.levelplot}} and
    \code{\link{panel.levelplot.raster}}. The most important ones are:
    \itemize{

      \item{layout: }{From the help page of
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
        animation.)}  \item{xlim, ylim: }{From the help page of
        \code{lattice::\link[lattice]{xyplot}}: A numeric vector
        of length 2 giving left and right limits for x-axis, and
        lower and upper limits for the y-axis.}
      
      \item{shrink: }{From the help page of
        \code{\link{panel.levelplot}}: Either a numeric vector of
        length 2 (meant to work as both x and y components), or a list
        with components x and y which are numeric vectors of length 2.
        This allows the rectangles to be scaled proportional to the
        z-value. The specification can be made separately for widths
        (x) and heights (y).  The elements of the length 2 numeric
        vector gives the minimum and maximum proportion of shrinkage
        (corresponding to min and max of z).}
      
      \item{border, border.lty, border.lwd: }{Graphical parameters
	(color, type of line, width of line, respectively) of each
	rectangle borders. See the help page of
	\code{\link{panel.levelplot}} for details.
      }
  }}
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
f <- system.file("external/test.grd", package="raster")
r <- raster(f)
levelplot(r)

## Change the color theme
levelplot(r, par.settings=GrTheme())
levelplot(r, par.settings=PuOrTheme())

myTheme=rasterTheme(region=brewer.pal('Blues', n=9))
levelplot(r, par.settings=myTheme)

## Define the legend breaks
my.at <- seq(100, 1850, 500)
levelplot(r, at=my.at)

myColorkey <- list(at=my.at, ## where the colors change
                   labels=list(
                     at=my.at ## where to print labels
                     ))
levelplot(r, at=my.at, colorkey=myColorkey)

myColorkey <- list(at=my.at, ## where the colors change
                   labels=list(
                     labels=letters[seq_along(my.at)], ## labels
                     at=my.at ## where to print labels
                     ))
levelplot(r, at=my.at, colorkey=myColorkey)

## shrink and border color
rCenter <- (maxValue(r) + minValue(r)) / 2
levelplot(r - rCenter, par.settings=RdBuTheme(), shrink=c(.8, 15), border='black')

## With subticks
levelplot(r, xscale.components=xscale.raster.subticks,
             yscale.components=yscale.raster.subticks)

levelplot(r, xscale.components=xscale.raster.subticks,
             yscale.components=yscale.raster.subticks,
             scales=list(x=list(rot=30, cex=0.8)))

## log-scale
levelplot(r^2, zscaleLog=TRUE, contour=TRUE)

## Customizing axis and title labels
levelplot(r, margin=FALSE,
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

## Use of layout
levelplot(s, layout=c(1, 3))
levelplot(s, layout=c(1, 1))

## names.attr to change the labels of each panel
levelplot(s, names.attr=c('R', 'R + 500', 'R - 500'))

## defining the scales for the marginal plot
levelplot(r, scales.margin=list(x=c(100, 600), y=c(100, 1000)))
## if a component of the list is null, it is internally calculated
levelplot(r, scales.margin=list(x=c(100, 1000)))

## Add a layer of sampling points
## and change the theme
pts <- sampleRandom(r, size=20, sp=TRUE)

## Using +.trellis and layer from latticeExtra
levelplot(r, par.settings = BTCTheme) + layer(sp.points(pts, col = 'red'))
contourplot(r, labels = FALSE) + layer(sp.points(pts, col = 'red'))

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

r3 <- init(r, function(n)sample(c(1, 3, 5), n, replace=TRUE))

## Multilayer categorical Raster* are displayed only if their RATs are the same
levels(r2) <- levels(r3) <- levels(r)

s <- stack(r, r2, r3)
names(s) <- c('A', 'B', 'C')

levelplot(s)
levelplot(s, att=2)

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

levelplot(SISmm, layers=1, FUN.margin=median, contour=TRUE)
}}
\keyword{methods}
\keyword{spatial}
