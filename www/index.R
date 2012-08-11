
## #+DATE: 2012-08-10
## #+CATEGORY: R
## #+TAGS: 
## #+DESCRIPTION: rasterVis
## #+TITLE: rasterVis
## #+PROPERTY:  tangle t
## #+PROPERTY:  comments org
## #+LANGUAGE:  en
## #+STYLE:    <link rel="stylesheet" type="text/css" href="styles.css" />
## #+OPTIONS:   num:nil toc:1


## The [[http://cran.r-project.org/web/packages/raster/index.html][raster]] package defines classes and methods for spatial raster data
## access and manipulation. The =rasterVis= package complements
## raster providing a set of methods for enhanced visualization and
## interaction. The stable release of =rasterVis= can be found at
## [[http://cran.r-project.org/web/packages/rasterVis/][CRAN]]. The development version is at [[https://r-forge.r-project.org/R/?group_id%3D1129][R-Forge]].

## This page has been generated with [[http://orgmode.org/][org-mode]]. You can download the [[http://rastervis.r-forge.r-project.org/index.org][org file]] and the [[http://rastervis.r-forge.r-project.org/index.r][R code]].

## Let's show some of its functionalities with some examples, using data
## from the
## [[http://www.cmsaf.eu/bvbw/appmanager/bvbw/cmsafInternet][CMSAF]]
## project as described
## [[http://procomun.wordpress.com/2011/06/17/raster-cmsaf-and-solar/][here]]
## ([[http://www.box.net/shared/rl51y1t9sldxk54ogd44][download data]]).

library(raster)
library(rasterVis)

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

## Level plots
##   :PROPERTIES:
##   :CUSTOM_ID: levelplot
##   :END:

## The first step is to display the content with a =levelplot=:

png(filename="figs/levelplot.png")
levelplot(SISmm)
dev.off()

## If only one layer is chosen, this method displays a marginal plot
## of a function across each coordinate:

png(filename="figs/levelplot_layer1.png")
levelplot(SISmm, layers=1, FUN.margin=median, contour=TRUE)
dev.off()

## The result of this call is a =trellis= object. The [[http://latticeextra.r-forge.r-project.org/][latticeExtra]] package
## provides the =layer= function to add contents. For example, let's add the administrative borders. 
## This information is available [[http://biogeo.ucdavis.edu/data/diva/adm/ESP_adm.zip][here]]:

png(filename="figs/levelplot_layer_borders.png")
library(maptools)
proj <- CRS('+proj=latlon +ellps=WGS84')
##Change to your folder
mapaSHP <- readShapeLines('ESP_adm/ESP_adm2.shp', proj4string=proj)

p <- levelplot(SISmm, layers=1, FUN.margin=median)
p + layer(sp.lines(mapaSHP, lwd=0.8, col='darkgray'))
dev.off()

## Log scale
##    :PROPERTIES:
##    :CUSTOM_ID: levelplot_logscale
##    :END:

## The =zscaleLog= argument controls whether the object will be log
## transformed before being passed to the panel function.  Defaults to
## ‘NULL’, in which case the Raster* is not transformed.  Other possible
## values are any number that works as a base for taking logarithm,
## ‘TRUE’ (which is equivalent to 10), and ‘"e"’ (for the natural
## logarithm).  As a side effect, the colorkey is labeled differently.

png(filename="figs/levelplot_logscale.png")
f <- system.file("external/test.grd", package="raster")
r <- raster(f)
levelplot(r^2, zscaleLog=TRUE, contour=TRUE)
dev.off()

## Themes
##   :PROPERTIES:
##   :CUSTOM_ID: themes
##   :END:

## The previous plots used the default theme of rasterVis,
## =rasterTheme=. This theme defines a sequential palette with yellow,
## orange and red. There are three more themes in =rasterVis=: =GrTheme=
## (with a grey palette), =BTCTheme= (defined with the =BTC= palette of
## the =hexbin= package) and =RdBuTheme= (with a diverging palette with
## red and blue). 

## The irradiation of August is:

Aug <- raster(SISmm, 8)

## and its overall mean is calculated with cellStats:

meanAug <- cellStats(Aug, mean)

## The diverging palette is specially well suited to this data:

png(filename="figs/levelplotAug.png")
levelplot(Aug-meanAug, par.settings=RdBuTheme)
dev.off()

## Besides, it is easy to define a new theme with a different
## palette. For example, using a sequential palette from
## [[http://cran.r-project.org/web/packages/colorspace][colorspace]]:

png(filename="figs/levelplot_colorspace.png")
library(colorspace)
myTheme=rasterTheme(region=sequential_hcl(10, power=2.2))
levelplot(Aug, par.settings=myTheme, contour=TRUE)
dev.off()

## or with the colour-blindness corrections from the [[http://cran.r-project.org/web/packages/dichromat/][dichromat]] package:

png(filename="figs/levelplot_dichromat.png")
library(dichromat)
myTheme <- rasterTheme(region=dichromat(terrain.colors(15)))
levelplot(Aug, par.settings=myTheme)
dev.off()

## Scatterplots and histograms
##   :PROPERTIES:
##   :CUSTOM_ID: scatterplot
##   :END:

## There are methods to show scatter plots and hexbin plots of the layers
## and coordinates of a =Raster= object:

png(filename="figs/xyplot_formula.png")
##Relation between the January & February versus July radiation for four
##differents longitude regions.
xyplot(Jan+Feb~Jul|cut(x, 4), data=SISmm, auto.key=list(space='right'))
dev.off()

png(filename="figs/hexbinplot_formula.png")
##Faster with hexbinplot
hexbinplot(Jan~Jul|cut(x, 6), data=SISmm)
dev.off()

## ...a method for scatter plot matrices:

png(filename="figs/splom.png")
splom(SISmm)
dev.off()

## ..and methods for histograms, [[http://procomun.wordpress.com/2011/04/02/violin-plot/][box-and-whisker and violin]] plots or density estimates:

png(filename="figs/histogram.png")
histogram(SISmm)
dev.off()

png(filename="figs/density.png")
densityplot(SISmm)
dev.off()

png(filename="figs/bwplot.png")
bwplot(SISmm)
dev.off()

## These methods accept a =FUN= argument to be applied to the =z= slot of
## the =Raster= object. The result of this function is used as the grouping
## variable of the plot:

png(filename="figs/histogram_FUN.png")
histogram(SISmm, FUN=as.yearqtr)
dev.off()

## Space-time plots
##   :PROPERTIES:
##   :CUSTOM_ID: spacetime
##   :END:

## The =z= slot of this =Raster= object stores a time index. This 3D
## space-time =Raster= object can be displayed with a [[http://en.wikipedia.org/wiki/Hovmoller_diagram][hovmoller diagram]].

## The =hovmoller= method uses the function =xyLayer=, which creates a
## =RasterLayer= from a function of the coordinates.

f <- system.file("external/test.grd", package="raster")
r <- raster(f)
dirXY <-xyLayer(r, sqrt(x^2 + y^2))
dirXY

## For example, the next code builds a hovmoller diagram showing the time
## evolution of the mean value along the latitude:

png(filename="figs/hovmoller.png")
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
layerNames(SST) <- as.character(idx)
hovmoller(SST, contour=FALSE, panel=panel.levelplot.raster,
          interpolate=TRUE, par.settings=RdBuTheme)
dev.off()

## The =horizonplot= and =xyplot= methods also are useful for the space-time =Raster= objects:

png(filename="figs/horizon.png")
horizonplot(SST)
dev.off()

## Vector field plots
##   :PROPERTIES:
##   :CUSTOM_ID: vectorplot
##   :END: 

## The function =slopeAspect= from =raster= provides the vector field
## (gradient) from a scalar field stored in a =RasterLayer= object. The
## magnitude (slope) and direction (aspect) of the vector field is
## usually displayed with a set of arrows (e.g. =quiver= in Matlab).

## =rasterVis= includes a method, =vectorplot=, to calculate and display
## this vector field. This method is not restricted to the =RasterLayer=
## class.

png(filename="figs/vectorplot.png")
df <- expand.grid(x=seq(-2, 2, .1), y=seq(-2, 2, .1))
df$z <- with(df, (3*x^2 + y)*exp(-x^2-y^2))

r <- rasterFromXYZ(df)
projection(r) <- CRS("+proj=longlat +datum=WGS84")

vectorplot(r, par.settings=RdBuTheme)
dev.off()

## Interaction
##   :PROPERTIES:
##   :CUSTOM_ID: interaction
##   :END:

## This package includes two functions to interact with the =trellis= objects. 

## The =identifyRaster= method labels and returns points of a trellis graphic
## according to mouse clicks. It is commonly used after =levelplot=,
## although it can be also used after =xyplot=, =hexbinplot= or even =splom=:

levelplot(SISmm)

##Do not close the last graphical window.  Use the left button of the
##mouse to identify points and the right button to finish

chosen <- identifyRaster(SISmm, layer=3, values=TRUE)
chosen

## The =chooseRegion= function provides a set of points (in the form of a
## =SpatialPoints= object) inside a region defined by several mouse clicks:

##Use the left button of the mouse to build a border with points, and
##the right button to finish.  The points enclosed by the border will
##be highlighted and returned as a SpatialPoints object.
reg <- chooseRegion()
summary(reg)
