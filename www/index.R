
## #+DATE: 2012-08-10
## #+CATEGORY: R
## #+TAGS: 
## #+DESCRIPTION: rasterVis
## #+TITLE: rasterVis
## #+PROPERTY:  session *R:2*
## #+PROPERTY:  tangle yes
## #+PROPERTY:  comments org
## #+LANGUAGE:  en
## #+STYLE:    <link rel="stylesheet" type="text/css" href="styles.css" />
## #+OPTIONS:   num:nil toc:1 ^:nil


## The [[http://cran.r-project.org/web/packages/raster/index.html][raster]] package defines classes and methods for spatial raster data
## access and manipulation. The =rasterVis= package complements
## raster providing a set of methods for enhanced visualization and
## interaction. The stable release of =rasterVis= can be found at
## [[http://cran.r-project.org/web/packages/rasterVis/][CRAN]]. The development version is at [[https://r-forge.r-project.org/R/?group_id%3D1129][R-Forge]].

## This page has been generated with [[http://orgmode.org/][org-mode]]. You can download the [[http://rastervis.r-forge.r-project.org/index.org][org file]] and the [[http://rastervis.r-forge.r-project.org/index.R][R code]].

## Let's show some of its functionalities with some examples, using data
## from the
## [[http://www.cmsaf.eu/bvbw/appmanager/bvbw/cmsafInternet][CMSAF]]
## project as described
## [[http://procomun.wordpress.com/2011/06/17/raster-cmsaf-and-solar/][here]]
## ([[http://www.box.net/shared/rl51y1t9sldxk54ogd44][download data]]).

library(raster)
library(rasterVis)

##change to your folder...
old <- setwd('~/Datos/CMSAF')
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

pdf(file="figs/levelplot.pdf")
levelplot(SISmm)
dev.off()

## [[file:figs/levelplot.png]]

## If only one layer is chosen, this method displays a marginal plot
## of a function across each coordinate:

pdf(file="figs/levelplot_layer1.pdf")
levelplot(SISmm, layers=1, FUN.margin=median, contour=TRUE)
dev.off()

## [[file:figs/levelplot_layer1.png]]

## The result of this call is a =trellis= object. The [[http://latticeextra.r-forge.r-project.org/][latticeExtra]] package
## provides the =layer= function to add contents. For example, let's add the administrative borders. 
## This information is available at the [[http://www.gadm.org/data/shp/ESP_adm.zip][GADM service]].

pdf(file="figs/levelplot_layer_borders.pdf")
library(maptools)
proj <- CRS('+proj=longlat +ellps=WGS84')
##Change to your folder
mapaSHP <- readShapeLines('~/Datos/ESP_adm/ESP_adm2.shp', proj4string=proj)

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

pdf(file="figs/levelplot_logscale.pdf")
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

pdf(file="figs/levelplotAug.pdf")
levelplot(Aug-meanAug, par.settings=RdBuTheme)
dev.off()

## [[file:figs/levelplotAug.png]]

## Besides, it is easy to define a new theme with a different
## palette. For example, using a sequential palette from
## [[http://cran.r-project.org/web/packages/colorspace][colorspace]]:

pdf(file="figs/levelplot_colorspace.pdf")
library(colorspace)
myTheme=rasterTheme(region=sequential_hcl(10, power=2.2))
levelplot(Aug, par.settings=myTheme, contour=TRUE)
dev.off()

## [[file:figs/levelplot_colorspace.png]]

## or with the colour-blindness corrections from the [[http://cran.r-project.org/web/packages/dichromat/][dichromat]] package:

pdf(file="figs/levelplot_dichromat.pdf")
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

pdf(file="figs/xyplot_formula.pdf")
##Relation between the January & February versus July radiation for four
##differents longitude regions.
xyplot(Jan+Feb~Jul|cut(x, 4), data=SISmm, auto.key=list(space='right'))
dev.off()

## [[file:figs/xyplot_formula.png]]

pdf(file="figs/hexbinplot_formula.pdf")
##Faster with hexbinplot
hexbinplot(Jan~Jul|cut(x, 6), data=SISmm)
dev.off()

## [[file:figs/hexbinplot_formula.png]]

## ...a method for scatter plot matrices:

pdf(file="figs/splom.pdf")
splom(SISmm)
dev.off()

## [[file:figs/splom.png]]

## ..and methods for histograms, [[http://procomun.wordpress.com/2011/04/02/violin-plot/][box-and-whisker and violin]] plots or density estimates:

pdf(file="figs/histogram.pdf")
histogram(SISmm)
dev.off()

## [[file:figs/histogram.png]]

pdf(file="figs/density.pdf")
densityplot(SISmm)
dev.off()

## [[file:figs/density.png]]

pdf(file="figs/bwplot.pdf")
bwplot(SISmm)
dev.off()

## [[file:figs/bwplot.png]]

## These methods accept a =FUN= argument to be applied to the =z= slot of
## the =Raster= object. The result of this function is used as the grouping
## variable of the plot:

pdf(file="figs/histogram_FUN.pdf")
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

## For example, the next code builds a hovmoller diagram showing the
## time evolution of the mean value along the latitude (data
## available at
## [[ftp://ftp.wiley.com/public/sci_tech_med/spatio_temporal_data/]]):

pdf(file="figs/hovmoller.pdf")
library(zoo)

url <- "~/Datos/Cressie/"
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
          yscale.components=yscale.raster.subticks,
          interpolate=TRUE, par.settings=RdBuTheme)
dev.off()

## [[file:figs/hovmoller.png]]

## The =horizonplot= and =xyplot= methods also are useful for the space-time =Raster= objects:

pdf(file="figs/horizon.pdf")
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

pdf(file="figs/vectorplot.pdf")
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

## Do not close the last graphical window.  Use the left button of the
## mouse to identify points and the right button to finish

chosen <- identifyRaster(SISmm, layer=3, values=TRUE)

## The =chooseRegion= function provides a set of points (in the form of a
## =SpatialPoints= object) inside a region defined by several mouse
## clicks. Use the left button of the mouse to build a border with points, and
## the right button to finish.  The points enclosed by the border will
## be highlighted and returned as a SpatialPoints object.

reg <- chooseRegion()
