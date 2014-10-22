
## Installation 

## The stable release of =rasterVis= can be found at [[http://cran.r-project.org/web/packages/rasterVis/][CRAN]].  The
## development version is at [[https://github.com/oscarperpinan/rastervis][GitHub]].

## Install the stable version with:

install.packages('rasterVis')

## To install the development version you need the devtools package:

install.packages('devtools')
devtools::install_github('rasterVis', 'oscarperpinan')

## Level plots
##   :PROPERTIES:
##   :CUSTOM_ID: levelplot
##   :END:

## This section discusses how to display quantitative data with
## =levelplot= with an example using data from the [[http://dx.doi.org/10.5676/EUM_SAF_CM/RAD_MVIRI/V001][CM SAF]] project, as
## described [[http://procomun.wordpress.com/2011/06/17/raster-cmsaf-and-solar/][here]].

  library(raster)
  library(rasterVis)
  
  ##Solar irradiation data from CMSAF 
  setwd(tempdir())
  download.file('https://raw.github.com/oscarperpinan/spacetime-vis/master/data/SISmm2008_CMSAF.zip',
                'SISmm2008_CMSAF.zip', method='wget')
  unzip('SISmm2008_CMSAF.zip')
  
  listFich <- dir(pattern='\.nc')
  stackSIS <- stack(listFich)
  stackSIS <- stackSIS*24 ##from irradiance (W/m2) to irradiation Wh/m2
  
  idx <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')
  
  SISmm <- setZ(stackSIS, idx)
  names(SISmm) <- month.abb

## Once the =Rasterstack= has been defined, it can be displayed easily
## with =levelplot=. Each panel of the graphic shows a layer of the
## =RasterStack= object using a trellis chart or [[http://en.wikipedia.org/wiki/Small_multiple][small-multiple
## technique]].

pdf(file="figs/levelplot.pdf")
levelplot(SISmm)
dev.off()

## [[file:figs/levelplot.png]]

## If only one layer is chosen, this method displays [[http://stackoverflow.com/a/18594679/964866][two marginal plots]],
## the row and column summaries of the =RasterLayer=, computed with the
## function defined by =FUN.margin= (which uses =mean= as default value):

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
## time evolution of the mean value along the latitude using data
## from the book "[[http://eu.wiley.com/WileyCDA/WileyTitle/productCd-EHEP002348.html][Statistics for Spatio-Temporal Data]]":

pdf(file="figs/hovmoller.pdf")
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
  names(SST) <- as.character(idx)
  hovmoller(SST, contour=FALSE, panel = panel.levelplot.raster,
            yscale.components = yscale.raster.subticks,
            interpolate = TRUE, par.settings = RdBuTheme)
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

## The function =terrain= from =raster= provides the vector field
## (gradient) from a scalar field stored in a =RasterLayer= object. The
## magnitude (slope) and direction (aspect) of the vector field is
## usually displayed with a set of arrows (e.g. =quiver= in Matlab).

## =rasterVis= includes a method, =vectorplot=, to calculate and display
## this vector field.

  proj <- CRS('+proj=longlat +datum=WGS84')
  df <- expand.grid(x=seq(-2, 2, .01), y=seq(-2, 2, .01))
  
  df$z <- with(df, (3*x^2 + y)*exp(-x^2-y^2))
  r <- rasterFromXYZ(df, crs=proj)

## #+RESULTS:

png(filename="figs/vectorplot.png")
  vectorplot(r, par.settings=RdBuTheme())
dev.off()

## #+RESULTS:
## [[file:figs/vectorplot.png]]

## If the =Raster*= object passed to =vectorplot= is a
## vector field (=isField=TRUE=), the =terrain= calculation is
## skipped.

## An alternative method to display a vector field plots streamlines
## along the field lines. Streamlines, a family of curves that are
## tangent to the vector field, show the direction an element
## (/droplet/) will follow under the effect of the field.
## =streamplot= displays streamlines with a procedure inspired
## by the [[http://christl.cg.tuwien.ac.at/research/vis/dynsys/frolic/frolic_crc.pdf][FROLIC algorithm]]: for each point
## (/droplet/) of a jittered regular grid, a short streamline
## portion (/streamlet/) is calculated by integrating the
## underlying vector field at that point. The main color of each
## streamlet indicates local vector magnitude
## (=slope=). Besides, streamlets are composed of points whose sizes,
## positions and color degradation encode the local vector direction
## (=aspect=).

png(filename="figs/streamplot.png")
  streamplot(r)
dev.off()

## #+RESULTS:
## [[file:figs/streamplot.png]]

## =streamplot= accepts two arguments (=droplets= and =streamlets=)
## to control the number of droplets, the length of the streamlets
## and the streamlet calculation step. The streamlet colour
## palette and the panel background color are defined with an
## specific theme for =streamplot=, =streamTheme=. The default
## options can be changed easily:

png(filename="figs/streamplotReds.png")
  df$z <- with(df, sqrt(x^2 + y^2))
  df$phi <- with(df, atan2(-y, x))
  r2 <- rasterFromXYZ(df, crs=proj)
  
  streamplot(r2, isField=TRUE, streamlet=list(L=30), droplet=list(pc=.3),
             par.settings=streamTheme(symbol=brewer.pal(n=5, name='Reds')))
  
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
