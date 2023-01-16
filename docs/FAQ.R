## #+DESCRIPTION: FAQ rasterVis
## #+TITLE: FAQs
## #+AUTHOR: Oscar Perpiñán Lamigueiro
## #+EMAIL:     oscar.perpinan@upm.es
## #+OPTIONS:   H:1 num:nil toc:1 \n:nil @:t ::t |:t ^:t -:t f:t *:t TeX:t LaTeX:nil skip:nil d:t tags:not-in-toc
## #+PROPERTY:  header-args :session *R-FAQs* :tangle yes :eval no-export :comments org :exports both
## #+LANGUAGE:  en
## #+SETUPFILE: setup.org


library(rasterVis)


##   :PROPERTIES:
##   :CUSTOM_ID: breaks
##   :END:

## Use the =at= argument to define the breaks.


f <- system.file("external/test.grd", package="raster")
r <- raster(f)

my.at <- seq(100, 1850, by = 250)
levelplot(r, at=my.at)



## #+RESULTS:
## [[file:figs/FAQ_at.png]]

## Moreover, if you want to define where the labels are located
## define the components of =colorkey=:


myColorkey <- list(at=my.at, ## where the colors change
                   labels=list(
                       at=my.at ## where to print labels
                     ))
levelplot(r, at=my.at, colorkey=myColorkey)



## #+RESULTS:
## [[file:figs/FAQ_at2.png]]

## Last, you can even define the contents of the labels:


myColorkey <- list(at=my.at, ## where the colors change
                   labels=list(
                     labels=letters[seq_along(my.at)], ## labels
                     at=my.at ## where to print labels
                     ))
levelplot(r, at=my.at, colorkey=myColorkey)


##   :PROPERTIES:
##   :CUSTOM_ID: axis_labels
##   :END:

## Use =xlab=NULL=, =ylab=NULL= and =draw=FALSE= inside =scale=.


  r <- raster()
  r <- init(r, runif)
  
  levelplot(r, xlab=NULL, ylab=NULL, scales=list(draw=FALSE))



## #+RESULTS:
## [[file:figs/FAQ_axis_labels.png]]


  levelplot(r, xlab=NULL, scales=list(x=list(draw=FALSE)))



## #+RESULTS:
## [[file:figs/FAQ_axis_labelsX.png]]


  levelplot(r, ylab=NULL, scales=list(y=list(draw=FALSE)))


##   :PROPERTIES:
##   :CUSTOM_ID: rotate_axis_label
##   :END:

## Define =main=, =xlab= and =ylab= as vectors or lists with
## arguments to be passed to =grid::gpar=.


  r <- raster(nrow=10, ncol=10)
  r[] <- runif(100)
  
  levelplot(r, margin=FALSE,
            main=list('My plot', col='red'),
            xlab=c('Longitude', 'X-Axis'),
            ylab=list('Latitude', rot=30, fontface='bold')
            )


##   :PROPERTIES:
##   :CUSTOM_ID: arrow
##   :END:
## Use =sp::layout.north.arrow= with =latticeExtra::layer=.

library(sp)

f <- system.file("external/test.grd", package="raster")
r <- raster(f)

levelplot(r, margin=FALSE, auto.key=FALSE, scales=list(draw=FALSE)) + 
  layer({
    SpatialPolygonsRescale(layout.north.arrow(),
                           offset = c(179000,332500),
                           scale = 400)
  })


##   :PROPERTIES:
##   :CUSTOM_ID: scalebar
##   :END:
## Use =grid.rect=, =grid.text= and =latticeExtra::layer=.


  f <- system.file("external/test.grd", package="raster")
  r <- raster(f)

  levelplot(r) +
    layer({
      xs <- seq(181000, 181400, by=100)
      grid.rect(x=xs, y=330500,
                    width=100, height=30,
                    gp=gpar(fill=rep(c('transparent', 'black'), 2)),
                    default.units='native')
      grid.text(x= xs - 50, y=330560, seq(0, 400, by=100),
                gp=gpar(cex=0.5), rot=30,
                default.units='native')
      })


##   :PROPERTIES:
##   :CUSTOM_ID: overlay
##   :END:

## Several layers can be combined using the =+.trellis= and =layer=
## functions from the =latticeExtra= package (which is automatically
## loaded with =rasterVis=).

## Let's build some data to play. You can skip this part if you already have a raster file and a shapefile.


library(maps)
library(mapdata)
library(maptools)

## raster
myRaster <- raster(xmn=-100, xmx=100, ymn=-60, ymx=60)
myRaster <- init(myRaster, runif)

## polygon shapefile
ext <- as.vector(extent(myRaster))

boundaries <- map('worldHires', fill=TRUE,
    xlim=ext[1:2], ylim=ext[3:4],
    plot=FALSE)

## read the map2SpatialPolygons help page for details
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
bPols <- map2SpatialPolygons(boundaries, IDs=IDs,
                              proj4string=CRS(projection(myRaster)))



## #+RESULTS:

## The polygons are printed with =sp::sp.polygons= using =+.trellis= and
## =layer= to display them over the result of =levelplot=.


levelplot(myRaster) + layer(sp.polygons(bPols))


##   :PROPERTIES:
##   :CUSTOM_ID: several_rasters
##   :END:

## Use =print.trellis= and its argument =split=. 

## For two different =RasterLayer=.

  r <- r2 <- raster()
  r[] <- runif(ncell(r))
  r2[] <- 1:10
  
  p <- levelplot(r, par.settings=RdBuTheme(), margin=FALSE)
  p2 <- levelplot(r2, par.settings=BTCTheme(), margin=FALSE)
  
  
  print(p, split=c(1, 1, 1, 2), more=TRUE)
  print(p2, split=c(1, 2, 1, 2))



## #+RESULTS:
## [[file:figs/FAQ_print_split.png]]

## A more sophisticated solution to print more than two layers.

  s <- stack(r, r2, 10*r+r2, 10*r-r2)
  nl <- nlayers(s)
  m <- matrix(1:nl, nrow=2)
  themes <- list(RdBuTheme(), BTCTheme(), GrTheme(), PuOrTheme())
  for (i in 1:nl){
      p <- levelplot(s, layers=i,
                     par.settings=themes[[i]],
                     margin=FALSE)
      print(p, split=c(col(m)[i], row(m)[i], ncol(m), nrow(m)), more=(i<nl))
  }


##   :PROPERTIES:
##   :CUSTOM_ID: panel_labels
##   :END:
## Use =names.attr=.


  r <- raster(nrow=10, ncol=10)
  r[] <- runif(100)
  s <- stack(lapply(1:8, function(i)r))
  
  levelplot(s, scales=list(draw=FALSE), colorkey=FALSE,
            par.settings=GrTheme(),
            names.attr=paste0('MNF-', 1:8))
