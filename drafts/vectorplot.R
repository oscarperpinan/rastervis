# Author: Oscar Perpinan Lamigueiro oscar.perpinan@upm.es
# Date :  September 2011
# Version 0.10
# Licence GPL v3

vectorplot <- function(object, layers, narrows=2e3, lwd.arrows=0.6, region=TRUE,...){
  if (!missing(layers)) {
    object <- subset(object, subset=layers)
  } else {}

  dat <- sampleRegular(object, size=narrows, asRaster=TRUE)
  x <- getValues(init(dat, v='x'))
  y <- getValues(init(dat, v='y'))


  fooSlopeAspect <- function(s){
    aspX <- xres(s)*0.6 ## maybe use a non-constant value...
    aspY <- yres(s)*0.6

    sa <- slopeAspect(s)
    slope <- getValues(subset(sa, 1))
    aspect <- getValues(subset(sa, 2))

    slope <- scale(slope, center=FALSE) ##center=FALSE to get only positive values of slope
    dx <- slope*sin(aspect)*aspX ##sin due to the angular definition of aspect
    dy <- slope*cos(aspect)*aspY

    data.frame(dx, dy)
  }
  if (nlayers(object)>1){ ##slopeAspect works only for RasterLayer
    sa <- lapply(unstack(dat), fooSlopeAspect)
    sa <- do.call(rbind, sa) 
  } else {
    sa <- fooSlopeAspect(dat)
  }

  fooPanel <- function(x, y, z, subscripts, sa, length, lwd.arrows,...) {
    if (region) {panel.levelplot.raster(x, y, z, subscripts, interpolate=TRUE,...)}
    xs <- x[subscripts]
    ys <- y[subscripts]
    zs <- z[subscripts]
    ## Although sa does not store the subscripts information,
    ## it is in the same order so it is safe to use it this way
    sas <- sa[subscripts,]
    panel.arrows(xs, ys, xs+sas$dx, ys+sas$dy, 
                 length=length,
                 lwd=lwd.arrows)
  }

  levelplot(object, maxpixels=narrows,
            colorkey=region, 
            sa=sa, length=unit(5e-2, 'npc'), lwd.arrows=lwd.arrows,
            panel=fooPanel, ...)
  
}


## trellis.device(pdf, file='arrows.pdf')
## arrowsRaster(SISmm, layout=c(1, 1))
## dev.off()
## system('pdf2swf -s framerate=1 arrows.pdf')


df <- expand.grid(x=seq(-2, 2, .1), y=seq(-2, 2, .1))
df$z <- with(df, (3*x^2 + y)*exp(-x^2-y^2))
r1 <- rasterFromXYZ(df)
df$z <- with(df, x*exp(-x^2-y^2))
r2 <- rasterFromXYZ(df)
df$z <- with(df, y*exp(-x^2-y^2))
r3 <- rasterFromXYZ(df)

projection(r1) <- projection(r2) <- projection(r3) <- CRS("+proj=longlat +datum=WGS84")

vectorplot(r1, par.settings=RdBuTheme)
vectorplot(r2, par.settings=RdBuTheme)
vectorplot(r3, par.settings=RdBuTheme)
