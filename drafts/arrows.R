
arrowsRaster <- function(object, maxpixels=2e3, ...){
  dat <- sampleRegular(object, maxpixels, asRaster=TRUE)
  x <- getValues(init(dat, v='x'))
  y <- getValues(init(dat, v='y'))

  aspX <- xres(dat)/2
  aspY <- yres(dat)/2

  sa <- slopeAspect(dat)

  dx <- overlay(sa, fun=function(slope, aspect)slope*cos(aspect))
  dx <- scale(getValues(dx))*aspX
  dy <- overlay(sa, fun=function(slope, aspect)slope*sin(aspect))
  dy <- scale(getValues(dy))*aspY
  
  p <- levelplot(object, ...)
  arrowsLayer <- layer(panel.arrows(x, y, x+dx, y+dy, length=0.05),
                       data=list(x=x, y=y, dx=dx, dy=dy))
  p + arrowsLayer
  }

f <- system.file("external/test.grd", package="raster")
r <- raster(f)

arrowsRaster(r)
