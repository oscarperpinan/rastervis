# Author: Oscar Perpinan Lamigueiro oscar.perpinan@upm.es
# Date :  September 2011
# Version 0.10
# Licence GPL v3
setGeneric('vectorplot', function(object, ...){standardGeneric('vectorplot')})

setMethod('vectorplot',
          signature(object='Raster'),
          definition = function(object, layers,
              narrows=2e3, lwd.arrows=0.6, length=unit(5e-2, 'npc'),
              maxpixels=1e5, region=TRUE,
              isField=FALSE, reverse=FALSE, unit='radians',
              ...){
              if (!missing(layers)) {
                  object <- subset(object, subset=layers)
              } else {}

              
              dat <- sampleRegular(object, size=narrows, asRaster=TRUE)
              x <- getValues(init(dat, v='x'))
              y <- getValues(init(dat, v='y'))


              fooSlopeAspect <- function(s, skip, unit, reverse){
                  aspX <- xres(s)*0.6 ## maybe use a non-constant value...
                  aspY <- yres(s)*0.6

                  if (!skip){s <- terrain(s, opt=c('slope', 'aspect'))}
                  ## If s is a vector field, the first layer is the
                  ## magnitude (slope) and the second is the angle (aspect)
                  slope <- getValues(subset(s, 1))
                  aspect <- getValues(subset(s, 2))
                  if (unit=='degrees' & skip) {aspect <- aspect/180*pi}
                  if (reverse) aspect <- aspect + pi


                  slope <- scale(slope, center=FALSE) ##center=FALSE to
                  ## get only positive
                  ## values of slope
                  dx <- slope*sin(aspect)*aspX ##sin due to the angular
                  ## definition of aspect
                  dy <- slope*cos(aspect)*aspY

                  x <- getValues(init(s, v='x'))
                  y <- getValues(init(s, v='y'))

                  data.frame(x, y, dx, dy)
              }

              if (nlayers(object)>1 && !isField){ ##slopeAspect works
                  ## only for RasterLayer
                  sa <- lapply(unstack(dat), fooSlopeAspect,
                               skip=FALSE, unit=unit, reverse=reverse)
                  sa <- do.call(rbind, sa)
              } else {
                  sa <- fooSlopeAspect(dat, skip=isField,
                                       unit=unit, reverse=reverse)
              }

              
              ##only uses the magnitude for the region
              if (isField) object <- subset(object, 1)

              if (is(region, 'Raster')) {
                  compareRaster(object, region)
                  object <- region
                  region <- TRUE
                  }

              levelplot(object,
                        maxpixels = maxpixels,
                        region = region,...) +
                  xyplot(y~x, data=sa, dx=sa$dx, dy=sa$dy,
                         length=length, lwd.arrows=lwd.arrows,
                         panel=function(x, y, dx, dy,
                             length, lwd.arrows, ...){
                             panel.arrows(x, y, x+dx, y+dy,
                                          length=length,
                                          lwd=lwd.arrows, ...)
                         })
          }
          )

