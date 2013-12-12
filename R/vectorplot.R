setGeneric('vectorplot', function(object, ...){standardGeneric('vectorplot')})

setMethod('vectorplot',
          signature(object='Raster'),
          definition = function(object, layers,
              narrows=2e3, lwd.arrows=0.6, length=unit(5e-2, 'npc'),
              maxpixels=1e5, region=TRUE, 
              isField=FALSE, reverse=FALSE,
              unit='radians', scaleSlope=TRUE,
              aspX=0.08, aspY=aspX,
              ...){
              
              if (!missing(layers)) {
                  object <- subset(object, subset=layers)
              }
              
              dat <- sampleRegular(object, size=narrows, asRaster=TRUE)
              x <- getValues(init(dat, v='x'))
              y <- getValues(init(dat, v='y'))

              if (isField=='dXY') {
                  isField=TRUE
                  dXY=TRUE
                  } else {
                      dXY=FALSE
                      }
              
              ## Compute slope and aspect layers
              fooSlopeAspect <- function(s, skip, dXY=FALSE,
                                         unit, reverse){
                  if (!skip){s <- terrain(s, opt=c('slope', 'aspect'))}
                  ## If s is a vector field, the first layer is the
                  ## magnitude (slope) and the second is the angle
                  ## (aspect)
                  x <- getValues(init(s, v='x'))
                  y <- getValues(init(s, v='y'))

                  if (!dXY) {
                      slope <- getValues(subset(s, 1))
                      aspect <- getValues(subset(s, 2))
                                        
                      if (unit=='degrees' & skip) {
                          aspect <- aspect/180*pi
                      }
                      if (reverse) aspect <- aspect + pi
                      ##center=FALSE to get only positive values of
                      ##slope
                      if (is.logical(scaleSlope) & isTRUE(scaleSlope)){
                          slope <- scale(slope, center=FALSE)
                          } else {
                              if (is.numeric(scaleSlope)) {
                                  slope <- slope/scaleSlope
                                  }
                              }
                      ##sin due to the angular definition of aspect
                      dx <- slope * sin(aspect) * aspX 
                      dy <- slope * cos(aspect) * aspY
                  } else {
                      dx <- getValues(subset(s, 1)) * aspX
                      dy <- getValues(subset(s, 2)) * aspY
                  }
                  data.frame(x, y, dx, dy)
              }

              if (nlayers(object)>1 && !isField){ ##slopeAspect works
                  ## only for RasterLayer
                  sa <- lapply(unstack(dat), fooSlopeAspect,
                               skip=FALSE, unit=unit, reverse=reverse)
                  sa <- do.call(rbind, sa)
              } else {
                  sa <- fooSlopeAspect(dat, skip=isField, dXY=dXY,
                                       unit=unit, reverse=reverse)
              }
              
              ##only uses the magnitude for the region
              if (isField) object <- subset(object, 1)

              if (is(region, 'Raster')) {
                  compareRaster(object, region, rowcol=FALSE)
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
                         }, ...)
          }
          )

