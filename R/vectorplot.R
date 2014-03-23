setGeneric('vectorplot', function(object, ...){standardGeneric('vectorplot')})

setMethod('vectorplot',
          signature(object='Raster'),
          definition = function(object, layers,
              narrows=2e3, lwd.arrows=0.6, length=unit(5e-2, 'npc'),
              maxpixels=1e5, region=TRUE, margin=FALSE,
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

              ## If region is a Raster, it is used as the background
              ## instead of the magnitude of the object
              if (is(region, 'Raster')) {
                  compareRaster(object, region, rowcol=FALSE)
                  object <- region
                  region <- TRUE
                  }

              levelplot(object,
                        maxpixels = maxpixels, 
                        region = region,
                        margin = margin,
                        ...) +
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


setMethod('vectorplot',
          signature(object='RasterStack'),
          definition = function(object, layers,
              narrows=2e3, lwd.arrows=0.6, length=unit(5e-2, 'npc'),
              maxpixels=1e5, region=TRUE, margin=FALSE,
              isField=FALSE, reverse=FALSE,
              unit='radians', scaleSlope=TRUE,
              aspX=0.08, aspY=aspX,
              uLayers, vLayers,
              ...){
              if (isField!='dXY' | (isField=='dXY' & nlayers(object)==2)) {
                  if (missing(layers)) layers=seq_len(nlayers(object))
                  callNextMethod(object, layers,
                                 narrows, lwd.arrows, length,
                                 maxpixels, region, margin,
                                 isField, reverse, unit,
                                 scaleSlope, aspX, aspY,
                                 ...)
              } else {## RasterStack with dXY
                  ## nlayers must be even
                  ## Maybe it should not be compulsory...
                  stopifnot((nlayers(object) %% 2) == 0)
                  ## u and v layers may be defined by the user. If
                  ## not, first half are u components, second half v
                  ## components
                  hl <- nlayers(object)/2
                  if (missing(uLayers) & missing(vLayers)) {
                      uLayers <- seq_len(hl)
                      vLayers <- uLayers + hl
                      ## If only u layers are defined v layers are the
                      ## remaining layers and viceversa
                  } else if (missing(vLayers) & !missing(uLayers)) {
                              vLayers <- setdiff(seq_len(nlayers(object)), uLayers)
                          } else if (missing(uLayers) & !missing(vLayers)) {
                              uLayers <- setdiff(seq_len(nlayers(object)), vLayers)
                          }
                  ## Convert the original RasterStack into a list
                  layList <- unstack(object)
                  ## Build a list of RasterStacks. Each element of the
                  ## list is a pair u,v according to uLayers and vLayers
                  uvIdx <- cbind(uLayers, vLayers)
                  objectList <- apply(uvIdx, 1, FUN=function(idx)stack(layList[idx]))
                  names(objectList) <- sapply(objectList,
                                              FUN=function(s)paste(names(s), collapse='.'))
                  ## Now use xyplot.list with FUN=vectorplot to
                  ## display each element of the list with the method
                  ## vectorplot for a RasterStack with dXY and only
                  ## two layers.  I have to embed xyplot.list in an
                  ## auxiliary function to remove names.attr from
                  ## '...' because it is not correctly passed to
                  ## levelplot.
                  removeNmsAttr <- function(..., names.attr){
                      xyplot.list(objectList, FUN=vectorplot,
                                  narrows=narrows, lwd.arrows=lwd.arrows, length=length,
                                  maxpixels=maxpixels, region=region, margin=margin,
                                  isField='dXY', reverse=reverse, unit=unit,
                                  scaleSlope=scaleSlope, aspX=aspX, aspY=aspY,
                                  ...)
                  }
                  p <- removeNmsAttr(...)
                  ## If names.attr is in '...' I use it directly with strip.custom
                  dots <- list(...)
                  if ('names.attr' %in% names(dots)) {
                      update(p, strip=strip.custom(factor.levels=as.character(dots$names.attr)))
                  } else p
              }
              })
