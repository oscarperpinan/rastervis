## Compute slope and aspect layers
extractSA <- function(s, skip, dXY = FALSE,
                      unit, reverse){
    if (!skip){
        sa <- terrain(s, opt=c('slope', 'aspect'))
    } else {
        if (!dXY) {
            ## If s is a vector field, the first layer is the
            ## magnitude (slope) and the second is the angle
            ## (aspect)
            slope <- subset(s, 1)
            aspect <- subset(s, 2)
            
            if (unit=='degrees') {
                aspect <- aspect/180*pi
            }
            if (isTRUE(reverse)) {
                aspect <- aspect + pi
            }
            sa <- stack(slope, aspect)
        } else {
            sa <- s
        }
    }
    sa
}

## Compute x, y and, dx, dy for panel.arrows
sa2xy <- function(sa, dXY = FALSE,
                  scaleSlope, aspX, aspY){
    if (!dXY) {
        slope <- subset(sa, 1)
        aspect <- subset(sa, 2)
        ##center=FALSE to get only positive values of
        ##slope
        if (is.logical(scaleSlope) & isTRUE(scaleSlope)){
            slope <- scale(slope, center = FALSE)
        } else {
            if (is.numeric(scaleSlope)) {
                slope <- slope/scaleSlope
            }
        }
        ##sin due to the angular definition of aspect
        dx <- slope * sin(aspect) 
        dy <- slope * cos(aspect)
    } else {
        dx <- subset(sa, 1)
        dy <- subset(sa, 2)
    }
    ## Returns a data.frame for panel.arrows
    dx <- getValues(dx) * aspX
    dy <- getValues(dy) * aspY
    x <- getValues(init(sa, v='x'))
    y <- getValues(init(sa, v='y'))
    data.frame(x, y, dx, dy)
}

fooSlopeAspect <- function(s, skip, dXY = FALSE,
                           unit, reverse,
                           scaleSlope, aspX, aspY){
    sa <- extractSA(s, skip = skip, dXY = dXY,
                    unit = unit, reverse = reverse)
    sa2xy(sa, dXY = dXY,
          scaleSlope = scaleSlope,
          aspX = aspX, aspY = aspY)
}
    
setGeneric('vectorplot',
           function(object, ...){
               standardGeneric('vectorplot')
           })

setMethod('vectorplot',
          signature(object='Raster'),
          definition = function(object, layers,
              narrows = 2e3,
              lwd.arrows = 0.6,
              col.arrows = 'black',
              length=unit(5e-2, 'npc'),
              maxpixels=1e5, region=TRUE, margin=FALSE,
              isField=FALSE, reverse=FALSE,
              unit='radians', scaleSlope=TRUE,
              aspX=0.08, aspY=aspX,
              key.arrow = NULL,
              ...){
              
              if (!missing(layers)) {
                  object <- subset(object, subset=layers)
              }
              
              dat <- sampleRegular(object, size=narrows, asRaster=TRUE)

              if (isField=='dXY') {
                  isField=TRUE
                  dXY=TRUE
                  } else {
                      dXY=FALSE
                      }
              

              if (nlayers(object)>1 && !isField){ ##slopeAspect works
                  ## only for RasterLayer
                  sa <- lapply(unstack(dat), fooSlopeAspect,
                               skip=FALSE, unit=unit, reverse=reverse,
                               scaleSlope = scaleSlope,
                               aspX = aspX, aspY = aspY)
                  sa <- do.call(rbind, sa)
              } else {
                  sa <- fooSlopeAspect(dat, skip=isField, dXY=dXY,
                                       unit=unit, reverse=reverse,
                                       scaleSlope = scaleSlope,
                                       aspX = aspX, aspY = aspY)
              }
              
              ## If 'region' is a Raster, it is used as the background
              if (is(region, 'Raster')) {
                  compareRaster(object, region, rowcol=FALSE)
                  object <- region
                  region <- TRUE
              } else if (isTRUE(isField)) {
                  if (isTRUE(dXY)) {
                      ## Computes slope and uses it as the background
                      u <- subset(object, 1)
                      v <- subset(object, 2)
                      object <- sqrt(u^2 + v^2)
                  } else {
                      ##only uses the magnitude for the region
                      object <- subset(object, 1)
              }
              }

              ## Ready to plot
              p <- levelplot(object,
                             maxpixels = maxpixels, 
                             region = region,
                             margin = margin,
                             ...) +
                                 xyplot(y ~ x, data = sa,
                                        dx = sa$dx, dy = sa$dy,
                                        length = length,
                                        lwd.arrows = lwd.arrows,
                                        col.arrows = col.arrows,
                                        panel=function(x, y, dx, dy,
                                            length, lwd.arrows, ...){
                                            panel.arrows(x, y, x+dx, y+dy,
                                                         length = length,
                                                         lwd=lwd.arrows,
                                                         col = col.arrows,
                                                         ...)
                                        }, ...)
              if (!is.null(key.arrow)) {
                  default.key.arrow <- list(size = 1, label = '')
                  key.arrow <- modifyList(default.key.arrow, key.arrow)
                  rgAxis <- diff(p$x.limits)
                  if (isField && !dXY){
                      if (isTRUE(scaleSlope)) {
                          scaleSlope <- cellStats(subset(object, 1), 'rms')
                      } else {}
                  } else {
                      scaleSlope <- 1
                  }
                  p$legend$top <- list(fun = legendArrow,
                                       args = list(size = key.arrow$size,
                                           unitLab = key.arrow$label,
                                           keyScale= aspX/(rgAxis*scaleSlope)))
              }
              p
          }
          )


setMethod('vectorplot',
          signature(object='RasterStack'),
          definition = function(object, layers,
              narrows=2e3, lwd.arrows=0.6, col.arrows = 'black',
              length=unit(5e-2, 'npc'),
              maxpixels=1e5, region=TRUE, margin=FALSE,
              isField=FALSE, reverse=FALSE,
              unit='radians', scaleSlope=TRUE,
              aspX=0.08, aspY=aspX,
              key.arrow = NULL,
              uLayers, vLayers,
              ...){
              if (isField!='dXY' | (isField=='dXY' & nlayers(object)==2)) {
                  if (missing(layers)) layers=seq_len(nlayers(object))
                  callNextMethod(object, layers = layers,
                                 narrows = narrows,
                                 lwd.arrows = lwd.arrows,
                                 col.arrows = col.arrows,
                                 length = length,
                                 maxpixels = maxpixels,
                                 region = region, margin = margin,
                                 isField = isField,
                                 reverse = reverse, unit = unit,
                                 scaleSlope = scaleSlope,
                                 aspX = aspX, aspY = aspY,
                                 key.arrow = key.arrow,
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
                                  narrows=narrows,
                                  lwd.arrows=lwd.arrows,
                                  col.arrows=col.arrows,
                                  length=length,
                                  maxpixels=maxpixels, region=region, margin=margin,
                                  isField='dXY', reverse=reverse, unit=unit,
                                  scaleSlope=scaleSlope, aspX=aspX, aspY=aspY,
                                  key.arrow = key.arrow,
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
