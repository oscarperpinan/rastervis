globalVariables('y')

##Create a Layer from a custom function of the coordinates
xyLayer <- function(object, dirXY=y){
  y <- init(object, fun='y')
  x <- init(object, fun='x')
  isLanguage <- try(is.language(dirXY), silent=TRUE)
  if (class(isLanguage)=='try-error' || !isLanguage) dirXY <- substitute(dirXY)
  dirLayer <- eval(dirXY)
}

##Hovmoller diagram
##http://en.wikipedia.org/wiki/Hovm%C3%B6ller_diagram

.hovmoller <- function(object, dirLayer, tt,
                       FUN, digits,
                       xlab, ylab,
                       par.settings,
                       xscale.components,
                       add.contour,
                       labels, region, ...){

    ##Calculate the matrix with the zonal function
    z <- zonal(object, dirLayer, FUN, digits = digits)
    ## zonal returns a data.frame with terra objects and a matrix with
    ## raster objects.
    z <- as.matrix(z)
    dat <- expand.grid(x = z[,1], y = tt)
    dat$z <- as.vector(as.numeric(z[,-1]))

    ##Labels of x-axis when isLonLat(object)==TRUE
    if (isLonLat(object)){
        direction=deparse(substitute(dirXY))
        if (missing(xlab)){
            if (direction=='x'){
                xlab='Longitude'
            } else {
                if (direction=='y'){
                    xlab='Latitude'
                }
            }
        }
        ## which xscale.components?
        if (direction=='x'){## Longitude
            xscale.components <- if (identical(xscale.components, xscale.raster))
                                     xscale.raster.EW
                                 else if (identical(xscale.components, xscale.raster.subticks))
                                     xscale.raster.EWsubticks
                                 else xscale.components
        } else {
            if (direction=='y'){## Latitude
                xscale.components <- if (identical(xscale.components, xscale.raster))
                                         xscale.raster.NS
                                     else if (identical(xscale.components, xscale.raster.subticks))
                                         xscale.raster.NSsubticks
                                     else xscale.components
            }}}

    ##Create the trellis object
    if (add.contour){
        p <- contourplot(z~x*y, data = dat,
                         xlab = xlab, ylab = ylab,
                         labels = labels,
                         xscale.components = xscale.components,
                         par.settings = par.settings,
                         region = region, ...)
    } else {
        p <- levelplot(z~x*y, data = dat,
                       par.settings = par.settings,
                       xscale.components = xscale.components,
                       xlab = xlab, ylab = ylab,
                       ...)
    }
    p
}

setGeneric('hovmoller', function(object, ...){standardGeneric('hovmoller')})

setMethod('hovmoller', signature='RasterStackBrick',
          definition=function(object, dirXY=y,
                              FUN='mean', digits=2,
                              xlab='Direction', ylab='Time',
                              par.settings=rasterTheme(),
                              xscale.components=xscale.raster,
                              add.contour=FALSE,
                              labels=FALSE, region=TRUE, ...)
          {
              dirLayer <- xyLayer(object, substitute(dirXY))
              tt <- getZ(object)
              if (is.null(tt))
                  stop('z slot of the object is NULL. Use setZ.')

              .hovmoller(object, dirLayer, tt,
                         FUN, digits,
                         xlab, ylab,
                         par.settings,
                         xscale.components,
                         add.contour,
                         labels, region,
                         ...)
          })

setMethod('hovmoller', signature='SpatRaster',
          definition=function(object, dirXY=y,
                              FUN='mean', digits=2,
                              xlab='Direction', ylab='Time',
                              par.settings=rasterTheme(),
                              xscale.components=xscale.raster,
                              add.contour=FALSE,
                              labels=FALSE, region=TRUE, ...)
          {
              
              dirLayer <- xyLayer(object, dirXY=substitute(dirXY))
              tt <- time(object)
              if (is.null(tt))
                  stop('time index of the object is NULL. Use time().')

              .hovmoller(object, dirLayer, tt,
                         FUN, digits,
                         xlab, ylab,
                         par.settings,
                         xscale.components,
                         add.contour,
                         labels, region,
                         ...)
          })



