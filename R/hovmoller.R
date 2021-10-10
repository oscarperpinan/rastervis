##Hovmoller diagram
##http://en.wikipedia.org/wiki/Hovm%C3%B6ller_diagram

setGeneric('hovmoller', function(object, ...){standardGeneric('hovmoller')})

.hovmoller <- function(dat, direction,
                       isLL,
                       FUN, digits,
                       xlab, ylab,
                       par.settings,
                       xscale.components,
                       add.contour, labels, region,
                       ...)
{
    ##Labels of x-axis when isLonLat(object)==TRUE
    if (isLL)
    {
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
            xscale.components <-
                if (identical(xscale.components, xscale.raster))
                    xscale.raster.EW
                else if (identical(xscale.components, xscale.raster.subticks))
                    xscale.raster.EWsubticks
                else xscale.components
        } else {
            if (direction=='y'){## Latitude
                xscale.components <-
                    if (identical(xscale.components, xscale.raster))
                        xscale.raster.NS
                    else if (identical(xscale.components, xscale.raster.subticks))
                        xscale.raster.NSsubticks
                    else xscale.components
            }}
    }
    
    ##Create the trellis object
    if (add.contour){
        p <- contourplot(z ~ x * y, data = dat,
                         xlab = xlab, ylab = ylab,
                         labels = labels,
                         xscale.components = xscale.components,
                         par.settings = par.settings,
                         region = region, ...)
    } else {
        p <- levelplot(z ~ x * y, data = dat,
                       par.settings = par.settings,
                       xscale.components = xscale.components,
                       xlab = xlab, ylab = ylab,
                       ...)
    }
    p
}

setMethod('hovmoller', signature='RasterStackBrick',
          definition=function(object, dirXY=y,
                              FUN='mean', digits=2,
                              xlab='Direction', ylab='Time',
                              par.settings=rasterTheme(),
                              xscale.components=xscale.raster,
                              add.contour=FALSE,
                              labels=FALSE, region=TRUE, ...)
{
    
    dirLayer <- xyLayer(object, substitute(dirXY), vector = FALSE)
    direction <- deparse(substitute(dirXY))
    tt <- getZ(object)
    if (is.null(tt))
        stop('z slot of the object is NULL. Use setZ.')

    ##Calculate the matrix with the zonal function
    z <- raster::zonal(object, dirLayer, fun = FUN, digits = digits)
    dat <- expand.grid(x = z[,1], y = tt)
    dat$z <- as.vector(as.numeric(z[,-1]))

    isLL <- isLonLat(object)

    .hovmoller(dat, direction,
               isLL,
               FUN, digits,
               xlab, ylab,
               par.settings,
               xscale.components,
               add.contour, labels, region,
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
    
    dirLayer <- xyLayer(object, substitute(dirXY), vector = FALSE)
    direction <- deparse(substitute(dirXY))
    tt <- time(object)
    if (is.null(tt))
        stop('time index of the object is NULL. Use time().')

    ##Calculate the matrix with the zonal function
    z <- terra::zonal(object, dirLayer, fun = FUN, digits = digits)
    ## zonal returns a data.frame with terra objects
    z <- as.matrix(z)
    dat <- expand.grid(x = z[,1], y = tt)
    dat$z <- as.vector(as.numeric(z[,-1]))

    isLL <- is.lonlat(object)

    .hovmoller(dat, direction,
               isLL,
               FUN, digits,
               xlab, ylab,
               par.settings,
               xscale.components,
               add.contour, labels, region,
               ...)
})


