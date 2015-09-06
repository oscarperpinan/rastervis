globalVariables('y')

##Create a Layer from a custom function of the coordinates
xyLayer <- function(object, dirXY=y){
  y <- init(object, v='y')
  x <- init(object, v='x')
  isLanguage <- try(is.language(dirXY), silent=TRUE)
  if (class(isLanguage)=='try-error' || !isLanguage) dirXY <- substitute(dirXY)
  dirLayer <- eval(dirXY)
}

##Hovmoller diagram
##http://en.wikipedia.org/wiki/Hovm%C3%B6ller_diagram

setGeneric('hovmoller', function(object, ...){standardGeneric('hovmoller')})

setMethod('hovmoller', signature='RasterStackBrick',
          definition=function(object, dirXY=y,
            FUN='mean', digits=2,
            xlab='Direction', ylab='Time',
            par.settings=rasterTheme(),
            xscale.components=xscale.raster,
            add.contour=FALSE,
            labels=FALSE, region=TRUE, ...){
            
            idx=getZ(object)
            if (is.null(idx))
            stop('z slot of the object is NULL. Use setZ.')

            ##Calculate the matrix with the zonal function
            dirLayer <- xyLayer(object, dirXY=substitute(dirXY))
            z <- zonal(object, dirLayer, FUN, digits=digits)
            dat <- expand.grid(x=z[,1], y=idx)
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
              p <- contourplot(z~x*y, data=dat,
                               xlab=xlab, ylab=ylab,
                               labels=labels,
                               xscale.components=xscale.components,
                               par.settings=par.settings,
                               region=region, ...)
            } else {
              p <- levelplot(z~x*y, data=dat,
                             par.settings=par.settings,
                             xscale.components=xscale.components,
                             xlab=xlab, ylab=ylab,
                             ...)
            }
            p
          }
            )



