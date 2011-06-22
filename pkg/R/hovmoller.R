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

setMethod('hovmoller', signature='RasterStackBrick',##signature='RasterTime',
          definition=function(object, dirXY=y,
            FUN=mean, digits=2,
            xlab='Direction', ylab='Time', 
            add.contour=TRUE, ...){
            idx=getZ(object)
            dirLayer <- xyLayer(object, dirXY=substitute(dirXY))
            z <- zonal(object, dirLayer, FUN, digits=digits)
            dat <- expand.grid(x=z[,1], y=idx)
            dat$z <- as.vector(as.numeric(z[,-1]))

            if (add.contour){
              p <- contourplot(z~x*y, data=dat,
                               xlab=xlab, ylab=ylab,
                               labels=list(cex=0.7),
                               region=TRUE, ...)
            } else {
              p <- levelplot(z~x*y, data=dat,
                             xlab=xlab, ylab=ylab,
                             ...)
              }
            p
            }
            )



