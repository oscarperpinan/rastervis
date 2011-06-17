## library(zoo)##For horizonplot
## library(lattice)
## library(latticeExtra)##For horizonplot and xyplot (glayer)

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
          definition=function(object, dirXY=y, digits=2,
            xlab='Direction', ylab='Time', 
            add.contour=TRUE, ...){
            idx=getZ(object)
            dirLayer <- xyLayer(object, dirXY=substitute(dirXY))
            z <- zonal(object, dirLayer, mean, digits=digits)
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

##Horizonplot from latticeExtra
##http://www.perceptualedge.com/articles/visual_business_intelligence/time_on_the_horizon.pdf
setGeneric('horizonplot')

setMethod('horizonplot', signature='RasterStackBrick',##signature='RasterTime',
          definition=function(x, data=NULL, dirXY=y, xlab='Time', ylab='direction', digits=0, ...){
            idx=getZ(x)
            dirLayer <- xyLayer(x, dirXY=substitute(dirXY))
            z <- zonal(x, dirLayer, mean, digits=digits)
            nRows <- nrow(z)
            zz <- as.data.frame(t(z[,-1]), row.names='')
            names(zz) <- z[,1]
            zz <- zoo(zz, order.by=idx)
            p <- horizonplot(zz, xlab=xlab, ylab=ylab, layout=c(1, nRows), 
                 colorkey=TRUE, colorkey.digits=1, origin=mean(zz),
                 scales=list(y=list(relation="same")))
            p
          }
          )

##xyplot for directions created with xyLayer
##setGeneric('xyplot')

setMethod('xyplot', signature='RasterStackBrick',#Time',
          definition=function(x, data=NULL, dirXY=y, xlab='Time', ylab='', digits=0, ...){
            idx=getZ(x)
            dirLayer <- xyLayer(x, dirXY=substitute(dirXY))
            z <- zonal(x, dirLayer, mean, digits=digits)
            nRows <- nrow(z)
            zz <- as.data.frame(t(z[,-1]), row.names='')
            names(zz) <- z[,1]
            zz <- zoo(zz, order.by=idx)
            p <- xyplot(zz, xlab=xlab, ylab=ylab, superpose=TRUE, auto.key=FALSE)
            p + glayer(panel.text(x[1], y[1], group.value, cex=0.7))
          }
          )

