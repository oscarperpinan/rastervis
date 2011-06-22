##xyplot for directions created with xyLayer
setGeneric('xyplot')

setMethod('xyplot', signature='RasterStackBrick',#Time',
          definition=function(x, data=NULL, dirXY=y, xlab='Time', ylab='', digits=0, par.settings=rasterTheme,...){
            idx=getZ(x)
            dirLayer <- xyLayer(x, dirXY=substitute(dirXY))
            z <- zonal(x, dirLayer, mean, digits=digits)
            nRows <- nrow(z)
            zz <- as.data.frame(t(z[,-1]), row.names='')
            names(zz) <- z[,1]
            zz <- zoo(zz, order.by=idx)
            p <- xyplot(zz, xlab=xlab, ylab=ylab, superpose=TRUE, auto.key=FALSE, par.settings=par.settings, ...)
            p + glayer(panel.text(x[1], y[1], group.value, cex=0.7))
          }
          )
