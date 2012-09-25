# Author: Oscar Perpinan Lamigueiro oscar.perpinan@upm.es
# Date :  June 2011
# Version 0.10
# Licence GPL v3

setGeneric('horizonplot')
setMethod('horizonplot',
          signature(x='RasterStackBrick', data='missing'),
          definition=function(x, data=NULL, dirXY=y, digits=0,
            xlab='Time', ylab='direction',
            colorkey=TRUE, colorkey.digits=1,
            scales=list(y=list(relation="same")),
            ...){
            idx=getZ(x)
            if (is.null(idx)) stop('z slot of the object is NULL. Use setZ.')
            dirLayer <- xyLayer(x, dirXY=substitute(dirXY))
            z <- zonal(x, dirLayer, digits=digits)
            nRows <- nrow(z)
            zz <- as.data.frame(t(z[,-1]), row.names='')
            names(zz) <- z[,1]
            zz <- zoo(zz, order.by=idx)
            p <- horizonplot(zz, xlab=xlab, ylab=ylab, layout=c(1, nRows), 
                             colorkey=colorkey,
                             colorkey.digits=colorkey.digits,
                             origin=mean(zz),
                             scales=scales)
            p
          })
