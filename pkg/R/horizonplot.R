# Author: Oscar Perpinan Lamigueiro oscar.perpinan@upm.es
# Date :  June 2011
# Version 0.10
# Licence GPL v3

setGeneric('horizonplot')
setMethod('horizonplot',
          signature(x='RasterStackBrick', data='missing'),
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
