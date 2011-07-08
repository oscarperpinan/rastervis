# Author: Oscar Perpinan Lamigueiro oscar.perpinan@upm.es
# Date :  June 2011
# Version 0.10
# Licence GPL v3


setGeneric('histogram')
setMethod('histogram',
          signature(x='RasterLayer', data='missing'),
          definition=function (x, data=NULL, maxpixels = 1e+05, breaks=100,
            xlab='', ylab='', main='', col='gray',...){
            dat <- raster2dat(x, maxpixels=maxpixels)
            p <- histogram(dat,
                           data=NULL,
                           breaks=breaks, col=col,
                           xlab=xlab, ylab=ylab, main=main)
            p
          }
)  


setMethod('histogram',
          signature(x='RasterStackBrick', data='missing'),
          definition=function (x, data=NULL, layers, FUN,
            maxpixels = 1e+05, breaks=100,
            xlab='', ylab='', main='', col='gray',
            between=list(x=0.5, y=0.2),
            as.table=TRUE,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            par.settings=rasterTheme,
            ...) {
            if (!missing(layers)) x <- subset(x, layers)
            nl=nlayers(x)
            if (nl > 1) {
              dat <- raster2dat(x, FUN, maxpixels)
              p <- histogram(~values|ind, data=dat,
                             as.table=as.table,
                             par.settings=par.settings,
                             between=between,
                             xscale.components=xscale.components,
                             yscale.components=yscale.components,
                             scales=list(x=list(relation='free'),
                               y=list(relation='free',
                                 draw=FALSE)),
                             breaks=breaks, col=col,
                             xlab=xlab, ylab=ylab, main=main,
                             strip.names=c(TRUE, TRUE))
            } else {
              p <- histogram(x, maxpixels = maxpixels, breaks=breaks,
                             main = main, ylab=ylab, xlab=xlab, col=col,...)
            }
            p
          }
          )
