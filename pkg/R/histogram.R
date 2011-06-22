setGeneric('histogram')
setMethod('histogram',
          signature=c(x='RasterLayer', data='missing'),
          definition=function (x, maxpixels = 1e+05, breaks=100,
            xlab='', ylab='', main='', col='gray',...){
            dat <- sampleRandom(x, maxpixels)
            p <- histogram(dat,
                           data=NULL,
                           breaks=breaks, col=col,
                           xlab=xlab, ylab=ylab, main=main)
            p
          }
)  


setMethod('histogram',
          signature=c(x='RasterStackBrick', data='missing'),
          definition=function (x, layer, FUN,
            maxpixels = 1e+05, breaks=100,
            xlab='', ylab='', main='', col='gray',
            between=list(x=0.5, y=0.2),
            as.table=TRUE,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            par.settings=rasterTheme,
            ...) {
            if (!missing(layer)) x <- subset(x, layer)
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
