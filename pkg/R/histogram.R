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
                           xlab=xlab, ylab=ylab,
                           main=main, ...)
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
            scales=list(x=list(relation='free'),
              y=list(relation='free',
                draw=FALSE)),
            strip.names=c(TRUE, TRUE),
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
                             scales=scales,
                             breaks=breaks, col=col,
                             xlab=xlab, ylab=ylab, main=main,
                             strip.names=strip.names,
                             ...)
            } else {
              p <- histogram(x, maxpixels = maxpixels, breaks=breaks,
                             main = main, ylab=ylab, xlab=xlab, col=col,...)
            }
            p
          }
          )

setMethod('histogram', signature(x='formula', data='Raster'),
          definition=function(x, data, dirXY, maxpixels=1e+05,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            par.settings=rasterTheme,...){

            nms <- layerNames(data)
            nl <- nlayers(data)

            data <- sampleRegular(data, maxpixels, asRaster=TRUE)
            df <- getValues(data)
            df <- as.data.frame(df)
            names(df) <- make.names(nms)

            xLayer <- getValues(init(data, v='x'))
            yLayer <- getValues(init(data, v='y'))

            df <- cbind(data.frame(x=xLayer, y=yLayer), df)

            if (!missing(dirXY)) {
              dirXY <- getValues(xyLayer(data, dirXY=substitute(dirXY)))
              df <- cbind(df, dirXY)
            }

            p <- histogram(x=x, data=df,
                             xscale.components=xscale.components,
                             yscale.components=yscale.components,                            
                             par.settings=par.settings, ...)
            p
          }
            )
