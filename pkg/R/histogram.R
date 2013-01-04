# Author: Oscar Perpinan Lamigueiro oscar.perpinan@upm.es
# Date :  June 2011
# Version 0.10
# Licence GPL v3


setGeneric('histogram')
setMethod('histogram',
          signature(x='RasterLayer', data='missing'),
          definition=function (x, data=NULL, maxpixels = 1e+05, nint=100,
            xlab='', ylab='', main='', col='gray',...){
            dat <- raster2dat(x, maxpixels=maxpixels)
            p <- histogram(dat,
                           data=NULL,
                           nint=nint, col=col,
                           xlab=xlab, ylab=ylab,
                           main=main, ...)
            p
          }
)  


setMethod('histogram',
          signature(x='RasterStackBrick', data='missing'),
          definition=function (x, data=NULL, layers, FUN,
            maxpixels = 1e+05, nint=100,
            xlab='', ylab='', main='', col='gray',
            between=list(x=0.5, y=0.2),
            as.table=TRUE,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            scales=list(x=list(relation='free'),
              y=list(relation='free',
                draw=FALSE)),
            strip.names=c(TRUE, TRUE),
            par.settings=rasterTheme(),
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
                             nint=nint, col=col,
                             xlab=xlab, ylab=ylab, main=main,
                             strip.names=strip.names,
                             ...)
            } else {
              p <- histogram(x, maxpixels = maxpixels, nint=nint,
                             main = main, ylab=ylab, xlab=xlab, col=col,...)
            }
            p
          }
          )

setMethod('histogram', signature(x='formula', data='Raster'),
          definition=function(x, data, dirXY, maxpixels=1e+05,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            strip=TRUE, par.settings=rasterTheme(),
            ...){

            ## names replace layerNames with raster version 2.0-04
            rasterVersion <- as.character(packageVersion('raster'))
            nms <- if (compareVersion(rasterVersion, '2.0-04') == -1) layerNames(data) else names(data)

            nl <- nlayers(data)
            isFactor <- is.factor(data)
            levelsData <- levels(data)[[which(isFactor)]][[1]][,2]

            data <- sampleRegular(data, maxpixels, asRaster=TRUE)
            df <- getValues(data)
            df <- as.data.frame(df)
            names(df) <- make.names(nms)

            ## Categorical data
            if (any(isFactor)){
               df[, isFactor] <- as.factor(levelsData[df[, isFactor]])
               if (isTRUE(strip)) strip <- strip.custom(strip.levels=TRUE)
               }

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
                           strip=strip,
                           par.settings=par.settings, ...)
            p
          }
          )
