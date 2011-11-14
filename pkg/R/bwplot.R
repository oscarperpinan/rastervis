# Author: Oscar Perpinan Lamigueiro oscar.perpinan@upm.es
# Date :  June 2011
# Version 0.10
# Licence GPL v3

setGeneric("bwplot", function(x, data, ...)
           standardGeneric("bwplot"))


setMethod('bwplot',
          signature(x='RasterStackBrick', data='missing'),
          definition=function(x, data=NULL, layers, FUN,
            maxpixels = 1e+05,
            xlab='', ylab='', main='',
            violin=TRUE,
            par.settings=rasterTheme,
            ...) {
            if (!missing(layers)) x <- subset(x, layers)
            nl=nlayers(x)
            if (nl > 1) {
              dat <- raster2dat(x, FUN, maxpixels)
              bwplot(values~ind,
                     data=dat, 
                     xlab=xlab, ylab=ylab,
                     horizontal=FALSE,
                     panel = function(..., box.ratio) {
                       if (violin) {panel.violin(..., col = "lightblue",
                                    varwidth = FALSE, box.ratio = box.ratio)}
                       panel.bwplot(..., col='black',
                                    cex=0.8, pch='|', fill='gray', box.ratio = .1)
                     },
                     par.settings = list(box.rectangle=list(col='black'),
                       plot.symbol = list(pch='.', cex = 0.1)),
                     scales=list(x=list(rot=45, cex=0.5))
                     )
            } else {
              stop('bwplot is defined only for Raster objects with two or more layers.')
              }
          }
          )


## setMethod('bwplot', signature(x='formula', data='Raster'),
##           definition=function(x, data, dirXY, maxpixels=1e5,
##             xscale.components=xscale.raster,
##             yscale.components=yscale.raster,
##             horizontal=FALSE,
##             violin=TRUE,
##             par.settings=rasterTheme,...){

##             localSets = list(box.rectangle=list(col='black'),
##               plot.symbol = list(pch='.', cex = 0.1))

##             if (is.list(par.settings)){
##               par.settings <- modifyList(par.settings, localSets)
##             } else if (is.function(par.settings)){
##               par.settings <- modifyList(par.settings(), localSets)
##             } else par.settings <- localSets
                

##             nms <- layerNames(data)
##             nl <- nlayers(data)

##             data <- sampleRegular(data, maxpixels, asRaster=TRUE)
##             df <- getValues(data)
##             df <- as.data.frame(df)
##             names(df) <- make.names(nms)

##             xLayer <- getValues(init(data, v='x'))
##             yLayer <- getValues(init(data, v='y'))

##             df <- cbind(data.frame(x=xLayer, y=yLayer), df)

##             if (!missing(dirXY)) {
##               dirXY <- getValues(xyLayer(data, dirXY=substitute(dirXY)))
##               df <- cbind(df, dirXY)
##             }

##             p <- bwplot(x=x, data=df,
##                         horizontal=horizontal,
##                         xscale.components=xscale.components,
##                         yscale.components=yscale.components,                            
##                         par.settings=par.settings, ...,
##                         panel = function(..., box.ratio) {
##                           if (violin) {panel.violin(..., col = "lightblue",
##                                                     varwidth = FALSE, box.ratio = box.ratio)}
##                           panel.bwplot(..., col='black',
##                                        cex=0.8, pch='|', fill='gray', box.ratio = .1)
##                         }
##                         )
##             p
##           }
##             )



## ##http://neo.sci.gsfc.nasa.gov/Search.html?group=64
## pop <- raster('/home/oscar/temp/DescargasMozilla/875430rgb-167772161.0.FLOAT.TIFF')
## pop[pop==99999] <- NA
## levelplot(pop, zscaleLog=10, par.settings=BTCTheme)

## ##http://neo.sci.gsfc.nasa.gov/Search.html?group=20
## landClass <- raster('/home/oscar/temp/DescargasMozilla/241243rgb-167772161.0.TIFF')
## landClass[landClass==254] <- NA
## levelplot(landClass, par.settings=custom.theme)

## s <- stack(pop, landClass)
## layerNames(s) <- c('pop', 'landClass')

## bwplot(asinh(pop) ~ landClass|cut(y, 4), data=s, violin=FALSE)
