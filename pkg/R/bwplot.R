# Author: Oscar Perpinan Lamigueiro oscar.perpinan@upm.es
# Date :  June 2011
# Version 0.10
# Licence GPL v3

setGeneric('bwplot')

setMethod('bwplot',
          signature=c(x='RasterStackBrick', data='missing'),
          definition=function(x, layer, FUN,
            maxpixels = 1e+05,
            xlab='', ylab='', main='',
            violin=TRUE,
            par.settings=rasterTheme,
            ...) {
            if (!missing(layer)) x <- subset(x, layer)
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
            }
          }
          )


