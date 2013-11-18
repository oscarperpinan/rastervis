setGeneric("bwplot", function(x, data, ...)
           standardGeneric("bwplot"))

setMethod('bwplot',
          signature(x='RasterStackBrick', data='missing'),
          definition=function(x, data=NULL, layers, FUN,
            maxpixels = 1e+05,
            xlab='', ylab='', main='',
            violin=TRUE,
            par.settings=rasterTheme(),
            scales=list(x=list(rot=45, cex=0.8)),
            ...) {
            if (!missing(layers)) x <- subset(x, layers)
            nl=nlayers(x)
            if (nl > 1) {
              dat <- raster2dat(x, FUN, maxpixels)
              bwplot(values~ind,
                     data=dat, 
                     xlab=xlab, ylab=ylab, main=main,
                     horizontal=FALSE,
                     panel = function(..., box.ratio) {
                       if (violin) {panel.violin(..., col = "lightblue",
                                    varwidth = FALSE, box.ratio = box.ratio)}
                       panel.bwplot(..., col='black',
                                    cex=0.8, pch='|', fill='gray', box.ratio = .1)
                     },
                     par.settings = list(box.rectangle=list(col='black'),
                       plot.symbol = list(pch='.', cex = 0.1)),
                     scales=scales
                     )
            } else {
              stop('bwplot is defined only for Raster objects with two or more layers.')
              }
          }
          )


setMethod('bwplot', signature(x='formula', data='Raster'),
          definition=function(x, data, dirXY, maxpixels=1e+05,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            horizontal=FALSE,
            violin=TRUE,
            par.settings=rasterTheme(),...){

            localSets = list(box.rectangle=list(col='black'),
              plot.symbol = list(pch='.', cex = 0.1))

            if (is.list(par.settings)){
              par.settings <- modifyList(par.settings, localSets)
            } else if (is.function(par.settings)){
              par.settings <- modifyList(par.settings(), localSets)
            } else par.settings <- localSets
                
            nms <- names(data)

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

            p <- bwplot(x=x, data=df,
                        horizontal=horizontal,
                        xscale.components=xscale.components,
                        yscale.components=yscale.components,                            
                        par.settings=par.settings, ...,
                        panel = function(..., box.ratio) {
                          if (violin) {panel.violin(..., col = "lightblue",
                                                    varwidth = FALSE, box.ratio = box.ratio)}
                          panel.bwplot(..., col='black',
                                       cex=0.8, pch='|', fill='gray',
                                       box.ratio = ifelse(violin, .1, box.ratio))
                        }
                        )
            p
          }
            )



