setGeneric("bwplot", function(x, data, ...)
           standardGeneric("bwplot"))

setMethod('bwplot',
          signature(x='RasterStackBrick', data='missing'),
          definition=function(x, data = NULL, layers, FUN,
            maxpixels = 1e+05,
            xlab='', ylab='', main='',
            violin = TRUE, draw.points = FALSE, do.out = FALSE,
            par.settings = bwTheme(),
            violin.ratio = 1, box.ratio = 0.5,
            scales = list(x = list(rot = 45, cex = 0.8)),
            ...) {
            if (!missing(layers)) x <- subset(x, layers)
            nl=nlayers(x)
            if (nl > 1) {
              dat <- raster2dat(x, FUN, maxpixels)
              bwplot(values~ind,
                     data = dat, 
                     xlab = xlab, ylab = ylab, main = main,
                     horizontal = FALSE,
                     violin.ratio = violin.ratio, box.ratio = box.ratio,
                     do.out = do.out,
                     panel = function(..., violin.ratio, box.ratio, do.out) {
                         
                         if (isTRUE(violin))
                             panel.violin(...,
                                          varwidth = FALSE,
                                          box.ratio = violin.ratio)

                         if (isTRUE(draw.points))
                             panel.stripplot(...,
                                             jitter.data = TRUE,
                                             factor = 2 * box.ratio)
                         panel.bwplot(...,
                                      pch='|', do.out = do.out,
                                      box.ratio = box.ratio)
                         
                     },
                     par.settings = par.settings,
                     scales=scales
                     )
            } else {
              stop('bwplot is defined only for Raster objects with two or more layers.')
              }
          }
          )


setMethod('bwplot', signature(x='formula', data='Raster'),
          definition=function(x, data, dirXY, maxpixels = 1e+05,
            xscale.components = xscale.raster,
            yscale.components = yscale.raster,
            horizontal = FALSE,
            violin = TRUE, draw.points = FALSE, do.out = FALSE,
            violin.ratio = 1, box.ratio = 0.5,
            par.settings = bwTheme(),
            ...){

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
              
              p <- bwplot(x = x, data = df,
                          horizontal=horizontal,
                          xscale.components=xscale.components,
                          yscale.components=yscale.components,                            
                          violin.ratio = violin.ratio, box.ratio = box.ratio,
                          do.out = do.out,
                     panel = function(..., violin.ratio, box.ratio, do.out) {
                         
                         if (isTRUE(violin))
                             panel.violin(...,
                                          varwidth = FALSE,
                                          box.ratio = violin.ratio)

                         if (isTRUE(draw.points))
                             panel.stripplot(...,
                                             jitter.data = TRUE,
                                             factor = 2 * box.ratio)
                         panel.bwplot(...,
                                      pch='|', do.out = do.out,
                                      box.ratio = box.ratio)
                         
                     },
                     par.settings=par.settings, ...)
              p
          })



