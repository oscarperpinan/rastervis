setGeneric("bwplot", function(x, data, ...)
           standardGeneric("bwplot"))

########################################################################
## bwplot for data
########################################################################

bwData <- function(x, data, layers, FUN,
                   maxpixels,
                   xlab, ylab, main,
                   violin, draw.points, do.out,
                   par.settings,
                   violin.ratio, box.ratio,
                   scales,
                   ...)
{
    if (!missing(layers)) x <- subset(x, layers)
    dat <- raster2dat(x, FUN, maxpixels)
    bwplot(values~ind,
           data = dat, 
           xlab = xlab, ylab = ylab, main = main,
           horizontal = FALSE,
           violin.ratio = violin.ratio,
           box.ratio = box.ratio,
           do.out = do.out,
           panel = function(...,
                            violin.ratio,
                            box.ratio,
                            do.out) {
               
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
}

setMethod('bwplot',
          signature(x = 'RasterStackBrick', data = 'missing'),
          definition=function(x, data = NULL, layers, FUN,
            maxpixels = 1e+05,
            xlab = '', ylab = '', main = '',
            violin = TRUE, draw.points = FALSE, do.out = FALSE,
            par.settings = bwTheme(),
            violin.ratio = 1, box.ratio = 0.5,
            scales = list(x = list(rot = 45, cex = 0.8)),
            ...)
          {

              if (nlayers(x) > 1)
                  bwData(x, data, layers, FUN,
                         maxpixels,
                         xlab, ylab, main,
                         violin, draw.points, do.out,
                         par.settings,
                         violin.ratio, box.ratio,
                         scales,
                         ...)
              else 
                  stop('bwplot is defined only for objects with two or more layers.')
          }
          )

setMethod('bwplot',
          signature(x = 'SpatRaster', data = 'missing'),
          definition=function(x, data = NULL, layers, FUN,
            maxpixels = 1e+05,
            xlab = '', ylab = '', main = '',
            violin = TRUE, draw.points = FALSE, do.out = FALSE,
            par.settings = bwTheme(),
            violin.ratio = 1, box.ratio = 0.5,
            scales = list(x = list(rot = 45, cex = 0.8)),
            ...)
          {

              if (nlyr(x) > 1)
                  bwData(x, data, layers, FUN,
                         maxpixels,
                         xlab, ylab, main,
                         violin, draw.points, do.out,
                         par.settings,
                         violin.ratio, box.ratio,
                         scales,
                         ...)
              else 
                  stop('bwplot is defined only for objects with two or more layers.')
          }
          )

########################################################################
## bwplot for formula
########################################################################


setMethod('bwplot', signature(x='formula', data='Raster'),
          definition=function(x, data, dirXY, maxpixels = 1e+05,
            xscale.components = xscale.raster,
            yscale.components = yscale.raster,
            horizontal = FALSE,
            violin = TRUE, draw.points = FALSE, do.out = FALSE,
            violin.ratio = 1, box.ratio = 0.5,
            par.settings = bwTheme(),
            ...){

              df <- dfRegular(data, maxpixels)
              
              p <- bwplot(x = x, data = df,
                          horizontal=horizontal,
                          xscale.components=xscale.components,
                          yscale.components=yscale.components,
                          violin.ratio = violin.ratio,
                          box.ratio = box.ratio,
                          do.out = do.out,
                          panel = function(...,
                                           violin.ratio,
                                           box.ratio,
                                           do.out) {
                              
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


setMethod('bwplot', signature(x='formula', data='SpatRaster'),
          definition=function(x, data, dirXY, maxpixels = 1e+05,
            xscale.components = xscale.raster,
            yscale.components = yscale.raster,
            horizontal = FALSE,
            violin = TRUE, draw.points = FALSE, do.out = FALSE,
            violin.ratio = 1, box.ratio = 0.5,
            par.settings = bwTheme(),
            ...){

              df <- dfRegular(data, maxpixels)
              
              p <- bwplot(x = x, data = df,
                          horizontal=horizontal,
                          xscale.components=xscale.components,
                          yscale.components=yscale.components,
                          violin.ratio = violin.ratio,
                          box.ratio = box.ratio,
                          do.out = do.out,
                          panel = function(...,
                                           violin.ratio,
                                           box.ratio,
                                           do.out) {
                              
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
