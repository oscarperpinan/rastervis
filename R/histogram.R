setGeneric('histogram', function(x, data, ...)
           standardGeneric("histogram"))

########################################################################
## Histogram for data
########################################################################
## Only one layer
histLayer <- function(x, data,
                      maxpixels, nint,
                      xlab, ylab, main, col,
                      between,
                      as.table,
                      scales,
                      names.attr,
                      par.settings,
                      att,
                      ...)
{
    dat <- raster2dat(x, maxpixels = maxpixels, att = att)
    p <- histogram(dat,
                   data = NULL,
                   nint = nint, col = col,
                   xlab = xlab, ylab = ylab,
                   main = main, ...)
    p
}

## Multilayer objects
histMLayer <- function(x, data,
                       layers, FUN,
                       maxpixels, nint,
                       xlab, ylab, main, col,
                       between,
                       as.table,
                       scales,
                       names.attr,
                       par.settings,
                       att,
                       ...)
{
    if (!missing(layers))
        x <- subset(x, layers)
    
    if (missing(names.attr)){
        nms <- names(x)
        ## Ensure valid names
        names.attr <- make.names(nms, unique = TRUE)
    } else {
        ## Do not coerce with as.character to allow formulas 
        if (length(names.attr) != nlayers(x))
            stop('Length of names.attr should match number of layers.')
    }
    
    dat <- raster2dat(x, FUN, maxpixels = maxpixels, att = att)
    p <- histogram(~values | ind, data = dat,
                   as.table = as.table,
                   par.settings = par.settings,
                   between = between,
                   scales = scales,
                   nint = nint, col = col,
                   xlab = xlab, ylab = ylab, main = main,
                   strip = strip.custom(factor.levels = names.attr),
                   ...)
    p
}

setMethod('histogram',
          signature(x='Raster', data = 'missing'),
          definition=function (x, data = NULL, layers, FUN,
                               maxpixels = 1e+05, nint = 100,
                               xlab = '', ylab='', main = '', col = 'gray',
                               between = list(x=0.5, y=0.2),
                               as.table = TRUE,
                               scales=list(x = list(relation = 'free'),
                                           y=list(relation = 'free',
                                                  draw = FALSE)),
                               names.attr,
                               par.settings = rasterTheme(),
                               att = 1,
                               ...)
          {
              if (nlayers(x) > 1)
                  histMLayer(x, data,
                             layers, FUN,
                             maxpixels, nint,
                             xlab, ylab, main, col,
                             between,
                             as.table,
                             scales,
                             names.attr,
                             par.settings,
                             att,
                             ...)
              else
                  histLayer(x, data,
                            maxpixels, nint,
                            xlab, ylab, main, col,
                            between,
                            as.table,
                            scales,
                            names.attr,
                            par.settings,
                            att,
                            ...)
          })

setMethod('histogram',
          signature(x='SpatRaster', data = 'missing'),
          definition=function (x, data = NULL, layers, FUN,
                               maxpixels = 1e+05, nint = 100,
                               xlab = '', ylab='', main = '', col = 'gray',
                               between = list(x=0.5, y=0.2),
                               as.table = TRUE,
                               scales=list(x = list(relation = 'free'),
                                           y=list(relation = 'free',
                                                  draw = FALSE)),
                               names.attr,
                               par.settings = rasterTheme(),
                               att = 1,
                               ...)
          {
              if (nlyr(x) > 1)
                  histMLayer(x, data,
                             layers, FUN,
                             maxpixels, nint,
                             xlab, ylab, main, col,
                             between,
                             as.table,
                             scales,
                             names.attr,
                             par.settings,
                             att,
                             ...)
              else
                  histLayer(x, data,
                            maxpixels, nint,
                            xlab, ylab, main, col,
                            between,
                            as.table,
                            scales,
                            names.attr,
                            par.settings,
                            att,
                            ...)
              
          })

########################################################################
## Histogram for formula and data
########################################################################

histogramFormula <- function(x, data,
                             maxpixels, dirXY,
                             strip, par.settings,
                             att,
                             ...)
{
              df <- dfRegular(data, maxpixels)
              
              if (!missing(dirXY))
              {
                  dirXY <- xyLayer(data,
                                   dirXY = substitute(dirXY),
                                   maxpixels = maxpixels)

                  df <- cbind(df, dirXY)
              }

              p <- histogram(x = x, data = df,
                             strip = strip,
                             par.settings = par.settings,
                             att = att,
                             ...)
              p
}
setMethod('histogram', signature(x='formula', data='Raster'),
          definition = function(x, data,
                                dirXY,
                                maxpixels = 1e+05,
                                strip = TRUE,
                                par.settings = rasterTheme(),
                                att = 1,
                                ...)
          {
              histogramFormula(x = x, data = data,
                               maxpixels = maxpixels,
                               strip = strip,
                               att = att,
                               ...)
          }
          )


setMethod('histogram', signature(x='formula', data='SpatRaster'),
          definition = function(x, data,
                                dirXY,
                                maxpixels = 1e+05,
                                strip = TRUE,
                                par.settings = rasterTheme(),
                                att = 1,
                                ...)
          {
              histogramFormula(x = x, data = data,
                               maxpixels = maxpixels,
                               strip = strip,
                               att = att,
                               ...)
          })
