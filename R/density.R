globalVariables('ind')

setGeneric('densityplot')

########################################################################
## densityplot for data
########################################################################
## Only one layer
densityLayer <- function(x,
                         maxpixels,
                         xlab, ylab, main,
                         par.settings, draw.labels, auto.key,
                         att,
                         ...)
{
    dat <- raster2dat(x, maxpixels = maxpixels, att = att)
    p <- densityplot(dat,
                     data = NULL,
                     par.settings = par.settings,
                     pch = '.',
                     xlab = xlab, ylab = ylab, main = main,
                     ...) 
    p
}

## Multilayer objects
densityMLayer <- function(x,
                          layers, FUN,
                          maxpixels,
                          xlab, ylab, main,
                          par.settings, draw.labels, auto.key,
                          att,
                          ...)
{
    if (!missing(layers))
        x <- subset(x, layers)
    
    dat <- raster2dat(x, FUN, maxpixels, att)
    if (draw.labels == TRUE)
        p <- densityplot(~values,
                         data = dat, groups = ind,
                         breaks = 100,
                         par.settings = par.settings, pch = '.',
                         xlab = xlab, ylab = ylab, main = main,
                         panel = panel.superpose,
                         panel.groups = function(x, group.value,
                                                 col.line, ...)
                         {
                             panel.densityplot(x,
                                               col.line = col.line,
                                               plot.points = FALSE,
                                               ...)
                             d <- density(x, na.rm = TRUE)
                             i <- which.max(d$y)
                             ltext(d$x[i],d$y[i],
                                   group.value,
                                   adj = c(0.3,0),
                                   col = col.line,
                                   cex = 0.7)
                         })
    
    else ##draw.labels = FALSE
        p <- densityplot(~values,
                         data = dat, groups = ind,
                         breaks = 100,
                         par.settings = par.settings, pch = '.',
                         xlab = xlab, ylab = ylab, main = main,
                         auto.key = auto.key, ...)                

    p
}

##################################################################
##Methods for data
##################################################################

setMethod('densityplot',
          signature(x='Raster', data='missing'),
          definition=function (x, data = NULL, layers, FUN,
            maxpixels = 1e+05,
            xlab = '', ylab = '', main = '',
            par.settings = rasterTheme(),
            draw.labels = TRUE,
            auto.key = list(space = "right"),
            att = 1,
            ...)
          {
              
              if (nlayers(x) > 1)
                  densityMLayer(x,
                                layers, FUN,
                                maxpixels,
                                xlab, ylab, main,
                                par.settings, draw.labels, auto.key,
                                att,
                                ...)
              else
                  densityLayer(x,
                               maxpixels,
                               xlab, ylab, main,
                               par.settings, draw.labels, auto.key,
                               att,
                               ...)
          })


setMethod('densityplot',
          signature(x='SpatRaster', data='missing'),
          definition=function (x, data = NULL, layers, FUN,
            maxpixels = 1e+05,
            xlab = '', ylab = '', main = '',
            par.settings = rasterTheme(),
            draw.labels = TRUE,
            auto.key = list(space = "right"),
            att = 1,
            ...)
          {
              
              if (nlyr(x) > 1)
                  densityMLayer(x,
                                layers, FUN,
                                maxpixels,
                                xlab, ylab, main,
                                par.settings, draw.labels, auto.key,
                                att,
                                ...)
              else
                  densityLayer(x,
                               maxpixels,
                               xlab, ylab, main,
                               par.settings, draw.labels, auto.key,
                               att,
                               ...)
              
          })

########################################################################
## densityplot for formula
########################################################################

setMethod('densityplot', signature(x = 'formula', data = 'Raster'),
          definition = function(x, data, dirXY, maxpixels = 1e+05,
            xscale.components = xscale.raster,
            yscale.components = yscale.raster,
            auto.key = list(space = 'right'), 
            par.settings = rasterTheme(),...){

              df <- dfRegular(data, maxpixels)

              p <- densityplot(x = x, data = df,
                               xscale.components = xscale.components,
                               yscale.components = yscale.components,
                               auto.key = auto.key, 
                               par.settings = par.settings, ...)
              p
          }
          )


setMethod('densityplot', signature(x = 'formula', data = 'SpatRaster'),
          definition = function(x, data, dirXY, maxpixels = 1e+05,
            xscale.components = xscale.raster,
            yscale.components = yscale.raster,
            auto.key = list(space = 'right'), 
            par.settings = rasterTheme(),...){

              df <- dfRegular(data, maxpixels)

              p <- densityplot(x = x, data = df,
                               xscale.components = xscale.components,
                               yscale.components = yscale.components,
                               auto.key = auto.key, 
                               par.settings = par.settings, ...)
              p
          }
          )

