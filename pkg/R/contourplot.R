# Author: Oscar Perpinan Lamigueiro oscar.perpinan@upm.es
# Date :  June 2011
# Version 0.10
# Licence GPL v3

setGeneric('contourplot')

setMethod('contourplot',
          signature(x='Raster', data='missing'),
          definition=function(x, data=NULL, layers,
            ...){
            dots <- list(...)
            settings <- list(
                             margin=TRUE, FUN.margin=mean, scales.margin = NULL, 
                             maxpixels=1e5,            
                             between=list(x=0.5, y=0.2),
                             as.table=TRUE,
                             xlab='', ylab='', main='',
                             scales=list(draw=TRUE),
                             xscale.components=xscale.raster,
                             yscale.components=yscale.raster,
                             zscaleLog = NULL, 
                             cuts=7,
                             labels = TRUE,
                             contour = TRUE,
                             pretty = TRUE,
                             region = FALSE,
                             colorkey = FALSE
                             )
            call <- modifyList(settings, list(...))
            call$x <- x
            call$data <- NULL
            if (!missing(layers)) call$layers <- layers
            p <- do.call(levelplot, call)
            p
            }
          )

