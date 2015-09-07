setGeneric('contourplot')

setMethod('contourplot',
          signature(x='Raster', data='missing'),
          definition=function(x, data=NULL, layers,
              ...){
              dots <- list(...)
              settings <- list(
                  margin=list(), 
                  scales=list(),
                  maxpixels=1e5,            
                  between=list(x=0.5, y=0.2),
                  as.table=TRUE,
                  xlab='', ylab='', main='',
                  xscale.components=xscale.raster,
                  yscale.components=yscale.raster,
                  zscaleLog = NULL, 
                  cuts=7,
                  labels = TRUE,
                  contour = TRUE,
                  pretty = TRUE,
                  region = FALSE,
                  colorkey = FALSE,
                  FUN.margin=NULL,
                  scales.margin = NULL, axis.margin = NULL)
              call <- modifyList(settings, list(...))
              call$x <- x
              call$data <- NULL
              if (!missing(layers)) call$layers <- layers
              p <- do.call(levelplot, call)
              p
          }
          )

