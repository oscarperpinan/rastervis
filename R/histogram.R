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
            scales=list(x=list(relation='free'),
              y=list(relation='free',
                     draw=FALSE)),
            names.attr,
            par.settings=rasterTheme(),
            ...) {
            if (!missing(layers)) x <- subset(x, layers)
            nl=nlayers(x)
            if (nl > 1) {
                ## Names of each panel
                if (missing(names.attr)){
                    names.attr <- names(x)
                } else {
                    ## Do not coerce with as.character to allow formulas 
                    if (length(names.attr) != nlayers(x))
                        stop('Length of names.attr should match number of layers.')
                }

              dat <- raster2dat(x, FUN, maxpixels)
              p <- histogram(~values|ind, data = dat,
                             as.table = as.table,
                             par.settings = par.settings,
                             between = between,
                             scales = scales,
                             nint = nint, col = col,
                             xlab = xlab, ylab = ylab, main = main,
                             strip = strip.custom(factor.levels = names.attr),

                             ...)
            } else {
              p <- histogram(x, maxpixels = maxpixels, nint = nint,
                             main = main, ylab = ylab, xlab = xlab, col = col,...)
            }
            p
          }
          )

setMethod('histogram', signature(x='formula', data='Raster'),
          definition=function(x, data, dirXY, maxpixels=1e+05,
              strip=TRUE, par.settings=rasterTheme(),
              ...){

              isFactor <- which(is.factor(data))
              levelsData <- levels(data)[[isFactor]][[1]][,2]

              if (!missing(dirXY)) {
                  dirXY <- xyLayer(data, dirXY=substitute(dirXY))
                  names(dirXY) <- 'dirXY'
                  data <- stack(data, dirXY)
              }

              df <- as.data.frame(sampleRegular(data,
                                                maxpixels,
                                                xy=TRUE))
              ## Categorical data
              if (any(isFactor)){
                  df[, isFactor + 2] <- as.factor(
                      levelsData[df[, isFactor + 2]])
                  if (isTRUE(strip)) {
                      strip <- strip.custom(strip.levels=TRUE)
                  }
              }

              p <- histogram(x=x, data=df,
                             strip=strip,
                             par.settings=par.settings, ...)
              p
          }
          )
