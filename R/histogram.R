setGeneric('histogram', function(x, data, ...)
           standardGeneric("histogram"))

setMethod('histogram',
          signature(x='RoT', data = 'missing'),
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
              if (!missing(layers))
                  x <- subset(x, layers)

              if (is(x, "SpatRaster"))
                  nly  <- nlyr(x)
              else
                  nly  <- nlayers(x)

              if (nly > 1)
              {## Multilayer Objects
                  if (missing(names.attr)){
                      nms <- names(x)
                      ## Ensure valid names
                      names.attr <- make.names(nms, unique = TRUE)
                  } else {
                      ## Do not coerce with as.character to allow formulas 
                      if (length(names.attr) != nlayers(x))
                          stop('Length of names.attr should match number of layers.')
                  }
                  
                  dat <- raster2dat(x, FUN, maxpixels=maxpixels, att=att)
                  p <- histogram(~values|ind, data = dat,
                                 as.table = as.table,
                                 par.settings = par.settings,
                                 between = between,
                                 scales = scales,
                                 nint = nint, col = col,
                                 xlab = xlab, ylab = ylab, main = main,
                                 strip = strip.custom(factor.levels = names.attr),

                                 ...)
              } else
              {
                  dat <- raster2dat(x, maxpixels=maxpixels, att=att)
                  p <- histogram(dat,
                                 data = NULL,
                                 nint = nint, col = col,
                                 xlab = xlab, ylab = ylab,
                                 main = main, ...)
              }
              p
          })

setMethod('histogram', signature(x='formula', data='RoT'),
          definition = function(x, data,
                                dirXY,
                                maxpixels = 1e+05,
                                strip = TRUE,
                                par.settings = rasterTheme(),
                                att = 1,
                                ...)
          {

              ## Names of layers
              nms <- names(data)
              ## Ensure valid names
              nms <- make.names(nms, unique = TRUE)
              
              ## Number of layers
              if (is(data, "SpatRaster"))
                  nly  <- nlyr(data)
              else
                  nly  <- nlayers(data)

              if (is(data, "SpatRaster"))
              {
                  dataSample <- spatSample(data,
                                           size = maxpixels,
                                           as.raster = TRUE)
                  xLayer <- init(dataSample, fun='x')
                  yLayer <- init(dataSample, fun='y')

                  df <- values(dataSample)
                  xLayer <- values(xLayer)
                  yLayer <- values(yLayer)
              }
              else 
                {
                    dataSample <- sampleRegular(data,
                                                size = maxpixels,
                                                asRaster = TRUE)
                    xLayer <- init(dataSample, fun='x')
                    yLayer <- init(dataSample, fun='y')

                    df <- getValues(dataSample)
                    xLayer <- getValues(xLayer)
                    yLayer <- getValues(yLayer)
                }
                  
              df <- as.data.frame(df)
              names(df) <- nms
              
              df <- cbind(data.frame(x=xLayer, y=yLayer), df)
              
              if (!missing(dirXY))
              {
                  dirXY <- xyLayer(dataSample,
                                   dirXY = substitute(dirXY))

                  if (is(data, "Raster"))
                      dirXY <- getValues(dirXY)
                  else
                      dirXY <- values(dirXY)

                  df <- cbind(df, dirXY)
              }

              p <- histogram(x = x, data = df,
                             strip = strip,
                             par.settings = par.settings,
                             ...)
              p
          }
          )
