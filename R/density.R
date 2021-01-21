globalVariables('ind')

setGeneric('densityplot')

setMethod('densityplot',
          signature(x='RoT', data='missing'),
          definition=function (x, data = NULL, layers, FUN,
            maxpixels = 1e+05,
            xlab = '', ylab = '', main = '',
            par.settings = rasterTheme(),
            draw.labels = TRUE,
            auto.key = list(space = "right"),
            att = 1,
            ...)
          {
              if (!missing(layers))
                  x <- subset(x, layers)
              
              if (is(x, "SpatRaster"))
                  nly  <- nlyr(x)
              else
                  nly  <- nlayers(x)
              
              if (nly > 1) {
                  dat <- raster2dat(x, FUN, maxpixels, att)
                  if (draw.labels == TRUE)
                      p <- densityplot(~values,
                                       data = dat, groups = ind,
                                       breaks = 100,
                                       par.settings = par.settings, pch = '.',
                                       xlab = xlab, ylab = ylab, main = main,
                                       panel = panel.superpose,
                                       panel.groups = function(x, group.value, col.line,...){
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
              } 
              else
              { ## Number of layers = 1
                  dat <- raster2dat(x, maxpixels = maxpixels, att = att)
                  p <- densityplot(dat,
                                   data = NULL,
                                   par.settings = par.settings,
                                   pch = '.',
                                   xlab = xlab, ylab = ylab, main = main,
                                   ...) 
              }
              p
          }
          )

                 

setMethod('densityplot', signature(x='formula', data='RoT'),
          definition=function(x, data, dirXY, maxpixels=1e+05,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            auto.key = list(space = 'right'), 
            par.settings=rasterTheme(),...){


              if (is(data, "Raster"))
              {
                  dataSample <- sampleRegular(data,
                                              size = maxpixels,
                                              asRaster = TRUE)
                  df <- as.data.frame(dataSample, xy = TRUE)
                  
              } else
              {
                  idx <- spatSample(data,
                                    size = maxpixels,
                                    cells = TRUE)
                  vals <- data[idx]
                  xy <- xyFromCell(data, idx)
                  df <- cbind(xy, vals)
              }

            p <- densityplot(x = x, data = df,
                             xscale.components = xscale.components,
                             yscale.components = yscale.components,
                             auto.key = auto.key, 
                             par.settings = par.settings, ...)
            p
          }
          )
