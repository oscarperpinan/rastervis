setGeneric('splom')

setMethod('splom',
          signature=c(x='RasterStackBrick', data='missing'),
          definition=function(x, maxpixels=1e5, plot.loess=FALSE, varname.cex=0.6,...){
            nms <- layerNames(x)
            dat <- sampleRandom(x, maxpixels)
            colnames(dat) <- nms
            diag.panel = function(x,...){
              yrng <- current.panel.limits()$ylim
              d <- density(x)
              d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
              panel.lines(d)
              diag.panel.splom(x,...)
            }
            lower.panel = function(x, y, plot.loess=plot.loess,...){
              panel.hexbinplot(x, y, ...)
              if (plot.loess) panel.loess(x, y, ..., col = 'red')
            }
            splom(~dat,
                  colramp=BTC,
                  varname.cex=varname.cex, 
                  plot.loess=plot.loess,
                  panel=panel.hexbinplot,
                  diag.panel=diag.panel,
                  lower.panel=lower.panel,
                  pscale=0, ...)
          }
          )

