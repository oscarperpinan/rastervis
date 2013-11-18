setGeneric('splom')

setMethod('splom',
          signature(x='RasterStackBrick', data='missing'),
          definition=function(x, data=NULL, maxpixels=1e5,
            plot.loess=FALSE, colramp=BTC, varname.cex=0.6,...){

            nms <- names(x)

            if (maxpixels < ncell(x)) {
              dat <- sampleRandom(x, maxpixels)
            } else {
              dat <- getValues(x)
            }
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
                  colramp=colramp,
                  varname.cex=varname.cex,
                  plot.loess=plot.loess,
                  panel=panel.hexbinplot,
                  diag.panel=diag.panel,
                  lower.panel=lower.panel,
                  pscale=0, ...)
          }
          )

