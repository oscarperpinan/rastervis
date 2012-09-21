# Author: Oscar Perpinan Lamigueiro oscar.perpinan@upm.es
# Date :  June 2011
# Version 0.10
# Licence GPL v3

setGeneric('levelplot')

setMethod('levelplot',
          signature(x='Raster', data='missing'),
          definition=function(x, data=NULL, layers,
            margin=TRUE, FUN.margin=mean, 
            maxpixels=1e5,
            par.settings=rasterTheme,
            between=list(x=0.5, y=0.2),
            as.table=TRUE,
            xlab='', ylab='', main='',
            names.attr,
            scales=list(), scales.margin=NULL,
            xscale.components=xscale.raster,
            yscale.components=yscale.raster,
            zscaleLog=NULL,
            colorkey=list(space='right'),
            panel=panel.levelplot,
            contour=FALSE, region=TRUE, labels=FALSE,
            ...) {
            
            if (!missing(layers)) {
              object <- subset(x, subset=layers)
            } else {object <- x}
  
            dat <- sampleRegular(object, size=maxpixels, asRaster=TRUE)
            nms <- make.names(layerNames(dat))

            ##Extract coordinates from (sampled) raster
            coords <- xyFromCell(dat, seq_len(ncell(dat)))
            
            ## Extract values 
            dat <- getValues(dat)

            ## If zscale is not NULL, transform the values and choose a function
            ## to calculate the labels
            if (!is.null(zscaleLog)){
              zlogbase <- if (is.logical(zscaleLog)) {
                10
              } else {
                if (is.numeric(zscaleLog)) {
                  zscaleLog
                } else {
                  if (zscaleLog == "e") {
                    exp(1)
                  }
                }}
              if (zlogbase==10) {
                zscale.components <- zscale.components.log10ticks
              } else {
                zscale.components <- zscale.components.logpower
              }
              dat <- log(dat, zlogbase)
            }
            ## Calculate the range (for zscale.components)
            zlim <- lattice:::extend.limits(range(dat, finite=TRUE))
            
            ## Convert to a data.frame for conventional levelplot  
            dat <- as.data.frame(dat)
            names(dat) <- nms
            df <- cbind(coords, dat)
            
            ##aspect and scales(from sp:::spplot.grid, sp:::longlat.scales, sp:::mapasp)
            bb <- extent(object)
            xlim=c(bb@xmin, bb@xmax)
            ylim=c(bb@ymin, bb@ymax)
  
            if (isLonLat(object)){
              
              if (xlab=='') xlab='Longitude'
              if (ylab=='') ylab='Latitude'
            
              aspect=(diff(ylim)/diff(xlim))/cos((mean(ylim) * pi)/180)

              xscale.components <- if (identical(xscale.components, xscale.raster))
                xscale.raster.EW
              else if (identical(xscale.components, xscale.raster.subticks))
                xscale.raster.EWsubticks
              else xscale.components

              yscale.components <- if (identical(yscale.components, yscale.raster))
                yscale.raster.NS
              else if (identical(yscale.components, yscale.raster.subticks))
                yscale.raster.NSsubticks
              else yscale.components
              
            } else { ## !isLonLat
              aspect='iso'
              }

            if (region==FALSE) colorkey=FALSE
           
            has.colorkey <- (is.logical(colorkey) && colorkey) || is.list(colorkey)
##            if (region==TRUE && !has.colorkey) colorkey=has.colorkey=TRUE
            has.margin <- nlayers(object)==1 && margin
            has.contour <- (contour==TRUE) ##(!missing(contour) && contour==TRUE)

            ## Build the zscale.components and colorkey paying attention to zscaleLog
            if (!is.null(zscaleLog)){
              zscale <- zscale.components(zlim, zlogbase)
              if (has.colorkey){
                colorkey.default=list(labels=zscale, raster=TRUE, interpolate=TRUE)
                if (is.logical(colorkey)){
                  colorkey=colorkey.default
                } else {
                  colorkey=modifyList(colorkey, colorkey.default)
                }
              }
            }
            
            ## Some fixes for the margin
            if (has.margin){
              if (is.function(par.settings)) par.settings <- par.settings()
              par.settings=modifyList(par.settings,
                list(
                     layout.widths=list(right.padding=10),
                     layout.heights=list(top.padding=10,
                       xlab.key.padding=3)
                     )
                )
              if (has.colorkey){## put the colorkey at the bottom to leave space for the margin
                if (is.logical(colorkey)){
                  colorkey=list(space='bottom')                
                } else {
                  colorkey=modifyList(colorkey, list(space='bottom'))
                }}
            }

            ## Build the formula for levelplot
            form <- as.formula(paste(paste(names(df)[-c(1,2)], collapse='+'), 'x*y', sep='~'))

            ## Which panel did the user requested? (panel.levelplot or panel.levelplot.raster)
            requestedPanel <- panel



            ## Build a custom panel useful for zscaleLog with contour
            panelMixed <- function(...,
                                   contour, region,
                                   at, labels, levelPanel=requestedPanel,
                                   lwd=c(1, 0.4) # Two line widths, for main and minor contour lines
                                   ){
              ##Draw the regions with the panel requested by the user
              if (region)levelPanel(..., at=at, contour=FALSE, labels=FALSE)
              has.labels <- (is.logical(labels) && labels) || is.list(labels)
              mainTicks=(as.character(zscale$labels)!=FALSE)
              mainLabels <- list(labels=zscale$labels[mainTicks], cex=0.8)
              minorTicks=!mainTicks
              ## Contour lines for main divisions (log10Ticks or logpower)
              ## Their width is lwd[1]
              panel.contourplot(...,
                                at=zscale$at[mainTicks], lwd=lwd[1],
                                labels=if (has.labels) {
                                  mainLabels
                                } else {FALSE},
                                region=FALSE, contour=TRUE)
              ## Contour lines for minor divisions (only log10Ticks)
              if (any(minorTicks)) {
                panel.contourplot(...,
                                  at=zscale$at[minorTicks], lwd=lwd[2],
                                  region=FALSE, contour=TRUE)
              }
            }
            
            ## And finally, the levelplot call
            if (missing(names.attr)){
              names.attr <- layerNames(object)
              } else {
                if (length(names.attr) != nlayers(object))
                  stop('Length of names.attr should match number of layers.')
                }
            p <- levelplot(form, data=df,
                           scales=scales,
                           aspect=aspect,
                           xlab=xlab, ylab=ylab, main = main, 
                           par.settings=par.settings,
                           between=between,
                           as.table=as.table,
                           xscale.components=xscale.components,
                           yscale.components=yscale.components,
                           colorkey=colorkey, 
                           contour=contour, region=region, labels=labels,
                           strip=strip.custom(factor.levels=names.attr),
                           ## The panel depends on zscaleLog and contour
                           panel=if (!is.null(zscaleLog) && has.contour) {
                             panelMixed 
                           } else {
                             requestedPanel 
                           },
                           ...)
            ## with the margins if needed
            if (nlayers(object)==1 && margin) {
              marginsLegend <- list(right=list(
                                      fun=legendY,
                                      args=list(p, FUN=FUN.margin, scale.y=scales.margin$y)),
                                    top=list(
                                      fun=legendX,
                                      args=list(p, FUN=FUN.margin, scale.x=scales.margin$x))
                                    )
              if (is.null(p$legend)) p$legend <- list()
              p$legend <- modifyList(p$legend, marginsLegend)
              
            }
            p
          }
          )
