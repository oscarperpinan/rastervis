# Author: Oscar Perpinan Lamigueiro oscar.perpinan@upm.es
# Date :  January 2013
# Version 0.10
# Licence GPL v3
setGeneric('streamplot', function(object, ...){standardGeneric('streamplot')})

setMethod('streamplot',
          signature(object='Raster'),
          definition = function(object, layers,
            droplet = list(), streamlet = list(),
            par.settings=streamTheme(),
            isField = FALSE, reverse=FALSE, ##unit = 'radians',
            parallel=TRUE, ...){
            stopifnot(is.list(droplet))
            stopifnot(is.list(streamlet))
            ## uniVector extracts the unitary vector at point
            ## xy. It needs xrange, yrange and aspectVals as
            ## "global" variables
            uniVector <- function(xy){
              idx <- findInterval(xy[1], xrange, rightmost.closed=TRUE, all.inside=TRUE)
              idy <- findInterval(xy[2], rev(yrange), rightmost.closed=TRUE, all.inside=TRUE)
              idy <- nrow(aspectVals) - idy + 1
              ang <- aspectVals[idy, idx]
              c(sin(ang), cos(ang))
            }
            ## rk4 calculates the integral curve with Runge-Kutta
            ## order 4 from point p with step h
            rk4 <- function(p, h){
              xy <- p
              v <- uniVector(p)
              k1 <- h*v
              v2 <- uniVector(xy + 1/2*k1)
              k2 <- h*v2
              v3 <- uniVector(xy + 1/2*k2)
              k3 <- h*v3
              v4 <- uniVector(xy + k3)
              k4 <- h*v4
              xy + 1/6*k1 + 1/3*k2 + 1/3*k3 + 1/6*k4
            }
            ## streamLine creates a streamlet starting from point
            ## p, with L steps of length h both in forward and
            ## backward directions. Starting point is included in
            ## the result, therefore total length is 2*(L*h) + 1).
            streamLine <- function(p, h=0.5, L=10, pal, cex){
              ## Color class
              cl <- p[3]
              ## Coordinates
              p <- matrix(p[1:2], ncol=2)
              ## Forward direction
              forw <- matrix(nrow=L, ncol=2)
              xy <- p
              for (i in 1:L){
                xy <- rk4(xy, h=h)
                forw[i,] <- xy
              }
              ## Backward direction
              back <- matrix(nrow=L, ncol=2)
              xy <- p
              for (i in 1:L){
                xy <- rk4(xy, h=-h)
                back[i,] <- xy
              }
              ## Streamlet: backward -> starting point -> forward
              pts <- rbind(back[L:1,], p, forw)
              ## Only non-NA points are useful
              whichNA <- apply(pts, 1, function(x)any(is.na(x)))
              pts <- pts[!whichNA,]


              ## Perhaps only the starting point remains...
              if (!is.matrix(pts)) stream <- list(x=pts[1], y=pts[2])
              else stream <- list(x=pts[,1], y=pts[,2])
              ## Color and size
              ncolors <- length(stream$x)
              color <- pal[cl]
              stream$colors <- rev(colorRampPalette(c(color, 'gray50'))(ncolors))
              stream$cexs <- seq(.3, cex, length=ncolors)
              stream
            }

            if (isField) {
              field <- object 
              } else {
              field <- terrain(object, opt = c("slope", "aspect"))##, unit=unit)
              }
            ## Values to be used by uniVector
            xrange <- xFromCol(field, 1:ncol(field))
            yrange <- yFromRow(field, 1:nrow(field))
            aspectVals <- as.matrix(subset(field, 2))
            ## Should streamlets go from sinks to sources?
            if (reverse) aspectVals <- aspectVals + pi
            
            ## Droplets and streamlets configuration
            default.droplet <- list(cropExtent = .97, pc = .5)
            droplet <- modifyList(default.droplet, droplet)

            default.streamlet <- list(L=10, h = mean(res(object)))
            streamlet <- modifyList(default.streamlet, streamlet)

            ## Crop original object to avoid droplets at boundaries
            cropField <- crop(field, extent(field)*droplet$cropExtent)
            nDroplets <- ncell(cropField) * droplet$pc/100
            ## Build a regular grid ...
            texture <- sampleRegular(cropField, nDroplets, sp=TRUE)
            ## but only with coordinates where slope (texture[[1]]
            ## is not NA
            crds <- coordinates(texture[!is.na(texture[[1]]),])
            ## Add jitter to regular grid
            crdsNoise <- cbind(jitter(crds[,1]), jitter(crds[,2]))
            ## "Grid" of droplets
            texture <- SpatialPointsDataFrame(crdsNoise, data.frame(extract(field, crdsNoise)))
            pts <- data.frame(t(coordinates(texture)))
            npts <- length(texture)

            ## Color classes
            pars <- if (is.function(par.settings)) {
              par.settings()$superpose.symbol
            } else {
              par.settings$superpose.symbol
            }
            slopeVals <- texture[[1]]
            nClasses <- length(pars$col)
            colClasses <- pretty(slopeVals, n=nClasses)
            indCol <- findInterval(slopeVals, colClasses,
                                   rightmost.closed=TRUE, all.inside=TRUE)
            ## colClasses <- cut(slopeVals, nClasses)
            ## indCol <- as.numeric(colClasses)

            ## Stream Lines Calculation
            pts <- rbind(pts, indCol)
            h <- streamlet$h
            L <- streamlet$L
            ## Disable parallel if runinng Windows
            if (.Platform$OS.type == "windows") parallel <- FALSE

            if (parallel && require(parallel)) {
              streamList <- mclapply(pts, streamLine, h=h, L=L,
                                     pal=pars$col, cex=pars$cex,
                                     mc.cores=detectCores())
            } else {
              streamList <- lapply(pts, streamLine, h=h, L=L,
                                     pal=pars$col, cex=pars$cex)
            }

            ## Points with low slope values are displayed earlier
            idOrdered <- order(slopeVals)
            streamList <- streamList[order(slopeVals)]

            ## key <- list(space='right', at=colClasses, col=pal)
            
            p <- levelplot(object, layers=1,
                           margin=FALSE, colorkey=FALSE,
                           par.settings=par.settings) + ##,
                           ##colorkey=key) + 
                             layer(lapply(streamList, function(streamlet){
                               panel.points(streamlet$x, streamlet$y,
                                            col=streamlet$colors, cex=streamlet$cexs)
                               }), data= list(streamList=streamList))
                               ##{
                               ## pointPars <- trellis.par.get("superpose.symbol")
                               ## pal <- pointPars$col
                               ## for (i in seq_along(streamList)) {
                               ##   streamlet <- streamList[[i]]
                                 ## ## Assign colors to slope classes
                                 ## color <- pal[streamlet$cl]
                                 ## ncolors <- length(streamlet$x)
                                 ## colors <- rev(colorRampPalette(c(color, 'gray50'))(ncolors))
                                 ## cexs <- seq(.3, pointPars$cex, length=ncolors)
                                 ## panel.points(streamlet$x, streamlet$y,
                                 ##              col=colors, cex=cexs)
                                 ## panel.points(streamlet$x, streamlet$y,
                                 ##              col=streamlet$colors, cex=streamlet$cexs)
                             ##   }
                             ## }, data= list(streamList=streamList))
            p
          }
          )


setMethod('streamplot',
          signature(object='RasterStack'),
          definition = function(object, layers,
            droplet = list(), streamlet = list(),
            par.settings=streamTheme(),
            isField = FALSE, reverse=FALSE, ##unit = 'radians',
            parallel=TRUE, ...){
            if (isField) callNextMethod(object, layers, droplet, streamlet,
                                        par.settings, isField=TRUE, reverse,
                                        parallel, ...)
            else {
              if (!missing(layers)) {
                object <- subset(object, subset=layers)
              }
              streamplotList <- lapply(unstack(object), streamplot,
                                       ## layers=layers,
                                       droplet=droplet, streamlet=streamlet,
                                       par.settings=par.settings, parallel=parallel,
                                       isField=FALSE, reverse=reverse, ...)##unit=unit, ...)
              names(streamplotList) <- names(object)
              dots <- list(...)
              streamplotList <- c(streamplotList, dots)
              p <- do.call(c, streamplotList)
              p
              }
            })
