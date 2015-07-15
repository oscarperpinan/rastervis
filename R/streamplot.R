setGeneric('streamplot', function(object, ...){standardGeneric('streamplot')})

setMethod('streamplot',
          signature(object='Raster'),
          definition = function(object, layers,
            droplet = list(), streamlet = list(),
            par.settings=streamTheme(),
            isField = FALSE, reverse=FALSE, ##unit = 'radians',
            parallel=TRUE, mc.cores=detectCores(), cl=NULL,
            ...){
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
              colClass <- p[3]
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
              color <- pal[colClass]
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
            texture <- SpatialPointsDataFrame(crdsNoise,
                                              data.frame(extract(field, crdsNoise)))
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

            ## Stream Lines Calculation
            pts <- rbind(pts, indCol)
            h <- streamlet$h
            L <- streamlet$L

            ## Forking does not work with Windows
            if (.Platform$OS.type == "windows" & is.null(cl)) parallel <- FALSE

            if (isTRUE(parallel)) {
              if (!is.null(cl)) { ## parallel with a cluster
                streamList <- parLapply(cl, pts, streamLine, h=h, L=L,
                                        pal=pars$col, cex=pars$cex)
              } else { ## parallel with forking
                streamList <- mclapply(pts, streamLine, h=h, L=L,
                                       pal=pars$col, cex=pars$cex,
                                       mc.cores=mc.cores)
              }
            } else { ## without parallel
              streamList <- lapply(pts, streamLine, h=h, L=L,
                                   pal=pars$col, cex=pars$cex)
            }

            ## Points with low slope values are displayed earlier
            idOrdered <- order(slopeVals)
            streamList <- streamList[order(slopeVals)]

            p <- levelplot(object, layers=1,
                           margin=FALSE, colorkey=FALSE,
                           par.settings=par.settings, ...) + 
                             layer(lapply(streamList, function(streamlet){
                               panel.points(streamlet$x, streamlet$y,
                                            col=streamlet$colors, cex=streamlet$cexs)
                               }), data= list(streamList=streamList))

            p
          }
          )


setMethod('streamplot',
          signature(object='RasterStack'),
          definition = function(object, layers,
            droplet = list(), streamlet = list(),
            par.settings=streamTheme(),
            isField = FALSE, reverse=FALSE, ##unit = 'radians',
            parallel=TRUE, mc.cores=detectCores(), cl=NULL,
            ...){
              if (missing(layers)) layers=seq_len(nlayers(object))

              if (isField == 'dXY'){
                  isField <- TRUE
                  u <- subset(object, 1)
                  v <- subset(object, 2)
                  slope <- sqrt(u^2 + v^2)
                  aspect <- atan2(v, u)
                  aspect <- (pi/2 - aspect) %% (2 * pi)
                  object <- stack(slope, aspect)
              }
              
              if (isTRUE(isField))
                  callNextMethod(object, layers, droplet,
                                 streamlet,
                                 par.settings,
                                 isField=TRUE,
                                 reverse,
                                 parallel,
                                 mc.cores, cl, ...)
            else {
              if (!missing(layers)) {
                  object <- subset(object, subset=layers)
              }
              objectList <- unstack(object)
              names(objectList) <- names(object)
              p <- xyplot.list(objectList, FUN=streamplot,
                               droplet=droplet, streamlet=streamlet,
                               par.settings=par.settings,
                               isField=FALSE, reverse=reverse,
                               parallel=parallel, mc.cores=mc.cores,
                               cl, ...)
              p
          }
          })
