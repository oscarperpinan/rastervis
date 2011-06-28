##Customization of lattice
xscale.raster <- function(...){ans <- xscale.components.default(...); ans$top=FALSE; ans}
yscale.raster <- function(...){ans <- yscale.components.default(...); ans$right=FALSE; ans}

rasterTheme <- function(pch=19, cex=0.7, region=rev(brewer.pal(9, 'YlOrRd')), ...) {

  theme <- custom.theme.2(pch=pch, cex=cex, region=region, ...)

  theme$strip.background$col='transparent' ##'lightgray'
  theme$strip.shingle$col='transparent'
  theme$strip.border$col='transparent'

  ## highlight.gpar=lattice.getOption('highlight.gpar')
  ## highlight.gpar$col <- 'black'
  ## theme$highlight.gpar <- highlight.gpar

  theme

}

  ##Auxiliary function for densityplot, histogram and bwplot

raster2dat <- function(x, FUN, maxpixels){
  nl <- nlayers(x)
  dat <- sampleRandom(x, maxpixels)
  dat <- as.data.frame(dat)
  names(dat) <- 1:nl
  dat <- stack(dat)
  z <- getZ(x)
  if (!missing(FUN) & !is.null(z)){
    FUN <- match.fun(FUN)   
    dat$ind <- factor(FUN(z))[dat$ind]
  } else {
    nms <- layerNames(x)
    nms <- reorder(factor(nms), 1:nl)
    dat$ind <- nms[dat$ind]
  }
  dat
}
