# Author: Oscar Perpinan Lamigueiro oscar.perpinan@upm.es
# Date :  June 2011
# Version 0.10
# Licence GPL v3

##Customization of lattice
xscale.raster <- function(...){ans <- xscale.components.default(...); ans$top=FALSE; ans}
yscale.raster <- function(...){ans <- yscale.components.default(...); ans$right=FALSE; ans}

rasterTheme <- function(pch=19, cex=0.7, region=rev(brewer.pal(9, 'YlOrRd')), ...) {
  theme <- custom.theme.2(pch=pch, cex=cex, region=region, ...)
  theme$strip.background$col='transparent'
  theme$strip.shingle$col='transparent'
  theme$strip.border$col='transparent'
  theme
}

RdBuTheme <- function(region=brewer.pal(9, 'RdBu'), ...) {
  theme <- rasterTheme(region=region, ...)
  theme
}

GrTheme <- function(region=rev(brewer.pal(9, 'Greys')), ...) {
  theme <- rasterTheme(region=region, ...)
  theme
}

BTCTheme <- function(region=BTC(n=9), ...) {
  theme <- rasterTheme(region=region, ...)
  theme
}


##Auxiliary function for densityplot, histogram and bwplot

raster2dat <- function(x, FUN, maxpixels){
  nl <- nlayers(x)
  dat <- sampleRandom(x, maxpixels)
  dat <- as.data.frame(dat)
  ##http://r.789695.n4.nabble.com/Column-order-in-stacking-unstacking-td3349953.html
  idx <- sprintf("%s%03d", "X", 1:nl) 
  names(dat) <- idx
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
