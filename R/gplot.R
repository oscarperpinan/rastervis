# Author: Robert J. Hijmans, based on an example by Paul Hiemstra
# Date :  June 2011
# Version 1.1
# Updated: Oscar Perpiñán January 2012 (melt is not used anymore)
# Licence GPL v3


if (!isGeneric("gplot")) {
	setGeneric("gplot", function(x, ...)
		standardGeneric("gplot"))
}	

setMethod("gplot", signature(x='Raster'), 
          function(x, maxpixels=50000, ...)  {
            stopifnot(require(ggplot2))
            nl <- nlayers(x)
            if (ncell(x) > maxpixels) {
              x <- sampleRegular(x, maxpixels, asRaster=TRUE)
            }

            ## if (nl == 1) {
            ## 	x <- data.frame(xyFromCell(x, 1:ncell(x)), value=values(x))
            ## 	x <- melt(x, id.vars = c('x','y'), na.rm = TRUE)
            ## } else {
            ## 	x <- data.frame(xyFromCell(x, 1:ncell(x)), values(x))
            ## 	x <- melt(x, id.vars = c('x','y'), na.rm = TRUE)
            ## }

            coords <- xyFromCell(x, seq_len(ncell(x)))
            ## Extract values 
            dat <- stack(as.data.frame(getValues(x)))
            names(dat) <- c('value', 'variable')

            dat <- cbind(coords, dat)

            ggplot(aes(x=x, y=y), data=dat, ...) 
          }
          )
