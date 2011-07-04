# Author: Robert J. Hijmans, based on an example by Paul Hiemstra
# Date :  June 2011
# Version 1.0
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
		if (nl == 1) {
			x <- data.frame(xyFromCell(x, 1:ncell(x)), value=values(x))
			x <- melt(x, id.vars = c('x','y'), na.rm = TRUE)
		} else {
			x <- data.frame(xyFromCell(x, 1:ncell(x)), values(x))
			x <- melt(x, id.vars = c('x','y'), na.rm = TRUE)
		}
		ggplot(aes(x=x, y=y), data=x, ...) 
	}
)

