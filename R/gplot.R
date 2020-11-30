if (!isGeneric("gplot")) {
	setGeneric("gplot", function(x, ...)
		standardGeneric("gplot"))
}	


.gplot <- function(x, ...)  {
    if (!requireNamespace("ggplot2", quietly = TRUE))
	{
		stop("ggplot2 is required for the gplot method.")
	}

	coords <- xyFromCell(x, seq_len(ncell(x)))
	dat <- stack(as.data.frame(values(x)))
	names(dat) <- c('value', 'variable')
	dat <- cbind(coords, dat)
	ggplot2::ggplot(ggplot2::aes(x = x, y = y),
                                  data=dat, ...)
}


setMethod("gplot", signature(x='Raster'), 
          function(x, maxpixels = 50000, ...)  {
              x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
              .gplot(x, ...)
          }
          )



setMethod("gplot", signature(x='SpatRaster'), 
          function(x, maxpixels = 50000, ...)  {
              x <- terra::spatSample(x, maxpixels, "regular", as.raster = TRUE)
              .gplot(x, ...)
          }
          )

