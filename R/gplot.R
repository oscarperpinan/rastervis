if (!isGeneric("gplot")) {
	setGeneric("gplot", function(x, ...)
		standardGeneric("gplot"))
}	

setMethod("gplot", signature(x='Raster'), 
          function(x, maxpixels = 50000, ...)
          {
              if (!requireNamespace("ggplot2", quietly = TRUE))
              {
                  stop("ggplot2 is required for the gplot method.")
              }
              x <- sampleRegular(x, maxpixels, asRaster = TRUE)
              coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
              dat <- stack(as.data.frame(raster::values(x)))
              names(dat) <- c('value', 'variable')
              dat <- cbind(coords, dat)
              ggplot2::ggplot(ggplot2::aes(x = x, y = y),
                              data=dat, ...)

          }
          )



setMethod("gplot", signature(x='SpatRaster'), 
          function(x, maxpixels = 50000, ...)  {
              if (!requireNamespace("ggplot2", quietly = TRUE))
              {
                  stop("ggplot2 is required for the gplot method.")
              }
              x <- spatSample(x, maxpixels, "regular", as.raster = TRUE)
              coords <- terra::xyFromCell(x, seq_len(terra::ncell(x)))
              dat <- stack(as.data.frame(terra::values(x)))
              names(dat) <- c('value', 'variable')
              dat <- cbind(coords, dat)
              ggplot2::ggplot(ggplot2::aes(x = x, y = y),
                              data=dat, ...)
          }
          )

