globalVariables(c("value", "variable"))

if (!isGeneric("gplot")) {
    setGeneric("gplot", function(x, ...)
        standardGeneric("gplot"))
}	

setMethod("gplot", signature(x='Raster'), 
          function(x, maxpixels = 50000, palette = "magma", ...)
          {
              if (!requireNamespace("ggplot2", quietly = TRUE))
              {
                  stop("ggplot2 is required for the gplot method.")
              }
              xs <- sampleRegular(x, maxpixels, asRaster = TRUE)
              coords <- raster::xyFromCell(xs, seq_len(raster::ncell(xs)))
              dat <- stack(as.data.frame(raster::values(xs)))
              names(dat) <- c('value', 'variable')
              dat <- cbind(coords, dat)
              ## wkt provides the CRS in the format required by coord_sf
              mycrs <- raster::wkt(x)
              p <- ggplot2::ggplot(...) +
                  ## Set CRS with crs and axis labels with datum (if
                  ## datum is not used, coordinates are transformed
                  ## and shown as lat-lon values)
                  ggplot2::coord_sf(crs = mycrs, datum = mycrs,
                                    expand = FALSE) +
                  ggplot2::geom_raster(data=dat,
                                       ggplot2::aes(x = x, y = y,
                                                    fill = value)) + 
                  ggplot2::scale_fill_viridis_c(option = palette, na.value="transparent")
              ## Multilayer objects
              if (nlayers(x) > 1)
                  p + ggplot2::facet_wrap(~ variable)
              else
                  p
          }
          )


setMethod("gplot", signature(x='SpatRaster'), 
          function(x, maxpixels = 50000, palette = "magma", ...)  {
              if (!requireNamespace("ggplot2", quietly = TRUE))
              {
                  stop("ggplot2 is required for the gplot method.")
              }
              xs <- spatSample(x, maxpixels, "regular", as.raster = TRUE)
              coords <- terra::xyFromCell(xs, seq_len(terra::ncell(xs)))
              dat <- stack(as.data.frame(terra::values(xs)))
              names(dat) <- c('value', 'variable')
              dat <- cbind(coords, dat)
              mycrs <- terra::crs(x)
              p <- ggplot2::ggplot(...) +
                  ggplot2::coord_sf(crs = mycrs, datum = mycrs,
                                    expand = FALSE) + 
                  ggplot2::geom_raster(data = dat,
                                       ggplot2::aes(x = x, y = y,
                                                    fill = value)) +
                  ggplot2::scale_fill_viridis_c(option = palette, na.value="transparent")
              if (nlyr(x) > 1)
                  p + ggplot2::facet_wrap(~ variable)
              else
                  p
          }
          )

