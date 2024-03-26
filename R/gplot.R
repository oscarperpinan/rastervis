globalVariables(c("value", "variable"))

if (!isGeneric("gplot")) {
    setGeneric("gplot", function(x, ...)
        standardGeneric("gplot"))
}	

.gplot <- function(df, nly, isll, crs, palette, show.legend, ...)
{
    
    p <- ggplot2::ggplot(...) +
        ## Set CRS with crs and axis labels with datum (if
        ## datum is not used, coordinates are transformed
        ## and shown as lat-lon values)
        ggplot2::coord_sf(crs = crs, datum = crs,
                          expand = FALSE) +
        ggplot2::geom_raster(data = df,
                             ggplot2::aes(x = x, y = y,
                                          fill = value),
                             show.legend = show.legend)
    if (is.character(palette))
        p <- p + ggplot2::scale_fill_viridis_c(option = palette,
                                               na.value="transparent")
    else
        p <- p + palette
    if (isTRUE(isll))
        p <- p + xlab("Longitude") + ylab("Latitude")
    ## Multilayer objects
    if (nly > 1)
        p + ggplot2::facet_wrap(~ variable)
    else
        p
}
 
setMethod("gplot", signature(x='Raster'), 
          function(x, maxpixels = 50000, palette = "magma", show.legend = TRUE, ...)
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
              
              .gplot(df = dat,
                     nly = raster::nlayers(x),
                     isll = raster::isLonLat(x), ## Is longitude-latitude?
                     crs = raster::wkt(x), ## wkt provides the CRS in
                                           ## the format required by
                                           ## coord_sf
                     palette = palette,
                     show.legend = show.legend,
                     ...)
          }
          )


setMethod("gplot", signature(x='SpatRaster'), 
          function(x, maxpixels = 50000, palette = "magma", show.legend = TRUE, ...)  {
              if (!requireNamespace("ggplot2", quietly = TRUE))
              {
                  stop("ggplot2 is required for the gplot method.")
              }
              xs <- spatSample(x, maxpixels, "regular", as.raster = TRUE)
              coords <- terra::xyFromCell(xs, seq_len(terra::ncell(xs)))
              dat <- stack(as.data.frame(terra::values(xs)))
              names(dat) <- c('value', 'variable')
              dat <- cbind(coords, dat)
              
              .gplot(df = dat,
                     nly = terra::nlyr(x),
                     isll = terra::is.lonlat(x), ## Is longitude-latitude?
                     crs = terra::crs(x), ## wkt provides the CRS in
                                           ## the format required by
                                           ## coord_sf
                     palette = palette,
                     show.legend = show.legend,
                     ...)
          }
          )

