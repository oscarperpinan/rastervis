dfRegular <- function(data, maxpixels)
{
    if (is(data, "Raster"))
    {
        dataSample <- sampleRegular(data,
                                    size = maxpixels,
                                    asRaster = TRUE)
        df <- raster::as.data.frame(dataSample, xy = TRUE)
        names(df) <- c("x", "y", names(data))
    } else
    {
        idx <- spatSample(data,
                          size = maxpixels,
                          method = "regular",
                          cells = TRUE)
        vals <- data[idx]
        xy <- terra::xyFromCell(data, idx)
        df <- cbind(xy, vals)
    }

    df
}
