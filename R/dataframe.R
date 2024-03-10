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
        df <- spatSample(data,
                          size = maxpixels,
                          method = "regular",
                          xy = TRUE)
        names(df) <- c("x", "y", names(data))
    }
    df
}
