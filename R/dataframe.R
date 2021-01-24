dfRegular <- function(data, maxpixels)
{
    ## Names of layers
    nms <- names(data)
    ## Ensure valid names
    nms <- make.names(nms, unique = TRUE)
    
    
    if (is(data, "SpatRaster"))
    {
        nly  <- nlyr(data)
        dataSample <- spatSample(data,
                                 size = maxpixels,
                                 as.raster = TRUE)
        xLayer <- init(dataSample, fun='x')
        yLayer <- init(dataSample, fun='y')
        
        df <- values(dataSample)
        xLayer <- values(xLayer)
        yLayer <- values(yLayer)
    }
    else 
    {
        nly  <- nlayers(data)
        dataSample <- sampleRegular(data,
                                    size = maxpixels,
                                    asRaster = TRUE)
        xLayer <- init(dataSample, fun='x')
        yLayer <- init(dataSample, fun='y')
        
        df <- getValues(dataSample)
        xLayer <- getValues(xLayer)
        yLayer <- getValues(yLayer)
    }
    
    df <- as.data.frame(df)
    
    df <- cbind(data.frame(x=xLayer, y=yLayer), df)
    names(df) <- c("x", "y", nms)
    
    df
}
