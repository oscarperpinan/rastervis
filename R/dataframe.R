dfRegular <- function(data, maxpixels)
{
    ## Names of layers
    nms <- names(data)
    ## Ensure valid names
    nms <- make.names(nms, unique = TRUE)
    
    ## Number of layers
    if (is(data, "SpatRaster"))
        nly  <- nlyr(data)
    else
        nly  <- nlayers(data)
    
    if (is(data, "SpatRaster"))
    {
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
    names(df) <- nms
    
    df <- cbind(data.frame(x=xLayer, y=yLayer), df)
    
    df
}
