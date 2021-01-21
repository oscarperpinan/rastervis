##Auxiliary function for densityplot, histogram and bwplot
raster2dat <- function(x, FUN, maxpixels, att){
    if (is(x, "SpatRaster"))
    {
        nl <- nlyr(x)
        
        if (maxpixels < ncell(x))
            dat <- spatSample(x, maxpixels, method = "sample")
        else
            dat <- values(x)

        z <- time(x)
    }
    else
    {
        nl <- nlayers(x)

        if (maxpixels < ncell(x))
            dat <- sampleRandom(x, maxpixels)
        else
            dat <- getValues(x)

        z <- getZ(x)
     }


    ## Is factor?
    factorLayers <- is.factor(x)
    isFactor <- all(factorLayers)
    anyFactor <- any(factorLayers)
    
    if (anyFactor & !isFactor) {
        stop('Raster* with factor and numeric layers cannot be displayed.')
    }
    
    if (nl > 1)
    {
        dat <- as.data.frame(dat)
        ##http://r.789695.n4.nabble.com/Column-order-in-stacking-unstacking-td3349953.html
        idx <- sprintf("%s%03d", "X", 1:nl) 
        names(dat) <- idx
        dat <- stack(dat)
        if (!missing(FUN) & !is.null(z)){
            FUN <- match.fun(FUN)   
            dat$ind <- factor(FUN(z))[dat$ind]
        } else {
            nms <- names(x)
            nms <- reorder(factor(nms), 1:nl)
            dat$ind <- nms[dat$ind]
        }
    }

    if (isFactor)
    {
        rat <- levels(x)
        ## It works correctly only if all the layers
        ## share the same RAT
        if (length(rat)>1 && any(!duplicated(rat)[-1])){
            stop('all the layers must share the same RAT.')
        }
        else
        {   ## Next line fails for terra objects. There is not a
            ## data.farame method defined for the result of the
            ## "levels" function.
            rat <- as.data.frame(rat[[1]])
            ## choose which level to use for the legend
            if (is.numeric(att)) att = att + 1
        }
        if (nl > 1)
            dat$values <- factor(dat$values, rat$ID, rat[,att])
        else
            dat <- factor(dat, rat$ID, rat[,att])
    }

    dat
}

