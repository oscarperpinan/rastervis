drawCK <- function(key, space = 'right')
{
    ## Get the grob produced by draw.colorkey
    ck <- draw.colorkey(key)
    ## Define the graphical parameter using the axis.text values as default
    if (is.null(key$title.gpar)) key$title.gpar <- list()
    title.gpar <- modifyList(trellis.par.get("axis.text"),
                             key$title.gpar)
    title.gpar <- do.call(gpar, title.gpar)
    ## Add the title in a suitable position, depending on the location
    ## of the colorkey (defined by "space").  Although draw.colorkey
    ## defines a layout with three rows/cols, but two of them are
    ## empty (reserved?), I am not able to use the free
    ## rows/cols. Instead, using the row/col occupied by the color bar
    ## with packGrob, and respecting the height/width with
    ## grobHeight/grobWidth, I get correct results
    ck <- switch(space,
                 right = packGrob(ck,
                                  textGrob(key$title,
                                           x = 0,
                                           y = unit(1, 'npc') + unit(0.6, 'lines'),
                                           just = 'left', gp = title.gpar),
                                  row = 2, col = 3,
                                  height = grobHeight(ck)),
                 left = packGrob(ck,
                                 textGrob(key$title,
                                          x = 1,
                                          y = unit(1, 'npc') + unit(0.6, 'lines'),
                                          just = 'right', gp = title.gpar),
                                 row = 2, col = 1,
                                 height = grobHeight(ck)),
                 bottom = packGrob(ck,
                                   textGrob(key$title,
                                            y = 0.5,
                                            x = unit(0, 'npc') - unit(0.6, 'lines'),
                                            just = 'right', gp = title.gpar),
                                   row = 3, col = 2,
                                   width = grobWidth(ck)),
                 top = packGrob(ck,
                                textGrob(key$title,
                                         y = 0.5,
                                         x = unit(0, 'npc') - unit(0.6, 'lines'),
                                         just = 'right', gp = title.gpar),
                                row = 1, col = 2,
                                width = grobWidth(ck))
                 )
    ck
}
