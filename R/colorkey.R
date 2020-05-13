drawCK <- function(key, space = 'right')
{
    ## Get the grob produced by draw.colorkey
    ck <- draw.colorkey(key)
    ## Define the graphical parameter using the axis.text values as
    ## default
    if (is.null(key$title.gpar)) key$title.gpar <- list()
    title.gpar <- modifyList(trellis.par.get("axis.text"),
                             key$title.gpar)
    title.gpar <- do.call(gpar, title.gpar)
    ## Add the title in a suitable position, depending on the location
    ## of the colorkey (defined by "space").  
    title.grob <-
        switch(space,
               right = textGrob(key$title,
                                x = 0,
                                y = unit(1, 'npc') + unit(0.6, 'lines'),
                                just = 'left', gp = title.gpar),
               left = textGrob(key$title,
                               x = 1,
                               y = unit(1, 'npc') + unit(0.6, 'lines'),
                               just = 'right', gp = title.gpar),
               bottom = textGrob(key$title,
                                 y = 0.5,
                                 x = unit(0, 'npc') - unit(0.6, 'lines'),
                                 just = 'right', gp = title.gpar),
               top = textGrob(key$title,
                              y = 0.5,
                              x = unit(0, 'npc') - unit(0.6, 'lines'),
                              just = 'right', gp = title.gpar),
               )
    zw <- unit(0, "mm")
    ck <- switch(space,
                 right = packGrob(ck, title.grob,
                                  row = 1, col = 3,
                                  height = zw),
                 left = packGrob(ck, title.grob,
                                 row = 1, col = 1,
                                 height = zw),
                 bottom = packGrob(ck, title.grob,
                                   row = 3, col = 1,
                                   width = zw),
                 top = packGrob(ck, title.grob,
                                row = 1, col = 1,
                                width = zw))
    ck
}
