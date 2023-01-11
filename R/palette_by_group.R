palette_by_group <- function(nGroups, nIter) {

    green_funct <- colorRampPalette(c('lightgreen','darkgreen'))
    orange_funct <- colorRampPalette(c('orange', 'darkred'))
    blue_funct <- colorRampPalette(c('lightblue', 'darkblue'))
    purple_funct <- colorRampPalette(c('magenta', 'purple'))
    gold_funct <- colorRampPalette(c('tan', 'gold'))
    grey_funct <- colorRampPalette(c('lightgrey', 'black'))

    list_of_colours <- vector(mode = 'character')

    if (nGroups > 0) list_of_colours <- c(list_of_colours,
                                          green_funct(nIter))

    if (nGroups > 1) list_of_colours <- c(list_of_colours,
                                          orange_funct(nIter))

    if (nGroups > 2) list_of_colours <- c(list_of_colours,
                                          blue_funct(nIter))

    if (nGroups > 3) list_of_colours <- c(list_of_colours,
                                          gold_funct(nIter))

    if (nGroups > 4) list_of_colours <- c(list_of_colours,
                                          purple_funct(nIter))

    if (nGroups > 5) list_of_colours <- c(list_of_colours,
                                          grey_funct(nIter))

    # plot(rep(1,100), col=green_funct(100), pch=19, cex=3)
    # plot(rep(1,100), col=orange_funct(100), pch=19, cex=3)
    # plot(rep(1,100), col=blue_funct(100), pch=19, cex=3)
    # plot(rep(1,100), col=purple_funct(100), pch=19, cex=3)
    # plot(rep(1,100), col=gold_funct(100), pch=19, cex=3)
    # plot(rep(1,100), col=grey_funct(100), pch=19, cex=3)

    # Exclude this bit?
    # structure(.Data = list_of_colours,
    #           name = 'groupalette',
    #           class = 'palette')
    #
    return(list_of_colours)

}
