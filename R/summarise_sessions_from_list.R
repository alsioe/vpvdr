#' Summarises the returned output list from a simulation
#'
#'
#' @param list
#'
#' @return
#' @export
#'
#' @examples
summarise_sessions_from_list <- function(list) {
    nSubj <- length(list$subjID)
    nGroups <- length(unique(list$grouping))
    nTrials <- length(list$trial)

    stim_left <- list$stim_left
    stim_right <- list$stim_right

    trial_type <- stim_left + stim_right
    trial_type[trial_type == 50] <- 'neg'
    trial_type[trial_type == 100] <- 'vd'
    trial_type[trial_type == 150] <- 'pos'

    p_right <- list$p_right
    chose_right <- list$chose_right
    outcome <- list$outcome

    # long_df <- data.frame(
    #                 subjID = vector(mode = 'integer'),
    #                 grouping = vector(mode = 'integer'),
    #                 trial = vector(mode = 'integer'),
    #                 stim_left = vector(mode = 'character'),
    #                 stim_right = vector(mode = 'character'),
    #                 p_right = vector(mode = 'numeric'),
    #                 chose_right = vector(mode = 'logical'),
    #                 outcome = vector(mode = 'logical')
    #                     )
    long_df <- data.frame()


    # We need to bind all the elements from the list together;
    # this might be faster with more complex rep() functions
    # and c() for the the matrix-to-vector conversions
    for (i in 1:nSubj) {
        wDf <- data.frame(
            subjID = rep(x = i, times = nTrials),
            grouping = rep(x = list$grouping[i], times = nTrials),
            trial = 1:nTrials,
            stim_left = stim_left,
            stim_right = stim_right,
            trial_type = trial_type,
            p_right = p_right[, i],
            chose_right = chose_right[, i],
            outcome = outcome[, i]
        )


        # Enter choice of stimulus
        wDf$choice <- NA
        wDf$choice[wDf$chose_right == 0] <-
            wDf$stim_left[wDf$chose_right == 0]
        wDf$choice[wDf$chose_right == 1] <-
            wDf$stim_right[wDf$chose_right == 1]

        # Enter correct; remember '50' is correct on 'neg' trials (0 vs. 50)
        wDf$correct <- FALSE
        wDf$correct[wDf$trial_type == 'neg'] <-
            wDf$choice[wDf$trial_type == 'neg'] == 50
        wDf$correct[wDf$trial_type == 'vd'] <-
            wDf$choice[wDf$trial_type == 'vd'] == 100
        wDf$correct[wDf$trial_type == 'pos'] <-
            wDf$choice[wDf$trial_type == 'pos'] == 100

        # Enter session
        wDf$session <- NA
        wDf$session <- ceiling(wDf$trial / 200)

        # Turn into factors
        wDf$trial_type <- factor(x = wDf$trial_type,
                                     levels = c('vd',
                                                'pos',
                                                'neg')
        )
        wDf$subjID <- as.factor(wDf$subjID)
        wDf$grouping <- as.factor(wDf$grouping)

        long_df <- rbind(long_df,
                         wDf %>%
                             group_by(subjID, trial_type, session) %>%
                             summarise(mean = mean(correct),
                                       n = n(),
                                       quantile(correct,
                                                c(0.025, 0.25, 0.5, 0.75, 0.975),
                                                q = c(0.025, 0.25, 0.5, 0.75, 0.975)
                                                )
                                       )
                         )

    }

    return(long_df)

}



#     # Enter choice of stimulus
#     long_df$choice <- NA
#     long_df$choice[long_df$chose_right == 0] <-
#         long_df$stim_left[long_df$chose_right == 0]
#     long_df$choice[long_df$chose_right == 1] <-
#         long_df$stim_right[long_df$chose_right == 1]
#
#     # Enter correct; remember '50' is correct on 'neg' trials (0 vs. 50)
#     long_df$correct <- FALSE
#     long_df$correct[long_df$trial_type == 'neg'] <-
#         long_df$choice[long_df$trial_type == 'neg'] == 50
#     long_df$correct[long_df$trial_type == 'vd'] <-
#         long_df$choice[long_df$trial_type == 'vd'] == 100
#     long_df$correct[long_df$trial_type == 'pos'] <-
#         long_df$choice[long_df$trial_type == 'pos'] == 100
#
#     # Enter session
#     long_df$session <- NA
#     long_df$session <- ceiling(long_df$trial / 200)
#
#     # Turn into factors
#     long_df$trial_type <- factor(x = long_df$trial_type,
#                                  levels = c('vd',
#                                             'pos',
#                                             'neg')
#                                  )
#     long_df$subjID <- as.factor(long_df$subjID)
#     long_df$grouping <- as.factor(long_df$grouping)
#
#     #RETURN
#     long_df %>%
#         group_by(session,
#                  trial_type,
#                  subjID,
#                  grouping) %>%
#         summarise(mean = mean(correct),
#                   n = n())
#
# }
