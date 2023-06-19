#' Summarises data by subject (not group)
#'
#' @param list Data structure from simulate_choices_vpvd
#'
#' @param groups Names of the groups/conditions in the experiment.
#'
#' @return Data frame with session data for each subject and trial type
#' @export
#'
#' @examples
summarise_correct_by_subj <- function(list,
                                      groups = 1:length(unique(list$grouping))
                                      ) {
    nSubj <- length(list$subjID)
    nGroups <- length(unique(list$grouping))
    nTrials <- length(list$trial)

    df <- data.frame(
        subjID = sort(rep(x = unique(list$subjID),
                          times = nTrials)
                      ),
        grouping = sort(rep(x = unique(list$grouping),
                            times = nSubj / nGroups * nTrials)
                        ),
        trial = rep(x = 1:nTrials,
                    times = nSubj),
        stim_left = rep(x = list$stim_left,
                        times = nSubj),
        stim_right = rep(x = list$stim_right,
                         times = nSubj),
        p_right = c(list$p_right),
        chose_right = c(list$chose_right),
        outcome = c(list$outcome)
        )

    # Enter trial type
    df$trial_type <- df$stim_left + df$stim_right
    df$trial_type[df$trial_type == 50] <- 'neg'
    df$trial_type[df$trial_type == 100] <- 'vd'
    df$trial_type[df$trial_type == 150] <- 'pos'

    # Enter choice of stimulus
    df$choice <- NA
    df$choice[df$chose_right == 0] <-
        df$stim_left[df$chose_right == 0]
    df$choice[df$chose_right == 1] <-
        df$stim_right[df$chose_right == 1]

    # Enter correct; remember '50' is correct on 'neg' trials (0 vs. 50)
    df$correct <- FALSE
    df$correct[df$trial_type == 'neg'] <-
        df$choice[df$trial_type == 'neg'] == 50
    df$correct[df$trial_type == 'vd'] <-
        df$choice[df$trial_type == 'vd'] == 100
    df$correct[df$trial_type == 'pos'] <-
        df$choice[df$trial_type == 'pos'] == 100

    # Enter session
    df$session <- NA
    df$session <- ceiling(df$trial / 200)

    # Turn into factors
    df$trial_type <- factor(x = df$trial_type,
                            levels = c('vd',
                                       'pos',
                                       'neg')
                            )

    df$subjID <- as.factor(df$subjID)

    df$grouping <- factor(x = df$grouping,
                          labels = groups)

    #RETURN
    df %>%
        group_by(session,
                 trial_type,
                 subjID,
                 grouping) %>%
        summarise(correct = mean(correct))

}
