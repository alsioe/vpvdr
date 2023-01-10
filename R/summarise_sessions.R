summarise_sessions <- function(df) {

    # Enter trial types
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

    # Enter correct
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

    df$trial_type <- factor(df$trial_type, levels = c('vd',
                                                      'pos',
                                                      'neg'))
    df$subjID <- as.factor(df$subjID)
    df$grouping <- as.factor(df$grouping)

    #RETURN
    df %>%
        group_by(session,
                 trial_type,
                 subjID,
                 grouping) %>%
        summarise(mean = mean(correct),
                  n = n())

}
