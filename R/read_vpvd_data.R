#' Read VPVD trail-by-trial data to overlay posterior probability and real data
#'
#' @param vpvd_file
#' @param dose_file
#'
#' @return
#' @export
#'
#' @examples
read_vpvd_data <- function(vpvd_file,
                           dose_file) {

    data <- read_csv(vpvd_data_file)

    doses <- read_csv(vpvd_dose_file)

    names(data) <- c('subjID',
                     'session',
                     'sess_trial',
                     'trial',
                     'cs_plus',
                     'stim_left',
                     'stim_right',
                     'stim_chosen',
                     'is_probe',
                     'outcome')
    dim(data)
    # Remove missing data
    data <- data[!is.na(data$subjID), ]

    # Correct for misnamed stimulus 'backslash'
    # "backslash"  "backslah.jpg" "backslash (2).jpg"

    # Using this logic
    # grep(pattern = 'backs',
    #      x = data$stim_left,
    #      value = FALSE)

    data$stim_left[grep(pattern = 'backs',
                        x = data$stim_left,
                        value = FALSE)] <- 'backslash'

    data$stim_right[grep(pattern = 'backs',
                         x = data$stim_right,
                         value = FALSE)] <- 'backslash'

    data$cs_plus[grep(pattern = 'backs',
                      x = data$cs_plus,
                      value = FALSE)] <- 'backslash'

    # Recode the sessions to start from 1
    data$session <- data$session - 200

    # Discard sessions higher than 14
    data <- data[data$session < 15, ]

    # Recode the stimulus names to numbers (1, 2, 9)
    # Left stimulus ...
    data$stim_left[data$stim_left == 'Diamonds'] <- 9
    data$stim_left[data$stim_left == data$cs_plus] <- 1
    data$stim_left[(data$stim_left != '1') & (data$stim_left != '9')] <- 2
    unique(data$stim_left)

    # ... and right stimulus
    data$stim_right[data$stim_right == 'Diamonds'] <- 9
    data$stim_right[data$stim_right == data$cs_plus] <- 1
    data$stim_right[(data$stim_right != '1') & (data$stim_right != '9')] <- 2
    unique(data$stim_right)

    data$stim_left <- as.numeric(data$stim_left)
    data$stim_right <- as.numeric(data$stim_right)

    # Create a variable that captures the trial type
    data$trial_type <- data$stim_left + data$stim_right

    # Recording the trial types
    map_trl <- data.frame(
        sum = c(3, 10, 11),
        type = c('vd', 'pos', 'neg')
    )

    data$trial_type <- map_trl$type[match(x = data$trial_type,
                                          table = map_trl$sum)]

    # Adding a variable to show whether the rats chose the optimal stimulus
    map_opt <- data.frame(
        trial_type = c('vd', 'pos', 'neg'),
        optimal = c(1, 1, 9)
    )

    data$optimal <- map_opt$optimal[match(x = data$trial_type,
                                          table = map_opt$trial_type)]

    data$chose_optimal <- data$stim_chosen == data$optimal

    # Adding a variable to track whether the rats chose the right-hand stimulus
    data$chose_right <- data$stim_chosen == data$stim_right

    # Importing and re-cording the doses
    map_dose <- doses

    data$dose <- map_dose$Dose[match(x = data$subjID,
                                     table = map_dose$Subject)]

    data$dose <- factor(data$dose,
                        levels = c(0, 0.03, 0.1),
                        labels = c('veh', 'low', 'high'))

    data$trial_type <- factor(data$trial_type,
                              levels = c('vd', 'pos', 'neg'))

    data$subjID <- factor(data$subjID)

    criterion <- data %>%
                    filter(trial_type == 'vd') %>%
                    group_by(subjID, session, dose) %>%
                    summarise(accuracy = mean(chose_optimal)) %>%
                    group_by(subjID, dose) %>%
                    summarise(vd_max = max(accuracy))

    criterion$passed <- criterion$vd_max >= 0.8

    criterion %>%
        group_by(dose) %>%
        summarise(passed = sum(passed))

    # NEED TO CHECK WHETHER MONA HAS USED THIS CRITERION
    rats_to_include <- criterion$subjID[criterion$passed == TRUE]

    data_passed_criterion <- data[data$subjID %in% rats_to_include, ]

    # Create summary data frame by group
    grp_by_session <- data_passed_criterion %>%
                        group_by(dose, trial_type, session) %>%
                        summarise(mean = mean(chose_optimal))

    # Plot summary data frame
    ggplot(data = grp_by_session,
           mapping = aes(x = session,
                         y = mean,
                         colour = dose)) +
        facet_grid(. ~ trial_type) +
        geom_line() +
        ylim(0, 1) +
        theme_bw() +
        geom_hline(yintercept = 0.5,
                   linetype = 'dotted')

    # Create summary data frame by subjID
    #
    subj_by_session <- data_passed_criterion %>%
        group_by(subjID, dose, trial_type, session) %>%
        summarise(mean = mean(chose_optimal))

    str(subj_by_session)

    ggplot(data = subj_by_session,
           mapping = aes(x = session,
                         y = mean,
                         colour = subjID)) +
        facet_grid(. ~ trial_type) +
        geom_line() +
        ylim(0, 1) +
        theme_bw() +
        geom_hline(yintercept = 0.5,
                   linetype = 'dotted')

    return(grp_by_session)

}
