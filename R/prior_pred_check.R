#' Prior Predictive Checks for the Valence Probe Visual Discrimination Task
#'
#' @param alpha
#' @param alpha_w
#' @param alpha_l
#' @param beta
#' @param kappa
#' @param tau
#' @param upsilon
#' @param eta
#' @param phi
#' @param nTrials
#'
#' @return
#' @export
#'
#' @examples
#'
prior_pred_check <- function (alpha = NA,
                              alpha_w = 0.002,
                              alpha_l = 0.002,
                              beta = 1.5,
                              kappa = 0.2,
                              tau = 0.1,
                              upsilon = 5,
                              eta = NA,
                              phi = NA,
                              subjID = 1,
                              grouping = 1,
                              nTrials = 2000) {

############################################################################

    # There should be a data file for the trial sequence with the package
    trials <- trials[1:nTrials, ]

    #choice <- data$choice
    #outcome <- data$outcome
    #chose_right <- data$chose_right
    #stim_left <- data$stim_left
    #stim_right <- data$stim_right

    choice <- vector(mode="integer", length = nTrials)
    outcome <- vector(mode="logical", length = nTrials)
    chose_right <- vector(mode="logical", length = nTrials)
    stim_left <- trials$stim_left[1:nTrials]
    stim_right <- trials$stim_right[1:nTrials]

    # alpha_w <- par[1]
    # alpha_l <- par[2]
    # beta <- par[3]
    # kappa <- par[4]
    # tau <- par[5]
    # upsilon <- par[6]

    v_left <- vector(mode="numeric", length = nTrials)
    v_right <- vector(mode="numeric", length = nTrials)

    v_neg <- vector(mode="numeric", length = nTrials)
    v_prob <- vector(mode="numeric", length = nTrials)
    v_pos <- vector(mode="numeric", length = nTrials)

    cloc_left <- vector(mode="numeric", length = nTrials)
    cloc_right <- vector(mode="numeric", length = nTrials)

    cstim_left <- vector(mode="numeric", length = nTrials)
    cstim_right <- vector(mode="numeric", length = nTrials)

    p_left <- vector(mode="numeric", length = nTrials)
    p_right <- vector(mode="numeric", length = nTrials)

    for (t in 1:nTrials) { # trial for loop

        ########################################################
        # First trial only
        ########################################################

        if (t == 1) {
            v_neg[t] <- 1
            v_prob[t] <- 0.5
            v_pos[t] <- 0

            cloc_left[t] <- 0
            cloc_right[t] <- 0

            cstim_left[t] <- 0
            cstim_right[t] <- 0
        }

        ########################################################
        # All except first trial
        ########################################################

        if (t != 1) {

            # Set up c values for location and stimulus stickiness
            cloc_left[t] <- as.integer(chose_right[t-1] == 0)
            cloc_right[t] <- as.integer(chose_right[t-1] == 1)

            cstim_left[t] <- as.integer(stim_left[t] == choice[t - 1])
            cstim_right[t] <- as.integer(stim_right[t] == choice[t - 1])

        }

        ########################################################
        # All trials
        ########################################################

        # Recode the stimulus value to a position value
        if (stim_left[t] == 0) v_left[t] <- v_neg[t]
        if (stim_left[t] == 50) v_left[t] <- v_prob[t]
        if (stim_left[t] == 100) v_left[t] <- v_pos[t]

        if (stim_right[t] == 0) v_right[t] <- v_neg[t]
        if (stim_right[t] == 50) v_right[t] <- v_prob[t]
        if (stim_right[t] == 100) v_right[t] <- v_pos[t]

        # Choice rule
        p_left[t] <-
            exp(beta * v_left[t] + kappa * cloc_left[t] + tau * cstim_left[t]) /
            (exp(beta * v_left[t] + kappa * cloc_left[t] + tau * cstim_left[t]) +
                 exp(beta*v_right[t] + kappa * cloc_right[t] + tau * cstim_right[t]))

        p_right[t] <- 1 - p_left[t]

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # SIMULATE chose_right (from p_right, i.e. probability to choose right)
        chose_right[t] <- as.logical(rbinom(n = 1,
                                            size = 1,
                                            prob = p_right[t])
                                     )

        if (chose_right[t] == FALSE) {
            choice[t] <- stim_left[t]
        } else if (chose_right[t] == TRUE) {
            choice[t] <- stim_right[t]
        }

        # SIMULATE OUTCOME (from choice, i.e. stimulus actually chosen)
        if (choice[t] == 0) outcome[t] <- FALSE
        if (choice[t] == 50) {
            outcome[t] <- as.logical(rbinom(n = 1,
                                            size = 1,
                                            prob = 0.5)
                                     )
        }
        if (choice[t] == 100) outcome[t] <- TRUE
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        ########################################################
        # All except last trial
        ########################################################

        if (t != nTrials) {

            # Value updating rule
            if (choice[t] == 0) {
                v_neg[t + 1] <- v_neg[t] +
                    alpha_w * (outcome[t] - v_neg[t]) * outcome[t] +
                    alpha_l * (outcome[t] - v_neg[t]) * (1 - outcome[t])
                v_prob[t + 1] <- v_prob[t]
                v_pos[t + 1] <- v_pos[t]
            } else if (choice[t] == 50) {
                v_neg[t + 1] <- v_neg[t]
                v_prob[t + 1] <- v_prob[t] +
                    alpha_w / (1 + upsilon) * (outcome[t] - v_prob[t]) * outcome[t] +
                    alpha_l / (1 + upsilon) * (outcome[t] - v_prob[t]) * (1 - outcome[t])
                v_pos[t + 1] <- v_pos[t]
            } else if (choice[t] == 100) {
                v_neg[t + 1] <- v_neg[t]
                v_prob[t + 1] <- v_prob[t]
                v_pos[t + 1] <- v_pos[t] +
                    alpha_w * (outcome[t] - v_pos[t]) * outcome[t] +
                    alpha_l * (outcome[t] - v_pos[t]) * (1 - outcome[t])
            } # end of choice if statement

        } # end of all-except-last-trial if statement

    } # end of trial for loop

    # Here is an intermediate step just to highlight that we need
    # the p_right and the choice column to be able to run the final bit
    summary <- data.frame(
        subjID = subjID,
        grouping = grouping,
        trial = 1:nTrials,
        stim_left = stim_left,
        stim_right = stim_right,
        p_right = p_right,
        chose_right = chose_right,
        outcome = outcome
    )

    return(summary)

    }
