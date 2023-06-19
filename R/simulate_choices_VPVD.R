#' Simulates choices and probabilities in VPVD
#'
#' @param nIter
#' @param alpha
#' @param alpha_w
#' @param alpha_l
#' @param beta
#' @param kappa
#' @param tau
#' @param upsilon
#' @param eta
#' @param phi
#' @param subjID
#' @param grouping
#' @param nTrials
#'
#' @return
#' @export
#'
#' @examples
simulate_choices_vpvd <-
    function (nIter = 10,
              nTrials = 2000,
              nGroups = 1,
              alpha = NA,
              alpha_w = matrix(data = rep(0.002, nIter * nGroups),
                               ncol = nGroups),
              alpha_l = matrix(data = rep(0.005, nIter * nGroups),
                               ncol = nGroups),
              beta = matrix(data = rep(1.5, nIter * nGroups),
                            ncol = nGroups),
              kappa = matrix(data = rep(0.2, nIter * nGroups),
                             ncol = nGroups),
              tau = matrix(data = rep(0.1, nIter * nGroups),
                           ncol = nGroups),
              upsilon = matrix(data = rep(5, nIter * nGroups),
                               ncol = nGroups),
              eta = NA,
              phi = NA
              ) {

    ############################################################################

    # How many simulated 'subjects'/conditions in total?
    nSubj <- nIter * nGroups

    # There is a file with an example trial sequence included in the package
    trials <- trials[1:nTrials, ]

    # Make vectors with subject ID and group
    subjID <- c(1:nSubj)
    grouping <- sort(
                    rep(x = 1:nGroups,
                        times = nIter)
                    )

    # Make parameter VECTORS from paramater MATRICES
    alpha_w <- c(alpha_w)
    alpha_l <- c(alpha_l)
    beta <- c(beta)
    kappa <- c(kappa)
    tau <- c(tau)
    upsilon <- c(upsilon)

    # Make parameter VECTORS from paramater MATRICES
    # alpha_w <- c(w)
    # alpha_l <- c(l)
    # beta <- c(b)
    # kappa <- c(k)
    # tau <- c(t)
    # upsilon <- c(u)

    #choice <- data$choice
    #outcome <- data$outcome
    #chose_right <- data$chose_right
    #stim_left <- data$stim_left
    #stim_right <- data$stim_right

    choice <- matrix(data=NA, nrow = nTrials, ncol = nSubj)
    outcome <- matrix(data=NA, nrow = nTrials, ncol = nSubj)
    chose_right <- matrix(data=NA, nrow = nTrials, ncol = nSubj)
    # This simulates all sessions with identical stimuli
    # -- consider updating this: at least randomise the order?
    stim_left <- trials$stim_left[1:nTrials]
    stim_right <- trials$stim_right[1:nTrials]

    # alpha_w <- par[1]
    # alpha_l <- par[2]
    # beta <- par[3]
    # kappa <- par[4]
    # tau <- par[5]
    # upsilon <- par[6]

    v_left <- matrix(data=NA, nrow = nTrials, ncol = nSubj)
    v_right <- matrix(data=NA, nrow = nTrials, ncol = nSubj)

    v_neg <- matrix(data=NA, nrow = nTrials, ncol = nSubj)
    v_prob <- matrix(data=NA, nrow = nTrials, ncol = nSubj)
    v_pos <- matrix(data=NA, nrow = nTrials, ncol = nSubj)

    cloc_left <- matrix(data=NA, nrow = nTrials, ncol = nSubj)
    cloc_right <- matrix(data=NA, nrow = nTrials, ncol = nSubj)

    cstim_left <- matrix(data=NA, nrow = nTrials, ncol = nSubj)
    cstim_right <- matrix(data=NA, nrow = nTrials, ncol = nSubj)

    p_left <- matrix(data=NA, nrow = nTrials, ncol = nSubj)
    p_right <- matrix(data=NA, nrow = nTrials, ncol = nSubj)

    for (t in 1:nTrials) { # trial for loop

        ########################################################
        # First trial only
        # Sets up values for each iteration (column)
        ########################################################

        if (t == 1) {
            v_neg[t, ] <- 1
            v_prob[t, ] <- 0.5
            v_pos[t, ] <- 0

            cloc_left[t, ] <- 0
            cloc_right[t, ] <- 0

            cstim_left[t, ] <- 0
            cstim_right[t, ] <- 0
        }

        ########################################################
        # All except first trial
        # Assigns values for each iteration (column)
        ########################################################

        if (t != 1) {

            # Set up c values for location and stimulus stickiness
            cloc_left[t, ] <- as.integer(chose_right[t-1, ] == 0)
            cloc_right[t, ] <- as.integer(chose_right[t-1, ] == 1)

            cstim_left[t, ] <- as.integer(stim_left[t] == choice[t - 1, ])
            cstim_right[t, ] <- as.integer(stim_right[t] == choice[t - 1, ])

        }

        #############################################################
        # All trials
        # 'Vectorised' - we run this over all subjects simultaneously
        #############################################################

        # Recode the stimulus value to a position value
        v_left[t, stim_left[t] == 0] <- v_neg[t, stim_left[t] == 0]
        v_left[t, stim_left[t] == 50] <- v_prob[t, stim_left[t] == 50]
        v_left[t, stim_left[t] == 100] <- v_pos[t, stim_left[t] == 100]

        v_right[t, stim_right[t] == 0] <- v_neg[t, stim_right[t] == 0]
        v_right[t, stim_right[t] == 50] <- v_prob[t, stim_right[t] == 50]
        v_right[t, stim_right[t] == 100] <- v_pos[t, stim_right[t] == 100]

        # Choice rule
        p_left[t, ] <-
          exp(beta * v_left[t, ] + kappa * cloc_left[t, ] + tau * cstim_left[t, ]) /
          (exp(beta * v_left[t, ] + kappa * cloc_left[t, ] + tau * cstim_left[t, ]) +
            exp(beta * v_right[t, ] + kappa * cloc_right[t, ] + tau * cstim_right[t, ]))

        p_right[t, ] <- 1 - p_left[t, ]

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # SIMULATE chose_right (from p_right, i.e. probability to choose right)
        chose_right[t, ] <- as.logical(rbinom(n = nSubj,
                                            size = 1,
                                            prob = p_right[t, ])
                                       )

        choice[t, chose_right[t, ] == FALSE] <- stim_left[t]
        choice[t, chose_right[t, ] == TRUE] <- stim_right[t]

        # SIMULATE OUTCOME (from choice, i.e. stimulus actually chosen)
        outcome[t, choice[t, ] == 0] <- FALSE
        outcome[t, choice[t, ] == 50] <-
            as.logical(rbinom(n = sum(choice[t, ] == 50),
                              size = 1,
                              prob = 0.5)
                       )
        outcome[t, choice[t, ] == 100] <- TRUE
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        ########################################################
        # All except last trial
        ########################################################

        if (t != nTrials) {

            # VALUE UPDATING RULE - vectorised
            # Filters
            filt_neg <- choice[t, ] == 0
            filt_prob <- choice[t, ] == 50
            filt_pos <- choice[t, ] == 100

            # If choice[t] is "0"
            v_neg[t + 1, filt_neg] <- v_neg[t, filt_neg] +
                alpha_l[filt_neg] * (outcome[t, filt_neg] - v_neg[t, filt_neg])
            v_prob[t + 1, filt_neg] <- v_prob[t, filt_neg]
            v_pos[t + 1, filt_neg] <- v_pos[t, filt_neg]

            # If choice[t] is "50"
            v_neg[t + 1, filt_prob] <- v_neg[t, filt_prob]
            v_prob[t + 1, filt_prob] <- v_prob[t, filt_prob] +
                alpha_w[filt_prob] / (1 + upsilon[filt_prob]) *
                    (outcome[t, filt_prob] - v_prob[t, filt_prob]) * outcome[t, filt_prob] +
                alpha_l[filt_prob] / (1 + upsilon[filt_prob]) *
                    (outcome[t, filt_prob] - v_prob[t, filt_prob]) * (1 - outcome[t, filt_prob])
            v_pos[t + 1, filt_prob] <- v_pos[t, filt_prob]

            # If choice[t] is "100"
            v_neg[t + 1, filt_pos] <- v_neg[t, filt_pos]
            v_prob[t + 1, filt_pos] <- v_prob[t, filt_pos]
            v_pos[t + 1, filt_pos] <- v_pos[t, filt_pos] +
                alpha_w[filt_pos] * (outcome[t, filt_pos] - v_pos[t, filt_pos])

        } # end of all-except-last-trial if statement

    } # end of trial for loop

    # Pack everything into a list to return
    summary <- list(
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
