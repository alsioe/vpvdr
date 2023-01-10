#' Posterior predictive check
#'
#' @param alpha_w
#' @param alpha_l
#' @param beta
#' @param kappa
#' @param tau
#' @param upsilon
#' @param nTrials
#'
#' @return
#' @export
#'
#' @examples
post_pred_check <- function (alpha_w = 0.002,
                             alpha_l = 0.002,
                             beta = 1.5,
                             kappa = 0.2,
                             tau = 0.1,
                             upsilon = 5,
                             nTrials = 2000,
                             nIter = 10) {

    ############################################################################

    alpha_w <- as.data.frame(alpha_w)
    alpha_l <- as.data.frame(alpha_l)
    beta <- as.data.frame(beta)
    kappa <- as.data.frame(kappa)
    tau <- as.data.frame(tau)
    upsilon <- as.data.frame(upsilon)

    nGroups <- dim(beta)[2]

    simulated_data <- data.frame(
        stim_left = integer(),
        stim_right = integer(),
        p_right = double(),
        chose_right = logical(),
        choice = integer(),
        outcome = logical(),
        trial = integer(),
        grouping = character(),
        iteration = integer()
    )

    for (j in 1:nGroups) {
        for (i in 1:nIter) {

            wDf <- prior_pred_check(alpha_w = alpha_w[i, j, ],
                                    alpha_l = alpha_l[i, j, ],
                                    beta = beta[i, j, ],
                                    kappa = kappa[i, j, ],
                                    tau = tau[i, j, ],
                                    upsilon = upsilon[i, j, ],
                                    nTrials = nTrials,
                                    subjID = i,
                                    grouping = j
                                    )
            wDf$trial <- 1:nTrials
            wDf$grouping <- j
            wDf$iteration <- i

            simulated_data <- rbind(simulated_data, wDf)

        }
    }

    return(simulated_data)

}
