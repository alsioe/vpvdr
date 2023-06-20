#' Visualise density plots from posteriors
#'
#' @param params These are the posterior parameters that you want to use for the simulation
#'
#' @param grouping Names of the groups/conditions in the experiment.
#'
#' @return Plots a cowplot of one density plot for each parameter
#' @export
#'
#' @examples
visual_posteriors <- function(params,
                              grouping = 1:dim(params[[1]])[2]
                              ) {

    # Create a list of plots - to add: FLEXIBILITY in the parameters
    six_parameters <- c('reward_rate_by_group_drug',
                        'punish_rate_by_group_drug',
                        'prob_discount_by_group_drug',
                        'reinf_sensitivity_by_group_drug',
                        'side_stickiness_by_group_drug',
                        'stimulus_stickiness_by_group_drug')

    parameter_list <- names(params)

    # Enter some logic here for group vs. subject if required

    make_density_plot <- function(param,
                                  grouping) {
        df <- as.data.frame(param)

        names(df) <- grouping

        long_df <- pivot_longer(data = df,
                                cols = everything(),
                                names_to = 'grouping',
                                values_to = 'value')

        # Make sure the factors have the right order
        long_df$grouping <- factor(x = long_df$grouping,
                                   levels = grouping)

        outplot <- ggplot(long_df,
                         aes(x=value,
                             fill = grouping)) +
            geom_density(alpha = 0.5,
                         position = 'identity') +
            labs(x="Value", y = "Count") +
            scale_fill_brewer(palette = 'Dark2')

        # This logic doesn't work - is there a bug? Replaced with if statements
        # for (i in 1:length(grouping)) {
        #         outplot <- outplot +
        #             geom_vline(aes(xintercept = as.double(quantile(df[,i], 0.5))),
        #                                                     linetype="dashed")
        #
        #     }

        outplot <- outplot +
            geom_vline(aes(xintercept = as.double(quantile(df[,1], 0.5))),
                           linetype="dashed")

        if (length(grouping) > 1) {
            outplot <- outplot +
                geom_vline(aes(xintercept = as.double(quantile(df[,2], 0.5))),
                               linetype="dashed")
        }

        if (length(grouping) > 2) {
            outplot <- outplot +
                geom_vline(aes(xintercept = as.double(quantile(df[,3], 0.5))),
                           linetype="dashed")
        }

        if (length(grouping) > 3) {
            outplot <- outplot +
                geom_vline(aes(xintercept = as.double(quantile(df[,4], 0.5))),
                           linetype="dashed")
        }

        if (length(grouping) > 4) {
            outplot <- outplot +
                geom_vline(aes(xintercept = as.double(quantile(df[,5], 0.5))),
                           linetype="dashed")
        }

        if (length(grouping) > 5) {
            outplot <- outplot +
                geom_vline(aes(xintercept = as.double(quantile(df[,6], 0.5))),
                           linetype="dashed")
        }

        return(outplot)
    }

    # Create a plot for each parameter in params that matches six_parameters
    for (p in six_parameters) {

        if (p == six_parameters[1]) {
            to_plot <- vector(mode = 'character')
        }

        if (p %in% parameter_list) {
            do.call('<-',
                    list(paste0('plot_', p),
                         make_density_plot(param = params[[match(p, names(params))]],
                                           grouping = grouping)
                         )
                    )

            to_plot <- append(to_plot, paste0('plot_', p))

        }

    }


    for (i in 1:length(to_plot)) {
        if (i == 1) plotlist <- list()

        plotlist[[i]] <- get(to_plot[i]) + theme(legend.position = 'none')
    }

    # For more info, see shared legends in cowplot (Wilke lab)
    # https://wilkelab.org/cowplot/articles/shared_legends.html
    legend_b <- get_legend(
        plot_reinf_sensitivity_by_group_drug + theme(legend.position = 'bottom')
        )

    # RETURN
    master_plot <-
        plot_grid(plotlist = plotlist,
                  # list(plot_reward_rate_by_group_drug + theme(legend.position = "none"),
                  #      plot_punish_rate_by_group_drug + theme(legend.position = "none"),
                  #      # plot_prob_discount + theme(legend.position = "none"),
                  #      plot_reinf_sensitivity_by_group_drug + theme(legend.position = "none"),
                  #      plot_side_stickiness_by_group_drug + theme(legend.position = "none")
                  #      # plot_stimulus_stickiness + theme(legend.position = "none")
                       # ),
                  labels = to_plot,
                  label_x = 0.1,
                  label_y = 1,
                  hjust = 0,
                  ncol = 2
                  )

    return(plot_grid(plotlist = list(master_plot, legend_b),
                     ncol = 1,
                     rel_heights = c(1, .1)
                     )
           )

}
