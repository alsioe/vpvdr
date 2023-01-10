#' Visualise density plots from posteriors
#'
#' @param params
#' @param grouping
#'
#' @return
#' @export
#'
#' @examples
visual_posteriors <- function(params,
                              grouping = 1:dim(params)[[1]][2]
                              ) {

    # Enter some logic here for group vs. subject if required

    make_density_plot <- function(param,
                                  grouping) {
        df <- as.data.frame(param)

        names(df) <- grouping

        long_df <- gather(df,
                          key = 'grouping',
                          value = 'value')

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

    # Create a list of plots - to add: FLEXIBILITY in the parameters
    #parameter_list <- names(group_params)

    plot_reward_rate <- make_density_plot(param = params$reward_rate_by_group_drug,
                                          grouping = grouping)
    plot_punish_rate <- make_density_plot(param = params$punish_rate_by_group_drug,
                                          grouping = grouping)
    plot_prob_discount <- make_density_plot(param = params$prob_discount_by_group_drug,
                                            grouping = grouping)
    plot_reinf_sensitivity <- make_density_plot(param = params$reinf_sensitivity_by_group_drug,
                                                grouping = grouping)
    plot_side_stickiness <- make_density_plot(param = params$side_stickiness_by_group_drug,
                                              grouping = grouping)
    plot_stimulus_stickiness <- make_density_plot(param = params$stimulus_stickiness_by_group_drug,
                                                  grouping = grouping)

    master_plot <-

    # For more info, see shared legends in cowplot (Wilke lab)
    # https://wilkelab.org/cowplot/articles/shared_legends.html
    legend_b <- get_legend(
        plot_reinf_sensitivity + theme(legend.position = 'bottom')
        )

    # RETURN
    master_plot <-
        plot_grid(plotlist =
                  list(plot_reward_rate + theme(legend.position = "none"),
                       plot_punish_rate + theme(legend.position = "none"),
                       plot_prob_discount + theme(legend.position = "none"),
                       plot_reinf_sensitivity + theme(legend.position = "none"),
                       plot_side_stickiness + theme(legend.position = "none"),
                       plot_stimulus_stickiness + theme(legend.position = "none")
                       ),
                  labels = c('Reward rate',
                             'Punish rate',
                             'Probe discount rate',
                             'Reinf. sensitity',
                             'Side stickiness',
                             'Stimulus stickiness'),
                  label_x = 0.1,
                  label_y = 1,
                  hjust = 0,
                  nrow = 3
                  )

    return(plot_grid(plotlist = list(master_plot, legend_b),
                     ncol = 1,
                     rel_heights = c(1, .1)
                     )
           )

}
