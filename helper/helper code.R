library('devtools')
library('tidyverse')
library('cowplot')
library('RColorBrewer')

# TO USE EXISTING R PACKAGE
# library('devtools')
# use_package('dplyr')
# use_package('tidyr')
# use_package('purrr')
# use_package('ggplot2')
# use_package('rstan')
# use_package('stringr')
# use_package('cowplot')
# use_package('RColorBrewer')

# TO CREATE NEW FUNCTION
# use_r('prior_pred_check')
# use_r('visual_posteriors')
# use_r('post_pred_check')
# use_r('summarise_sessions')
# use_r('simulate_choices_VPVD')
# use_r('summarise_sessions_from_list')
# use_r('summarise_correct')
# use_r('palette_by_group')

# TO USE DATA IN PACKAGE
# Example trials (could be made variable in the future, currently it is not)
# trials <- read.csv('helper/vpvd_trials4000.csv')
# use_data(trials)
#
# Example fit (this is huge - to be updated with smaller one!)
# example_fit <- readRDS("helper/reversals_fit10.rds")
# use_data(example_fit)

# What files to ignore?
# use_build_ignore('helper')

# Let's start working ...
#
# I've added this to the package's data
# fit0 <- readRDS("helper/reversals_fit10.rds")
# Meaning we can just do this:
data(example_fit)

# Read in parameters using rstan::extract
# params <- rstan::extract(example_fit)

# There are a number of parameters, only some of which we need!
names(params)

params <- get_posteriors(example_fit)

# Sanity check
names(params$group_params)
names(params$subject_params)

group_params <- params$group_params
subject_params <- params$subject_params

# The param data are still in lists
typeof(group_params)

# We have a 3 dimensional array for each parameter, where
# the 1st dimension is the number of SAMPLES in the posterior
# the 2nd dimension is the number of GROUPS
# the 3rd dimension is irrelevant
dim(group_params[[1]])

# We can visualise the grouped parameters like so
# Currently struggling to run this without separately loading the
# libraries/packages (tidyr, ggplot2, cowplot). They are all under
# dependencies in DESCRIPTION already so this needs further testing.
visual_posteriors(params = group_params,
                  grouping = c('veh', 'low', 'high')
                  )


# Lets simulate some VPVD behaviour from a couple of quick settings:

nIter <- 1000
nGroups <- 3
nTrials <- 4000

simulated_rat <- simulate_choices_vpvd(nTrials = nTrials)

summarise_correct(simulated_rat) %>%

    ggplot(aes(x = session,
               y = mean)) +
    geom_line(alpha = 0.5) +
    facet_grid(. ~ trial_type) +
    ylim(0, 1)


##########################################################
# Example
##########################################################

# See if the prior predictive check makes sense
simulated_rat <- prior_pred_check(nTrials = 4000)
# simulated_rat$trial_type <- simulated_rat$stim_left + simulated_rat$stim_right
# simulated_rat$trial_type[simulated_rat$trial_type == 50] <- 'neg'
# simulated_rat$trial_type[simulated_rat$trial_type == 100] <- 'vd'
# simulated_rat$trial_type[simulated_rat$trial_type == 150] <- 'pos'
# simulated_rat$trial_type <- as.factor(simulated_rat$trial_type)
#
# simulated_rat$choice <- NA
# simulated_rat$choice[simulated_rat$chose_right == 0] <-
#     simulated_rat$stim_left[simulated_rat$chose_right == 0]
# simulated_rat$choice[simulated_rat$chose_right == 1] <-
#     simulated_rat$stim_right[simulated_rat$chose_right == 1]
#
# simulated_rat$correct <- FALSE
# simulated_rat$correct[simulated_rat$trial_type == 'neg'] <-
#     simulated_rat$choice[simulated_rat$trial_type == 'neg'] == 50
# simulated_rat$correct[simulated_rat$trial_type == 'vd'] <-
#     simulated_rat$choice[simulated_rat$trial_type == 'vd'] == 100
# simulated_rat$correct[simulated_rat$trial_type == 'pos'] <-
#     simulated_rat$choice[simulated_rat$trial_type == 'pos'] == 100
#
# simulated_rat$session <- NA
# simulated_rat$session <- ceiling(simulated_rat$trial / 200)

summarise_sessions(simulated_rat) %>%

ggplot(aes(x = session,
           y = mean)) +
    geom_line(alpha = 0.5) +
    facet_grid(. ~ trial_type) +
    ylim(0, 1)



# Locate your stanfit file and read it into the workspace
#
fit <- readRDS("helper/reversals_fit10.rds")

# Extract the relevant parameters
params <- get_posteriors(stanfit = fit)
group_params <- params$group_params
subject_params <- params$subject_params

str(group_params)
dim(group_params[[1]])
dim(subject_params[[1]])

# Visualise the grouped parameters
visual_posteriors(params = group_params,
                  grouping = c('veh', 'low', 'high')
                  )
dim(group_params$reward_rate_by_group_drug)
group_params$reward_rate_by_group_drug[1:10, , 1]

# Simulate VPVD behaviour from the posterior distributions
test <- post_pred_check(nIter = 10)
dim(test)

# system.time(
# test <- post_pred_check(alpha_w = group_params$reward_rate_by_group_drug,
#                         alpha_l = group_params$punish_rate_by_group_drug,
#                         beta = group_params$reinf_sensitivity_by_group_drug,
#                         kappa = group_params$side_stickiness_by_group_drug,
#                         tau = group_params$stimulus_stickiness_by_group_drug,
#                         upsilon = group_params$prob_discount_by_group_drug,
#                         nTrials = 4000,
#                         nIter = 100)
# )

# Now, simulate VPVD behaviour with a vectorised simulator
# Let's create a test bed
#
nIter <- 10
nGroups <- 3
nTrials <- 4000

draws <- sample(x = dim(group_params$reinf_sensitivity_by_group_drug)[1],
                      size = nIter,
                      replace = FALSE)

w <- group_params$reward_rate_by_group_drug[draws, 1:nGroups, 1]
l <- group_params$punish_rate_by_group_drug[draws, 1:nGroups, 1]
b <- group_params$reinf_sensitivity_by_group_drug[draws, 1:nGroups, 1]
k <- group_params$side_stickiness_by_group_drug[draws, 1:nGroups, 1]
t <- group_params$stimulus_stickiness_by_group_drug[draws, 1:nGroups, 1]
u <- group_params$prob_discount_by_group_drug[draws, 1:nGroups, 1]

# Simulate all subjects in parallel - 8.52 seconds elapsed
system.time(
    test_vect <- simulate_choices_vpvd(nTrials = nTrials,
                                       nGroups = nGroups,
                                       nIter = nIter,
                                       alpha_w = w,
                                       alpha_l = l,
                                       beta = b,
                                       kappa = k,
                                       tau = t,
                                       upsilon = u)
)

str(test_vect)

###############################
# New version of function - now runs in 60 seconds ...
# BUT THERE IS AN ERROR - ALL THREE GROUPINGS GIVE THE SAME NUMBERS
system.time(
    df <- summarise_correct(test_vect)
)


str(df)
object.size(df)/1048576

###

head(df)

ggplot(data = df,
       aes(x = session,
           y = mean,
           colour = subjID)) +
    geom_line(alpha = 0.5) +
    facet_grid(grouping ~ trial_type) +
    ylim(0, 1) +
    scale_colour_manual(values = palette_by_group(nGroups = nGroups, nIter = nIter))

###############################################################################
# IGNORE ALL BELOW THIS LINE
df <- as.data.frame(group_params$reward_rate_by_group_drug)
names(df) <- 1:dim(df)[2]
long_df <- gather(df,
                  key = 'grouping',
                  value = 'value')

#long_df[sample(1:dim(long_df)[1], 20),]


outplot <- ggplot(long_df,
                  aes(x=value,
                      fill = group_as)) +
    # facet_grid(. ~ group_as) +
    geom_density(alpha = 0.5,
                 position = 'identity') +
    labs(x="Value", y = "Count")

# geom_vline(aes(xintercept = as.double(quantile(df[,1], 0.5))),
#            linetype="dashed") +
# geom_vline(aes(xintercept = as.double(quantile(df[,2], 0.5))),
#            linetype="dashed") +
# geom_vline(aes(xintercept = as.double(quantile(df[,3], 0.5))),
#            linetype="dashed") +
# geom_vline(aes(xintercept = as.double(quantile(df[,4], 0.5))),
#            linetype="dashed")

return(outplot)
}

plot_alpha_win <- make_density_plot(params$reward_rate_by_group_drug)

return(plot_alpha_win)
