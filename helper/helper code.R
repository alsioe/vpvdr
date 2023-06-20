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

# TO CREATE A NEW VIGNETTE
# use_vignette('vpvdr', title = 'How to use the vpvdr package')

# TO CREATE NEW FUNCTION
# use_r('prior_pred_check')
# use_r('visual_posteriors')
# use_r('post_pred_check')
# use_r('summarise_sessions')
# use_r('simulate_choices_VPVD')
# use_r('summarise_sessions_from_list')
# use_r('summarise_correct')
# use_r('palette_by_group')
# use_r('summarise_correct_by_subj')
# use_r('summarise_correct_by_group')

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
# use_build_ignore('R\\helper code.R')

# Let's start working ...

# Tabula rasa
rm(list=ls())

# The fits I have for citalopram are too large (ca 80 MB) to add to Github as
# data so you'll need to copy the file locally and run a readRDS
list.files('helper/')
fit <- readRDS("helper/reversals_fit10.rds")
fit <- readRDS("helper/2A_reversals_fit1.rds")
fit <- readRDS("helper/2C_reversals_fit6.rds")

# Read in parameters using rstan::extract - this happens inside get_posteriors()
# params <- rstan::extract(fit)

# Try this function
?get_posteriors
params <- get_posteriors(fit)

# Sanity check
# There are many parameters, but get_posteriors only pulls out the ones we need.
names(params$group_params)
names(params$subject_params)

# Let's make them into separate lists
group_params <- params$group_params
subject_params <- params$subject_params

# We have a 3 dimensional array for each parameter, where
# the 1st dimension is the number of SAMPLES in the posterior
# the 2nd dimension is the number of GROUPS (here, doses)
# the 3rd dimension is the number of within-subject levels (e.g. dose)
dim(group_params[[1]])

# We can visualise the grouped parameters for a reality check.
# Currently struggling to run this without separately loading the
# libraries/packages (tidyr, ggplot2, cowplot). They are all under
# dependencies in DESCRIPTION already so this needs some further testing.
?visual_posteriors
visual_posteriors(params = group_params,
                  grouping = c('veh', 'low', 'high')
                  )

# OK, if that looks like decent posteriors that Stan could have thrown back
# if the modelling was working, we are almost ready to simulate behaviour!

# First, let's simulate some random VPVD behaviour, to make sure the code
# to run the simulations is working OK:

nIter <- 100 # number of samples per condition
nGroups <- 3 # number of conditions
nTrials <- 2800 # number of trials of VPVD (200 per session)

?simulate_choices_vpvd
simulated_rats <- simulate_choices_vpvd(nTrials = nTrials,
                                       nIter = nIter,
                                       nGroups = nGroups,
                                       alpha_w = c(rep(0.002, nIter),
                                                   rep(0.004, nIter),
                                                   rep(0.006, nIter)
                                                   ),
                                       alpha_l = rep(0.002, nIter * nGroups),
                                       beta = rep(2, nIter * nGroups),
                                       kappa = rep(0.2, nIter * nGroups),
                                       tau = rep(0.1, nIter * nGroups),
                                       upsilon = rep(5, nIter * nGroups))

?summarise_correct_by_subj
df_subj <- summarise_correct_by_subj(simulated_rats)
df_group <- summarise_correct_by_group(df_subj)

# Let's plot that out in ggplot, using geom_ribbon() to visualise the 50% HDI
df_group %>%
    ggplot(aes(x = session,
               y = mean,
               colour = grouping)) +
    geom_ribbon(aes(ymin = q.250,
                    ymax = q.750,
                    fill = grouping,
                    colour = NA),
                alpha = 0.2) +
    geom_line() +
    facet_grid(. ~ trial_type) +
    ylim(0, 1) +
    geom_hline(yintercept = 0.5,
               linetype = 'dashed') +
    theme_bw() +
    scale_color_brewer(palette = 'Dark2') +
    scale_fill_brewer(palette = 'Dark2')

# If we really want to see the performance of each iteration, here's that.
df_subj %>%
    ggplot(aes(x = session,
               y = correct,
               colour = subjID)) +
    geom_line(alpha = 0.1) +
    facet_grid(. ~ trial_type) +
    ylim(0, 1) +
    geom_hline(yintercept = 0.5,
               linetype = 'dashed') +
    theme_bw() +
    theme(legend.position = 'NONE') +
    scale_colour_manual(values = palette_by_group(nGroups = nGroups,
                                                  nIter = nIter))

# Next, let's take our actual posterior and feed that in to the simulator code.
nIter <- 1000 # a minimum of 1000 to get accurate posterior y's
nGroups <- 3
nTrials <- 2800

# Let's create a list of samples ('draws') from the posterior
# - this is to ensure that the the parameters we use for the simulation are
# all from the same iteration
draws <- sample(x = dim(group_params$reinf_sensitivity_by_group_drug)[1],
                size = nIter,
                replace = FALSE)

# This will be baked into a function at some point
w <- group_params$reward_rate_by_group_drug[draws, 1:nGroups, 1]
l <- group_params$punish_rate_by_group_drug[draws, 1:nGroups, 1]
b <- group_params$reinf_sensitivity_by_group_drug[draws, 1:nGroups, 1]
k <- group_params$side_stickiness_by_group_drug[draws, 1:nGroups, 1]
t <- group_params$stimulus_stickiness_by_group_drug[draws, 1:nGroups, 1]
u <- group_params$prob_discount_by_group_drug[draws, 1:nGroups, 1]

# Simulate all subjects in parallel
# nIter = 1000; groups = 3 takes <10 seconds over here
system.time(
    post_pred_check <- simulate_choices_vpvd(nTrials = nTrials,
                                       nGroups = nGroups,
                                       nIter = nIter,
                                       alpha_w = w,
                                       alpha_l = l,
                                       beta = b,
                                       kappa = k,
                                       tau = t,
                                       upsilon = u)
)

# Again, this is a large list that needs to be deleted and
# we will do this automatically at some point
object.size(post_pred_check)/1048576 # outputs the size in MB

ppc_subj <- summarise_correct_by_subj(post_pred_check,
                                      c('veh', 'low', 'high'))

ppc_group <- summarise_correct_by_group(ppc_subj)

# Same plot as above
ppc_group %>%
    ggplot(aes(x = session,
               y = mean,
               colour = grouping)) +
    geom_ribbon(aes(ymin = q.250,
                    ymax = q.750,
                    fill = grouping,
                    colour = NA),
                alpha = 0.2,
                show.legend = FALSE) +
    geom_line() +
    facet_grid(. ~ trial_type) +
    ylim(0, 1) +
    geom_hline(yintercept = 0.5,
               linetype = 'dashed') +
    theme_bw() +
    scale_color_brewer(palette = 'Dark2') +
    scale_fill_brewer(palette = 'Dark2')

###############################################################################
# IGNORE ALL BELOW THIS LINE

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
    scale_colour_manual(values = palette_by_group(nGroups = nGroups,
                                                  nIter = nIter))  +
    theme_bw() +
    legend

###############################################################################
# IGNORE EVEN MORE ALL BELOW THIS LINE
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

plot_alpha_win <- make_density_plot(params$reward_rate_by_group_drug)

return(plot_alpha_win)
