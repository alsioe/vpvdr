# This is an example code for Mona 20.1.2023
#
# Uncomment and run in case you don't have these installed
# install.packages('devtools')
# install.packages('tidyverse')

library('devtools')
library('tidyverse')
library('cowplot')
library('RColorBrewer')

# Get vpvdr from github using
devtools::install_github("alsioe/vpvdr")
library('vpvdr')

# The fits I have for citalopram are too large (ca 80 MB) to add to Github as
# data so you'll need to copy the file locally and run a readRDS
# fit <- readRDS("../helper/reversals_fit10.rds")

# Or download straight from github, which occasionally works for me ...
# I think it doesn't work right now since I'm working on the train.
githubURL <-
    'https://github.com/alsioe/vpvdr/raw/main/helper/reversals_fit10.rds'
download.file(url = githubURL,
              destfile = 'test_fit10.rds')
fit <- readRDS('test_fit10.rds')

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
object.size(post_pred_check)/1048576 # output the size in MB

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
