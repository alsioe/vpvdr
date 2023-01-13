summarise_correct_by_group <- function(df_subj) {
    df_subj %>%
        group_by(session,
                 trial_type,
                 grouping) %>%
        summarise(mean = mean(correct),
                  sem = sd(correct) / sqrt(n()),
                  q.025 = quantile(correct, probs = 0.025),
                  q.250 = quantile(correct, probs = 0.25),
                  q.500 = quantile(correct, probs = 0.50),
                  q.750 = quantile(correct, probs = 0.75),
                  q.975 = quantile(correct, probs = 0.975)
                  )
}
