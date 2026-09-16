# =====================================================================
# 05 · Probability & Inference — SCRIPT SKELETON
# Ecological Statistics
#
# Save this as scripts/05_probability_inference.R in your project.
# Work down the worksheet and type your code under each header below.
# Run a line with Ctrl/Cmd + Enter.  Run everything with Ctrl/Cmd + Shift + Enter.
#
# Part 1.3 (summary_stats) is already filled in for you — it is a tool,
# not a lesson. Read it, run it, then use it wherever you need those numbers.
# =====================================================================

library(janitor) # round_half_up()
library(tidyverse)

# Open the .Rproj first, then:
# g_df <- read_csv("data/05_probability/gray_I3_I8.csv")

#    Part 1.1 · Setup and Data -------
library(janitor)
library(tidyverse)

g_df <- read_csv("data/05_probability/gray_I3_I8.csv")

i3_df <- g_df %>% filter(lake == "I3")
i8_df <- g_df %>% filter(lake == "I8")
head(g_df)


#    Part 1.2 · Setup and Data -------
stats_df <- g_df %>%
  group_by(lake) %>%
  summarize(
    mean_length = mean(length_mm, na.rm = TRUE),
    sd_length = sd(length_mm, na.rm = TRUE),
    n = sum(!is.na(length_mm)),
    se_length = sd_length / sqrt(n),
    .groups = "drop"
  )
stats_df


#    Part 1.3 · A reusable summary function -----
# note this can also go in another file called functions and you can source("funtions")
# this is the way to do things you do over and over and it reduces code / mistakes

summary_stats <- function(data, variable) {
  data %>%
    summarize(
      n = sum(!is.na({{ variable }})),
      mean = mean({{ variable }}, na.rm = TRUE),
      sd = sd({{ variable }}, na.rm = TRUE),
      se = sd / sqrt(n),
      ci_lower = mean - qt(0.975, df = n - 1) * se,
      ci_upper = mean + qt(0.975, df = n - 1) * se,
      .groups = "drop"
    ) %>%
    mutate(across(c(mean, sd, se, ci_lower, ci_upper), ~ round_half_up(.x, 2)))
}

# then run -----
summary_stats(g_df, length_mm)


# # or for later
# summary_stats_2 <- function(data, ...) {
#   data %>%
#     pivot_longer(cols = c(...), names_to = "variable", values_to = "value") %>%
#     group_by(variable, .add = TRUE) %>% # keeps any grouping already on `data`
#     summarize(
#       n = sum(!is.na(value)),
#       mean = mean(value, na.rm = TRUE),
#       sd = sd(value, na.rm = TRUE),
#       se = sd / sqrt(n),
#       ci_lower = mean - qt(0.975, df = n - 1) * se,
#       ci_upper = mean + qt(0.975, df = n - 1) * se,
#       .groups = "drop"
#     ) %>%
#     mutate(across(c(mean, sd, se, ci_lower, ci_upper), ~ round_half_up(.x, 2)))
# }

# summary_stats_2(i3_df, length_mm, mass_g)
#    Part 1.4 · A reusable summary function ------------

#    Part 2.1 · The Normal Distribution ------------
i3_mean <- mean(i3_df$length_mm, na.rm = TRUE)
i3_sd <- sd(i3_df$length_mm, na.rm = TRUE)

i3_df %>%
  ggplot(aes(x = length_mm)) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 10,
    fill = "lightblue",
    colour = "white"
  ) +
  stat_function(
    fun = dnorm,
    args = list(mean = i3_mean, sd = i3_sd),
    colour = "red",
    linewidth = 1
  ) +
  labs(x = "Length (mm)", y = "Density")

#    Part 3.1 · Z-Scores ------------
i3_df <- i3_df %>%
  mutate(z_score = (length_mm - i3_mean) / i3_sd)

i3_df %>%
  select(lake, length_mm, z_score) %>%
  head()

i3_df %>%
  ggplot(aes(x = z_score)) +
  geom_histogram(binwidth = 0.25, fill = "blue", alpha = 0.7) +
  labs(x = "z (SDs from the mean)", y = "Count")

#    Part 3.2 · Z-Scores ------------
within_1sd <- mean(abs(i3_df$z_score) <= 1, na.rm = TRUE)
round_half_up(100 * within_1sd, 3)

#    Part 4.1 · Area Under the Curve ------------
pnorm(2) # area to the LEFT of 1.96
1 - pnorm(1.96) # area to the RIGHT
qnorm(0.978) # the z with 97.5% to its left - the inverse of pnorm


#    Part 4.2 · Area Under the Curve ------------
z_300 <- (300 - i3_mean) / i3_sd
round_half_up(z_300, 2) # should be about 1.22

round_half_up(100 * (1 - pnorm(z_300)), 1) # % of fish LONGER than 300 mm

#    Part 4.3 · Area Under the Curve ------------
z_top5 <- qnorm(0.95)
length_top5 <- i3_mean + z_top5 * i3_sd
round_half_up(length_top5, 1)

#    Part 4.4 · Area Under the Curve ------------
z_240 <- (240 - i3_mean) / i3_sd
z_240
pnorm(z_240)

round_half_up(100 * pnorm(240, i3_mean, i3_sd), 1) # 18.3, same answer

#    Part 4.5 · Area Under the Curve ------------
i3_df %>%
  ggplot(aes(x = length_mm)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  geom_vline(xintercept = 300, colour = "red", linetype = "dashed")

#    Part 5.1 · Checking the Assumption ------------

shapiro.test(i8_df$length_mm)


qqnorm(i8_df$length_mm, main = "Q-Q Plot: length_mm")
qqline(i8_df$length_mm, col = "red", lwd = 2)

i8_df %>%
  ggplot(aes(sample = length_mm)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  labs(x = "Theoretical quantiles", y = "Sample quantiles")

#    Part 5.2 · Checking the Assumption ------------
round_half_up(100 * (1 - pnorm(300, i3_mean, i3_sd)), 1) # normal theory
round_half_up(100 * mean(i3_df$length_mm > 300, na.rm = TRUE), 1) # actual fish


#    Part 5.3 · Checking the Assumption ------------
i8_df <- g_df %>% filter(lake == "I8")
shapiro.test(i8_df$length_mm)


qqnorm(i8_df$length_mm, main = "Q-Q Plot: length_mm")
qqline(i8_df$length_mm, col = "red", lwd = 2)

i8_df %>%
  ggplot(aes(sample = length_mm)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  labs(x = "Theoretical quantiles", y = "Sample quantiles")
#    Part 6.1 · SD, SE, and Confidence Intervals
i3_n <- sum(!is.na(i3_df$length_mm))
i3_se <- i3_sd / sqrt(i3_n)

tibble(
  n = i3_n,
  SD = round_half_up(i3_sd, 1),
  SE = round_half_up(i3_se, 1)
)

#    Part 6.2 · SD, SE, and Confidence Intervals
t_crit <- qt(0.975, df = i3_n - 1) # two-tailed, 95%
t_crit

ci_lower <- i3_mean - t_crit * i3_se
ci_upper <- i3_mean + t_crit * i3_se

round_half_up(c(ci_lower, ci_upper), 1)

#    Part 6.3 · SD, SE, and Confidence Intervals
t.test(i3_df$length_mm)$conf.int # R's answer

i3_df %>% summary_stats(length_mm) # your function's answer

#    Part 6.4 · SD, SE, and Confidence Intervals
set.seed(42)
small_sample <- i3_df %>% slice_sample(n = 10)

s_mean <- mean(small_sample$length_mm)
s_se <- sd(small_sample$length_mm) / sqrt(10)

tibble(
  method = c("z (wrong here)", "t (correct)"),
  crit = round_half_up(c(1.96, qt(0.975, df = 9)), 3),
  lower = round_half_up(s_mean - crit * s_se, 1),
  upper = round_half_up(s_mean + crit * s_se, 1),
  width = round_half_up(upper - lower, 1)
)

#    Part 7.1 · One-Sample t-Test ------------
t.test(i3_df$length_mm, mu = 3200)
help(t.test)

g_df %>%
  ggplot(aes(lake, length_mm)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 4)

g_df %>%
  ggplot(aes(x = length_mm)) +
  geom_histogram(aes(color = lake))

#    Part 8.1 · Two-Sample t-Test ------------
t.test(length_mm ~ lake, data = g_df, alternative = "less")


#    Part 8.2 · Two-Sample t-Test ------------
t.test(length_mm ~ lake, data = g_df, alternative = "two.sided")
