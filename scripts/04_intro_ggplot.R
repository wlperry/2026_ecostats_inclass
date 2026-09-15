# 04_intro_ggplot

# packages
install.packages(patchwork)

# libraries
library(janitor)
library(patchwork)
library(readxl)
library(tidyverse)

pine_df <- read_csv("data/pine_data.csv")

plot_1 <- pine_df %>%
  ggplot(aes(side, needle_length_mm)) +
  geom_point(position = position_jitter(width = 0.15))


plot_2 <- pine_df %>%
  ggplot(aes(side, needle_length_mm)) +
  geom_point(position = position_dodge2(width = 0.15))


plot_1 + plot_2

pine_df %>%
  ggplot(aes(side, needle_length_mm)) +
  # ylim(12, 15)+
  scale_y_continuous(limits = c(14, 15)) +
  geom_boxplot() +
  geom_point(position = position_dodge2(width = 0.15))


# histogram
histo_plot <- pine_df %>%
  ggplot(aes(x = needle_length_mm)) +
  geom_histogram(binwidth = 2)

pine_df %>%
  ggplot(aes(x = needle_length_mm)) +
  geom_density() +
  facet_wrap(~side)

density_plot <- pine_df %>%
  ggplot(aes(x = needle_length_mm)) +
  geom_density() +
  facet_grid(side ~ .)

histo_plot + density_plot


histo_plot <- pine_df %>%
  ggplot(aes(x = needle_length_mm, fill = side)) +
  geom_histogram(binwidth = 2, position = position_dodge(.95))
histo_plot


pine_df %>%
  ggplot(aes(side, needle_length_mm, color = needle_width_mm)) +
  geom_boxplot() +
  geom_point(position = position_dodge2(width = 0.15))


pine_df %>%
  ggplot(aes(side, needle_length_mm)) +
  geom_boxplot(color = "blue") +
  geom_point(
    aes(fill = side, color = team),
    shape = 21,
    position = position_dodge2(width = 0.15)
  )

source("r_themes_for_3_sizes.R")


pine_df %>%
  group_by(team, side) %>%
  summarize(needle_length_mm = mean(needle_length_mm, na.rm = TRUE)) %>%
  ggplot(aes(side, needle_length_mm, color = team)) +
  geom_point() +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    color = "limegreen",
    width = 0.2,
    linewidth = 0.9
  ) +
  stat_summary(fun = mean, geom = "point", color = "violet", size = 10) +
  theme_regular()
