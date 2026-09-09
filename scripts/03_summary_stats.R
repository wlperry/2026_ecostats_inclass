library(tidyverse)

pine_df <- read_csv("data/pine_data.csv")

shady_df <- pine_df %>% filter(side == "shady")
sunny_df <- pine_df %>% filter(side == "sunny")

shady_df %>%
  summarize(
    mean = mean(needle_length_mm),
    median = median(needle_length_mm)
  )

sun_stat <- sunny_df %>%
  summarize(
    mean = mean(needle_length_mm),
    median = median(needle_length_mm)
  ) %>%
  mutate(side = "sunny")

pine_df %>%
  group_by(side) %>%
  summarize(
    mean = mean(needle_length_mm, na.rm = TRUE),
    median = median(needle_length_mm)
  )


pine_df %>%
  group_by(side) %>%
  summarize(
    mean = mean(needle_length_mm, na.rm = TRUE),
    median = median(needle_length_mm),
    variance = var(needle_length_mm),
    sd = sd(needle_length_mm, na.rm = TRUE),
    n = sum(!is.na(needle_length_mm)),
    se = sd / n^.5,
    se2 = sd(needle_length_mm, na.rm = TRUE) / sum(!is.na(needle_length_mm))^.5,
  )

# install.packages("skimr")
library(skimr)
pine_df %>%
  group_by(side) %>%
  skim()

mean_df <- pine_df %>%
  group_by(team, side) %>%
  summarize(
    needle_length_mm = mean(needle_length_mm, na.rm = TRUE),
    needle_width_mm = mean(needle_width_mm, na.rm = TRUE)
  )

mean_df %>%
  group_by(side) %>%
  summarize(
    needle_length_mm = mean(needle_length_mm, na.rm = TRUE),
    needle_width_mm = mean(needle_width_mm, na.rm = TRUE, , .groups = "drop")
  )

mean_df %>%
  ggplot(aes(side, needle_length_mm)) +
  geom_boxplot() +
  geom_point((aes(color = team), position = position_dodge(width = 0.3))) +
  geom_line(
    aes(color = team, group = team),
    position = position_dodge(width = 0.3)
  ) +
  stat_summary(fun = "mean", geom = "point", color = "red", size = 3) +
  stat_summary(
    fun.data = "mean_se",
    geom = "errorbar",
    color = "red",
    width = 0.2,
    linewidth = 0.9
  )


mean_df %>%
  ggplot(aes(side, needle_width_mm)) +
  geom_boxplot() +
  geom_point(aes(color = team), position = position_dodge(width = 0.3)) +
  geom_line(
    aes(color = team, group = team),
    position = position_dodge(width = 0.3)
  ) +
  stat_summary(fun = "mean", geom = "point", color = "red", size = 3) +
  stat_summary(
    fun.data = "mean_se",
    geom = "errorbar",
    color = "red",
    width = 0.2,
    linewidth = 0.9
  )
