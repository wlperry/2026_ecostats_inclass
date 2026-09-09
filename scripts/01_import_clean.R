# install packages so we can use them
# install.packages("tidyverse")
# install.packages("fs")
library(tidyverse)
library(fs)

pine_1_df <- read_csv("data/individual_files/heal_thome_pineneedles.csv")
pine_2_df <- read_csv(
  "data/individual_files/Bartell_Shingledecker_tree_needle_data_2026.csv"
)
pine_3_df <- read_csv("data/individual_files/Kehr_and_Baalke.csv")
pine_4_df <- read_csv("data/individual_files/Needle_Length_Suhr:Bulthuis.csv")

pine_df <- bind_rows(pine_1_df, pine_2_df, pine_3_df, pine_4_df)

# how to read all in at one time...
pine_files <- list.files("data", pattern = "\\.csv$", full.names = TRUE) %>%
  # set_names(basename(.))
  set_names()

pine_files <- dir_ls("data", regexp = "\\.csv$")

pine_df <- pine_files %>%
  map(read_csv) %>%
  list_rbind(names_to = "source_file")


write_csv(pine_df, "output/pine_data.csv")


pine_df %>%
  ggplot(aes(side, needle_length_mm)) +
  geom_boxplot() +
  geom_point(position = position_dodge2(width = 0.2))


pine_df %>%
  ggplot(aes(side, needle_width_mm)) +
  geom_boxplot() +
  geom_point(position = position_dodge2(width = 0.2))

pine_red_df <- pine_df %>%
  group_by(team, side) %>%
  summarize(
    needle_length_mm = mean(needle_length_mm, na.rm = TRUE),
    needle_width_mm = mean(needle_width_mm, na.rm = TRUE),
    .groups = "drop_last"
  )


pine_red_df %>%
  ggplot(aes(side, needle_length_mm)) +
  geom_boxplot() +
  geom_point(position = position_dodge2(width = 0.2)) +
  geom_line(aes(group = team), position = position_dodge2(width = 0.2))


pine_red_df %>%
  ggplot(aes(side, needle_width_mm)) +
  geom_boxplot() +
  geom_jitter(position = position_dodge2(width = 0.2)) +
  geom_line(aes(group = team), position = position_dodge2(width = 0.2))
