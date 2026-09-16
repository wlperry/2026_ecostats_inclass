# install packages so we can use them
# install.packages("tidyverse")
# install.packages("fs")
library(tidyverse)
library(fs)


pine_1_df <- read_csv(
  "data/01_04_pine_needles/individual_files/heal_thome_pineneedles.csv"
)
pine_2_df <- read_csv(
  "data/01_04_pine_needles/individual_files/Bartell_Shingledecker_tree_needle_data_2026.csv"
)
pine_3_df <- read_csv(
  "data/01_04_pine_needles/individual_files/Kehr_and_Baalke.csv"
)
pine_4_df <- read_csv(
  "data/01_04_pine_needles/individual_files/Needle_Length_Suhr:Bulthuis.csv"
)

# bind the rows to a single file ------
pine_df <- bind_rows(pine_1_df, pine_2_df, pine_3_df, pine_4_df)


# how to read all in at one time... with path names ------
pine_files <- list.files(
  "data/01_04_pine_needles/individual_files",
  pattern = "\\.csv$",
  full.names = TRUE
) %>%
  set_names(basename(.)) # will do only the name of the file
# set_names()

pine_files <- dir_ls(
  "data/01_04_pine_needles/individual_files",
  regexp = "\\.csv$"
) %>%
  set_names(basename(.)) # will do only the name of the file

pine_df <- pine_files %>%
  map(read_csv) %>%
  list_rbind(names_to = "source_file")

# write out the file
write_csv(pine_df, "output/pine/pine_data.csv")
write_csv(pine_df, "data/01_04_pine_needles/pine_data.csv")


# simple graph
pine_df %>%
  ggplot(aes(side, needle_length_mm)) +
  geom_boxplot() +
  geom_point(position = position_dodge2(width = 0.2))

# simple graph
pine_df %>%
  ggplot(aes(side, needle_width_mm)) +
  geom_boxplot() +
  geom_point(position = position_dodge2(width = 0.2))

# simple graph
pine_red_df <- pine_df %>%
  group_by(team, side) %>%
  summarize(
    needle_length_mm = mean(needle_length_mm, na.rm = TRUE),
    needle_width_mm = mean(needle_width_mm, na.rm = TRUE),
    .groups = "drop_last"
  )

# simple graph
pine_red_df %>%
  ggplot(aes(side, needle_length_mm)) +
  geom_boxplot() +
  geom_point(position = position_dodge2(width = 0.2)) +
  geom_line(aes(group = team), position = position_dodge2(width = 0.2))

# simple graph
pine_red_df %>%
  ggplot(aes(side, needle_width_mm)) +
  geom_boxplot() +
  geom_jitter(position = position_dodge2(width = 0.2)) +
  geom_line(aes(group = team), position = position_dodge2(width = 0.2))
