# install packages so we can use them
# install.packages("tidyverse")

library(tidyverse)

pine_df <- read_csv("data/pine_data.csv")


write_csv(pine_df, "output/pine_data.csv")

# part 2 Filter
pine_csv_df <- pine_df %>%
  mutate(group_length = if_else(needle_length_mm <= 18, "long", "longer"))

pine_df <- pine_df %>%
  mutate(
    size_class = case_when(
      needle_length_mm < 16 ~ "short",
      needle_length_mm < 20 ~ "medium",
      TRUE ~ "long"
    )
  )

pine_df <- pine_df %>%
  mutate(
    size_class = case_when(
      needle_length_mm < 12 ~ "short",
      needle_length_mm < 15 ~ "medium",
      needle_length_mm < 18 ~ "long",
      TRUE ~ "out of range "
    )
  )

unique(pine_df$team)

pine_df <- pine_df %>%
  mutate(
    number_team = case_when(
      team == "grace_jace" ~ "1",
      team == "one" ~ "2",
      team == "waterbugs" ~ "3",
      team == "botany" ~ "4",
      TRUE ~ "99999"
    )
  )

n_distinct(pine_df)


ggplot(pine_df, aes(side, needle_length_mm)) +
  geom_boxplot()


pine_df %>%
  mutate(needle_length_cm = needle_length_mm / 10) %>%
  ggplot(aes(side, needle_length_cm)) +
  geom_boxplot()
