library(tidyverse)
indycar <- read.csv("https://indycar_2019.csv")
head(indycar)


indycar %>%
  ggplot(aes(x = start, y = finish)) +
  geom_point() +
  theme_bw()
# seems to be little correlation 
# appears that no one starting lower than 8th has won a race, confirmed from table below
indycar %>%
  filter(finish == 1)


indycar %>%
  group_by(drive_id) %>%
  summarise(total_points = sum(points)) %>%
  arrange(desc(total_points))


indycar %>%
  group_by(drive_id) %>%
  summarise(total_points = sum(points)) %>%
  ggplot(aes(x = total_points)) +
  geom_histogram(binwidth = 50) +
  theme_bw()


indycar %>%
  group_by(drive_id) %>%
  summarise(total_points = sum(points)) %>%
  ggplot(aes(x = total_points)) +
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  theme_bw()
  
  
library(tidyverse)
indycar %>%
  select(race_id, finish, points, drive_id, track_type) %>%
  group_by(drive_id, track_type) %>%
  mutate(points_earned = sum(points)) %>%
  group_by(drive_id) %>%
  mutate(races = n()) %>%
  filter(races == 17) %>%
  mutate(total_points = sum(points)) %>%
  filter(total_points > 500) %>%
  ggplot(aes(x = drive_id,y = points, fill = track_type)) +
  geom_col() +
  coord_flip() +
  theme_bw()


indycar %>%
  filter(finish %in% c(1:10)) %>%
  ggplot(aes(x = track_type, fill = chassis_engine_tires)) +
  geom_bar(position = "fill") +
  theme_bw()


table(indycar$chassis_engine_tires)





