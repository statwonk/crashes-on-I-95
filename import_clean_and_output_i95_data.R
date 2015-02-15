library(dplyr)
library(ggplot2)
library(leafletR)
library(ggthemes)

d <- load_data() %>%
  arrange(time) %>%
  filter(cntoffatl > 0) %>% # just fatal crashes
#  filter(cntofsvinj > 0) %>% # just severe injuries
# filter(cntoffatl > 0 | cntofsvinj > 0) %>% # fatal and severe injuries
  mutate(color = ifelse(cntoffatl > 0, "#ff000", "#ffff00"),
         opacity = ifelse(cntoffatl > 0, 1, 0.3)) %>%
  select(crashnum, time,
         cntofpers, cntoffatl, cntofsvinj,
         direction, color, opacity,
         latitude, longitude)

# make_json_output(d)

# Some times depending on how you group and
# count the data, it's possible to not have at
# least one value for each year, hour, or day, etc.
# In this case, I use a map called date_extent by
# joining it to our counted data and then converting
# NA values to 0 values.
date_extent <- tbl_df(expand.grid(
  direction = c("north", "south"),
#   date = seq(as.POSIXct(format(as.POSIXct(
#     min(d$time)), "%Y-%m-%d")),
#     as.POSIXct(max(d$time)), "day"),
  hour_of_day = str_pad(seq(0, 23, 1), 2, pad = "0"),
stringsAsFactors = F))

d <- left_join(date_extent,
               tbl_df(d) %>%
                 mutate(time = as.POSIXct(time),
                        hour_of_day = format(time, "%H")) %>%
                 group_by(direction, hour_of_day) %>%
                 summarise(crashes = n()),
               by = c("direction", "hour_of_day")) %>%
  mutate(crashes = ifelse(is.na(crashes), 0, crashes),
         hour_of_day = as.numeric(hour_of_day))

d
library(scales); library(grid)
write.csv(d, "number_of_severe_crashes_by_hour.csv")
ggplot(d,
       aes(x = hour_of_day,
           y = crashes,
           fill = factor(direction))) +
  geom_bar(stat = "identity",
           position = position_dodge(),
           size = 1) +
  scale_x_continuous(breaks = seq(0, 23, 1)) +
  geom_hline(yintercept = mean(d$crashes), colour = "black") +
  scale_fill_tableau(name = "Direction", labels = c("North", "South")) +
  new_tropic_theme(base_size = 25) +
  ylab("Accidents") +
  ggtitle("I-95 Fatal Express Lane Accidents") +
  theme(legend.title = element_text(size = 50),
        legend.text = element_text(size = 45, face = "bold"),
        axis.text.y = element_text(hjust = 1, vjust = 1),
        axis.text.x = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.title.y = element_text(colour = "black",
                                    size = 30,
                                    angle = 90))
