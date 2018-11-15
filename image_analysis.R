
library(readxl)
library(tidyr)
library(dplyr)

setwd("C:/Users/Alyssa/Dropbox/R Group/") # Change to your directory
sytox_data <- read_excel("./Sytox Data.xlsx", sheet = 1)




sytox_data = sytox_data %>%
  separate(Source, into = c("drop", 'timepoint', 'treatment', 'myelin', 'glucose', "drop2"), sep= ' ', remove = TRUE) %>%
  unite(mye.treatment, c("treatment", "myelin")) %>%
  select(-drop) %>%
  select(-drop2)

head(sytox_data)

sytox_data = sytox_data %>%
  mutate(total_cells = DAPI + Sytox) %>%
  mutate(percent_dead = (Sytox/total_cells)*100)

head(sytox_data)


mean_stats = sytox_data %>%
  group_by(glucose) %>%
  summarise(percent_dead_mean = mean(percent_dead), sd_percent = sd(percent_dead))
mean_stats

library(ggplot2)

ggplot(mean_stats, aes(x= glucose, y =  percent_dead_mean)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=percent_dead_mean-sd_percent, ymax=percent_dead_mean+sd_percent),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  ylab("Percentage of Dead Cells") +
  xlab("Glucose Concentration") +
  labs(title = "Barplot of Data using ggplot2") +
  theme_bw()


sytox_data2 <- read_excel("./Sytox Data.xlsx", sheet = 2)

sytox_data2 = sytox_data2 %>%
  separate(Source, into = c("drop", 'timepoint', 'treatment', 'myelin', 'glucose', "drop"), sep= ' ', remove = TRUE) %>%
  unite(mye.treatment, c("treatment", "myelin")) %>%
  select(-drop) %>%
  mutate(total_cells = DAPI + Sytox) %>%
  mutate(percent_dead = (Sytox/total_cells)*100)


sytox_data_full = bind_rows(sytox_data, sytox_data2)

mean_stats_full = sytox_data_full %>%
  group_by(glucose, mye.treatment) %>%
  summarise(percent_dead_mean = mean(percent_dead), sd_percent = sd(percent_dead)) %>%
  arrange(mye.treatment)
mean_stats_full



ggplot(mean_stats_full, 
       aes(x= glucose, y = percent_dead_mean, fill = mye.treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=percent_dead_mean-sd_percent, ymax=percent_dead_mean+sd_percent),
                size=.3,    # Thinner lines
                width=.2,
                position = position_dodge(width = 0.9)) +
  ylab("Percentage of Dead Cells") +
  xlab("Glucose Concentration (g/L)") +
  labs(title = "Barplot of Data using ggplot2") +
  theme_bw()

