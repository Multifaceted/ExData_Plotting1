library(tidyverse)
library(lubridate)
library(gridExtra)

data <- read_delim('household_power_consumption.txt', delim = ';')
data$Date  <- dmy(data$Date)
data <- filter(data, data$Date >= ymd('2007-02-01'), data$Date <= ymd('2007-02-02'))

active <- data %>%
  filter(wday(Date, lab = TRUE) >= 'Thu', wday(Date, lab = TRUE) <= 'Sat') %>%
  group_by(wday(Date, lab = TRUE), Time) %>%
  summarize(power = mean(Global_active_power)) %>%
  ggplot(aes(x = 1:2880, y = power)) +
  geom_line() +
  scale_x_continuous(breaks = c(1, 1440, 2880), labels = c('Thu', 'Fri', 'Sat')) +
  labs(x = NULL, y = 'Global Active Power (kilowatts)')

ggsave('plot2.png', active, width = 5, height = 5, dpi = 96, scale = 1)
