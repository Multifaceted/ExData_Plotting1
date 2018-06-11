library(tidyverse)
library(lubridate)
library(gridExtra)

data <- read_delim('household_power_consumption.txt', delim = ';')
data$Date  <- dmy(data$Date)
data <- filter(data, data$Date >= ymd('2007-02-01'), data$Date <= ymd('2007-02-02'))

active_hist <- data %>%
  select(Global_active_power) %>%
  ggplot(aes(x = Global_active_power)) +
  geom_histogram(binwidth = 0.5, fill = 'red', breaks = seq(0,8, 0.5), col = 'black') + 
  labs(x = 'Global Active Power (kilowatts)', y = 'Frequency', title = 'Global Active Power') + 
  scale_y_continuous(breaks = seq(0, 1200, 200))