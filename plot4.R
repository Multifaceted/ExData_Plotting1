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

active <- data %>%
  filter(wday(Date, lab = TRUE) >= 'Thu', wday(Date, lab = TRUE) <= 'Sat') %>%
  group_by(wday(Date, lab = TRUE), Time) %>%
  summarize(power = mean(Global_active_power)) %>%
  ggplot(aes(x = 1:2880, y = power)) +
  geom_line() +
  scale_x_continuous(breaks = c(1, 1440, 2880), labels = c('Thu', 'Fri', 'Sat')) +
  labs(x = NULL, y = 'Global Active Power (kilowatts)')

meter <- data %>%
  filter(wday(Date, lab = TRUE) >= 'Thu', wday(Date, lab = TRUE) <= 'Sat') %>%
  select(Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3) %>%
  group_by(wday(Date, lab = TRUE), Time) %>%
  summarize(Sub_metering_1= mean(Sub_metering_1), Sub_metering_2 = mean(Sub_metering_2), Sub_metering_3 = mean(Sub_metering_3)) %>%
  gather(index, value, -1, -2) %>%
  ggplot(aes(x = rep(1:2880, 3), y = value, col = index)) +
  geom_line() +
  scale_x_continuous(breaks = c(1, 1440, 2880), labels = c('Thu', 'Fri', 'Sat')) +
  labs(x = NULL, y = 'Energy sub metering') +
  scale_color_manual(values=c("black", "red", "blue")) +
  theme(legend.position = c(0.85, 0.9), legend.title = element_blank())

voltage <- data %>%
  filter(wday(Date, lab = TRUE) >= 'Thu', wday(Date, lab = TRUE) <= 'Sat') %>%
  group_by(wday(Date, lab = TRUE), Time) %>%
  summarize(voltage = mean(Voltage)) %>%
  ggplot(aes(x = 1:2880, y = voltage)) +
  geom_line() +
  scale_x_continuous(breaks = c(1, 1440, 2880), labels = c('Thu', 'Fri', 'Sat')) +
  labs(x = 'datatime', y = 'Voltage')

reactive <- data %>%
  filter(wday(Date, lab = TRUE) >= 'Thu', wday(Date, lab = TRUE) <= 'Sat') %>%
  group_by(wday(Date, lab = TRUE), Time) %>%
  summarize(reactive = mean(Global_reactive_power)) %>%
  ggplot(aes(x = 1:2880, y = reactive)) +
  geom_line() +
  scale_x_continuous(breaks = c(1, 1440, 2880), labels = c('Thu', 'Fri', 'Sat')) +
  labs(x = 'datatime', y = 'Global_reactive_power')

multi <- grid.arrange(active, voltage, meter, reactive, nrow = 2)
ggsave('plot4.png', multi, width = 5, height = 5, dpi = 96, scale = 1)
