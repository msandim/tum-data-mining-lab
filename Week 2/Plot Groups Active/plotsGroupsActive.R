library("openxlsx")
library(dplyr)
library(plotly)

data <- read.xlsx("../../globalterrorismdb_0616dist.xlsx", sheet = 1)
data_backup <- data

# Cleaning
data <- data[data$gname != "Unknown" & data$gname != "Other" & data$gname != "Unaffiliated Individual(s)",]
data[data$imonth == 0, "imonth"] <- 1
data[data$iday == 0, "iday"] <- 1
data <- data[data$doubtterr == 1,]
data <- data[!is.na(data$region_txt),]
data$date <- as.Date(paste0(data$iyear, "/", data$imonth, "/", data$iday))
data <- data %>% select(eventid, date, everything())

#  Arrange data
data_plot_region <- data %>%
  group_by(region_txt, gname) %>%
  arrange(date) %>%
  summarise(start = first(date), end = last(date), number_incidents = n()) %>%
  arrange(region_txt, desc(number_incidents)) %>%
  mutate(gname = paste0(gname, " | ", sprintf("%04d", number_incidents))) %>%
  mutate(Duration = end - start) %>%
  rename(Region = region_txt, Group = gname, Start = start, End = end, Incidents = number_incidents)

data_plot_total <- data %>%
  group_by(gname) %>%
  arrange(date) %>%
  summarise(start = first(date), end = last(date), number_incidents = n()) %>%
  arrange(desc(number_incidents)) %>%
  mutate(gname = paste0(gname, " | ", sprintf("%04d", number_incidents))) %>%
  mutate(Duration = end - start) %>%
  rename(Group = gname, Start = start, End = end, Incidents = number_incidents)

write.csv(data_plot_region, "data_by_region.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(data_plot_total, "data_total.csv", row.names = FALSE, fileEncoding = "UTF-8")
