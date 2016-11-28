library("openxlsx")
library(dplyr)
library(plotly)

data <- read.xlsx("../../globalterrorismdb_0616dist.xlsx", sheet = 1)
data_backup <- data

# Cleaning
data <- data[data$gname != "Unknown" & data$gname != "Other" & data$gname != "Unaffiliated Individual(s)",]
data[data$imonth == 0, "imonth"] <- 1
data[data$iday == 0, "iday"] <- 1
data <- data[!is.na(data$doubtterr) & data$doubtterr == 0,]
data <- data[!is.na(data$region_txt),]
data$date <- as.Date(paste0(data$iyear, "/", data$imonth, "/", data$iday))
data <- data %>% select(eventid, date, everything())

#  Arrange data
groups_region <- data %>%
  group_by(region_txt, gname) %>%
  arrange(date) %>%
  summarise(start = first(date), end = last(date), number_incidents = n(),
            mean_number_kills = sum(nkill, na.rm = TRUE)) %>%
  arrange(region_txt, desc(number_incidents)) %>%
  #mutate(gname = paste0(gname, " | ", sprintf("%04d", number_incidents))) %>%
  mutate(Duration = end - start) %>%
  rename(Region = region_txt, Group = gname, Start = start, End = end,
         Incidents = number_incidents,
         Kills = mean_number_kills)

groups_total <- data %>%
  group_by(gname) %>%
  arrange(date) %>%
  summarise(start = first(date), end = last(date), number_incidents = n(),
            mean_number_kills = sum(nkill, na.rm = TRUE)) %>%
  arrange(desc(number_incidents)) %>%
  #mutate(gname = paste0(gname, " | ", sprintf("%04d", number_incidents))) %>%
  mutate(Duration = end - start) %>%
  rename(Group = gname, Start = start, End = end, Incidents = number_incidents,
         Kills = mean_number_kills)

# Get the top 30 groups with longer periods, incidents and killings
groups_total_top <-
  bind_rows(top_n(groups_region %>% group_by(Region), 30, Incidents),
            top_n(groups_region %>% group_by(Region), 30, Kills),
            top_n(groups_region %>% group_by(Region), 30, Duration))

groups_total_top <- unique(groups_total_top$Group)

#groups_total_top <- bind_rows(top_n(groups_total, 30, Incidents),
#                              top_n(groups_total, 30, Kills),
#                              top_n(groups_total, 30, Duration)) %>%
#  distinct(Group, .keep_all = TRUE)

groups_region_top <-
  bind_rows(top_n(groups_region %>% group_by(Region), 30, Incidents),
            top_n(groups_region %>% group_by(Region), 30, Kills),
            top_n(groups_region %>% group_by(Region), 30, Duration)) %>%
  distinct(Region, Group, .keep_all = TRUE)
  

write.csv(groups_region_top, "top_groups_region.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(groups_total_top, "top_groups_total.csv", row.names = FALSE, fileEncoding = "UTF-8")
