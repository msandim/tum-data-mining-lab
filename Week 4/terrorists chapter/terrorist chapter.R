library(dplyr)
library(ggplot2)

data <- read.csv("../../terrorism.csv")
group_names <- read.csv("final_names.csv")

data <- data[data$gname != "Unknown" & data$gname != "Other" & data$gname != "Unaffiliated Individual(s)",]

# Top 15 most active groups
data1 <- data %>% group_by(gname) %>% summarise(number_incidents = n()) %>% 
  arrange(desc(number_incidents)) %>% head(15)

ggplot(data1, aes(x = reorder(gname, number_incidents), y = number_incidents)) +
  geom_histogram(stat="identity", fill = "blue") +
  coord_flip() + 
  xlab("Group name") +
  ylab("Incidents")

# Regions in which the terrorists attack
data1 <- data %>% group_by(region_txt) %>% summarise(number_incidents = n()) %>% 
  arrange(desc(number_incidents)) %>% head(15)

ggplot(data1, aes(x = reorder(region_txt, number_incidents), y = number_incidents)) +
  geom_histogram(stat="identity", fill = "blue") +
  coord_flip() + 
  xlab("Region") +
  ylab("Incidents")

# Countries in which the terrorists attack
data1 <- data %>% group_by(country_txt) %>% summarise(number_incidents = n()) %>% 
  arrange(desc(number_incidents)) %>% head(15)

ggplot(data1, aes(x = reorder(country_txt, number_incidents), y = number_incidents)) +
  geom_histogram(stat="identity", fill = "blue") +
  coord_flip() + 
  xlab("Country") +
  ylab("Incidents")

# Median number of perpetratiors in regions and countries, we used median to avoid outliers
data1 <- data %>% filter(nperps != "Unknown" & nperps != "-99") %>% group_by(region_txt) %>%
  summarise(n_perps = median(nperps, na.rm = TRUE)) %>% 
  arrange(desc(n_perps)) %>% head(15)

ggplot(data1, aes(x = reorder(region_txt, n_perps), y = n_perps)) +
  geom_histogram(stat="identity", fill = "blue") +
  coord_flip() + 
  xlab("Region") +
  ylab("Incidents")

data1 <- data %>% filter(nperps != "Unknown" & nperps != "-99") %>% group_by(country_txt) %>%
  summarise(n_perps = median(nperps, na.rm = TRUE)) %>% 
  arrange(desc(n_perps)) %>% head(15)

ggplot(data1, aes(x = reorder(country_txt, n_perps), y = n_perps)) +
  geom_histogram(stat="identity", fill = "blue") +
  coord_flip() + 
  xlab("Country") +
  ylab("Incidents")

## get top5 groups
best_groups <- data %>% group_by(gname) %>% summarise(number_incidents = n()) %>% 
  arrange(desc(number_incidents)) %>% head(15)

##what kind of attack
data1 <- data %>% filter(attacktype1_txt != "Unknown") %>%
  group_by(attacktype1_txt) %>%
  summarise(count = n())

ggplot(data1, aes(x = reorder(attacktype1_txt, count), y = count)) +
  geom_histogram(stat="identity", fill = "blue") +
  coord_flip() + 
  xlab("Country") +
  ylab("Incidents")


## what kind of attack per group

data1 <- data %>% filter(gname %in% best_groups$gname & attacktype1_txt != "Unknown")

ggplot(data1, aes(x = factor(gname, levels = rev(best_groups$gname)), fill = attacktype1_txt)) +
  geom_bar(position = "fill") +
  scale_fill_discrete(name = "Attack type") +
  coord_flip() +
  xlab("Terrorist groups") +
  ylab("Percentage of incidents")

## Type of weapons
levels(data$weaptype1_txt)[levels(data$weaptype1_txt)=="Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)"] <- "Vehicle"

data1 <- data %>% filter(weaptype1_txt != "Other" & weaptype1_txt != "Unknown") %>%
  group_by(weaptype1_txt) %>% summarise(count = n()) %>% arrange(desc(count))

ggplot(data1, aes(x = reorder(weaptype1_txt, count), y = count)) +
  geom_histogram(stat="identity", fill = "blue") +
  coord_flip() + 
  xlab("Group name") +
  ylab("Incidents")

