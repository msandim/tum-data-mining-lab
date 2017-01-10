library(dplyr)

# Have the 5 groups with more incidents for each area
df <- read.csv("../terrorism.csv", stringsAsFactors = FALSE)

top_names <- df %>%
  group_by(region_txt, gname) %>%
  summarise(freq = n()) %>%
  arrange(region_txt, desc(freq)) %>%
  group_by(region_txt) %>%
  filter(row_number() <= 5L)

# generate a new dataset with all the names that aren't the 5 groups marked as other
# se o nome %in% top_names[region_txt], nome, senão Other
new_df <- df %>%
  inner_join(select(top_names, region_txt, gname), by="region_txt") %>%
  mutate(gname = ifelse(gname.x == gname.y, gname.x, "Other"))
  #mutate(gname = ifelse(gname %in% (filter(top_names, region_txt = )))