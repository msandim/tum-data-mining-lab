library(dplyr)

# Have the 5 groups with more incidents for each area
df <- read.csv("../terrorism.csv", stringsAsFactors = FALSE)

top_names <- df %>%
  group_by(region_txt, gname) %>%
  summarise(freq = n()) %>%
  arrange(region_txt, desc(freq)) %>%
  group_by(region_txt) %>%
  filter(row_number() <= 5L) %>%
  ungroup()

# generate a new dataset with all the names that aren't the 5 groups marked as other
# se o nome %in% top_names[region_txt], nome, senão Other
#new_df <- df %>%
#  inner_join(select(top_names, region_txt, gname), by="region_txt") %>%
#  mutate(gname = ifelse(gname.x == gname.y, gname.x, "Other"))
  #mutate(gname = ifelse(gname %in% (filter(top_names, region_txt = )))

top_names2 <- lapply(unique(top_names$region_txt), function(x) top_names %>% filter(region_txt == x) %>% .$gname %>% as.character)
names(top_names2) <- unique(top_names$region_txt)

# Label the groups according to the top we did
for(i in 1:nrow(df))
{
  df[i, "gname"] <- ifelse(df[i, "gname"] %in% top_names2[[df[i, "region_txt"]]], df[i, "gname"], "Other")
}

# Clean the variables we want (with median)
df[is.na(df$nperps) | df$nperps == -99, "nperps"] <- df %>% filter(nperps != -99) %>% .$nperps %>% median(na.rm = TRUE)
df[is.na(df$nperpcap) | df$nperpcap == -99, "nperpcap"] <- df %>% filter(nperpcap != -99) %>% .$nperpcap %>% median(na.rm = TRUE)
df[is.na(df$nkill), "nkill"] <- median(df$nkill, na.rm = TRUE)
df[is.na(df$nwound), "nwound"] <- median(df$nwound, na.rm = TRUE)
df[is.na(df$nhostkid), "nhostkid"] <- 0
df[is.na(df$nhostkid), "nhostkid"] <- df %>% filter(nhostkid != -99) %>% .$nhostkid %>% median(na.rm = TRUE)
df[is.na(df$ransomamt), "ransomamt"] <- 0
df[is.na(df$ransomamt), "ransomamt"] <- df %>% filter(ransomamt != -99) %>% .$ransomamt %>% median(na.rm = TRUE)
df[is.na(df$ransompaid), "ransompaid"] <- 0
df[is.na(df$ransompaid), "ransompaid"] <- df %>% filter(ransompaid != -99) %>% .$ransompaid %>% median(na.rm = TRUE)
df[is.na(df$nreleased), "nreleased"] <- 0
df[is.na(df$nreleased), "nreleased"] <- df %>% filter(nreleased != -99) %>% .$nreleased %>% median(na.rm = TRUE)


write.csv(df
          %>% select(iyear,
                     #imonth,
                     #iday,
                     multiple,
                     success,
                     suicide,
                     nperps,
                     nperpcap,
                     nkill,
                     nwound,
                     #property,
                     #ishostkid,
                     nhostkid,
                     #ransom,
                     ransomamt,
                     ransompaid,
                     nreleased,
                     region_txt,
                     gname), "terrorism_cleaned_group.csv", fileEncoding = "UTF-8", row.names=FALSE)

# df %>% inner_join(select(top_names, region_txt, gname), by = "region_txt") %>% View


######
#df <- df %>% mutate(yes = gname %in% top_names2[region_txt])


#dfs <- lapply(unique(df$region_txt), function(region) df %>% filter(region_txt == region))
#names(dfs) <- unique(df$region_txt)

#for(region in regions)
#{
#  df[df$region_txt == region & df$gname %in% top_names2[df$region_txt], "gname"] <- ifelse()
#}

###
#apply(df, 1, function(x) 
#  {
#    if (!(x["gname"] %in% top_names2[x["region_txt"]]))
#        x["gname"] <- "Other"
#  })

##