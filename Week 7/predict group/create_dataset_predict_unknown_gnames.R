library(dplyr)
library(caret)
set.seed(2)


df_original <- read.csv("terrorism_with_unknown.csv", stringsAsFactors = FALSE)

generate_dataset <- function()
{
  df <- df_original
  
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
  
  ### Attack Types
  
  
  ### Target Types
  
  # Nationalities:
  df[df$natlty1_txt == ".", "natlty1_txt"] <- "Unknown"
  
  ## Weapon types
  df[df$weaptype1_txt == "Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)", "weaptype1_txt"] <- "Vehicle"
  df[df$weapsubtype1_txt == ".", "weapsubtype1_txt"] <- "Unknown"
  
  # Feature selection:
  df <- df %>% select(iyear,
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
                      
                      # new categorical variables:
                      attacktype1_txt,
                      targtype1_txt,
                      natlty1_txt,
                      weaptype1_txt,
                      weapsubtype1_txt,
                      
                      region_txt,
                      gname)
  
  write.csv(df, "terrorism_with_unknown_cleaned.csv", fileEncoding = "UTF-8", row.names=FALSE)
}

generate_dataset()