library(dplyr)
library(caret)
set.seed(2)


df_original <- read.csv("../../terrorism.csv", stringsAsFactors = FALSE)

generate_dataset <- function(number_groups)
{
  df <- df_original
  
  top_names <- df %>%
    filter(gname != "Other") %>%
    group_by(region_txt, gname) %>%
    summarise(freq = n()) %>%
    arrange(region_txt, desc(freq)) %>%
    group_by(region_txt) %>%
    filter(row_number() <= number_groups) %>%
    ungroup()
  
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
  
  ## create directory
  dir.create(file.path("datasets", number_groups))
  
  for(region in unique(df$region_txt))
  {
    #print(region)
    
    df_region <- df %>% filter(region_txt == region)
    
    #inTraining <- createDataPartition(df_region$gname, p=0.6, list=FALSE)
    #train <- df_region[inTraining,]
    #Totalvalidation.set <- df_region[-inTraining,]
    # This will create another partition of the 60% of the data, so 20%-testing and 20%-validation
    #inValidation <- createDataPartition(Totalvalidation.set$gname, p=0.5, list=FALSE)
    #test <- Totalvalidation.set[inValidation,]
    #valid <- Totalvalidation.set[-inValidation,]
    
    write.csv(df_region, paste0("datasets/",number_groups,"/",region,".csv"), fileEncoding = "UTF-8", row.names=FALSE)
    #write.csv(train, paste0("datasets/",number_groups,"/",region,"_train.csv"), fileEncoding = "UTF-8", row.names=FALSE)
    #write.csv(valid, paste0("datasets/",number_groups,"/",region,"_valid.csv"), fileEncoding = "UTF-8", row.names=FALSE)
    #write.csv(test, paste0("datasets/",number_groups,"/",region,"_test.csv"), fileEncoding = "UTF-8", row.names=FALSE)
  }
}

lapply(1:50, function(x) generate_dataset(x))