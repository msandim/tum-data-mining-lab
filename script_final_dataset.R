setwd("~/tum-data-mining-lab")

library(wordcloud)
library(tm)
library(dplyr)
library(openxlsx)

mydf <- read.xlsx("globalterrorismdb_0616dist.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

### added by Miguel:
final_group_names <- read.csv("final_group_names.csv")
###

colnames(mydf)
na <- is.na(mydf)

variables <- c("eventid",
              "iyear",              
              "imonth",
              "iday",
              "extended",
              "resolution",
              "country_txt",
              "region_txt",
              "city",
              "latitude",
              "longitude",
              "specificity",
              "vicinity",
              "summary",
              "doubtterr",
              "multiple",
              "success",
              "suicide",
              "attacktype1_txt",
              "attacktype2_txt",
              "attacktype3_txt",
              "targtype1_txt",
              "targtype2_txt",
              "targtype3_txt",
              "corp1",
              "target1",
              "target2",
              "target3",
              "natlty1_txt",
              "gname",
              "ingroup",
              "motive",
              "guncertain1",
              "nperps",
              "nperpcap",
              "claimed",
              "weaptype1_txt",
              "weapsubtype1_txt",
              "weaptype2_txt",
              "weapsubtype2_txt",
              "weaptype3_txt",
              "weapsubtype3_txt",
              "weaptype4_txt",
              "weapsubtype4_txt",
              "weapdetail",
              "nkill",
              "nwound",
              "property",
              "propextent_txt",
              "ishostkid",
              "nhostkid",
              "nhours",
              "ndays",
              "divert",
              "kidhijcountry",
              "ransom",
              "ransomamt",
              "ransomamtus",
              "ransompaid",
              "ransomnote",
              "hostkidoutcome_txt",
              "nreleased",
              "addnotes"
              )

new_df <- mydf[,variables]
colnames(new_df)

new_df <- new_df %>% filter(doubtterr != 1)

### Added my Miguel
new_df <- new_df[new_df$gname %in% final_group_names$x,]
###

write.csv(new_df, "terrorism.csv", fileEncoding = "UTF-8", row.names=FALSE)
