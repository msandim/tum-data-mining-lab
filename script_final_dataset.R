setwd("~/tum-data-mining-lab")

library(wordcloud)
library(tm)
library(dplyr)
library(openxlsx)

mydf <- read.xlsx("globalterrorismdb_0616dist.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

colnames(mydf)
na <- is.na(mydf)

variables <- c("eventid",
              "iyear",              
              "imonth",
              "iday",
              "extended",
              "resolution",
              "country",
              "country_txt",
              "region",
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
              "attacktype1",
              "attacktype1_txt",
              "targtype1",
              "targtype1_txt",
              "corp1",
              "target1",
              "natlty1",
              "natlty1_txt",
              "gname",
              "ingroup",
              "motive",
              "guncertain1",
              "nperps",
              "nperpcap",
              "claimed",
              "weaptype1",
              "weaptype1_txt",
              "weapsubtype1",
              "weapsubtype1_txt",
              "weapdetail",
              "nkill",
              "nwound",
              "property",
              "propextent",
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
              "hostkidoutcome",
              "hostkidoutcome_txt",
              "nreleased",
              "addnotes"
              )

new_df <- mydf[,variables]
colnames(new_df)

new_df <- new_df %>% filter(doubtterr != 1)
write.csv(new_df, "terrorism.csv", fileEncoding = "UTF-8", row.names=FALSE)
