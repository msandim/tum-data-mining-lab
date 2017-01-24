library(dplyr)
library(caret)
library(randomForest)
library(pROC)

set.seed(2)

regions <- c("Australasia & Oceania",
             "Central America & Caribbean",
             "Central Asia",
             "East Asia",
             "Eastern Europe",
             "Middle East & North Africa",
             "North America",
             "South America",
             "South Asia",
             "Southeast Asia",
             "Sub-Saharan Africa",
             "Western Europe")


get_accuracy <- function(number_groups, region)
{
  train <- read.csv(paste0("datasets/",number_groups,"/",region,"_train.csv"))
  valid <- read.csv(paste0("datasets/",number_groups,"/",region,"_valid.csv"))
  
  ## Fix levels
  for(attr in colnames(train))
  {
    if (is.factor(train[[attr]]))
    {
      levels(valid[[attr]]) <- levels(train[[attr]])
    }
  }
  
  print(train$gname)
  print(valid$gname)
  
  model <- randomForest(gname ~ . - region_txt, data=train)
  
  preds <- predict(model, newdata = valid)
  
  print(sum(valid$gname == preds) / length(valid$gname))
}

get_accuracy(10, "South Asia")
