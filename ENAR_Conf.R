library(corrplot)
library(ggplot2)
library(tidyverse)
library(caret)
library(dplyr)
library(dplyr)
library(tidyr)
library(janitor)

#install.packages("janitor")
df<-read.csv("/Users/lakeside/Documents/ENAR conference/data_AD_csv.csv", sep=',', header= TRUE) #loading of the ra
str(df) #data structure
glimpse(df)
#Rows: 178,539
#Columns: 36
#head(df)

clean<-clean_names(df)
colnames(clean)
clean %>% tabyl(x_2) %>% adorn_pct_formatting(digits =2,affix_sign=TRUE) #checking EDA of the element inside the columns
clean_x<-clean %>% remove_empty(whic=c("cols")) #This remove columns that are totally empty for example x, x_1..
glimpse(clean_x)
#Rows: 178,539
#Columns: 29

#stratification_category_id1
#stratification_id1
#stratification_category_id2
#stratification_id2  
#data_value_unit, 
#data_value_type_id, 
#stratification_category1, 
#stratification_category2
#x_2

# Remove/drop multiple columns
df2 <- subset(clean_x, select = -c(stratification_category_id1, stratification_id1, 
                                   stratification_category_id2, stratification_id2, 
                                   data_value_unit, data_value_type_id, 
                                   stratification_category1, stratification_category2, x_2, geolocation))

#head(df2) # Displays the first few rows
#tail(df2) # Displays the last rows
dim(df2)
# row: 178539     19
glimpse(df2)# display the STRUCTURE of the dataset professionally

#str(df2)  
View(df2) #Create a dataset in excel format
summary(df2)

unique(df2$class)

#df2 %>% tabyl(df2$class) %>% adorn_pct_formatting(digits =2,affix_sign=TRUE)
vnames <- colnames(df2)
n <- nrow(df2)
out <- NULL
for (j in 1:ncol(df2)){
  vname <- colnames(df2)[j]
  x <- as.vector(df2[,j])
  n1 <- sum(is.na(x), na.rm=TRUE)  # NA
  n2 <- sum(x=="NA", na.rm=TRUE) # "NA"
  n3 <- sum(x==" ", na.rm=TRUE)  # missing
  nmiss <- n1 + n2 + n3
  nmiss <- sum(is.na(x))
  ncomplete <- n-nmiss
  out <- rbind(out, c(col.num=j, v.name=vname, mode=mode(x), n.level=length(unique(x)),
                      ncom=ncomplete, nmiss= nmiss, miss.prop=nmiss/n))
}
out <- as.data.frame(out) 
row.names(out) <- NULL
out

colMeans(is.na(df2))

########  Missing values by visualization ###############
library(naniar)
par(mfrow = c(2, 2))
vis_miss(df2, warn_large_data=FALSE)
gg_miss_var(df2)

########### omitting data #########
data <- na.omit(df2) 
vis_miss(data, warn_large_data=FALSE)
gg_miss_var(data)


vnames <- colnames(data)
n <- nrow(data)
out <- NULL
for (j in 1:ncol(data)){
  vname <- colnames(data)[j]
  x <- as.vector(data[,j])
  n1 <- sum(is.na(x), na.rm=TRUE)  # NA
  n2 <- sum(x=="NA", na.rm=TRUE) # "NA"
  n3 <- sum(x==" ", na.rm=TRUE)  # missing
  nmiss <- n1 + n2 + n3
  nmiss <- sum(is.na(x))
  ncomplete <- n-nmiss
  out <- rbind(out, c(col.num=j, v.name=vname, mode=mode(x), n.level=length(unique(x)),
                      ncom=ncomplete, nmiss= nmiss, miss.prop=nmiss/n))
}
out <- as.data.frame(out) 
row.names(out) <- NULL
out
data1 <- data %>% rename(gender = stratification2)
data1 <- data1 %>% rename(age_range = stratification1)

head(data1) # Displays the first few rows
tail(data1) # Displays the last rows
dim(data1) #120750     19
str(data1)  # display the STRUCTURE variable
View(data1)
summary(data1)

write.csv(data1, file="/Users/lakeside/Documents/ENAR conference/data_cleaned.csv", row.names =FALSE)




