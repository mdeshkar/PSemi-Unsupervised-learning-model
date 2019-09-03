######################################################################
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(standardize)

###### Reading the Data Set ##########################################
data <- read.csv("psemi.csv")
dim(data) # 90636     4

### Cleaning the Data set ############################################
df <- data[1:3]
dim(df) #  90636     3

########## Changing the variable names #######################################
names(df) <- c("wafer_id","failure_type", "failure_rate")
df$failure_type <- as.factor(df$failure_type)

########## Identifying the Missing values ###################################
summary(df) # 3237 NA's in Faliure type  
na_prop <-  sum(is.na(df$failure_type == TRUE)) / length(df$failure_type) # 0.0357

# Given: 3237 wafers having 27 unique failure categories. Neglecting the NA values does 
# not affect the goal of our analysis as it restores all the failure categories for 3237
# wafers uniquely.
# Thus for the analysis we will consider this 27 categeories and neglect the "NA" values. 
# and it is around 3.5 percent of the the current data set. 

######### Getting rid of the NA values ######################################
df_clean <- na.omit(df)
dim(df_clean) # 87399     3

######### Summary Statistics for the clean data set #########################
summary(df_clean) # Failure rates seems to have unusual high values. 
str(df_clean)
glimpse(df_clean)

#Ploting the points for the failure rates Vs failure types 
ggplot(df_clean, aes(x = as.factor(failure_type), y = failure_rate)) +
  geom_jitter(alpha = 0.2) 

# From the summary statistics and the jitter plot It can be seen that the 
# data is having high failure_rate for some failure_types and for most of the type
# it is centering around 0. 
 #analyzing the failure rate group wise

########### Failure Type Group wise Summary Statistics ############################### 
df_clean_group_summary <- df_clean %>%
                            group_by(failure_type) %>%
                            summarise(mean = mean(failure_rate),
                                      median = median(failure_rate),
                                      sd = sd(failure_rate),
                                      min = min(failure_rate),
                                      max = max(failure_rate))
View(df_clean_group_summary)

### comparing the scaled values within group is advisible as the range of the data in some 
### types is too high and for some it is too low. 

######### scaling the failure_rate within each failure_type ######################## 
df_clean$failure_scaled_by_type <- scale_by(failure_rate ~ failure_type, df_clean)
View(df_clean)      

# Generaly clustering methodes are used in order to find the similarities between the
# the data point. Hirarchical clustering and k-means clustering. The earlier one required to built
# the distance matrix which requires high memory for processing and is beyond the scope of this
# machine. Moreover, k-means can be used and usually gives the good result. 


######## Employing k-means clustering algorithm on the scaled data ############
# clust_df <- df_clean[c(2,4)]
set.seed(2)

km.out <- kmeans(df_clean[c(2,4)], centers = 6)

clust_km6 <- km.out$cluster

clust_df <- mutate(df_clean, group = clust_km6)

ggplot(clust_df, aes(x = failure_type, y = failure_rate, color = factor(group)))+
    geom_jitter(alpha = 0.5)

write.csv(clust_df[c(1,2,3,5)],'Data.csv') 

###########################################################################################
# non_zero_df <- df_clean %>% filter(failure_rate != 0)
# dim(non_zero_df)
# 
# View(non_zero_df)
# 
# non_zero_df_grouped <- non_zero_df %>% group_by(failure_type) %>% summarise(count = n())
# 
# View(non_zero_df_grouped)
# 
# non_zero_df$failure_scaled_by_type <- scale_by(failure_rate ~ failure_type, non_zero_df)
# 
# ########################################################################################
# 
# set.seed(142)
# 
# km.out <- kmeans(non_zero_df[c(2,4)], centers = 6)
# 
# clust_km6 <- km.out$cluster
# 
# clust_df <- mutate(non_zero_df, group = clust_km6)
# 
# ggplot(clust_df, aes(x = failure_type, y = failure_rate, color = factor(group)))+
#   geom_jitter(alpha = 0.5)
# 
