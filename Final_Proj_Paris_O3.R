#########################################################################################
############################ Paris Ozone Pollution Project ##############################
#########################################################################################

# Using the data collected at the Eiffel Tower station, we are going to model ozone (O3)
#concentration using the other collected data, in an effort to predict 03 concentrations
#when the captor is down but the other pollutant captors are working


# In order to run this project, the following packages are required:
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")

#######################################################################################
# Data Set - downloaded on May 24nd from (and afterwards imported to github repository:
#https://www.airparif.asso.fr/telechargement/telechargement-station
#######################################################################################


# downloading the raw data set from the github repository and saving as "Paris_data"
url <- "https://github.com/fgoujard/Paris_Air/raw/master/19991008_20200524-EIFF3_auto.csv"
#read CSV file
dat <- read_csv2(url) #using csv2 because data is separated by ;"
download.file(url, "19991008_20200524-EIFF3_auto.csv")
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
Paris_data <- read_csv2(tmp_filename)
rm(tmp_filename) #deleting temporaty files

head(Paris_data) # confirming the format and that everything looks good


# Further cleaning and formating of the Data
Paris_data <- Paris_data[-1,] # taking out the row with the units
colnames(Paris_data)<- c("Date","Hour","SO2","NO2", "O3") #translating column names from French to English
Paris_data <- Paris_data %>% mutate(Date = dmy(Date), SO2 = as.numeric(SO2), 
                                    NO2 = as.numeric(NO2), O3 = as.numeric(O3)) #formating data classes



# Exploring and Visualization of the Data

is.na(Paris_data) %>% summary() #first look at how much of the data is missing
#not too bad for O3 nor for NO2 (a percursor to O3 formation), but around 70% of
#SO2 measurements are missing

#SO2 measures for the entire data set
Paris_data %>% 
  ggplot(aes(Date, SO2, color = SO2)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  labs(x="Date", y = "SO2 in microgrammes/m3",
       title = "SO2 concentration measures")

#NO2 measures for the entire data set
Paris_data %>% 
  ggplot(aes(Date, NO2, color = NO2)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  labs(x="Date", y = "NO2 in microgrammes/m3",
       title = "NO2 concentration measures")

#O3 measures for the entire data set
Paris_data %>% 
  group_by(Date) %>%
  ggplot(aes(Date, O3, color = O3)) +
  geom_point() +
  geom_smooth() +
  geom_hline(aes(yintercept=120)) + #corresponding to the maximum concentration for good air quality
  geom_hline(aes(yintercept=180)) + #corresponding to the first alert level concentration
  theme_classic() +
  labs(x="Date", y = "O3 in microgrammes/m3",
       title = "O3 concentration measures")

# Maximum ever measured during the timeframe
Paris_data[which.max(Paris_data$O3),] 

# Visualizing by month - Seasonality (Sunlight is required for O3 formation)
Paris_data %>%
  group_by(Date) %>%
  ggplot(aes(month(Date), O3, color = O3)) +
  geom_point() + 
  theme_classic() +
  labs(x="Month", y = "O3 in microgrammes/m3",
       title = "03 concentration measures - Monthly profile")
#summer highs (sunny days), winter lows

# Visualizing by hour - Afternoon gets slightly higher 03 concentration (Sunlight is required for O3 formation)
Paris_data %>%
  ggplot(aes(Hour, O3, color = O3)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  labs(x="Hour", y = "O3 in microgrammes/m3",
       title = "03 concentration measures - Hourly profile")


#using daily averages to get a better vue os the evolution through time
Paris_data %>%
  group_by(Date) %>%
  mutate(D_NO2 = mean(NO2)) %>%
  ggplot(aes(Date, D_NO2, color = D_NO2)) +
  geom_point() + 
  geom_smooth() +
  theme_classic() +
  labs(x="Date", y = "Daily mean NO2 in microgrammes/m3",
       title = "Daily mean NO2 concentration variation with time")
#NO2 pollution is slowly decreasing with time, showing that traffic
#control measurements implemented through the years are effective

Paris_data %>%
  group_by(Date) %>%
  mutate(D_O3 = mean(O3)) %>%
  ggplot(aes(Date, D_O3, color = D_O3)) +
  geom_point() + 
  geom_smooth() +
  theme_classic() +
  labs(x="Date", y = "Daily mean O3 in microgrammes/m3",
       title = "Daily mean O3 concentration variation with time")
#Slightly increase for O3 in the last few years - more sunlight? The last 
# few years have been particularly hot (sunny) which is a factor for O3 formation

# Slimming dow the data set, to 2017 onwards, so that there are no changes
# due to new environmental policies in place - dataset "Paris"
Paris <- Paris_data %>% 
  filter(year(Date) >="2017") %>% 
  select(Date, Hour, NO2, O3) %>% #removing SO2 which is not a percursor of O3 formation
  na.omit() #removing all NAs for modelling later
dim(Paris) #still a rather large dataset due to the hourly measurements
head(Paris) # checking everything is OK
Paris[which.max(Paris$O3),] # the dataset includes alert level points

# Visualizing the Paris data set for O3
Paris %>%
  group_by(Date) %>%
  ggplot(aes(Date, O3, color = O3)) +
  geom_point() + 
  geom_smooth() +
  theme_classic() +
  labs(x="Date", y = "O3 in microgrammes/m3",
       title = "O3 concentration variation from 2017 untill today")
#extremely clear the seasonality effect (sunlight)

Paris %>%
  group_by(Date) %>%
  ggplot(aes(Date, NO2, color = NO2)) +
  geom_point() + 
  geom_smooth() +
  theme_classic() +
  labs(x="Date", y = "NO2 in microgrammes/m3",
       title = "NO2 concentration variation from 2017 untill today")
# the seasonality effect is reversed for NO2 - low at summer time, 
#when there is a major decrease of traffic in Paris

#visualization of the pollutants concentration using smooth
Paris %>% 
  filter(year(Date) == "2018") %>%
  ggplot() +
  geom_smooth(aes(Date,NO2, colour = "NO2")) +
  geom_smooth(aes(Date, O3, colour = "O3")) +
  theme_classic() +
  labs(x="Date", y = "Pollutant in microgrammes/m3",
       title = "Smoothed NO2 & O3 measures for 2018")
#doesn't seem to exist a linear correlation between just these 2 pollutants
Paris %>% 
  ggplot(aes(NO2,O3, color = year(Date))) +
  geom_point() +
  theme_classic() +
  labs(x="NO2 (microgrammes/m3)", y = "O3 (microgrammes/m3)",
       title = "NO2 & O3 measures for 2017-2020")

###################################################################################
##################################### Models ######################################
###################################################################################

#create validation + training  set (using the caret package)
## Validation set will be 20% of data (data set = Paris)
set.seed(7, sample.kind="Rounding") # for reproductibility
test_index <- createDataPartition(y = Paris$O3, times = 1, p = 0.2, list = FALSE)
train_set <- Paris[-test_index,]
validation <- Paris[test_index,]

dim(train_set)
dim(validation)

###################################################################################

### Model Training ###

set.seed(7, sample.kind="Rounding") # for reproductibility
ctrl <- trainControl(method="repeatedcv",repeats = 3, summaryFunction = defaultSummary) 
#Repeated (3 times)  10-fold Cross Validation for all models

#Linear Model
lmFit <- train(O3 ~ ., #all predictors can be used for the model (nd for all models trained)
               data = train_set, 
               method = "lm", 
               trControl=ctrl, 
               preProc = c("center", "scale")) #Preprocessing of the data (which is very 
                                               #dispersed at times) by centering and
                                               #scalling

#K-Nearest Neighbors
knnFit <- train(O3 ~ ., 
                data = train_set, 
                method = "knn", 
                trControl=ctrl, 
                tuneGrid = data.frame(k = seq(15, 25, 1)), #letting the model automatically tune k
                preProc = c("center", "scale"))

#Kernel Partial Least Squares
kplsFit <- train(O3 ~ ., 
                 data = train_set, 
                 method = "kernelpls", 
                 trControl=ctrl, 
                 preProc = c("center", "scale"))

#Random Forest
rfFit <- train(O3 ~ ., 
               data = train_set, 
               method = "rf", 
               trControl=ctrl, 
               preProc = c("center", "scale"))

#Bayesian Model Regularized Neural Network
brnnFit <- train(O3 ~ ., 
                 data = train_set, 
                 method = "brnn", 
                 trControl=ctrl, 
                 preProc = c("center", "scale"))

#Generalized Addictive Model using LOESS
gLoessFit <- train(O3 ~ ., 
                   data = train_set, 
                   method = "gamLoess", 
                   trControl=ctrl, 
                   preProc = c("center", "scale"))



### Comparison of the models performance ###

results <- resamples(list(LM=lmFit, KNN=knnFit, KPLS=kplsFit, GAML = gLoessFit, 
                          BRNN=brnnFit, RF=rfFit))

summary(results) 
#using R-squared as metric for best model - Random Forest is best

# whisker plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
dotplot(results, scales=scales)

#getting a better look at K-NN model, to see if the range specified for k tuning was 
#good

plot(knnFit)
knnFit$results 
knnFit$bestTune 
#seems good but not good enough performance compared to Random Forest

#Looking at the Random Forest Model in more detail:
rfFit$finalModel #A R-squared of 0.852 and 86.08% of Var explained is very satisfactory
rfFit$results 

ggplot(varImp(rfFit)) # we can see here what data visualization already showed us, date
                      # (inderectly sunlight) is important, as well as NO2 (percursor)


rf_res = resid(rfFit) # summer time is hardest to model

ggplot() +
  geom_point(aes(train_set$Date, rf_res), col = "#009999") +
  geom_hline(aes(yintercept=0)) +
  labs(x="Date", y = "Residuals",
       title = "O3 - residuals - rfFit model")



### Testing the RF Model with the validation set ###

y_hat <- predict(rfFit, validation) #predicted O3

#a nice correlation can be seen between the predicted and the real O3 measurements
validation %>% 
  ggplot() +
  geom_point(aes(O3, y_hat), col = "#009999") +
  scale_x_continuous(limits = c(0, 200)) + 
  scale_y_continuous(limits = c(0, 200)) +
  labs(x="O3 measurements (microgrammes/m3)", y = "y_hat (predicted O3) (microgrammes/m3)",
       title = " RF Model predicted vs O3 measures for the validation set")

#performance results based on validation set
res_rf <- as.data.frame(validation$O3) %>% 
  add_column(as.data.frame(y_hat))

colnames(res_rf) <- c("obs", "pred")

defaultSummary(res_rf) # very good R-squared and coherent with the results obtained during
                       #model training - 0.86

#visualization for the validation set between real measurements and estimated ones (lighter green)
ggplot() +
  geom_point(aes(validation$Date, validation$O3), col = "#003333") +
  geom_point(aes(validation$Date, y_hat), col = "#009999") +
  theme_classic() +
  labs(x="Date", y = "Predicted O3 & O3 measured (microgrammes/m3)",
       title = " RF Model predicted & O3 measured for the validation set")

########################################################################################
### Applying the model for predictions ###

### Predicting missing values with our model ###

#creating a new dataset that ontains all points were the NO2 concentration is available 
#to input our model, and the O3 is missing for an unknown reason
miss_O3 <- Paris_data %>% 
  filter(is.na(O3) & !is.na(NO2) & year(Date) >= "2017") %>% #same time perimeter than model
  select(Date, Hour, NO2)

dim(miss_O3) #1638 times O3 measures where missing during this time frame
head(miss_O3)

#predicting the missing  (y_miss)
y_miss <- predict(rfFit, miss_O3)

max(y_miss) #checking to see of there was any alarm level concentration predicted

miss_O3[which.max(y_miss),] 

pred_miss <- miss_O3 %>% add_column(as.data.frame(y_miss))

#visualization of all data using the predicted y_miss for the missing 03 measurements
#and when NO2 measurements were available
ggplot() +
  geom_point(aes(Paris$Date, Paris$O3), col = "#999999") +
  geom_point(aes(pred_miss$Date, pred_miss$y_miss), col = "#009999") +
  theme_classic() +
  labs(x="Date", y = "O3 (measured & missing predicted) (microgrammes/m3)",
       title = " RF Model missing predicted O3 & measured")





