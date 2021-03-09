# %% [markdown]
# **Project title** :- ****Bike Renting using R****

# %% [markdown]
# **Problem statement :-**
# 
#    The objective of this Case is to Predication of bike rental count on daily based on the
#    environmental and seasonal settings.

# %% [markdown]
# **Exploratery Data Analysis**

# %% [markdown]
# **Importing the metapackage library**

# %% [code] {"_execution_state":"idle"}
#import tidyverse metapackage library
install.packages(c("dplyr", "plyr", "reshape", "ggplot2", "data.table"))
install.packages("dmm")
library(tidyverse) 

list.files(path = "../input")



# %% [markdown]
# **Importing the Data**

# %% [code]
#Importing the csv file
bike_day <-read.csv("../input/day.csv")
head(bike_day,5)

# %% [code]
tail(bike_day,5)

# %% [markdown]
# Remove the casual and registered variable due to total_count is combined of both.
# 

# %% [code]
#Create new dataset excluding casual and registered variables
bike_day <- subset(bike_day,select=-c(casual,registered))
head(bike_day,5)


# %% [markdown]
# **Dimension of Dataset**

# %% [code]
#Dimension of dataset
dim(bike_day)

# %% [markdown]
# **Summary of the Dataset**

# %% [code]
#Summary of the dataset
summary(bike_day)

# %% [markdown]
# **Structure of Dataset**

# %% [code]
#Structure of dataset
str(bike_day)

# %% [markdown]
# Get column names

# %% [code]
colnames(bike_day)

# %% [markdown]
# **Typecasting the datetime and numerical attributes**

# %% [code]
#Typecasting the datetime and numerical attributes to category

bike_day$dteday <- as.Date(bike_day$dteday)
bike_day$yr <- as.factor(bike_day$yr)
bike_day$mnth <- as.factor(bike_day$mnth)
bike_day$season <- as.factor(bike_day$season)
bike_day$holiday <- as.factor(bike_day$holiday)
bike_day$weekday <- as.factor(bike_day$weekday)
bike_day$workingday <- as.factor(bike_day$workingday)
bike_day$weathersit <- as.factor(bike_day$weathersit)

# %% [markdown]
# Check all the data types, if correctly changed

# %% [code]
str(bike_day)

# %% [markdown]
# **Missing value analysis**

# %% [code]
#Missing values in dataset
miss_val <- data.frame(apply(bike_day,2,function(x){sum(is.na(x))}))
names(miss_val)[1]='missing_val'
miss_val

# %% [markdown]
# Outlier Analysis
# Box plot and analysis

# %% [code]
numeric_index = sapply(bike_day,is.numeric) #selecting only numeric

numeric_data = bike_day[,numeric_index]

cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(bike_day))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "blue" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot of count for",cnames[i])))
}



# %% [code]
cnames

# %% [code]
gridExtra::grid.arrange(gn1,gn2,ncol=3)

# %% [code]
gridExtra::grid.arrange(gn3,gn4,ncol=3)

# %% [code]
gridExtra::grid.arrange(gn5,ncol=2) # it can be seen that hum and windspeed variable both have outliers.


# %% [code]
#load the DMwR library
library(DMwR)
#create subset for windspeed and humidity variable
wind_hum <- subset(bike_day,select = c('windspeed','hum'))
#column names of wind_hum
cnames <- colnames(wind_hum)
for(i in cnames){
  val = wind_hum[,i][wind_hum[,i] %in% boxplot.stats(wind_hum[,i])$out] #outlier values
  wind_hum[,i][wind_hum[,i] %in% val]= NA  # Replace outliers with NA 
}
#Imputating the missing values using mean imputation method
wind_hum$windspeed[is.na(wind_hum$windspeed)] <- mean(wind_hum$windspeed,na.rm=T) 
wind_hum$hum[is.na(wind_hum$hum)] <- mean(wind_hum$hum,na.rm=T)

# %% [code]
#Remove the windspeed and hum variable in order to replace imputated data
new_day <- subset(bike_day,select = -c(windspeed,hum))
#Combined new_day and wind_hum data frames
bike_day <- cbind(new_day,wind_hum)
head(bike_day,5)

# %% [code]
#Missing values in dataset
miss_val <- data.frame(apply(bike_day,2,function(x){sum(is.na(x))}))
names(miss_val)[1]='missing_val'
miss_val

# %% [code]
# correlation matrix to determine relationship between continuous variables.
library(corrgram)
corrgram(bike_day[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

# %% [markdown]
# It can be observed that temp and atemp are highly correlated so we will ignore atemp variable for further analysis.

# %% [code]
#dimension reduction.
bike_day = subset(bike_day,select = -c(atemp))

# %% [code]
dim(bike_day) # we have dropped a temp variable

# %% [code]
library(rpart)
library(MASS)

#divide the data into train and test data.
train_index = sample(1:nrow(bike_day), 0.8 * nrow(bike_day))
train = bike_day[train_index,]
test = bike_day[-train_index,]

# %% [code]
# applying decision tree regression model
fit = rpart(cnt ~ ., data = train, method = "anova")

# %% [code]
prediction_DT = predict(fit, test[,-11])

# %% [code]
prediction_DT

# %% [code]
#define mape function : MAPE
mape = function(y, yhat){mean(abs((y-yhat)/y))*100}
mape(test[,11], prediction_DT)

# %% [code]
str(bike_day)

# %% [code]
cnames= c("dteday","season","mnth","weekday","weathersit")
bike_enco=bike_day[,cnames]
cnt=data.frame(bike_day$cnt)
names(cnt)[1]="cnt"
bike_enco <- fastDummies::dummy_cols(bike_enco)
bike_enco= subset(bike_enco,select = -c(dteday,season,mnth,weekday,weathersit))
d3 = cbind(bike_enco,bike_day)
d3= subset(d3,select = -c(dteday,season,mnth,weekday,weathersit,cnt))
bike_enco=cbind(d3,cnt)


# %% [code]
#divide encoded data into  test sets
#divide the data into train and test data.
encoded_index = sample(1:nrow(bike_enco), 0.8 * nrow(bike_enco))
train_enco = bike_enco[encoded_index,]
test_enco = bike_enco[-encoded_index,]
dim(test_enco)

# %% [code]
library(randomForest)
RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 200)

predictions_RF = predict(RF_model, test[,-11])
plot(RF_model)


# %% [code]
mape(test[,11], predictions_RF)

# %% [code]
test_enco

# %% [code]
#linear regression model
library(usdm)
lm_model = lm(cnt ~ ., data = train_enco)
summary(lm_model)

# %% [code]
prediction_Lr = predict(lm_model, test_enco[,-34])

# %% [code]
prediction_Lr

# %% [code]
mape(test_enco[,34], prediction_Lr)

# %% [code]
#from the mape function we can see that, of all the models Random Forest has the least abosulute error. So we choose Random Forest model as the best model for the given data.
test_input = test$cnt
Bike_rental_predict = data.frame(test_input, predictions_RF)
write.csv(Bike_rental_predict,'Bike_Renting_R.CSV',row.names=F)
Bike_rental_predict

# %% [code]
