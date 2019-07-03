rm(list=ls())

#set the working directory
setwd("C:/Users/Chaitrali/Downloads/Data/data01s2l1/Edwisor-Project/Bike_data")

#Load the required libraries
x=c("ggplot2", "DMwR", "corrgram", "Hmisc", "rpart", "randomForest")
lapply(x, require, character.only = TRUE)
rm(x)

#load  the data
Rental_train=read.csv("day.csv" , header= T)[,-1]
str(Rental_train)

#convert into required data type

fac= 1:8
nu = 9:15
Rental_train[,fac]= lapply (Rental_train[, fac], as.factor)
Rental_train[,nu]= lapply (Rental_train[, nu], as.numeric)

str(Rental_train)


#Missing value Analysis
missing_val = data.frame(apply(Rental_train,2,function(x){sum(is.na(x))}))
rm(missing_val)
#no Missing value present

############################################Outlier Analysis#############################################
# ## BoxPlots - Distribution and Outlier Check
numeric_data = Rental_train[,nu]
#excluding the output quantities
cnames = colnames(numeric_data)[1:4]
cnames


for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = Rental_train)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="y")+
           ggtitle(paste("Box plot of rental count for",cnames[i])))
}


## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3, gn4,ncol=2)

# #Remove outliers using boxplot method
df = Rental_train
Rental_train = df


#Replace all outliers with NA and impute
#create NA on outliers
for(i in cnames){
  val = Rental_train[,i][Rental_train[,i] %in% boxplot.stats(Rental_train[,i])$out]
  print(length(val))
  print(val)
  Rental_train[,i][Rental_train[,i] %in% val] = NA
}
#.805833

# Create a dummy NA and do trial
Rental_train[1,11]=NA
missing_val = data.frame(apply(Rental_train,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(Rental_train)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
# #
ggplot(data = missing_val[1:3,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
ggtitle("Missing data percentage (Train)") + theme_bw()

#Check which method is suitable for missing value imputation
#Mean Method
Rental_train$hum[is.na(Rental_train$hum)] = mean(Rental_train$hum, na.rm = T)
Rental_train[1,11]=NA

#Median Method
Rental_train$hum[is.na(Rental_train$hum)] = median(Rental_train$hum, na.rm = T)
Rental_train[1,11]
Rental_train[1,11]=NA
#
# kNN Imputation
Rental_train = knnImputation(Rental_train, k = 5)
# 
# #Mean=0.6276503
# #Median=0.62625
# #KNN=0.6266684

#since mean gives closest value, choose Mean method for imputation
#Put original value 
Rental_train[1,11]=0.805833

sum(is.na(Rental_train))
Rental_train$hum[is.na(Rental_train$hum)] = mean(Rental_train$hum, na.rm = T)
Rental_train$windspeed[is.na(Rental_train$windspeed)] = mean(Rental_train$windspeed, na.rm = T)

rm(i, gn1,gn2,gn3,gn4, val)
##########################3Feature Selection############################
  
## Correlation Plot 
corrgram(Rental_train[,nu], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Finding the correlation between the numeric variables
num_cor=round(cor(numeric_data), 3)


# ## Chi-squared Test of Independence
factor_data = Rental_train[,fac]
colnames(factor_data)

for (i in 1:8){
  for (p in 1:8){
    print(names(factor_data)[i])
    print(names(factor_data)[p])
    print(chisq.test(table(factor_data[,p],factor_data[,i]), p-value))
    }
  }

#to check the degree of association of categorical variable
library(GoodmanKruskal)
plot(GKtauDataframe( factor_data), corrColors = 'blue')


#Since cnt is the sum of casual and registered users we drop them too

reduced_train = subset(Rental_train,
                       select = -c(casual, registered, dteday))

####################################Model Development##########################
set.seed(123)
train_index = sample(1:nrow(Rental_train), 0.8 * nrow(Rental_train))
colnames(Rental_train)

train = Rental_train[train_index,-c(1,13,14)]#do not add column if already removed
test = Rental_train[-train_index,-c(1,13,14)]#do not add column if already removed

# Define MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y*100))
}

#Decision Tree
fit = rpart(cnt  ~. , data = train, method = "anova")
predictions_DT = predict(fit, test[,-12])
MAPE(test[,12], predictions_DT)
write.csv(predictions_DT, "DT_R_PRed.csv", row.names = F)
#Error 22.54
#Accuracy 77.46

#Random Forest
RF_model = randomForest(cnt ~.  , train, importance = TRUE, ntree = 300)
RF_Predictions = predict(RF_model, test[,-12])
MAPE(test[,12], RF_Predictions)
write.csv(RF_Predictions, "RF_r_pred.csv", row.names = F)
importance(RF_model, type = 1)
#error 16.47 for n=200
#Accuracy 83.53

#Linear Regression
lm_model = lm(cnt ~. , data = train)
summary(lm_model)
predictions_LR = predict(lm_model, test[,1:11])
MAPE(test[,12], predictions_LR)
write.csv(predictions_LR, "LR_R_pred.csv", row.names = F)
#error 20.88
#accuracy 79.12


##KNN Implementation
library(class)
#Predict test data
KNN_Predictions = knn(train[, 1:11], test[, 1:11], train$cnt, k = 1)
KNN_Predictions=as.numeric(as.character((KNN_Predictions)))
#Calculate MAPE
MAPE(test[,12], KNN_Predictions)
write.csv(KNN_Predictions, "KNN_r_pred.csv", row.names = F)
#error 22.14
#accuracy 


#Gaussian
poi= glm(formula = cnt~. , family = "gaussian" , train )
poi_pre = predict(poi, test[, 1:11])
MAPE(test[,12], poi_pre)
write.csv(poi_pre, "gaussian_r_pred.csv", row.names = F)
#Error 20.88