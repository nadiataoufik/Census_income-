# DATA ACQUISATION
census_income=read.csv("C:/Users/nadiataoufik/Documents/RPROGRAM/census-income.csv")
View(census_income)
#Convert factor columns to character
census_income$X
as.character(census_income$workclass)->workclass
as.character(census_income$education)->education
as.character(census_income$marital.status)->marital.status
as.character(census_income$occupation)->occupation
as.character(census_income$relationship)->relationship
as.character(census_income$race)->race
as.character(census_income$sex)->sex
as.character(census_income$native.country)->native.country
as.character(census_income$X)->X
#1.data processing 
#A. replace all the missing values with NA

census_income[census_income == " ?"] <- NA
#B.REMOVE ALL THE ROWS THAT CONTAIN NA VALUES 

na.omit(census_income)->census_income

sum(is.na(census_income))

#C. REMOVE ALL WHITESPACES FROM THE COLUMNS 
install.packages("dplyr")
library(dplyr)
library(stringr)
census_income <- census_income %>% mutate_if(is.character, str_trim) 
View(census_income)

#2. data manipulation 
census_income$capital.gain
census_income%>%select(education)->census_ed
census_income%>%select(age:relationship)->census_seq
census_income%>%select(c(5,8,11))->census_col
#extracting all the males with state gov workclass
male_gov <-  census_income %>% filter(sex == "Male" & workclass == "State-gov")
View(male_gov)
# extract all the 39 years old who either have bachlors or are native of usa 

census_income %>% filter(age == 39 & (education == "Bachelors" | native.country == "United-States")) -> census_us
View(census_us) 
# extract 200 random rowms from census and store it n census_200
census_income%>%sample_n(200)->census_200
View(census_200)
#get the count of different levels of the workclass column 
census_income%>%count(workclass)
# get the mean of capital.gain according to workclass 

census_income%>%group_by(workclass)%>%summarise(mean(capital.gain))

# 3.DATA VISUALIZATION 
install.packages("ggplot2")
library("ggplot2")
census_income$hours.per.week
ggplot(census_income,aes(x=relationship,fill=race))+geom_bar()
ggplot(census_income,aes(x=relationship,fill=race))+geom_bar()+labs(x="categories of relationships",y="count of categories")
ggplot(census_income,aes(x=relationship,fill=sex))+geom_bar()+labs(x="categories of relationships",y="count of categories")
ggplot(census_income,aes(x=relationship,fill=sex))+geom_bar(position = "dodge")+labs(x="categories of relationships",y="count of categories")
ggplot(census_income,aes(x=relationship,fill=sex))+geom_bar(position = "dodge")+labs(x="categories of relationships",y="count of categoris",title = "distribution of relationships")

#B. build a histogram of the age with number of bins =50
#i
ggplot(census_income,aes(x=age))+geom_histogram(bins = 50)
ggplot(census_income,aes(x=age,fill=X))+geom_histogram(bins = 50)
ggplot(census_income,aes(x=age,fill=X))+geom_histogram(bins = 50)+labs(title = "Distribution of age")
ggplot(census_income,aes(x=age,fill=X))+geom_histogram(bins = 50)+labs(title = "distribution of age",fill="yearly income")
ggplot(census_income,aes(x=age,fill=X))+geom_histogram(bins = 50)+labs(title = "distribution of age",fill="yearly income")+theme_bw()

#C. scatter_plot 
ggplot(census_income,aes(x=capital.gain,y=hours.per.week))+geom_point()
ggplot(census_income,aes(x=capital.gain,y=hours.per.week))+geom_point(alpha=0.4,size=2)
ggplot(census_income,aes(x=capital.gain,y=hours.per.week))+geom_point(alpha=0.4,size=2)+labs(title = "capital gain vs hours per week by income","yearly income")

#D. Box plot 

ggplot(census_income,aes(x=education,y=age))+geom_boxplot()
ggplot(census_income,aes(x=education,y=age,fill=sex))+geom_boxplot()
ggplot(census_income,aes(x=education,y=age,fill=sex))+geom_boxplot()+labs(title = "Box_plot of age by education and sex")


#4. Linear regression 
library(caTools)
set.seed(200)
census_income$occupation
sample.split(census_income$hours.per.week,SplitRatio = 0.7)->split_data
subset(census_income,split_data==T)->train
subset(census_income,split_data==F)->test
nrow(test)
nrow(train)
lm(hours.per.week~education.num,data = train)->model
predict(model,newdata = test)->predicted
cbind(actual=test$hours.per.week,predicted)->final_data
class(final_data)
as.data.frame(final_data)->final_data
class(final_data)
final_data$actual-final_data$predicted->error
cbind(final_data,error)->final_data
sqrt(mean((final_data$error)^2))->RMSE
RMSE
plot(census_income$education.num,census_income$hours.per.week)
abline(model)

#4 Logistic Regression 
#a 

sample.split(census_income$X, SplitRatio = 0.65)->split_tag
subset(census_income,split_tag==T)->training
subset(census_income,split_tag==F)->testing
glm(X~occupation,data=training,family = "binomial")->log_model
predict(log_model,newdata = testing,type="response")->predicted_values
library(ROCR)
table(actual=testing$X,predicted=predicted_values>0.4)
(6486+1324)/(6486+1304+1443+1324)
predict(log_model,training,type="response")->res
prediction(res,training$X)->ROCRREF
performance(ROCRREF,"tpr","fpr")->ROCRPER
plot(ROCRPER)

lm.pred <- ifelse(predicted_values > 0.47, ">50K", "<=50K")
install.packages("caret")
library("caret")
install.packages("lattice")
library("lattice")
confusionMatrix(factor(lm.pred), testing$X)
table(lm.pred,testing$X) -> tab

tab

sum(diag(tab))/ sum(tab)


########### Multiple Logistic Regtressuin 

library(caTools)
set.seed(222)
sample.split(census_income$X,SplitRatio = 0.7)->tag

subset(census_income,tag==T)->train_data
subset(census_income,tag==F)->test_data

glm(X~age+workclass+education,data=train_data,family = "binomial")->log_multi

predict(log_multi,newdata = test_data,type="response")->predicted_log


head(predicted_log)

range(predicted_log)
install.packages("ggplots")
library(ROCR)

prediction(predicted_log,test_data$X)->predict_log_roc

performance(predict_log_roc,"acc")->acc

plot(acc)
lm.pred <- ifelse(predicted_log > 0.45, ">50K", "<=50K")


table(lm.pred,test_data$X) -> tab

tab
sum(diag(tab))/ sum(tab)

class(lm.pred)

confusionMatrix(factor(lm.pred),test_data$X)

performance(predict_log_roc, "tpr", "fpr") -> auc

plot(auc)

abline(a = 0, b = 1)

auc_ROCR <- performance(predict_log_roc,"auc")

auc_ROCR <- auc_ROCR@y.values[[1]]

auc_ROCR




#6. Decision Tree :
library(caTools)
sample.split(census_income$X,SplitRatio = 0.7)->split_X
subset(census_income,split_X==T)->training_data
subset(census_income,split_X==F)->testing_data        
library(rpart)
install.packages("rpart.plot")
rpart(X~.,data = training_data,method = "class")->tree_model
plot(tree_model)
text(tree_model,use.n = T,pretty = T,cex=0.8)

predict(tree_model,newdata = testing_data ,type = "class")->predicted_X
library("caret")
install.packages("caret")
table(actual=testing_data$X,predicted=predicted_X)
confusionMatrix(table(predicted_X,testing_data$X))
install.packages("e1071")



#7. Random Forest
library(caTools)

set.seed(1)


sample.split(census_income$X,SplitRatio = 0.8)->split_RF
subset(census_income,split_RF==T)->train
subset(census_income,split_RF==F)->test

install.packages("randomForest")
library(randomForest)

randomForest(X~.,data = train,ntree=300)->model_RF

predict(model_RF,newdata = test,type = "class")->predicted_RF

table(actual=test$X,predicted=predicted_RF)
confusionMatrix(table(predicted_RF,test$X))
