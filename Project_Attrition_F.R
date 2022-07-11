#Har Har Mahadev

# Summary:
#   Dataset Structure: 1470 observations (rows), 35 features (variables)
# Missing Data: Luckily for us, there is no missing data! this will make 
# it easier to work with the dataset. Data Type: We only have two datatypes 
# in this dataset: factors and integers Label
# " Attrition is the label in our dataset and we would like to find out why 
# employees are leaving the organization!
# Imbalanced dataset: 1237 (84% of cases) employees did not leave the organization while
# 237 (16% of cases) did leave the organization making our dataset to be considered 
# imbalanced since more people stay in the organization than they actually leave.


#----Set Working Directory
setwd("D:/Capstone Projects/R")
getwd()

#----Library----
library(dplyr)
library(psych)
library(tidyverse)
library(ggplot2)
library(gplots)
library(superheat)
library(corrplot)
library(readr)
library(plotrix)
library(ggcorrplot)
library(purrr)
library(moments)
library(psych)
library(gridExtra)
library(DMwR)
library(car)
library(caret)
library(ResourceSelection)
library(pROC)
library(PRROC)
library(ROCR)
library(plotROC)
library(randomForest)
library(ISLR)
library(caTools)
library(tree)
library(rpart)

#----Importing data----
data1 = read.csv("HR-Employee-Attrition.csv", stringsAsFactors = T)
View(data1)
str(data1)
head(data1,5)
# Comment - Out of the 35 variables we have 34 independent variables 
#and one dependent/target variable which is Attrition
#----NA value check----
apply(is.na(data1), 2, sum)

#----Descriptive analysis----
summary(data1)
describe(data1)

#----Data Wrangling and cleaning
data1 <- data1 %>%
  mutate(Education = as.factor(if_else(Education == 1,"Below College", if_else(Education == 2, "College", if_else(Education == 3, "Bachelor", if_else(Education == 4, "Master","Doctor")))))
         ,EnvironmentSatisfaction = as.factor(if_else(EnvironmentSatisfaction == 1,"Low",if_else(EnvironmentSatisfaction == 2, "Medium", if_else(EnvironmentSatisfaction == 3, "High", "Very High"))))
         ,JobInvolvement = as.factor(if_else(JobInvolvement == 1,"Low",if_else(JobInvolvement == 2, "Medium",if_else(JobInvolvement == 3, "High", "Very High"))))
         ,JobSatisfaction = as.factor(if_else(JobSatisfaction == 1, "Low",if_else(JobSatisfaction == 2, "Medium",if_else(JobSatisfaction == 3, "High","Very High"))))
         ,PerformanceRating = as.factor(if_else(PerformanceRating == 1, "Low",if_else(PerformanceRating == 2, "Good", if_else(PerformanceRating == 3, "Excellent", "Outstanding"))))
         ,RelationshipSatisfaction = as.factor(if_else(RelationshipSatisfaction == 1, "Low",if_else(RelationshipSatisfaction == 2, "Medium", if_else(RelationshipSatisfaction == 3, "High", "Very High"))))
         ,WorkLifeBalance = as.factor(if_else(WorkLifeBalance == 1, "Bad",if_else(WorkLifeBalance == 2, "Good", if_else(WorkLifeBalance == 3, "Better", "Best")))),
         JobLevel = as.factor(JobLevel)
  )
str(data1)
describe(data1)

#Removing unique values, no contribution in analysis
data2 <-  select(data1, -c("EmployeeCount", "EmployeeNumber", 
                           "Over18", "StandardHours"))
str(data2)

#converting numeric to categorical 
data2$Education <- factor(data2$Education)
data2$EnvironmentSatisfaction <- factor(data2$EnvironmentSatisfaction)
data2$JobInvolvement <- factor(data2$JobInvolvement)
data2$JobLevel <- factor(data2$JobLevel)
data2$JobSatisfaction <- factor(data2$JobSatisfaction)
data2$PerformanceRating <- factor(data2$PerformanceRating)
data2$RelationshipSatisfaction <- factor(data2$RelationshipSatisfaction)
data2$StockOptionLevel <- factor(data2$StockOptionLevel)
data2$WorkLifeBalance <- factor(data2$WorkLifeBalance)

str(data2)

#Percentage of attrition----
d <- as.data.frame(table(data2$Attrition))
d
attrition_rate <- round((d[2,2] / sum(d$Freq))*100, 2)
print(attrition_rate)

data2 %>%
  group_by(Attrition) %>%
  tally() %>%
  ggplot(aes(x = Attrition, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="Attrition", y="Count of Attriation")+
  ggtitle("Attrition")+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))

names(data2)[1] <- "Age"
str(data2)

library(janitor)
library(CGPfunctions)

tabyl(data2, Gender, Attrition) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2)

#Removing variables with no explanation of categories
data3 <-  select(data2, -c("StockOptionLevel", "JobLevel"))

data3$Department <- gsub("Human Resources", "HR", x = data3$Department)
data3$Department <- gsub("Research & Development", "R&D", x = data3$Department)

str(data3)
table(data3$Department)

data_cat <- select_if(data3, is.factor)
str(data_cat)
Attrition <- data_cat[,1]

data_num <- select_if(data3, is.numeric)
str(data_num)

#Skewness Removal

length(data_num)
data_num1 <- data_num
print(paste(colnames(data_num1)," ",skew(data_num1)))


for (i in c(1:length(data_num1))){
  if(skew(data_num1[i]) > 0.75){
    data_num1[[i]] <- log1p(data_num1[[i]])
  }
}
print(paste(colnames(data_num1)," ",skew(data_num1)))

str(data3)
pairs.panels(data3[, c(2,1,3:9)],
             method = "pearson", #coorelation method
             hist.col = "blue",
             main="Correlation of Attrition and other Variables",
             density = TRUE, # show density plots
             ellipses = TRUE, # show correlation ellipses
             lm=TRUE, #linear regression fits 
             cex.cor = 5,
             cex.labels = 0.85
)
pairs.panels(data3[, c(2,10:19)],
             method = "pearson", #coorelation method
             hist.col = "blue",
             main="Correlation of Attrition and other Variables",
             density = TRUE, # show density plots
             ellipses = TRUE, # show correlation ellipses
             lm=TRUE, #linear regression fits 
             cex.cor = 5,
             cex.labels = 0.85
)
pairs.panels(data3[, c(2,20:28)],
             method = "pearson", #coorelation method
             hist.col = "blue",
             main="Correlation of Attrition and other Variables",
             density = TRUE, # show density plots
             ellipses = TRUE, # show correlation ellipses
             lm=TRUE, #linear regression fits 
             cex.cor = 4,
             cex.labels = 0.7
)


library(caret)
prep_num = preProcess(data_num1, method=c("center", "scale"))
data_num2 = predict(prep_num, data_num1)
str(data_num2)

cor_mat<- cor(data_num2)
high_corr <- findCorrelation(cor_mat, cutoff = 0.8)
names(data_num1)[high_corr]

#Removing highly correlated variable
data3 <-  select(data3, -c("YearsAtCompany"))
dim(data3)

#----Outlier Detection----
lower_bound <- quantile(data_num$Attrition, 0.025)
upper_bound <- quantile(data_num$Attrition, 0.975)
outlier_ind <- which(data_num$Attrition < lower_bound | data_num$Attrition > upper_bound)
data_num[outlier_ind,]

str(data_num2)

dev.off()
par(mfrow=c(1,2))
dim(data_num2)
plot.new()
boxplot(data_num2, main = "Box plot of various features", notch = T, col = rainbow(14),
        horizontal = T, las = 2, cex.axis=1, cex.names = 0., boxwe1x = 0.5)

library(outliers)
outlier_scores <- scores(data_num2)
is_outlier <- outlier_scores > 3 | outlier_scores < -3
sum(is_outlier)

# Outlier treatment using IQR
data_num3 <- data_num2

livingar_lower <- (quantile(data_num3$YearsAtCompany, 0.25)) - 1.5*IQR(data_num3$YearsAtCompany)
livingar_upper <- (quantile(data_num3$YearsAtCompany, 0.75)) + 1.5*IQR(data_num3$YearsAtCompany)
data_num3$YearsAtCompany[data_num3$YearsAtCompany < livingar_lower | data_num3$YearsAtCompany > livingar_upper] <- NA
sum(is.na(data_num3$YearsAtCompany))

livingar_lower <- (quantile(data_num3$TrainingTimesLastYear, 0.25)) - 1.5*IQR(data_num3$TrainingTimesLastYear)
livingar_upper <- (quantile(data_num3$TrainingTimesLastYear, 0.75)) + 1.5*IQR(data_num3$TrainingTimesLastYear)
data_num3$TrainingTimesLastYear[data_num3$TrainingTimesLastYear < livingar_lower | data_num3$TrainingTimesLastYear > livingar_upper] <- NA
sum(is.na(data_num3$TrainingTimesLastYear))

livingar_lower <- (quantile(data_num3$TotalWorkingYears, 0.25)) - 1.5*IQR(data_num3$TotalWorkingYears)
livingar_upper <- (quantile(data_num3$TotalWorkingYears, 0.75)) + 1.5*IQR(data_num3$TotalWorkingYears)
data_num3$TotalWorkingYears[data_num3$TotalWorkingYears < livingar_lower | data_num3$TotalWorkingYears > livingar_upper] <- NA
sum(is.na(data_num3$TotalWorkingYears))

sum(is.na(data_num3))
data_num4 <- knnImputation(data = data_num3, k = 0.05*nrow(data_num3))
sum(is.na(data_num4))
data_num4$Attrition <- ifelse(Attrition == "Yes",1,0 )
str(data_num4)


model_num <- glm(Attrition~., family = binomial, data = data_num4)
summary(model_num)

data_num1 <- select(data_num, c("Age", "DistanceFromHome", "MonthlyIncome", 
                                "YearsSinceLastPromotion","NumCompaniesWorked", 
                                "YearsInCurrentRole", "YearsWithCurrManager"))
str(data_num1)

model_cat <- glm(Attrition~., family = binomial,data = data_cat)
summary(model_cat)

data_cat1 <- select(data_cat, c("BusinessTravel", "EnvironmentSatisfaction", 
                                "JobInvolvement", "JobRole", "OverTime", "MaritalStatus",
                                "WorkLifeBalance", "JobSatisfaction")) 
str(data_cat1)



#----Data Visualisation-----
dev.off()
#----Data Visualization foir Numerical Variables----
pn1 <- data3 %>%
  ggplot(aes(x = Age, fill = Attrition)) + 
  geom_density(alpha = 0.5) + ggtitle("Age") + 
  theme(plot.title = element_text(size =10),
        axis.text.x = element_text(size =7,angle = 45, hjust = 1),
        axis.title.x=element_blank())
pn2 <- data3 %>%
  ggplot(aes(x = DistanceFromHome, fill = Attrition)) + 
  geom_density(alpha = 0.5) + ggtitle("Distance From Home")  + 
  theme(plot.title = element_text(size =10),
        axis.text.x = element_text(size =7,angle = 45, hjust = 1),
        axis.title.x=element_blank())
pn3 <- data3 %>%
  ggplot(aes(x = NumCompaniesWorked, fill = Attrition)) + 
  geom_density(alpha = 0.5) + ggtitle("Number of Companies")  + 
  theme(plot.title = element_text(size =10),
        axis.text.x = element_text(size =7,angle = 45, hjust = 1),
        axis.title.x=element_blank())
pn4 <- data3 %>%
  ggplot(aes(x = PercentSalaryHike, fill = Attrition)) + 
  geom_density(alpha = 0.5) + ggtitle("Percentage Salary Hike") + 
  theme(plot.title = element_text(size =10),
        axis.text.x = element_text(size =7,angle = 45, hjust = 1),
        axis.title.x=element_blank())
grid.arrange(pn1, pn2, pn3, pn4, nrow = 2, ncol = 2)

dev.off()
pn5 <- data3 %>%
  ggplot(aes(x = HourlyRate, fill = Attrition)) + 
  geom_density(alpha = 0.5) + ggtitle("Hourly Rate") + 
  theme(plot.title = element_text(size =10),
        axis.text.x = element_text(size =7,angle = 45, hjust = 1),
        axis.title.x=element_blank())
pn6 <- data3 %>%
  ggplot(aes(x = DailyRate, fill = Attrition)) + 
  geom_density(alpha = 0.5) + ggtitle("Daily Rate") + 
  theme(plot.title = element_text(size =10),
        axis.text.x = element_text(size =7,angle = 45, hjust = 1),
        axis.title.x=element_blank())
pn7 <- data3 %>%
  ggplot(aes(x = MonthlyRate, fill = Attrition)) + 
  geom_density(alpha = 0.5)+ ggtitle("Monthly Rate") + 
  theme(plot.title = element_text(size =10),
        axis.text.x = element_text(size =7,angle = 45, hjust = 1),
        axis.title.x=element_blank())
grid.arrange(pn5, pn6, pn7)


pn8 <- data3 %>%
  ggplot(aes(x = MonthlyIncome, fill = Attrition)) + 
  geom_density(alpha = 0.5) + ggtitle("Monthly Income") + 
  theme(plot.title = element_text(size =10),
        axis.text.x = element_text(size =7,angle = 45, hjust = 1),
        axis.title.x=element_blank())
pn9 <- data3 %>%
  ggplot(aes(x = TotalWorkingYears, fill = Attrition)) + 
  geom_density(alpha = 0.5) + ggtitle("Total Working Years")  + 
  theme(plot.title = element_text(size =10),
        axis.text.x = element_text(size =7,angle = 45, hjust = 1),
        axis.title.x=element_blank())
pn10 <- data3 %>%
  ggplot(aes(x = YearsInCurrentRole, fill = Attrition)) + 
  geom_density(alpha = 0.5) + ggtitle("Years in Current Role") + 
  theme(plot.title = element_text(size =10),
        axis.text.x = element_text(size =7,angle = 45, hjust = 1),
        axis.title.x=element_blank())
pn11 <- data3 %>%
  ggplot(aes(x = YearsSinceLastPromotion, fill = Attrition)) + 
  geom_density(alpha = 0.5) + ggtitle("Years Since Last Promotion") + 
  theme(plot.title = element_text(size =10),
        axis.text.x = element_text(size =7,angle = 45, hjust = 1),
        axis.title.x=element_blank())
pn12 <- data3 %>%
  ggplot(aes(x = YearsWithCurrManager, fill = Attrition)) + 
  geom_density(alpha = 0.5) + ggtitle("Years With Current Manager") + 
  theme(plot.title = element_text(size =10),
        axis.text.x = element_text(size =7,angle = 45, hjust = 1),
        axis.title.x=element_blank())
grid.arrange(pn8, pn9, pn10, pn11, pn12 , nrow = 3, ncol = 2)


#----Data Visualization for Categorical Variables----
dev.off()
str(data_cat)
pc1 <- PlotXTabs2(data3, Gender, Attrition, results.subtitle = FALSE)
pc2 <- PlotXTabs2(data3, MaritalStatus, Attrition, results.subtitle = FALSE)
pc3 <- PlotXTabs2(data3, WorkLifeBalance, Attrition, results.subtitle = FALSE)
pc4 <- PlotXTabs2(data3, BusinessTravel, Attrition, results.subtitle = FALSE, x.axis.orientation = "slant")
grid.arrange(pc1, pc2, pc3, pc4, nrow = 2, ncol = 2)

pc5 <- PlotXTabs2(data3, Department, Attrition, results.subtitle = FALSE, x.axis.orientation = "slant")
pc6 <- PlotXTabs2(data3, Education, Attrition, results.subtitle = FALSE, x.axis.orientation = "slant")
pc7 <- PlotXTabs2(data3, EducationField, Attrition, results.subtitle = FALSE, x.axis.orientation = "slant")
grid.arrange(pc5, pc6, pc7, nrow = 2, ncol = 2)

pc8 <- PlotXTabs2(data3, JobInvolvement, Attrition, results.subtitle = FALSE)
pc9 <- PlotXTabs2(data3, OverTime, Attrition, results.subtitle = FALSE)
grid.arrange(pc8, pc9, nrow = 1, ncol = 2)

PlotXTabs2(data3, JobRole, Attrition, results.subtitle = FALSE, x.axis.orientation = "slant")

pc11 <- PlotXTabs2(data3, EnvironmentSatisfaction, Attrition, results.subtitle = FALSE)
pc12 <- PlotXTabs2(data3, JobSatisfaction, Attrition, results.subtitle = FALSE)
pc13 <- PlotXTabs2(data3, RelationshipSatisfaction, Attrition, results.subtitle = FALSE)
pc14 <- PlotXTabs2(data3, PerformanceRating, Attrition, results.subtitle = FALSE)
grid.arrange(pc11, pc12, pc13, pc14, nrow = 2, ncol = 2)

#----Final Data Building----
final_data = cbind(data_num1, data_cat1)
final_data$Attrition = Attrition
final_data$Attrition <- ifelse(final_data$Attrition == "Yes",1,0 )
str(final_data)

#Train & Test data
Train <- createDataPartition(final_data$Attrition, p=0.7, list=FALSE)
names(final_data)
training <- (final_data[ Train, ])
testing <- final_data[ -Train, -16]
str(training)
str(testing)
str(final_data)
table(final_data$Attrition)
#----Regression Model Development----
#Basic Model - MOdel0
model <- glm(Attrition~., family = binomial,data = training)
summary(model)

# Multicollinearity
library(car)
car::vif(model)

#Autocoreation
library(lmtest)
dwt(model)
dwtest(model)

#Hetroscadisticity 
bptest(model)
plot(model$residuals, ylab = "Residuals")

#Model1
training1 <- select(training, c("Age", "DistanceFromHome", "YearsSinceLastPromotion",
                                "BusinessTravel","EnvironmentSatisfaction","MaritalStatus",
                                "JobInvolvement", "OverTime", "WorkLifeBalance",
                                "JobSatisfaction", "JobRole", "Attrition")) 
str(training1)

model1 <- glm(Attrition~., family = binomial,data = training1)
summary(model1)
car::vif(model1)
dwtest(model1)

#Model2
training2 <- select(training, c("Age", "BusinessTravel","EnvironmentSatisfaction",
                                "MaritalStatus","JobInvolvement", "OverTime", 
                                "WorkLifeBalance", "JobRole", "Attrition")) 
str(training2)

model2 <- glm(Attrition~., family = binomial,data = training2)
summary(model2)

car::vif(model2)
dwtest(model2)

#----Model Analysis----
#Goodness of fit - hoslem.test 
library(ResourceSelection)
library(rcompanion)
hoslem.test(training$Attrition, fitted(model))
nagelkerke(model)

hoslem.test(training1$Attrition, fitted(model1))
nagelkerke(model1)


hoslem.test(training2$Attrition, fitted(model2))
nagelkerke(model2)

#Prediction value mutation for three models

predict1 <- predict(model,type = "response")
predict1

predict2 <- predict(model1,type = "response")
predict2

predict3 <- predict(model2,type = "response")
predict3

predict_glm <- predict(model1,type = "response")

training$predict1 <- predict1
training$predict2 <- predict2
training$predict3 <- predict3

#Calculating performance for the three models
library(ROCR)

pred1 <- prediction(training$predict1,training$Attrition)
perf1 <- performance(pred1,"tpr","fpr")
perf1

pred2 <- prediction(training$predict2,training$Attrition)
perf2 <- performance(pred2,"tpr","fpr")
perf2

pred3 <- prediction(training$predict3,training$Attrition)
perf3 <- performance(pred3,"tpr","fpr")
perf3

prediction_glm <- prediction(training$predict2,training$Attrition)
perf_glm <- performance(pred2,"tpr","fpr")

dev.off()
#par(mfrow = c(1,2))
plot(perf1, colorize = T, print.cutoffs.at = seq(0.1, by=0.05), main = "Model0")
plot(perf2, colorize = T, print.cutoffs.at = seq(0.1, by=0.05), main = "Model1")
plot(perf3, colorize = T, print.cutoffs.at = seq(0.1, by=0.05), main = "Model2")

#-----ROC Method------
library(ggplot2)
library(pROC)
library(PRROC)
library(ROCR)
library(plotROC)

#Model0 confusion matrix

predicted11 <- ifelse(predict1 > 0.48, 1, 0)
cnfmtrx11 <- table(prd=predicted11, training$Attrition)
confusionMatrix(cnfmtrx11)
OAA_m0 <- ((cnfmtrx11[1,1]+cnfmtrx11[2,2])/sum(cnfmtrx11))
OAA_m0
#Model 1 confusion matrix

predicted22 <- ifelse(predict2 > 0.48, 1, 0)
cnfmtrx22 <- table(prd=predicted22, training$Attrition)
confusionMatrix(cnfmtrx22)
OAA_m1 <- ((cnfmtrx22[1,1]+cnfmtrx22[2,2])/sum(cnfmtrx22))
OAA_m1
#Model 2 confusion matrix

predicted33 <- ifelse(predict3 > 0.48, 1, 0)
cnfmtrx33 <- table(prd=predicted33, training$Attrition)
confusionMatrix(cnfmtrx33)
OAA_m2 <- ((cnfmtrx33[1,1]+cnfmtrx33[2,2])/sum(cnfmtrx33))
OAA_m2

OAA_LG <- OAA_m1

#----Plotting Logistic Curve----
Plotmodl0 <- mutate(training, PrdVal=predict1, POutcome=predicted11)
head(Plotmodl0)
Plotmodl1 <- mutate(training, PrdVal=predict2, POutcome=predicted22)
head(Plotmodl1)
Plotmodl2 <- mutate(training, PrdVal=predict3, POutcome=predicted33)
head(Plotmodl2)

ggplot(Plotmodl0, aes(x=predict1, y=POutcome))  + 
  geom_point(shape=19, colour="blue", fill="blue") +
  geom_smooth(method="gam", formula=y~s(log(x)), se=FALSE) +
  labs(title="Binomial Regression Curve for Model0") +
  labs(x="") +
  labs(y="")

ggplot(Plotmodl1, aes(x=predict2, y=POutcome))  + 
  geom_point(shape=19, colour="blue", fill="blue") +
  geom_smooth(method="gam", formula=y~s(log(x)), se=FALSE) +
  labs(title="Binomial Regression Curve for Model1") +
  labs(x="") +
  labs(y="")

ggplot(Plotmodl2, aes(x=predict2, y=POutcome))  + 
  geom_point(shape=19, colour="blue", fill="blue") +
  geom_smooth(method="gam", formula=y~s(log(x)), se=FALSE) +
  labs(title="Binomial Regression Curve for Model1") +
  labs(x="") +
  labs(y="")


#Method for Finding AUC- Area under curve
str(training)
dev.off()
par(mfrow = c(2,2))
library(PRROC)
PRROC_obj1 <- roc.curve(scores.class0 = training$predict1, weights.class0=training$Attrition,
                        curve=TRUE)
plot(PRROC_obj1, main = "Model0")

PRROC_obj2 <- roc.curve(scores.class0 = training$predict2, weights.class0=training$Attrition,
                        curve=TRUE)
plot(PRROC_obj2, main = "Model1")

PRROC_obj3 <- roc.curve(scores.class0 = training$predict3, weights.class0=training$Attrition,
                        curve=TRUE)
plot(PRROC_obj3, main = "Model2")

#Finding precision and recall
library(precrec)
dev.off()
precrec_obj1 <- evalmod(scores = training$predict2, labels = training$Attrition)
autoplot(precrec_obj1, main = "Model0")

precrec_obj2 <- evalmod(scores = training$predict3, labels = training$Attrition)
autoplot(precrec_obj2, main = "Model1")

#----Predict test data----
str(training)
str(testing)

Predict_test <- predict(model1, testing, type="response")
Predict_testbin <- ifelse(Predict_test > 0.48, 1, 0)
Predict_testbin <- as.factor(Predict_testbin)
levels(Predict_testbin) <- c("0", "1")
Prd_Test <- mutate(testing, Result=Predict_test, Prd_Outcome = Predict_testbin)
head(Prd_Test)
table(Prd_Test$Prd_Outcome)

write.csv(Prd_Test,"Test_Prediction.csv")


#----Classification Model Development----
#---Decision Tree----
library(ISLR)
library(caTools)
library(tree)
str(final_data)
str(training)
str(testing)

training_DT <- training[, -c(17:19)]
testing_DT <- final_data[ -Train,]
str(training_DT)
str(testing_DT)

final_data$Attrition <- as.factor(final_data$Attrition)
training$Attrition <- as.factor(training$Attrition)
testing_DT$Attrition<- as.factor(testing_DT$Attrition)

table(final_data$Attrition)
prop.table(table(final_data$Attrition))
table(training$Attrition)
prop.table(table(training$Attrition))

#------Building the model on entire Attrition data set----
tree.attrition <- tree(Attrition~.,final_data)
summary(tree.attrition)
plot(tree.attrition)
text(tree.attrition,pretty = 0)

library(rattle)
library(rpart)
dev.off()
DTreeModel <- rpart(Attrition~.,data=training_DT,method="class")
fancyRpartPlot(DTreeModel, tweak = 1.25, cex= 0.35)
predDT <- predict(DTreeModel,newdata = testing_DT,type = "class")
pred_table <- table(testing_DT$Attrition,predDT)
OAA_DT <- ((pred_table[1,1]+pred_table[2,2])/sum(pred_table))
OAA_DT
#----Prediction on same Attrition ie entire data set---
tree.pred <- predict(tree.attrition,final_data,type = "class")
conf_tree.pred <- table(tree.pred,final_data$Attrition)
conf_tree.pred
#-----Calculating Classification Accuracy of Attrition data set---
OAA_DT2 <- ((conf_tree.pred[1,1]+conf_tree.pred[2,2])/sum(conf_tree.pred))
OAA_DT2

#----Prune the tree----
set.seed(123)
cv.Attrition <- cv.tree(tree.attrition, FUN = prune.misclass)
names(cv.Attrition)
cv.Attrition
#----Plotting error----
par(mfrow=c(1,2))
plot(cv.Attrition$size,cv.Attrition$dev,type="b",col="red",lwd=2)
plot(cv.Attrition$k,cv.Attrition$dev,type="b",col="blue",lwd=2)
#---Again build a tree with 6 terminal nodes---
prune.Attrition <- prune.misclass(tree.attrition,best=6)
dev.off()
plot(prune.Attrition)
text(prune.Attrition,petty=0)

#----Using the prune model on test data for prediction---
pred_prune <- predict(prune.Attrition, testing_DT,type = "class")
conf_prune_tree_pred1 <- table(pred_prune, testing_DT$Attrition)
conf_prune_tree_pred1
OAA_PR <- ((conf_prune_tree_pred1[1,1]+conf_prune_tree_pred1[2,2])/sum(conf_prune_tree_pred1))
OAA_PR

#-------Bagging------
library(randomForest)
#----Bagging will take all variables so mtry=15 ie all 15 variables except "Attrition"-----
set.seed(123)
bag.attrition <- randomForest(Attrition~.,final_data, subset=Train, mtry=22)
dim(final_data)  
#importance(bag.attrition)  
varImpPlot(bag.attrition,col="red",pch=10,cex=1.25)  
bag.attrition
str(train)
#----Using bagging model on test data for prediction----
pred_bag <- predict(bag.attrition,testing_DT,type = "class")
conf_bag_pred_test<-table(pred_bag,testing_DT$Attrition)  
conf_bag_pred_test  
OAA_BG <- (conf_bag_pred_test[1,1]+conf_bag_pred_test[2,2])/sum(conf_bag_pred_test)
OAA_BG

#----Random forest will take selected variables ie ~SQRT22(as there are 22 predictors)= 
#mtry = 4.6 = 5
rf.attrition <- randomForest(Attrition~.,final_data, subset = Train, mtry=5)
dim(final_data)
#importance(rf.attrition)
varImpPlot(rf.attrition, col = "red", pch = 10, cex = 1.25)

#-----Using random forest on test data for prediction----
pred_rf <- predict(rf.attrition, testing_DT, type = "class")
conf_test_pred_rf <- table(pred_rf, testing_DT$Attrition)
conf_test_pred_rf
OAA_RF <- (conf_test_pred_rf[1,1]+conf_test_pred_rf[2,2])/sum(conf_test_pred_rf)
OAA_RF

#---Final Conclusion----

glm_ROC <- predict(model1,type = "response")
pred_glm <- prediction(training$predict2,training$Attrition)
perf_glm <- performance(pred2,"tpr","fpr")

dt_ROC = predict(tree.attrition,testing_DT)
pred_dt = prediction(dt_ROC[,2],testing_DT$Attrition)
perf_dt = performance(pred_dt,"tpr","fpr")

RF_ROC = predict(rf.attrition,testing_DT,type="prob")
pred_RF = prediction(RF_ROC[,2],testing_DT$Attrition)
perf_RF = performance(pred_RF,"tpr","fpr")

BG_ROC = predict(bag.attrition,testing_DT,type="prob")
pred_BG = prediction(BG_ROC[,2],testing_DT$Attrition)
perf_BG = performance(pred_BG,"tpr","fpr")

PR_ROC = predict(prune.Attrition,testing_DT)
pred_PR = prediction(PR_ROC[,2],testing_DT$Attrition)
perf_PR = performance(pred_PR,"tpr","fpr")

auc_glm <- performance(pred_glm,"auc")
auc_glm <- round(as.numeric(auc_glm@y.values),3)
auc_dt <- performance(pred_dt,"auc")
auc_dt <- round(as.numeric(auc_dt@y.values),3)
auc_RF <- performance(pred_RF,"auc")
auc_RF <- round(as.numeric(auc_RF@y.values),3)
auc_BG <- performance(pred_BG,"auc")
auc_BG <- round(as.numeric(auc_BG@y.values),3)
auc_PR <- performance(pred_PR,"auc")
auc_PR <- round(as.numeric(auc_PR@y.values),3)
print(paste('AUC of Logistic Regression:',auc_glm))
print(paste('AUC of Decision Tree:',auc_dt))
print(paste('AUC of Random Forest:',auc_RF))
print(paste('AUC of Bagging Tree:',auc_BG))
print(paste('AUC of Pruning Tree:',auc_PR))

print(paste('Accuracy of Logistic Regression Model:',round(OAA_LG*100,2), "%"))
print(paste('Accuracy of Decision Tree Model:',round(OAA_DT*100,2), "%"))
print(paste('Accuracy of Random Forest Model:',round(OAA_RF*100,2), "%"))
print(paste('Accuracy of Bagging Tree MOdel:',round(OAA_BG*100,2), "%"))
print(paste('Accuracy of Pruning Tree Model:',round(OAA_PR*100,2), "%"))

dev.off()
plot(perf_glm, main = "ROC curves for the models", col='blue')
plot(perf_dt,add=TRUE, col='red')
plot(perf_RF, add=TRUE, col='green3')
plot(perf_BG,add=TRUE, col='yellow')
plot(perf_PR, add=TRUE, col='black')
legend('bottomright', c("Logistic Regression","Decision Tree", 
                   "Random Forest", "Bagging","Pruning"), 
       fill = c('blue','red','green3','yellow','black'), 
       bty='n', cex = 0.8)

