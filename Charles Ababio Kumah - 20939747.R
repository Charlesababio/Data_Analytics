#----------  PROJECT TITTLE ----------
#Insurance Premium Prediction 

#---------- METADA ----------
#The dataset contains 1338 observations (rows) and 7 features (columns). 
#The dataset contains 4 numerical features (Age, BMI, Children and Medical Expenses) 
#and 3 nominal features (Sex, Smoker and Region) that were converted into factors 
#with numerical value designated for each level.
# Data was extracted from Kaggle. The Author of data is nursnaaz

#---------- Aim of Project ----------
#The purposes of this project is to look into different features to observe 
#their relationship, and plot a multiple linear regression based on several 
#features of individual such as age, physical/family condition and location
#against their existing medical expense to be used for predicting 
#future medical expenses of individuals that help medical insurance to make 
#decision on charging the premium.


#---------- Install & Import Packages ----------
library(dplyr)
library(ggplot2)
library(lmtest)
library("GGally")
library(tidyr)
library(corrplot)
library(Hmisc)


#---------- Import Dataset  ----------

setwd("C:/Users/USER/OneDrive/Desktop/KNUST PostGrad/BDA 517 - DATA SCIENCE FOR BUSINESS/Project")


#----------  Read & Assign Dataset ----------

insurance <- read.csv("insurance.csv")


#---------- Check & Count missing values ----------
is.na(insurance)
sum(is.na(insurance)) #-- No empty data points


#---------- Data Understanding & Exploration  ----------
insurance
View(insurance)

str(insurance)
glimpse(insurance)
names(insurance)
dim(insurance)

head(insurance)
tail(insurance)

summary(insurance)


#----------  Corrletion Analysis ----------
#Drawing Pairs Plot of Data Frame Variables
ggpairs(insurance)

# Correlation analysis between Age, BMI, Medical Expenses and Children Count

#extracting Age, BMI, Medical Expenses and Children Variables
cor_insurance <- data.frame(insurance$Age, insurance$BMI, insurance$Children,
                            insurance$Medical.Expenses)

corrplot(cor(cor_insurance),
         method = "number",
         type = "upper" # show only upper side
)

#P-values 
P_Values <- rcorr(as.matrix(cor_insurance))
round(P_Values$P,3) 



#Drawing boxplots

ggplot(insurance,
       aes(x = Medical.Expenses,
           fill = Smoker))+
  geom_boxplot()

ggplot(insurance,
       aes(x = Age,
           fill = Smoker))+
  geom_boxplot()+
  coord_flip()

ggplot(insurance,
       aes(x = Medical.Expenses,
           fill = Sex))+
  geom_boxplot()

#Bar Chart of Categorical variables

ggplot(insurance,
       aes(Sex))+
  geom_bar(fill = "#cdad7d")+
  coord_flip()

ggplot(insurance,
       aes(Smoker))+
  geom_bar(fill = "#cdad7d")+
  coord_flip()

ggplot(insurance,
       aes(Region))+
  geom_bar(fill = "#cdad7d")+
  coord_flip()


#---------- Creating factors for the categorical Variables  ----------
#Creating factor for Sex, Smoker, Region
insurance_factor <- within(insurance, {
  Sex <- factor(Sex, labels = c("female", "male"))
  Smoker <- factor(Smoker, labels = c("yes", "no"))
  Region <- factor(Region, labels = c("southwest" ,"southeast", "northwest", "northeast"))
})

glimpse(insurance_factor)

# numerical value designated for each level 
insurance_data <- within(insurance_factor,{
  Sex <- as.numeric(insurance_factor$Sex)-1
  Smoker <- as.numeric(insurance_factor$Smoker)-1
  Region <- as.numeric(insurance_factor$Region)-1
})

glimpse(insurance_data)


#---------- Predictive Modeling  ----------
#Multiple linear regression
# Dependent variable : Medical Expenses 
# Independent variables : Age, BMI, Children, Sex, Smoker, Region

#Model Fitting

Prediction_Model <- lm (Medical.Expenses ~ Age + BMI + Children + Sex + Smoker + 
                          Region, data = insurance_data)


#----------  Test of Assumptions ----------
#Normality (use of the QQ plot), 
#Linearliy (use of the Residual VS Fitted), 
#Homoscedascity (use of Scale-Location), 
#The residual vs the leverage check for  influence or outliers
#Independence (use of Durbin Watson test)

plot(Prediction_Model)


#----------P-Value test of fitted Model----------


dwtest(Prediction_Model)

#----------  Model Output ----------

summary(Prediction_Model)


