getwd()
setwd(choose.dir())
hos<-read.csv("HospitalCosts.csv")
head(hos)
str(hos)
#500 entries were found upon observing the structure of the data set
#step 1: to implement data cleaning

#brief overview of the data
summary(hos)

# Attribute	      Description
# Age 	          Age of the patient discharged
# Female 	        A binary variable that indicates if the patient is female
# Los	            Length of stay in days
# Race 	          Race of the patient (specified numerically)
# Totchg	        Hospital discharge costs
# Aprdrg	        All Patient Refined Diagnosis Related Groups

#check structure of data set for appropriate data type
str(hos)
#female and race are categorical variables
#structure of data is appropriate hence moving to the next step

#finding out null values
colSums(is.na(hos))
#found only 1 null value
#since number of null values less than 5% we shall omit it
hos<-na.omit(hos)
colSums(is.na(hos))
# null values omitted and new data set evaluated

#since female and race are categorical variables we will convert them into factors
hos$RACE <- as.factor(hos$RACE)
hos$FEMALE <- as.factor(hos$FEMALE)

#step 2: data exploratory analysis

# 1. step 1: to find maximum expenditure based on age and frequency of visit  

# a. To find the category that has the highest frequency of hospital visit

#implementing data visualization in the form of histogram
# The as.factor() is used to make sure that the categories are not treated as numbers.

# Code: 

hist(hos$AGE)

summary(as.factor(hos$AGE))

# Result: From the histogram ,we can infer that infants with AGE = 0 have the maximum 
# frequency of hospital visit,  with a whopping value of. The summary of AGE attribute gives 
# the numerical output (after converting the age from numeric to factor) - and we 
# can see that there are 306 entries for those in the range of 0-1 year.

# b. To find the age category with the maximum expenditure

# we need to add the expenditure for each age, and find the maximum value 
# from the sum. Use the aggregate function to add the values of 
# total expenditure according to the values of age.


library(dplyr)

hos1 <- hos %>% group_by(AGE) %>% summarise(TotchgAge = sum(TOTCHG)) %>% arrange(desc(TotchgAge))
hos1
hos1[1,]

plot<-hos%>% ggplot2::aes("RACE","FEMALE")
plot

# To make sure that there is no malpractice, we analyze if the 
# race of the patient is related to the hospitalization costs.

# If there is any effect of RACE on TOTCHG

# verify if the races made an impact on the costs, perform an ANOVA with the 
# following variables:  

# ANOVA dependent variable: TOTCHG 
# Categorical/grouping variable: RACE Missing values: 1 NA value, use na.omit to remove the NA value   

# Code:  

str(hos$RACE)
str(hos$TOTCHG)

anova1 <- aov(TOTCHG ~ RACE, data = hos) 
# numerical/int ~ categorical varibale


# dependent variable ~ independent variable

summary(anova1)

a1<- alpha = 0.05

pv1<- pvalue = 0.943
# if pvalue < alpha is true, we do not reject null hypothesis
pv1 < a1 

# Result: We do not reject the null hypothesis. 
# The p-value is very high hence no relation between the race of patient and hospital cost.



# To properly utilize the costs, the agency has to analyze the severity of the 
# hospital costs by age and gender for proper allocation of resources.  

#Code:

anova2 <- aov(TOTCHG ~ AGE + FEMALE, data = hos)

summary(anova2)


a2<-alpha = 0.05

pva2<-pvalueAge = 0.00323

pvg2<-pvalueGender = 0.03638

pva2 < a2 # if this is true = whenever p_value is less than alpha; we reject the null hypothesis

pvg2 < a2 # if this is true = whenever p_value is less than alpha; we reject the null hypothesis


# Result: We reject the null hypothesis. The p-values is very low in case of Age and relatively low in case of Gender,
#implying there is a significant relation between age and gender of patient and the hospital cost.



#######################################################################################
#######################################################################################


# 5. Since, the length of stay is the crucial factor for inpatients,we see if the length of stay can be predicted from age, gender, and race.

# Since the length of stay is a continuous variable, we use linear regression to 
# predict the variable.  Dependent variable: LOS Independent variables: AGE, FEMALE, 
# RACE. Note that RACE and FEMALE should be converted into factors, whereas AGE is a 
# numerical variable.   

# length of stay can be predicted from age, female, and race.

lm1 <- lm(LOS ~ AGE + FEMALE + RACE, data = hos)
summary(lm1)

predict_los <- predict(lm1, hos)

pv3<-p_value = 0.7432

pv3 < a1 # if this is true = whenever p_value is less than alpha; we reject the null hypothesis

#Result: We can predict the length of stay from age, female and race. But the values from the model
#are very less accurate as P-value of the model is greater than default alpha.
#Hence, there is no relation between LOS and age, gender and race and not possible to predict correctly.

# No need to find RMSD and RMSE.


#######################################################################################
#######################################################################################

# 6. To perform a complete analysis, we find the variable that mostly affects the hospital costs.

#Code:

lm2 <- lm(TOTCHG ~ ., data = hos)
summary(lm2)

lm3 <- lm(TOTCHG ~ LOS, data = hos)
summary(lm3)

lm4 <- lm(TOTCHG ~ APRDRG, data = hos)
summary(lm4)

#length of stay is the main factor that significantly affects hospital costs and age bracket of infants, age=0 are the max frequenters in terms of hospital visits
