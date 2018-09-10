#################################
#--Task 1: Data Preprocessing --#
#################################

#Load in various packages
#install.packages("caTools")
library(caTools)

#install.packages("ggplot2")
library(ggplot2)

### Don't forget to set your working directory

setwd()
getwd()

#Read in DentalClaims.csv data
#---Columns---#
# ID: Dental Insurance Identification Number
# Tier: Dental insurance plan tier
# Gender: Gender of Insured
# Age: Age of Insured Individual
# Procedure: Code of dental procedure performed
# Occupation: Occupation of Insured Individual
# Provider: Code of the dentist (provider)
# Claim: 0=No dental claim, 1=Yes had dental claim
# Paid: The amount paid for the dental claim

Dental <- read.csv("DentalClaims.csv", na.strings=c("")) #Replace any empty cells with NA

str(Dental)
summary(Dental)

#Drop ID. Change Tier, Procedure, Occupation, Provider, Claim
#into factors
Dental$ID <- NULL
Dental$Tier <- factor(Dental$Tier)
Dental$Procedure <- factor(Dental$Procedure)
Dental$Occupation <- factor(Dental$Occupation)
Dental$Claim <- factor(Dental$Claim)

#Convert Gender from categorical to number mapping
Dental$Gender <- factor(Dental$Gender,
                        levels = c('M','F'),
                        labels = c(1,2))


#Convert Provider from categorical to number mapping
Dental$Provider <- factor(Dental$Provider,
                          levels = c('N','A','B','C'),
                          labels = c(0,1,2,3))

#Check
str(Dental)

#Dental Claims only
#A. Create a dataset where Claim == 1
DentalClaims <- Dental[Dental$Claim == 1,]
head(DentalClaims)
summary(DentalClaims)
nrow(DentalClaims)

#Reconciliation
summary(Dental) #342 claims

#############################################
#--Task 2: Create Simple Linear Regression--#
#############################################
str(DentalClaims)

#Drop Claims
DentalClaims$Claim <- NULL

#Create a training (75%) and test data set (25%)
set.seed(123)

###split the dataset into training vs. test
split = sample.split(DentalClaims$Paid, SplitRatio = 0.75)
training_set = subset(DentalClaims, split == TRUE)
test_set = subset(DentalClaims, split == FALSE)


# A. Create your best one variable linear regression
# where the dependent variable is Paid

reg <- lm(formula = Paid ~ .,
          data=training_set)

summary(reg)

#Only Provider as the independent variable
reg <- lm(formula = Paid ~ Provider,
          data=DentalClaims)  

summary(reg) #R-squared = 0.03226


reg <- lm(formula = Paid ~ Procedure,
          data=DentalClaims)  

summary(reg) #R-squared = 0.9752

test_set$PaidPredict <- predict(reg, 
                                newdata=test_set)
test_set$Residual = test_set$Paid - test_set$PaidPredict
test_set$AE = test_set$Paid / test_set$PaidPredict

##########################################################
#--Task 3: Create your best Multiple Linear Regression--#
#########################################################

mreg <- lm(formula = Paid ~ .,
           data=training_set)
summary(mreg) #Adjusted R-Squared = 0.9992

mreg <- lm(formula = Paid ~ Procedure +
             Provider,
           data=training_set)
summary(mreg) #Adjusted R-Squared = 0.9991

test_set$PaidPredictM <- predict(mreg,
                                 newdata = test_set)

test_set$ResidualM <- test_set$Paid - test_set$PaidPredictM

##########################################################
#--Task 4: Make a prediction using your best model    --#
#########################################################

# Using your best model, what's your prediction on the
# Paid Claim amount for the following characteristics:
# Tier 1, Gender = 'M', Age = 35, Procedure = 4,
# Occupation = 1, Provider = 3

mydf <- data.frame(
  Tier = c('1'),
  Gender = c('1'),
  Age = 35,
  Procedure = c('4'),
  Occupation = c('1'),
  Provider = c('3')
)

mypred <- predict(mreg, newdata = mydf)
mypred
# $345.40

##########################################################
#--Task 5: Create a Logistic Regression Model          
#-- Response variable is Claim.
#-- Independent variables are Tier, Age, Gender, Occupation
#########################################################

DentalBinary <- read.csv("DentalClaims.csv", na.strings=c("")) #Replace any empty cells with NA

DentalBinary <- DentalBinary[,c(2,3,4,6,8)]

###set the seed
#?set.seed()
set.seed(123)

###split the dataset into training vs. test datasets
#?sample.split()
split = sample.split(DentalBinary$Claim, SplitRatio = 0.75)
training_setGLM = subset(DentalBinary, split == TRUE)
test_setGLM = subset(DentalBinary, split == FALSE)

#Create Logistic Regression
logisticreg <- glm(formula = Claim ~ .,
                   family = binomial,
                   data = training_setGLM)

#Create predictions of the test_setGLM

prob <- predict(logisticreg, type = 'response', 
                newdata = test_setGLM[-5])

prob


# If probability is greater than 50%, then classify as 1
prediction = ifelse(prob > 0.5, 1, 0)

prediction

# Make the Confusion Matrix
# table(Actual Results Column, Predicted Results Column)
results = table(test_setGLM[, 5], prediction)

results
# Error Rate = (38+18) / (223) =  56 / 223 = 25% 

#Model accuracy guidelines
# Accuracy Rate: 90%-100% (too good), 80%-90% (very good model), 70%-80% (good okay)

summary(logisticreg)

#Attempt 2
logisticreg <- glm(formula = Claim ~ Tier + Gender,
                   family = binomial,
                   data = training_setGLM)

#Create predictions of the test_setGLM

prob <- predict(logisticreg, type = 'response', 
                newdata = test_setGLM[-5])

prob


# If probability is greater than 50%, then classify as 1
prediction = ifelse(prob > 0.5, 1, 0)

prediction

# Make the Confusion Matrix
# table(Actual Results Column, Predicted Results Column)
results = table(test_setGLM[, 5], prediction)

results
# Error Rate = (33+21) / (223) =  54 / 223 = 24%











