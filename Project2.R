# Task 1) Read in the DentalClaims.csv file into R. Name the resulting dataframe, Dental.
# Convert the columns (ID, Tier, Procedure, Occupation, Provider, Claim) into factor columns, not integer. Hint: use factor() 

##############################
#--Task 1: Handling of Data--#
##############################

#Load in various packages
#install.packages("tidyr")
library(tidyr)

#install.packages("dplyr")
library(dplyr)

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
Dental

summary(Dental)
str(Dental)

#Turn ID, Tier, Procedure, Occupation, Provider, Claim
#into factors, not integers
Dental$ID <- factor(Dental$ID)
Dental$Tier <- factor(Dental$Tier)
Dental$Procedure <- factor(Dental$Procedure)
Dental$Occupation <- factor(Dental$Occupation)
Dental$Provider <- factor(Dental$Provider)
Dental$Claim <- factor(Dental$Claim)

#Check
str(Dental)

#######################################
#--Task 2: Create various histograms--#
#######################################

# A. Histogram of the variable Claim, stat="count"
ggplot(data=Dental,aes(x=Claim)) + 
  geom_histogram(stat="count", aes(fill=Claim))
# Obs: More people did not have a claim vs. did.

# B. Histogram of the Claim by Gender, stat="count"
#    use facets
p <- ggplot(Dental, aes(x=Claim, color=Gender,
                        fill=Gender))
p + geom_histogram(stat="count", 
                   aes(fill=Gender))

p + geom_histogram(stat="count") +
  facet_grid(.~Gender)

# Obs: Of the females, more had claims.
# Obs: Not too many of the males had claims.

###############################################
#--Task 3: Visualize the insured with claims--#
###############################################

#A. Create a dataset where Claim == 1
DentalClaims <- Dental[Dental$Claim == 1,]
head(DentalClaims)
summary(DentalClaims)
nrow(DentalClaims)

#Reconciliation
summary(Dental) #342 claims

#B. Create a density plot of Age where Claim == 1
s <- ggplot(data=DentalClaims, aes(x=Age))
s + geom_density(fill="lightgreen")
# Obs: A lot of claims in the 20-40 age range.

#C. Create a density plot of Paid where Claim == 1
p <- ggplot(data=DentalClaims, aes(x=Paid))
p + geom_density(fill="lightblue")
# Obs: A lot of smaller and large paid claims.

###############################################
#--Task 4: Analyze the dental providers     --#
###############################################
#Which provider is the most expensive?
#A. Create a Box Plot of Provider and Paid
t <- ggplot(DentalClaims, aes(x=Provider, 
                              y=Paid, 
                              color=Provider))
t + geom_boxplot(size=1)
#Observation: Provider C is the most expensive.
t + geom_boxplot(size=1) + geom_jitter()
#Observation: There seems to be a pattern.


#B. For the box plot,
#   provide a title for the x-axis, y-axis, chart title,
#   and add a legend

b <- ggplot(DentalClaims, aes(x=Provider, 
                              y=Paid, 
                              color=Provider))
b + geom_boxplot(size=1) +
  geom_jitter() +
  xlab("Dental Provider Code") +
  ylab("Dental Claim Paid Dollars ($)") +
  ggtitle("How Much Do Dentists Charge?") +
  theme(axis.title.x = element_text(color="Black", size=20),
        axis.title.y = element_text(color="Black", size = 20),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=20),
        legend.text = element_text(size=10),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(color = "DarkGreen", 
                                  size = 20,
                                  family = "Arial"))

###############################################
#--Extra Credit: Scatterplots               --#
###############################################

p <- ggplot(DentalClaims,aes(x=Age,
                             y=Paid,
                             color=Provider))
p + geom_point() + 
  facet_grid(.~Procedure)

#Paid varies by procedure and provider.
#Paid seems flat for age. 

