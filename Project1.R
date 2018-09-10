#-- R acturial Project 1--#

#Task1: Create two vectors
Premium <- c(1000,800,710)
Premium
claims <- c(1100,790,650)
claims

#Task2: Create a Loss Ratio vector
LossRatio <- claims/Premium
LossRatio

#Task3: Mean premium, mean claims, mean loss ratio
MeanPremium <- mean(Premium)
MeanPremium
MeanClaims <- mean(claims)
MeanClaims
MeanLossRatio <- mean(LossRatio)
MeanLossRatio


#Task4: Determine where LR is higher than mean LR
HighLR<- LossRatio > MeanLossRatio
HighLR

#Task5: Create a matrix with rows for premium, claims, LR Remember cbind and rbind

IS<- rbind(Premium,claims,LossRatio)
IS


colnames(IS)<- c("2013","2014","2015")
IS


