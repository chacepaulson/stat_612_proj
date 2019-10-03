library(tidyverse)
library(dplyr)
library(ggplot2)
#-----------------------------------------------------------------------------#
#Basic understanding of DC data set
#no_blanks dataset removed 32 rows where many data fields blank or wrong input

#plot Request Date by count of incidents -- far less incidents have either
#happened or been recorded. Odd spike in late 2017
ggplot(Vision_Zero_Safety_no_blanks, aes(REQUESTDATE)) +
  geom_freqpoly(binwidth = 500)

#plot request date by request type - review of commonality of each type over time
ggplot(Vision_Zero_Safety_no_blanks, aes(REQUESTDATE, REQUESTTYPE)) +
  geom_point()

#plot Street Seg ID by request type -- basically stuff happens everywhere
ggplot(Vision_Zero_Safety_no_blanks, aes(REQUESTTYPE, STREETSEGID)) +
  geom_point()

#plot request date by request type; color code User Type for each type of involved 
# in the request type
ggplot(Vision_Zero_Safety_no_blanks, aes(REQUESTDATE, REQUESTTYPE, color = USERTYPE)) +
  geom_point()

#boxplot - another view on that the majority of issues happene
#only went up to 2016 though for some reason
ggplot(data = Vision_Zero_Safety_no_blanks, mapping = aes(x = REQUESTTYPE, y = REQUESTDATE)) + 
  geom_boxplot()

#----------------------------------------------------------------------------#

#Correlation analysis --- NOT WORKING YET; took from previous class and tried to
#quickly tweak code without success; I assume error in how data is structured

#error message => Error in rcorr(as.matrix(Vision_Zero_Safety_no_blanks)) : 
#NA/NaN/Inf in foreign function call (arg 1)
#In addition: Warning message:
  #In storage.mode(x) <- "double" : NAs introduced by coercion

require(Hmisc) # contains rcorr( )

rcorr(as.matrix(Vision_Zero_Safety_no_blanks))


rcorr(as.matrix(mtcars)) # an example datset from r used to test that working
#which it does

library(corrplot) # Library for correlation plots
mtCorr <- cor(mtcars) # Store the correlation object
corrplot(mtCorr, method="number") # Try method = "circle" 


# corrplot() for Visual Correlation
library(corrplot) # Library for correlation plots
Vision_Corr <- cor(Vision_Zero_Safety_no_blanks) # Store the correlation object
corrplot(Vision_Corr, method="number") # Try method = "circle" 


#---------------------------------------------------------------------------#
#Through in some linear regression code examples from previous classes
#library(MASS) # Contains the Boston data set
#lm.fit.null <- lm(medv~1, data=Boston) # Use 1 for no predictors
#summary(lm.fit.null) # The null model
#mean(medv) # Mean = intercept in null model
#lm.fit <- lm(medv~lstat, data=Boston) # Simple linear model
#summary(lm.fit) # With 1 predictor


#should we be thinking about making USERTYPE a dummy variable to break out the
#that way?

#ex. code
#library(ISLR) # Contains the "Carseats" data set
#lm.fit <- lm(Sales~Price+US, data=Carseats) # US is a dummy (Yes =1, No =0)
#summary(lm.fit)
#library(coefplot)
#coefplot(lm.fit)

