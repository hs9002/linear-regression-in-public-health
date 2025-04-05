

# Key assumptions that need to be satisfied in linear regression
# 1. Linearity between outcome (Y) and predictor (X) variables; 
# 2. The outcome variable is normally distributed across predictor values; 
# 3. The variance of outcome is the same across predictor values. 



# 1 
#------------------------------------------------------------------------------------#

require(data.table)

COPD = read.csv('C:/Users/hs_90/Dropbox/Coursera/Linear regression Public health/COPD_student_dataset.csv')

hist(COPD$MWT1Best, main="Histogram of MWT1Best", xlab="MWT1Best", breaks=12)

# look at a subset of data
check_subset1 = subset(COPD, MWT1Best > 650)

# should never delete an unusual value if the value is a possible one. Deleting unusual values will bias your results and cause you to underestimate the variability in the observations.

check_subset2 = subset(COPD, MWT1Best>600 | MWT1Best<150)

list("Summary" = summary(COPD$MWT1Best), 
     "Mean" = mean(COPD$MWT1Best, na.rm=TRUE), 
     "Standard Deviation" = sd(COPD$MWT1Best, na.rm=TRUE), 
     "Range" = range(COPD$MWT1Best, na.rm=TRUE), 
     "Inter-Quartile Range" = IQR(COPD$MWT1Best, na.rm=TRUE)) 

# Scatter plot
plot(COPD$FEV1, COPD$MWT1Best, xlab = "FEV1", ylab = "MWT1Best") 

# Pearson correlation test with missing values removed (otherwise there will be an error). 
cor.test(COPD$FEV1, COPD$MWT1Best, use="complete.obs", method="pearson")

# Spearman correlation test with missing values removed. 
cor.test(COPD$FEV1, COPD$MWT1Best, use="complete.obs", method="spearman")

# When the data are normally distributed, you should expect Pearson’s and Spearman’s to be reasonably similar - but there will be greater differences when distributions are skewed or there are outliers.  

# fitting a regression between walking distance (MWT1Best) and FEV1 (lung capacity)
par(mfrow=c(2,2)) # this is to view the plot of the model(later) all in one place. Has to come before the model statement. 
MWT1Best_FEV1 <- lm(MWT1Best~FEV1, data = COPD)
summary(MWT1Best_FEV1)
confint(MWT1Best_FEV1) # 95% change the true population parameter will lie within this range.If the confidence interval contains 0 then results is not significant at the 5% level. 
plot(MWT1Best_FEV1) # This generates plots to examine the linear regression model to check for linearity, homoscedasticity, independence, and normality of your assumptions. 

# note: Homoscedasticity is an assumption of equal or similar variances of one variable across another variable.

# note: Heteroscedasticity is the unequal variance of one variable across another variable (assessed using the 'Residuals vs Fitted' plot generated from the model)


# fitting a regression between walking distance (MWT1Best) and AGE
MWT1Best_AGE <- lm(MWT1Best~AGE, data=COPD) 
summary(MWT1Best_AGE)
confint(MWT1Best_AGE) 
plot(MWT1Best_AGE)

predictedVals = predict(MWT1Best_AGE)
residualVals = residuals(MWT1Best_AGE)

# note: QQ plot is a plot of the quartiles of the residuals against the quartiles of a theoretical normal distribution and if the residuals are normal then the observations will lie on a straight line. 

# histogram of residuals - another check (along with QQ plot) to see if residuals are normally distributed
hist(residualVals, main = "Histogram of residuals", xlab = "Residuals")

par(mfrow=c(1,1))
plot(COPD$AGE, COPD$MWT1Best, xlab = "AGE", ylab = "MWT1Best")


# 2
#------------------------------------------------------------------------------------#

# Multiple linear regression (more than 1 predictor variable)

# Collinearity: strong linear association between two predictor variables in the model. It causes a problem because the two variables are both explaining the same variance in the observations, which means the variance can't be petitioned between these two competing predictors. 

MWT1Best_FEV1_AGE = lm(MWT1Best~FEV1+AGE, data = COPD) 
summary(MWT1Best_FEV1_AGE) 
confint(MWT1Best_FEV1_AGE) 

# FVC and AGE separately first
MWT1Best_FVC = lm(MWT1Best~FVC, data = COPD)
summary(MWT1Best_FVC)
confint(MWT1Best_FVC)

MWT1Best_AGE = lm(MWT1Best~AGE, data = COPD)
summary(MWT1Best_AGE)
confint(MWT1Best_AGE)

plot(COPD$FVC, COPD$AGE, xlab="FVC", ylab="AGE")
cor.test(COPD$FVC, COPD$AGE, use="complete.obs", method="pearson")

# After examining the two above... now together
MWT1Best_FVC_AGE = lm(MWT1Best~FVC+AGE, data = COPD) 
summary(MWT1Best_FVC_AGE) 
# coefficients are now adjusted coefficients with B1 now giving the average increase in walking distance for every one unit increase in FVC, keeping age held constant. and B2 the average increase in walking distance for every one year increase in age, keeping FVC held constant. 
confint(MWT1Best_FVC_AGE) 

plot(MWT1Best_FVC_AGE)

# Variance inflation factor (VIF): a measure of the amount of multicollinearity in regression analysis. It is the Ratio of the variance of a parameter estimate when fitting a full model that includes other parameters to the variance of the parameter estimate if the model is fit with only the parameter on its own. The VIF provides an index that measures how much the variance (the square of the estimate's standard deviation) of an estimated regression coefficient is increased because of collinearity.


# Interactive term between binary variables
r2 = lm(MWT1Best~factor(Diabetes)+factor(AtrialFib)+factor(Diabetes*AtrialFib), data=COPD)
summary(r2)
confint(r2)

install.packages("prediction")
require(prediction)

list("Diabetes" = prediction(r2, at = list(Diabetes=c(0,1))),
     "AtrialFib" = prediction(r2, at = list(AtrialFib=c(0,1))),
     "Diabetes*AtrialFib" = prediction(r2, at = list(Diabetes=c(0,1), AtrialFib=c(0,1))))


r3 = lm(MWT1Best~factor(Diabetes)+factor(IDH)+factor(Diabetes*IDH), data=COPD)
summary(r2)
confint(r2)


# Interactive term between binary and continuous variables
# approach is the same as above 

# Don't test each variable at the five percent significance level as a way to select variables to include in your model, this approach is even worse than the step-wise regression, as relationships at the universal level can disappear and appear at the multivariable level. Don't use forwards or step-wise selection procedures at all, if you're tempted to use any of them, then backwards selection procedure is the least offensive. Finally, don't only include known predictors, if and only if they're statistically significant at the five percent level. If they're known predictors, they should be included in the model, regardless of their p-value, they will alter the estimates of the other regression coefficients in the model.


