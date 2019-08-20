                                # R Workshop - Multiple Regression

# Author: Ananthan Ambikairajah
# Email Address: ananthan.ambikairajah@anu.edu.au; a.ambikairajah@gmail.com
# Date of File Creation: 28/05/18
# Last Edited: 11/06/18
# Edit Log: AA
# R Version: 3.3.3

# 1. Set working directory and load packages ####
setwd("~/Desktop/GitHub/Statistics_Lessons/2. Linear Models - Multiple Regression/")

packages <- c("QuantPsyc", "car", "psych")
for (i in 1:length(packages)){
  library(packages[i], character.only = TRUE)
}

# 2. Exercise 1 - Pubs and Death####
# Load data and inspect
pubs_data <- read.delim("pubs.dat", header = TRUE)
head(pubs_data)
tail(pubs_data)
summary(pubs_data)
str(pubs_data)

# Simple linear regression
pubs_lm_0 <- lm(mortality ~ 1, data = pubs_data); summary(pubs_lm_0)
pubs_lm_1 <- lm(mortality ~ 1 + pubs, data = pubs_data); summary(pubs_lm_1)

# Plot the linear regression
par(mfrow = c(1,1))
plot(pubs_data$pubs, pubs_data$mortality, 
     col = "blue", type = "p",
     xlab = "Number of Pubs",
     ylab = "Deaths",
     main = "Deaths and Number of Pubs")
abline(pubs_lm_1, col = "red", lwd = 3)

# Inspect the residuals
pubs_lm_1$residuals

# Extension activity
# Simple linear regression without the outlier
pubs_lm_2 <- lm(mortality ~ 1 + pubs, data = pubs_data[c(1:7),]); summary(pubs_lm_2)

# Plot the new regression line on the old plot
par(mfrow = c(1,1))
plot(pubs_data$pubs, pubs_data$mortality, 
     col = "blue", type = "p",
     xlab = "Number of Pubs",
     ylab = "Deaths",
     main = "Deaths and Number of Pubs in 2019")
abline(pubs_lm_1, col = "red", lwd = 3)
abline(pubs_lm_2, col = "red", lwd = 3, lty = 2)

# 3. Exercise 2 - Building Models####
# Load data and inspect
album_data <- read.delim("Album Sales 2.dat", header = TRUE)
head(album_data)
tail(album_data)
summary(album_data)
str(album_data)

# Multiple Regression
album_lm_0 <- lm(sales ~ 1, data = album_data); summary(album_lm_0)
album_lm_1 <- lm(sales ~ 1 + adverts, data = album_data); summary(album_lm_1)
album_lm_2 <- lm(sales ~ 1 + adverts + airplay + attract, data = album_data); summary(album_lm_2)

# Compare R^2 between models
anova(album_lm_0, album_lm_1, album_lm_2)

# Compare AIC between models
AIC(album_lm_0, album_lm_1, album_lm_2)

# Best model
summary(album_lm_2)

# Confidence intervals for unstandardised beta-coefficients
confint(album_lm_2)

# Standardised beta-values
lm.beta(album_lm_2)

# 4. Exercise 3 - Model Diagnostics - Outliers and Influential Cases####
# Outliers
# Unstandardised residual - difference between observed data point and predicted data point
album_data$residuals <- resid(album_lm_2)
# Standardised residual - unstandardised residuals divided by an estimate of their standard deviation
album_data$standardised_residuals <- rstandard(album_lm_2)

# Influencial statistics
# Studentised residual - (adjusted predicted value minus the original observed value) divided by the standard error
album_data$studentised_residuals <- rstudent(album_lm_2)
# Cooks distance - accounts for leverage and residuals - values greater than 1 are a cause for concern
album_data$cooks_distance <- cooks.distance(album_lm_2)
# DFBeta - the difference between a parameter estimated using all cases and estimated when one case is excluded (Beta Values) 
album_data$dfbeta <- dfbeta(album_lm_2)
# DFFit - The difference between the adjusted predicted value (by removing one case) and the original predicted value (with the case included) - non-influencial cases should be equal to 0 - (difference in Y values)
album_data$dffit <- dffits(album_lm_2)
# Leverage values - distance of data points (for explanatory variables) from the mean. Average leverage is (k + 1)/n whereby k = number of predictors and n = number of participants
# Leverage values = 0 means no influence and 1 = complete influence. Most leverage values should be close to the average. Points that are two or three times the average leverage value should be investigated further. 
album_data$leverage <- hatvalues(album_lm_2)
# Covariance ratios should be close to 1. Cases > 1 + [3(k+1)/n] will damage the precision of some of the models parameters. Cases < 1 + [3(k+1)/n] will improve the precision of some of the models parameters. 
album_data$covariance_ratios <- covratio(album_lm_2)

# Check data
round(head(album_data), digits = 2)

# Plot data
plot(album_data$residuals)
plot(album_data$standardised_residuals) #notice the y axis change to standardised units
plot(album_data$studentised_residuals)
plot(album_data$cooks_distance)
plot(album_data$dfbeta[,"(Intercept)"])
plot(album_data$dfbeta[,"adverts"])
plot(album_data$dfbeta[,"airplay"])
plot(album_data$dfbeta[,"attract"])
plot(album_data$dffit)
plot(album_data$leverage)
plot(album_data$covariance_ratios)

# Inspect data diagnostics
# Large residuals (standardised)
album_data$large_residuals <- as.numeric(album_data$standardised_residuals > 1.96 | album_data$standardised_residuals < -1.96)
sum(album_data$large_residuals)

# Inspect cases for large residuals (standardised)
round(album_data[album_data$large_residuals == TRUE, c("sales", "airplay", "attract", "adverts", "standardised_residuals", "cooks_distance", "leverage", "covariance_ratios")], digits = 2)

# Really large residuals (standardised)
sum(album_data$standardised_residuals > 2.56 | album_data$standardised_residuals < -2.56)

# Large residuals (studentised)
album_data$large_residuals_studentised <- as.numeric(album_data$studentised_residuals > 1.96 | album_data$studentised_residuals < -1.96)
sum(album_data$large_residuals_studentised)
round(album_data[album_data$large_residuals_studentised == TRUE, c("sales", "airplay", "attract", "adverts", "standardised_residuals", "cooks_distance", "leverage", "covariance_ratios")], digits = 2)

# Cook's Distance
album_data$large_cooksd <- as.numeric(album_data$cooks_distance > 1)
sum(album_data$large_cooksd)

# Leverage values
(3 + 1)/200 # Average leverage values (k+1)/n
# Check cases 3 times leverage values of entire data-set
album_data$large_leverage <- as.numeric(album_data$leverage > 3*((3 + 1)/200))
sum(album_data$large_leverage)
round(album_data[album_data$large_leverage == TRUE,c("sales", "airplay", "attract", "adverts", "standardised_residuals", "cooks_distance", "leverage", "covariance_ratios")], digits = 2)

# Covariate ratio 1 +- (3*(k + 1)/n) - should be close to one for all cases
1 + (3*(3 + 1)/200) # Upper covariance ratio limit
1 - (3*(3 + 1)/200) # Lower covariance ratio limit
album_data$large_covariance <- as.numeric(album_data$covariance_ratios > 1 + (3*(3 + 1)/200) | album_data$covariance_ratios < 1 - (3*(3 + 1)/200))
sum(album_data$large_covariance)
round(album_data[album_data$large_covariance == TRUE, c("sales", "airplay", "attract", "adverts", "standardised_residuals", "cooks_distance", "leverage", "covariance_ratios")], digits = 2) 


# 5. Exercise 4 - Model Assumptions - Generalisability####
# Independent errors (no correlation of errors) - Durbin Watson Test - value should be around 2. Greater than 2 indicate negative correlation, less than 2 indicate positive correlation. Value greater than 3 or less than 1 should raise alarm bells.
dwt(album_lm_2)

# No Multicollinearity:
#Variance inflation factor should be less than 10
vif(album_lm_2)

# Tolerance statistics below 0.1 or 0.2 are a cause for concern
1/vif(album_lm_2)

# Average VIF should not be substantially greater than 1 - if it is, then the regression model may be biased
mean(vif(album_lm_2))

# Residual assumptions
plot(album_lm_2)
hist(album_data$standardised_residuals, 
     main = "Histogram of Standardised Residuals",
     xlab = "Standardised Residuals")