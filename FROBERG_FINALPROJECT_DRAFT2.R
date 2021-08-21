# Final Project Report 

library(dplyr)
library(plyr)
library(car)
library(lsmeans)

suicide <- read.csv("C:\\Users\\Travis Froberg\\Documents\\AA_MTU Graduate School\\Exerimental Design Course\\master.csv", header = TRUE,  stringsAsFactors = FALSE)
View(suicide)
attach(suicide)
names(suicide)

class(ï..country)
suicide2 <- suicide[c(589:960, 8739:9086, 26849:27220),]
View(suicide2)

suicide3 <- suicide2[ , c("ï..country", "age", "suicides.100k.pop")]
View(suicide3)
write.csv(suicide3, "C:\\Users\\Travis Froberg\\Documents\\AA_MTU Graduate School\\suicide.csv")

names(suicide3) <- c("Country", "Age", "Suicide_Rate" )
View(suicide3)
attach(suicide3)

suicide4 <- within(data = suicide3, {
  Age <- revalue(Age, c("5-14 years"=1, "15-24 years" = 2, "25-34 years" = 3, "35-54 years" = 4, "55-74 years" = 5, "75+ years" = 6))
  Age <- as.integer(Age)
  Country = unlist(Country)
  Country <- revalue(Country, c("Argentina"=1, "Finland"= 2, "United States" = 3))
  Country <- as.integer(Country)
  Treatment <- factor(as.numeric(factor(paste(suicide4$Country, suicide4$Age, sep = ""))))
  Possible_Ordering <- c(1:1092)
})
str(suicide4)
View(suicide4)
nrow(suicide4)

# Exploratory Analysis 

interaction.plot(x.factor = suicide4$Age, trace.factor = suicide4$Country, response = suicide4$Suicide_Rate, 
                 type = "b", xlab = "Age Range Group", pch = c(1, 2, 3), lty = c(2, 3, 4), col = c(2, 4, 6), # yaxt = "n", 
                 legend = FALSE, ylab = "Mean of Response", main = "Interaction Plot")
legend(x = 3, y = 7, legend = c("Argentina", "Finland", "United States"), pch = c(1, 2, 3), lty = c(2, 3, 4), col = c(2, 4, 6))

# ANOVA of two-way complete model

suicide.lm <- aov(Suicide_Rate ~ factor(Country) + factor(Age) + factor(Country):factor(Age) , data = suicide4)
anova(suicide.lm)
capture.output(summary(suicide.lm),file="test.xls")

# two-way complete model for overall analysis
suicide.0 <- aov(Suicide_Rate ~ 1, data = suicide4)
anova(suicide.0, suicide.lm)

# Checking Assumptions 
# I created the following cell-means model because it is the same as the two-way complete model
# and it is easier to work with when checking assumptions 
par(mfrow = c(2, 2))
suicide.lm <- aov(Suicide_Rate ~ factor(Treatment), data = suicide4)

# get standardized rasidauls
suicide.z <- resid(suicide.lm) / sd(resid(suicide.lm))
# checking model fitting and outliers
plot(suicide.z ~ suicide4$Treatment, xlab = "Treatment Levels", ylab = "Standardized Residuals", 
     main = "Residual Plot - Suicide Rate")
lines(x = c(-1, 20), y = c(0, 0))

# Could not check independnece because I do not know the order in which the data was collected
# However, assuming that the data points were collected in the order of the original data, the following 
# plot can be used to check independence 
plot(suicide.z ~ suicide4$Possible_Ordering, xlab = "Run Order", ylab = "Standardized Residuals", 
     main = "Residual Plot for Ordered Observations")
lines(x = c(-200, 1300), y = c(0, 0))

# checking equal variance assumption using plot
suicide.p <- fitted(suicide.lm)
plot(suicide.z ~ suicide.p, xlab = "Predcited Values", ylab = "Standardized Residuals", 
     main = "Residual Plot - Suicide Rates") 

# checking equal variance assumption using sample variance 
leveneTest(Suicide_Rate ~ Treatment, data = suicide4, center = mean)
# anova(aov(abs(bean.z) ~ factor(bean$Trtmt)))

# check for normality
n <- length(suicide.z)
suicide.z.sorted <- sort(suicide.z)
suicide.ns <- qnorm((1:n - 0.375) / (n + 0.25)) 	# normal scores
plot(suicide.z.sorted ~ suicide.ns, xlab = "Predcited Values", ylab = "Standardized Residuals", 
     main = "Normal Q-Q Plot - Suicide Rates") 
lines(x = c(-4, 4), y = c(-4, 4))
# check for normality with R functions qqnorm and qqline
qqnorm(y = suicide.z.sorted, ylab = "Residual Quantiles", main = "Normal Q-Q Plot - Suicide Rates")
qqline(y = suicide.z.sorted)

# Transforming the data to achieve equality of variances 
# find q in transformation
suicide.gm <- tapply(X = suicide4$Suicide_Rate, INDEX = suicide4$Treatment, FUN = mean)
suicide.gv <- tapply(X = suicide4$Suicide_Rate, INDEX = suicide4$Treatment, FUN = var)
summary(lm(log(suicide.gv) ~ log(suicide.gm)))
# From above program it is determined that q = 2.1237 which is rounded to 2 
# none of the values for the response variable are 0, so the natural log 
# will be taken of the response to transform the data
# In the following code, the transformation does not take place yet. To experiment with 
# new transformations run the following code 

suicide4 <- within(data = suicide3, {
  Age <- revalue(Age, c("5-14 years"=1, "15-24 years" = 2, "25-34 years" = 3, "35-54 years" = 4, "55-74 years" = 5, "75+ years" = 6))
  Age <- as.integer(Age)
  Country = unlist(Country)
  Country <- revalue(Country, c("Argentina"=1, "Finland"= 2, "United States" = 3))
  Country <- as.integer(Country)
  Treatment <- factor(as.numeric(factor(paste(suicide4$Country, suicide4$Age, sep = ""))))
  Possible_Ordering <- c(1:1092)
  # Suicide_Rate <- sqrt(Suicide_Rate)
})

# Contrasts 

# pairwise comparison of treament combinations with lsmeans and contrast

suicide.lm <- aov(Suicide_Rate ~ factor(Treatment), data = suicide4)

options(max.print = 2000)
suicide.lm.tr <- lsmeans(suicide.lm, ~ Treatment) 
summary(contrast(object = suicide.lm.tr, method = "pairwise", adjust = "none"),  level = 0.95, side = "two-sided", infer = c(T, T))
summary(contrast(object = suicide.lm.tr, method = "pairwise", adjust = "Bonferroni"),  level = 0.95, side = "two-sided", infer = c(T, T))
summary(contrast(object = suicide.lm.tr, method = "pairwise", adjust = "Scheffe"),  level = 0.95, side = "two-sided", infer = c(T, T))
summary(contrast(object = suicide.lm.tr, method = "pairwise", adjust = "Tukey"),  level = 0.95, side = "two-sided", infer = c(T, T))

