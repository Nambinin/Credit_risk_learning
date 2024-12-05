rm(list = ls())

# load data
data <- read.csv("data_loans.csv")

#shows all column names
colnames(data)

#compute the defaults rate
y <- data$Defaults / data$Accounts

#b - fit a regression model
model1 <- lm(y ~ data$Earnings + data$Experience + data$HomeOwner)
summary(model1)

#c- comment on the association between loan default rates and Earnings/Experience/HomeOwner

#d -investigate the homoskedasticity assumption and potential impact of the potential outliers
plot( model1$residuals ~ model1$fitted.values )
plot(model1, which=4)

#e- Fit and refine and extended model to predict loan default rates in terms of the variables in the dataset
model2 <- lm(y ~ data$Earnings + data$Experience + data$Age)

#f -Compare the models obtained in b) and e)
anova(model1, model2)
