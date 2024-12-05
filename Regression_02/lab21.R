rm(list = ls())

data <- read.csv("data_loans.csv")
head(data)

# Compute de design matrix of the regression model of default rates against earnings and experience
sum(is.na(data)) #check the missing values

# show the different colomns names
colnames(data)

# Compute the default rate
#View(data)

earning <- data$Earnings
experience <- data$Experience


#n <- length(earning)
n <- nrow(data)

#b) design matrxi(X)
y <- data$Defaults / data$Accounts
ONES <- rep(1, n)
X <- matrix( c(ONES, earning, experience), ncol= 3)
View(X)

#c) least square estimate
beta_hat <- solve( t(X)%*%X ) %*% t(X) %*%y
print(beta_hat)

#d) prediction
beta_hat[1] + (beta_hat[2]*150000) + (beta_hat[3]*2)

#f
model1 <- lm(y~ earning + experience, data=data)

#g
coef(model1)

vcov(model1)