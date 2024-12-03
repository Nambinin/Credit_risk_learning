#Load csv file
data <- read.csv("DataDefaults.csv")

#show the head of the data
head(data)

#show the differents colonnes's name

colnames(data)
Linear
x <- data$Earnings
#Accounts <- data$Accounts
y <- data$Defaults

n <- length(x)
x_bar <- mean(x)
y_bar <- mean(y)

var_x <- sum( (x-x_bar)^2 ) / (n-1)
var_y <- sum( (y-y_bar)^2 ) / (n-1)
covar_x_y <- sum( (x-x_bar)*(y-y_bar) )/ (n-1)

cor_x_y <- covar_x_y / (var_x*var_y)^0.5
cor_x_y

cor(x, y)

# Estimate the slope
beta_1 <- sum( (x - x_bar)* y_bar)/ sum( (x - x_bar)^2)
beta1 <- covar_x_y / var_x
beta_1
beta1

sd_x <- sqrt(var_x)
sd_y <- sqrt(var_y)
beta1_check <- cor_x_y * (sd_y / sd_x)
all.equal(beta1, beta1_check)  # Should return TRUE


#estimate the intercept
beta_zero <- y_bar-(beta_1 * x_bar)

beta_1
beta_zero

#estimate the variance parameter sigma
y_hat <- beta_zero + beta_1 * x

sigma2 <- sum( (y-beta_zero-(beta_1*x))^2 ) / (n-2)
sigma2


#model using lm() function
model <- lm(Earnings ~ defaults)

plot(defaults, Earnings,xlab = "x", ylab="y", pch=19, col="blue")
abline(model, col="red", lwd=2)
