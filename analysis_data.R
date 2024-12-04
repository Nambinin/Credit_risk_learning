#a - Load csv file
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

#b - estimate correlation
cor_x_y <- covar_x_y / (var_x*var_y)^0.5
cor_x_y

#check with function cor()
cor(x, y)

#c- Estimate the slope
beta_1 <- sum( (x - x_bar)* y)/ sum( (x - x_bar)^2)

#estimate the intercept
beta_zero <- y_bar-(beta_1 * x_bar)

beta_1 ;beta_zero

#d- relationship between estimated slope and correlation coefficient
correlation = beta_1 * ((var_x^0.5) / (var_y^0.5))
correlation
cor_x_y

#e - estimate the variance parameter sigma
sigma2 <- sum( (y-beta_zero-(beta_1*x))^2 ) / (n-2)
sigma2

#f - Estimate the covariance matrix

var_beta_1 <- sigma2 / sum((x - x_bar)^2)
var_beta_0 <- sigma2 * sum(x^2) / n* sum((x - x_bar)^2)
covar_beta <- -x_bar * sigma2 / sum((x - x_bar)^2)

cov_matrix <- matrix(
  c(var_beta_0, covar_beta,
    covar_beta, var_beta_1),
  nrow = 2, byrow = TRUE
)
cov_matrix

#g - i - Estimate the expected default rate for this applicant
y_new <- beta_zero + (beta_1 * 286510)
y_new

#g- ii - Estimate the standard error around this expected default rate
SE <- var(beta_zero+beta_1*x)
SE

