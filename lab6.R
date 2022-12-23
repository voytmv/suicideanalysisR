library(dplyr)
library(tidyverse)
library(gridExtra)
library(AER)
library(stargazer)

view(suicidal_behaviours)

y  <-suicidal_behaviours$Attempted_suicide
x <- suicidal_behaviours$Bullied

plot(x,y,col = "steelblue",ch = 20,xlab = "Percentage of Bullied", ylab = "Percentege of Attemted suicide",cex.main = 0.9)

# 1. Лінійна модель
linear_model<- lm(y ~ x )
summary(linear_model)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.84218    1.66442   0.506    0.614    
#x            0.43940    0.04905   8.959 1.41e-14 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 7 on 104 degrees of freedom
#Multiple R-squared:  0.4356,	Adjusted R-squared:  0.4302 
#F-statistic: 80.27 on 1 and 104 DF,  p-value: 1.414e-14


# додати лінію регресії до графіка
abline(linear_model,col = "red", lwd = 2)


#2. y = b0*(b1^x)
# Логарифмуємо та отримуємо: log y = log b0 + x * log b1

LogLin_model <- lm(log(y) ~ x) 
summary(LogLin_model)



exp(1.714091)
# 5.551627

exp(0.025633)
# 1.025964



#3 y = b0^(e^b1*x)
# Логарифмуємо та отримуємо: log y = log * b0 + b1 * x 

LogLin_model <- lm(log(y) ~ x) 
summary(LogLin_model)

#Residual standard error: 0.4522 on 104 degrees of freedom
#Multiple R-squared:  0.3863,	Adjusted R-squared:  0.3804 
#F-statistic: 65.47 on 1 and 104 DF,  p-value: 1.164e-12


# 4 y = e^(b0 + b1*x)
# Логарифмуємо та отримуємо: log y = b0 + b1 * x 

exp_model <- lm(log(y) ~ x)
summary(exp_model)

order_id <- order(x ) # сортування перед побудовою!


plot(x,y,col = "steelblue",ch = 20,xlab = "Percentage of Bullied", ylab = "Percentege of Attemted suicide",cex.main = 0.9)

lines(x = x [order_id],y = fitted(quadratic_model)[order_id],col = "purple", lwd = 2)




# 5 y = b0*x^b1 (степенева)
# Логарифмуємо та отримуємо: log y = log b0 + b1 * log x

LogLog_model <- lm(log(y) ~ log(x))
summary(LogLog_model)

exp(-0.22225)#0.8007152

#Residual standard error: 0.4474 on 104 degrees of freedom
#Multiple R-squared:  0.3991,	Adjusted R-squared:  0.3934 
#F-statistic: 69.08 on 1 and 104 DF,  p-value: 3.823e-13


# 6 y = b0 + b1*(1/x)

reverse_model <- lm(y ~ I(1/x))
summary(reverse_model)

# standard error: 8.003 on 104 degrees of freedom
# Multiple R-squared:  0.2624,	Adjusted R-squared:  0.2553 
# F-statistic:    37 on 1 and 104 DF,  p-value: 1.988e-08

# 7. Квадратична модель
quadratic_model <- lm(y  ~ x + I(x^2))
summary(quadratic_model)

#Residual standard error: 6.673 on 103 degrees of freedom
#Multiple R-squared:  0.492,	Adjusted R-squared:  0.4822 
#F-statistic: 49.88 on 2 and 103 DF,  p-value: 7.1e-16


# Для нашого випадку квадратична функція підходить найбільше

# Візуалізація, t-коефіцієнти та передбачення

order_id <- order(x ) # сортування перед побудовою!


plot(x,y,col = "steelblue",ch = 20,xlab = "Percentage of Bullied", ylab = "Percentege of Attemted suicide",cex.main = 0.9)

lines(x = x [order_id],y = fitted(quadratic_model)[order_id],col = "green", lwd = 2)

confint(quadratic_model)

new_data <- data.frame(x = c(33, 35))

# зробити передбачення
Y_hat <- predict(quadratic_model, newdata = new_data)

# обчислити різницю
diff(Y_hat) 
# 0.7455948


coeftest(quadratic_model, 
         vcov = vcovHC, type = "HC1")


#t test of coefficients:
  
#  Estimate Std. Error t value  Pr(>|t|)    
#(Intercept) 1.7140911  0.1182458 14.4960 < 2.2e-16 ***
#  x           0.0256326  0.0035207  7.2806 6.582e-11 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



