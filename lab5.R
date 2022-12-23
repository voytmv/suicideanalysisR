library(dplyr)
library(tidyverse)
library(gridExtra)
library(AER)
library(stargazer)



view(suicidal_behaviours)


# Завдання 2: Аналіз множинною регресії

# Побудувати лінійну модель (m1) за не менше ніж 5-ма параметрами

srl_injured = suicidal_behaviours$Got_Seriously_injured
use_marijuana = suicidal_behaviours$Use_Marijuana
sexual_relation = suicidal_behaviours$Had_sexual_relation
bullied = suicidal_behaviours$Bullied
close_friends = suicidal_behaviours$No_close_friends
attempted_suicide = suicidal_behaviours$Attempted_suicide

mult.mod <- lm(attempted_suicide ~ srl_injured + use_marijuana + sexual_relation + bullied + close_friends, data = suicidal_behaviours) 

# Визначити з summary() чому дорівнює RSE та порахувати вручну, а також перевірити чи вони співпадають.

summary(mult.mod)       

# RSE = 6.392 on 100 degrees of freedom

n <- nrow(suicidal_behaviours)
mod_summary <- summary(mult.mod)
SSR <- sum(mod_summary$residuals^2)

RSE <- sqrt(SSR / (n-2)) 
RSE
# 6.268126

# Значення похибки відрізняються на 0.1

# (C)	Створити модель (m2) в якої на 1-н параметр менше;

# Спробуємо побудувати ще одну множинну регресію, тільки на цей раз з 4 параметрами

mult.mod1 <- lm(attempted_suicide ~ srl_injured + use_marijuana + sexual_relation + close_friends, data = suicidal_behaviours) 

# Порівняти моделі (m1) та (m2) за допомогою функцій summary() та car::compareCoefs(m1, m2) на предмет: R^2, RSE, SE(β_i ). 
# Зробити висновок, яка модель краща.

summary(mult.mod1)   

car::compareCoefs(mult.mod, mult.mod1)

# В другій моделі вилучили фактор Bullied.Бачимо, що ефективнісит досить погіршилась,
# стандартна похибка збільшилась, Adjusted R Squared зменшився, тому перша модель краще



# Визначити t_кр для моделі (m1);

#	Визначити ступені вільності для (m1);


#Перевірити t-статистику для кожного з 5-ти коефіцієнтів моделі (m1);
  #Сформулювати гіпотези H_0 та H_1;
  #Вказати значення t-статистики (t-value) для відповідного коефіцієнта;
  #Значення p-значення (Pr(>|t|)) для відповідного коефіцієнта;
  #Вказати яка гіпотеза виконується;
  #Зробити графічне представлення;
  #Вказати довірчі інтервали для коефіцієнтів з рівнем надійності 95%, 90% та 99%;
  #Виконати масштабування (центрування) моделі (m2) та перевірити чи співпадають коефіцієнти β_1, β_2, β_3, β_4;



# Завдання 3: F-статистика

# (A)	Обчислити SST, SSR, SSE;

# визначити компоненти
n <- nrow(suicidal_behaviours)# кількість спостережень (рядків) 
k <- 5

y_mean <- mean(suicidal_behaviours$Attempted_suicide) # mean для середнього test-scores

SSR <- sum((fitted(mult.mod) - Csuicidal_behaviours$Attempted_suicide)^2) # сума квадратів залишків
SST <- sum((suicidal_behaviours$Attempted_suicide - y_mean )^2)# загальна сума квадратів
SSE <- sum((fitted(mult.mod) - y_mean)^2) # Пояснена сума квадратів

SER <- sqrt(1/(n-k-1) * SSR)  # standard error of the regression
Rsq <- 1 - (SSR / SST)# R^2
SSE/SST
adj_Rsq <- 1 - (n-1)/(n-k-1) * SSR/SST# adj. R^2

cof_F <- (SSE/k)/(SSR/(n-k-1))# (Rsq/k)/((1-Rsq)/(n-k-1))

# друк статистики в консоль
c("SER" = SER, "R2" = Rsq, "Adj.R2" = adj_Rsq)

#SER        R2    Adj.R2 
#6.3922595 0.5474976 0.5248725 

cof_F
# 24.19866

summary(mult.mod)



# -----------------

linear_model<- lm(suicidal_behaviours$Attempted_suicide ~ suicidal_behaviours$Bullied )
summary(linear_model)


plot( suicidal_behaviours$Bullied,suicidal_behaviours$Attempted_suicide ,
     col = "steelblue",
     pch = 20,
     xlab = "Bullied", 
     ylab = "Attemt",
     cex.main = 0.9)

# додати лінію регресії до графіка
abline(linear_model, 
       col = "red", 
       lwd = 2)

quadratic_model <- lm(suicidal_behaviours$Attempted_suicide  ~ suicidal_behaviours$Bullied + I(suicidal_behaviours$Bullied^2))
summary(quadratic_model)


order_id <- order(suicidal_behaviours$Bullied ) # сортування перед побудовою!

lines(x = suicidal_behaviours$Bullied [order_id], 
      y = fitted(quadratic_model)[order_id],
      col = "green", 
      lwd = 2)


new_data <- data.frame(Bullied  = c(23, 25))

# зробити передбачення
Y_hat <- predict(quadratic_model, new_data)

# обчислити різницю
diff(Y_hat)

view(suicidal_behaviours)




y1  <-suicidal_behaviours$Attempted_suicide
x4 <- suicidal_behaviours$Bullied
linear_model<- lm(y1 ~ x4, data = suicidal_behaviours)
summary(linear_model)

plot(x4, y1,
     col = "steelblue",
     pch = 20,
     xlab = "Bullied", 
     ylab = "Attempt",
     cex.main = 0.9,
     main = "Life Expectancy vs. Schooling and a Linear OLS Regression Function")

# додати лінію регресії до графіка
abline(linear_model, 
       col = "red", 
       lwd = 2)

quadratic_model <- lm(y1 ~ x4 + I(x4^2), data = suicidal_behaviours)
summary(quadratic_model)

order_id <- order(x4) # сортування перед побудовою!

lines(x = x4[order_id], 
      y = fitted(quadratic_model)[order_id],
      col = "green", 
      lwd = 2)

new_data <- data.frame(x4 = c(33, 35))

# зробити передбачення
Y_hat <- predict(quadratic_model, newdata = new_data)

# обчислити різницю
diff(Y_hat)


LogLog_model <- lm(log(y1) ~ log(x4))
summary(LogLog_model)

LinLog_model <- lm(y1 ~ log(x4))
summary(LinLog_model)

# надрукувати надійний підсумок коефіцієнтів на консоль
coeftest(LogLog_model, 
         vcov = vcovHC, type = "HC1")

order_id <- order(x4) # сортування перед побуд

lines(x = x4[order_id], 
      y = fitted(LogLog_model)[order_id],
      col = "blue", 
      lwd = 2)

lines(x = x4[order_id], 
      y = fitted(LinLog_model)[order_id],
      col = "yellow", 
      lwd = 2)
