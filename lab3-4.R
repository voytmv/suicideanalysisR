library(dplyr)
library(tidyverse)
library(gridExtra)

# Завдання 1
#(A) Визначити незалежну змінну 𝑥 та залежну – 𝑦 у вашому dataset. Ці змінні повинні бути
#числового типу. Вивести їх значення в ‘console’;


# Let's take our first variable for regression model: Bullied. It has quite high 
# correlation as we knew before so I suppose it's a nice idea to work a little bit more with it.

print(suicidal_behaviours$Bullied) # independent variable
print(suicidal_behaviour2$Attempted_suicide) # dependent variable

str(suicidal_behaviours$Bullied) # num
str(suicidal_behaviours$Attempted_suicide) # num


ggplot(suicidal_behaviours, aes(x = Bullied, y = Attempted_suicide)) +
  geom_point() +
  stat_smooth()
  

#(B) Знайти: 𝑥̅, 𝑦̅, 𝑉𝑎𝑟(X), 𝑉𝑎𝑟(Y), 𝐶𝑜𝑣(X,Y) відповідно до формул, які наведені вище;

Bullied <- suicidal_behaviours$Bullied
meanX <- mean(Bullied)
meanX
n <- nrow(suicidal_behaviours)
attempted_suicide <- suicidal_behaviours$Attempted_suicide
meanY <- mean(attempted_suicide)

# Варіація Х
varX <- 0
for(i in 1:n){
  varX = varX + (Bullied[i] - meanX)^2
}
varX = varX / n
varX

# Варіація У
varY <- 0
for(i in 1:n){
  varY = varY + (attempted_suicide[i] - meanY)^2
}
varY = varY / n
varY

# Коваріація
covXY <- 0
for(i in 1:n){
  covXY = covXY + (Bullied[i] - meanX)*(attempted_suicide[i] - meanY)
}
covXY = covXY / n
covXY

# Знайти параметри моделі Y = 𝑎 + 𝑏X використовуючи метод найменших квадратів;

beta_1 <- sum((Bullied - mean(Bullied)) * (attempted_suicide - mean(attempted_suicide))) / sum((Bullied - mean(Bullied))^2)

# обчислити beta_0_hat
beta_0 <- mean(attempted_suicide) - beta_1 * mean(Bullied)


linear_model <- lm(attempted_suicide ~ Bullied, data = suicidal_behaviours)
linear_model

# 0.8422 0.4394 значення отримані функцією lm

beta_1
beta_0

# значення співпадають - то є добре

plot(Bullied, attempted_suicide, main = "",
     xlab = "Bullied", ylab = "attempted_suicide",
     pch = 19, frame = FALSE)
abline(linear_model, col = "green")

summary(linear_model)

# ЛАБОРАТОРНА 4 -----------------------------------------------------


meanX <- mean(Bullied)
meanY <- mean(attempted_suicide)


# Визначити стандартне відхилення x_i та y_i
sdX <- sd(Bullied)
#sdX = 13.92934
sdY <- sd(attempted_suicide)
#sdY = 9.273621


#Визначити квантилі для x_i та y_i:

quantile(Bullied, probs = c(0.10, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9)) 
#15.250 22.075 25.200 27.550 29.800 40.025 48.050

quantile(attempted_suicide, probs = c(0.10, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9)) 
#5.800 9.400 10.900 11.850 14.000 18.425 23.900

cor(Bullied, attempted_suicide)

mod_summary <- summary(linear_model)

#Визначити SSR та TSS:
SSR <- sum(mod_summary$residuals^2)
TSS <- sum((attempted_suicide - mean(attempted_suicide))^2) 
SSR
#5096.565
TSS
#9030.004


# Визначити коефіцієнт детермінації 𝑅2 вручну та за допомогою функції summary() 
# і порівняти їх, а також зробити висновок результату про залежність між 𝑥𝑖 та 𝑦𝑖:
R2 <- 1 - SSR/TSS
R2
summary(linear_model)


# вручну:
SER <- sqrt(SSR / (n-2)) 
SER
#7.000388
#summary:
#Residual standard error: 7. Знову різниця в округленні.


#Завдання 2:
#Визначте стандартну похибку для оцінених коефіцієнтів 𝛽 та 𝛽 :
H_i <- 1 - mean(Bullied) / mean(Bullied^2) * Bullied
var_b0 <- var(H_i * mod_summary$residuals) / (n * mean(H_i^2)^2 )
var_b1 <- var( ( Bullied - mean(Bullied) ) * mod_summary$residuals) / (100 * var(Bullied)^2) 
var_b0
var_b1
#var_b0 = 6.994674
#var_b1 = 0.009954144

#Визначте для вашого набору чому дорівнює 𝑡кр:
qt(p = .05, df = 104, lower.tail=FALSE)
#1.659637
#Якщо дивитись по таблиці, то нам більше всього підходить 1.98



