library(dplyr)
library(tidyverse)
library(gridExtra)
library(AER)
library(stargazer)

# delete outliers
view(suicidal_behaviours)
suicidal_behaviours <- suicidal_behaviours[-c(71),] 
suicidal_behaviours <- suicidal_behaviours[-c(72),] 
suicidal_behaviours <- suicidal_behaviours[-c(18),] 
suicidal_behaviours <- suicidal_behaviours[-c(94),] 

summary(suicidal_behaviours$Bullied)
suicidal_behaviours$Binary_Have_Understanding_Parents  <- as.numeric(suicidal_behaviours$Have_Understanding_Parents >= 30)
suicidal_behaviours$Binary_Bullied <- as.numeric(suicidal_behaviours$Bullied >= 28)
suicidal_behaviours$Binary_Sex  <- as.numeric(suicidal_behaviours$Sex == "Male")

x1  <-suicidal_behaviours$Binary_Bullied
x2 <- suicidal_behaviours$Binary_Sex
y <- suicidal_behaviours$Attempted_suicide

# simple plot between Have_Understanding_Parents and Attempted_suicide includes Binary_Sex

plot(suicidal_behaviours$Bullied, suicidal_behaviours$Attempted_suicide,
     pch = 20,
     col = 1,
     main = "")

plot(suicidal_behaviours$Bullied,suicidal_behaviours$Attempted_suicide ,
     pch = 20,
     col = 3+suicidal_behaviours$Binary_Sex,
     main = "")
legend("topright", 
       pch = c(20, 20), 
       col = c("blue", "green"), 
       legend = c("Male", "Female"))


# the same two models 
# x1: suicidal_behaviours$Bullid
# x2: suicidal_behaviours$Binary_Sex
# y: suicidal_behaviours$Attempted_suicide


# Завдання 1: Взаємодія між двома бінарними змінними.

# y = b0 + b1D1 + b2D2
simple_bi_model <- lm(y ~ x1 + x2)
summary(simple_bi_model)


# y = b0 + b1*D1 + b2*D2 + b3(D1 x D2)
# однакові моделі які записані по різному
bi_model <- lm(y ~ x1 * x2)
bi_model_1 <- lm(y ~ x1 + x2 + x1 : x2)

summary(bi_model)
summary(bi_model_1)


# Завдання 2: Взаємодія між неперервною та бінарною змінною

# suicidal_behaviours$Have_Understanding_Parents - неперервна змінна

#	Y_i=β_0+β_1 X_i+β_2 D_i+u_i
model1 <- lm(y ~ suicidal_behaviours$Bullied + x2)
summary(model1)

#Y_i=β_0+β_1 X_i+β_2 D_i+β_3 (X_i×D_i )+u_i
model2 <- lm(y ~ suicidal_behaviours$Bullied + x2 + suicidal_behaviours$Bullied : x2)
summary(model2)

#Y_i=β_0+β_1 X_i+β_2 (X_i×D_i )+u_i
model3 <- lm(y ~ suicidal_behaviours$Bullied + suicidal_behaviours$Bullied : x2)
summary(model3)

# Y_i=β_0+β_1  ln⁡(X_i )+β_2 (ln⁡(X_i )×D_i )+u_i
model4 <- lm(y ~ log(suicidal_behaviours$Bullied) + log(suicidal_behaviours$Bullied) : x2)
summary(model4)

# ln⁡(Y_i )=β_0+β_1  ln⁡(X_i )+β_2 (ln⁡(X_i )×D_i )+u_i

model5 <- lm(log(y) ~ log(suicidal_behaviours$Bullied) + log(suicidal_behaviours$Bullied) : x2)
summary(model5)



summary(model5)


# b.	довірчі інтервали для коефіцієнтів моделі;
# гомоскетестичний розподіл
# -tкр. * Gbi + bi <= BetaI <= tкр. * Gbi + bi
# t кр = 1.98

b0Min = -1.98 * 0.34325  - 0.07131
b0Max = 1.98 * 0.34325  - 0.07131
b0Min
b0Max
# -0.750945 <= b0 <= 0.608325

b1Min = -1.98 * 0.10589 + 0.80126
b1Max = 1.98 * 0.10589 + 0.80126
b1Min
b1Max
# 0.5915978 <= b1 <= 1.010922

b2Min = -1.98 * 0.02626 - 0.06839
b2Max = 1.98 * 0.02626 - 0.06839
b2Min
b2Max
# -0.1203848 <= b2 <= -0.0163952


#  Знайдіть прогноз для медіанного значення x_i;  
#  log(y) = b0 + b1 * log(Xi) + b2*(log(Xi) : x2)
# Xi - median
# coefs - estimate coefficients
# di : 0 or 1

y0 = -0.07131 + 0.80126 * log(26.975)
y1 = -0.07131 + 0.80126 * log(26.975) - -0.06839 * log(26.975)
exp(y0)
exp(y1)
# 13.04976 - прогнозоване значення з бінарною змінною 0
# 16.34805 - прогнозоване значення з бінарною зміннною 1

summary(model5)

median(suicidal_behaviours$Bullied)
#26.975

# d.	побудуйте довірчий інтервал для прогнозу;
#  log(y) = b0 + b1 * log(Xi) + b2*(log(Xi) : x2)

# Xi - mediana

#26.975
# -0.750945 <= b0 <= 0.608325
# 0.5915978 <= b1 <= 1.010922
# -0.1203848 <= b2 <= -0.0163952

y0Min = -0.750945 + 0.5915978 * log(26.975) -0.1203848 *(log(26.975) * 1) #min
y0Max = 0.608325 + 1.010922 * log(26.975) -0.0163952*(log(26.975) * 1) # max

exp(y0Min)
exp(y0Max)
# 2.229238 < y < 48.67676  прогнозований інтервал для чоловіків

y1Min = -0.750945 + 0.5915978 * log(26.975) -0.1203848 *(log(26.975) * 0) #min
y1Max = 0.608325 + 1.010922 * log(26.975) -0.0163952*(log(26.975) * 0) # max

exp(y1Min)
exp(y1Max)

# 3.314533 < y < 51.37864 прогнозоване інтервал для жінок




# Передбачувані лінії регресії залежно від статі для моделі1

coefs <- model1$coefficients

coefs

id <- x2

par(mfrow = c(1, 1))
# нанесіть спостереження з HiEL = 0 у вигляді червоних крапок
plot(suicidal_behaviours$Bullied, suicidal_behaviours$Attempted_suicide,
     xlim = c(0, 70),
     ylim = c(0, 70),
     pch = 20,
     col = 2 + x2,
     main = "",
     xlab = "Bullied",
     ylab = "Attempted_suicide")
title(main="y = b0 + b1*Xi + b2*Di")


# накресліть передбачувану лінію регресії для HiEL = 0
abline(coef = c(coefs[1], coefs[2]),
       col = "red",
       lwd = 1.5)

# накресліть передбачувану лінію регресії для HiEL = 1
abline(coef = c(coefs[1] + coefs[3], coefs[2]),
       col = "green", 
       lwd = 1.5 )
legend("topright", 
       pch = c(20, 20), 
       col = c("red", "green"), 
       legend = c("FEMALE", "MALE"))

coefs <- model2$coefficients

coefs

id <- x2

par(mfrow = c(1, 1))
# нанесіть спостереження з HiEL = 0 у вигляді червоних крапок
plot(suicidal_behaviours$Bullied, y,
     xlim = c(0, 70),
     ylim = c(0, 70),
     pch = 20,
     col = 2 + x2,
     main = "",
     xlab = "Bullied",
     ylab = "Attempted_suicide")

title(main="y = b0 + b1*Xi + b2*Di + b3 * (Xi * Di)")

# накресліть передбачувану лінію регресії для HiEL = 0
abline(coef = c(coefs[1], coefs[2]),
       col = "red",
       lwd = 1.5)

# накресліть передбачувану лінію регресії для HiEL = 1
abline(coef = c(coefs[1] + coefs[3], coefs[2] + coefs[4]),
       col = "green", 
       lwd = 1.5 )
legend("topright", 
       pch = c(20, 20), 
       col = c("red", "green"), 
       legend = c("FEMALE", "MALE"))

coefs <- model3$coefficients

coefs

id <- x2

par(mfrow = c(1, 1))
# нанесіть спостереження з HiEL = 0 у вигляді червоних крапок
plot(suicidal_behaviours$Bullied, y,
     xlim = c(0, 70),
     ylim = c(0, 70),
     pch = 20,
     col = 2 + x2,
     main = "",
     xlab = "Bullied",
     ylab = "Attempted_suicide")

title(main="y = b0 + b1*Xi + b2*(Xi * Di)")

# накресліть передбачувану лінію регресії для HiEL = 0
abline(coef = c(coefs[1], coefs[2]),
       col = "red",
       lwd = 1.5)

# накресліть передбачувану лінію регресії для HiEL = 1
abline(coef = c(coefs[1], coefs[2] + coefs[3]),
       col = "green", 
       lwd = 1.5 )
legend("topright", 
       pch = c(20, 20), 
       col = c("red", "green"), 
       legend = c("FEMALE", "MALE"))

coefs <- model4$coefficients

coefs

id <- x2

par(mfrow = c(1, 1))
# нанесіть спостереження з HiEL = 0 у вигляді червоних крапок

plot(log(suicidal_behaviours$Bullied), y,
     xlim = c(0, 35),
     ylim = c(0, 35),
     pch = 20,
     col = 2 + x2,
     main = "",
     xlab = "Bullied",
     ylab = "Attempted_suicide")


title(main="y = b0 + b1*log(X)i + b2*(log(Xi) * Di)")

# накресліть передбачувану лінію регресії для HiEL = 0
abline(coef = c(coefs[1], coefs[2]),
       col = "red",
       lwd = 1.5)

# накресліть передбачувану лінію регресії для HiEL = 1
abline(coef = c(coefs[1], coefs[2] + coefs[3]),
       col = "green", 
       lwd = 1.5 )
legend("topright", 
       pch = c(20, 20), 
       col = c("red", "green"), 
       legend = c("FEMALE", "MALE"))


coefs <- model5$coefficients

coefs

id <- x2

par(mfrow = c(1, 1))
# нанесіть спостереження з HiEL = 0 у вигляді червоних крапок

plot(log(suicidal_behaviours$Bullied), log(y),
     xlim = c(0, 6),
     ylim = c(0, 6),
     main="",
     pch = 20,
     col=2 + x2,
     xlab = "Bullied",
     ylab = "Attempted_suicide")



title(main="log(y) = b0 + b1*log(X)i + b2*(log(Xi) * Di)")

# накресліть передбачувану лінію регресії для HiEL = 0
abline(coef = c(coefs[1], coefs[2]),
       col = "red",
       lwd = 1.5)

# накресліть передбачувану лінію регресії для HiEL = 1
abline(coef = c(coefs[1], coefs[2] + coefs[3]),
       col = "green", 
       lwd = 1.5 )
legend("topright", 
       pch = c(20, 20), 
       col = c("red", "green"), 
       legend = c("FEMALE", "MALE"))


# Завдання 3

x1 <- suicidal_behaviours$Bullied
x2 <- suicidal_behaviours$Got_Seriously_injured

##Завдання 3: Взаємодія між двома неперервними змінними 
##(A) Налаштувати 
##a. 𝑌𝑖 = 𝛽0 + 𝛽1𝑋1𝑖 + 𝛽2𝑋2𝑖 + 𝑢𝑖 
model_a <- lm(y ~ x1 + x2)

##b. 𝑌𝑖 = 𝛽0 + 𝛽1𝑋1𝑖 + 𝛽2𝑋2𝑖 + 𝛽3 (𝑋1𝑖 × 𝑋2𝑖 ) + 𝑢𝑖 
model_b <- lm(y ~ x1 + x2 + x1 : x2)

##c. 𝑌𝑖 = 𝛽0 + 𝛽1𝑋1𝑖 + 𝛽2𝑋1𝑖^2 + 𝛽3𝑋2𝑖 + 𝛽4 (𝑋1𝑖 × 𝑋2𝑖 ) + 𝛽5 (𝑋1𝑖 2 × 𝑋2𝑖 ) + 𝑢𝑖 
model_c <- lm(y ~ x1 + I(x1^2) + x2 + x1 : x2 + I(x1^2))

##d. 𝑌𝑖 = 𝛽0 + 𝛽1 ln(𝑋1𝑖 ) + 𝛽2𝑋2𝑖 + 𝛽3 (ln(𝑋1𝑖 ) × ln(𝑋2𝑖 )) + 𝑢𝑖 
model_d <- lm(y ~ log(x1) + x2 + log(x1) : log(x2))

##e. ln(𝑌𝑖 ) = 𝛽0 + 𝛽1 ln(𝑋1𝑖 ) + 𝛽2𝑋2𝑖 + 𝛽3 (ln(𝑋1𝑖 ) × ln(𝑋2𝑖 )) + 𝑢𝑖 
model_e <- lm(log(y) ~ log(x1) + x2 + log(x1) : log(x2))

##(B) Зробіть аналіз та виберіть найкращу:

summary(model_a)
summary(model_b)
summary(model_c)
summary(model_d)
summary(model_e)








