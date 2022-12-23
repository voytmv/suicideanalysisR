# (1) Оцініть кількісні характеристики 
    # 1. Кількість екзкмплярів - 106
    # 2. Залежні змінні - 16
    # 3. Незалежні змінні - 1
# (2) Опишіть атрибути
#   - Country 
#   - Year
#   - Age group
#   - Sex (Male/Female)
#   - Currently_drink_Alcohol(Percentage of students who currently drank alcohol 
#     at least one drink of alcohol on at least one day during the 30 days before the survey)
#   - Really_get_drunk(Percentage of students who ever drank so much alcohol that they were
#      really drunk one or more times during their life )
#   - Overweight(Percentage of students who were overweight (>+1SD from median for BMI by age and sex))
#   - Use_marijuana(Percentage of students who ever used marijuana one or more times during their life)
#   - Have_understanding_parents(Percentage of students who reported that their parents or guardians 
#     most of the time or always understood their problems
#     and worries during the 30 days before the survey )
#   - Missed_classes_without_permision
#   - Had_sexual_relation
#   - Smole_cig_currently
#   - Had fights
#   - Bullied
#   - Got_seriously_injured
#   - No_close_friends
#   - Attempted_suicide(Percentage of students who attempted suicide 
#     one or more times during the 12 months before the survey)


# (3) Постановка задачі

# Передбачити: Percentage of students who attempted
# suicide one or more times during the 12 months before the survey 
# враховуючи всі незалежні змінні

library(readr)
library(tidyverse)

library(tibble)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(rpart)

suicidal_behaviours <- read_csv("GHSH_Pooled_Data1.csv")

str(suicidal_behaviours)

# (4) Типи атрибутів

# 1.Categorical, nominal data
# 2. Numerical, discrete data
# 3. Categorical, ordinal data
# 4 Categorical, nominal
# 5 - 17. Numerical, continuous data

cor(suicidal_behaviours$Currently_Drink_Alcohol,
    suicidal_behaviours$Attempted_suicide)
#0.09873102

cor(suicidal_behaviours$Really_Get_Drunk,
    suicidal_behaviours$Attempted_suicide)
#0.2356455

cor(suicidal_behaviours$Overwieght,
    suicidal_behaviours$Attempted_suicide)
#0.2881137

cor(suicidal_behaviours$Use_Marijuana,
    suicidal_behaviours$Attempted_suicide)
#0.4030619

cor(suicidal_behaviours$Have_Understanding_Parents,
    suicidal_behaviours$Attempted_suicide)
#-0.08100106

cor(suicidal_behaviours$Missed_classes_without_permssion,
    suicidal_behaviours$Attempted_suicide)
#0.3415898

cor(suicidal_behaviours$Had_sexual_relation,
    suicidal_behaviours$Attempted_suicide)
#0.4058015

cor(suicidal_behaviours$Smoke_cig_currently,
    suicidal_behaviours$Attempted_suicide)
#0.2737612

cor(suicidal_behaviours$Had_fights,
    suicidal_behaviours$Attempted_suicide)
#0.3382329

cor(suicidal_behaviours$Bullied,
    suicidal_behaviours$Attempted_suicide)
#0.6638911

cor(suicidal_behaviours$Got_Seriously_injured,
    suicidal_behaviours$Attempted_suicide)
#0.4936414

cor(suicidal_behaviours$No_close_friends,
    suicidal_behaviours$Attempted_suicide)
#0.4525152
# Load dplyr
library(dplyr)

# group_by() on Country
group_by_country <-suicidal_behaviours  %>% group_by(Country)
View(group_by_country)

# summarise on groupped data.
group_by_county_with_mean <- group_by_country %>% summarise(mean(Attempted_suicide)) 
View(group_by_county_with_mean )

library(ggplot2)

colnames(group_by_county_with_mean )[1] ="Country"
colnames(group_by_county_with_mean )[2] ="Attempted_suicide"
View(group_by_county_with_mean )

group_by_county_with_mean$Country <- as.factor(group_by_county_with_mean$Country)

plot(group_by_county_with_mean$Country,group_by_county_with_mean$Attempted_suicide)

# (5) Для кожного атрибуту використати дослідний аналіз

summary.data.frame(suicidal_behaviours)

## here should be more graphs
suicidal_behaviours$Country <- as.factor(suicidal_behaviours$Country)
suicidal_behaviours$`Age Group` <- as.factor(suicidal_behaviours$`Age Group`)
suicidal_behaviours$Sex <- as.factor(suicidal_behaviours$Sex)

View(suicidal_behaviours)

plot(suicidal_behaviours$Country, suicidal_behaviours$Attempted_suicide)

plot(suicidal_behaviours$Year, suicidal_behaviours$Attempted_suicide)

plot(suicidal_behaviours$`Age Group`, suicidal_behaviours$Attempted_suicide)

plot(suicidal_behaviours$Sex, suicidal_behaviours$Attempted_suicide)


# (6) Встановіть цілі для попередньої обробки даних. (це може бути: змінити тип, створити нову змінну, виконати
#     групування, визначити важливі та неважливі атрибути, витягнути з атрибуту не пряму інформацію)

# 1. Deal with null data
# 2. Make some intervals variables and after can apply group by function
# 3. Get rid of none important variables
# 4. Look at the data in a different angle
# 5. Do more group by operations


# Лабораторна робота 2

# 1. Виконати заміну типу в атрибутах з неприйнятним типом;

suicidal_behaviours$Country <- factor(suicidal_behaviours$Country)
suicidal_behaviours$Sex <- factor(suicidal_behaviours$Sex)
suicidal_behaviours$`Age Group` <- factor(suicidal_behaviours$`Age Group`)

is.factor(suicidal_behaviours$Country)
is.factor(suicidal_behaviours$Sex)
is.factor(suicidal_behaviours$`Age Group`)

str(suicidal_behaviours)


# 2.Представте графічно зв'язок залежності кожного атрибута 
# від залежної змінної в кількісному та відсотковому значенні


summary.data.frame(suicidal_behaviours)

# make our y variables into two intervals to use it like binary variable
suicidal_behaviours$Suicide_intervals <- ifelse((suicidal_behaviours$Attempted_suicide > 12), 1, 0)



p1 <- suicidal_behaviours %>%
  ggplot(aes(x = factor(Suicide_intervals), fill = factor(Currently_Drink_Alcohol))) +
  geom_bar(width = 0.5) +
  xlab("Currently_Drink_Alcohol") +
  ylab("Загальна кількість") +
  labs(fill = "Suicide_intervals")+
  ggtitle("")

p2 <- suicidal_behaviours %>%
  ggplot(aes(x = factor(Suicide_intervals), fill = factor(Currently_Drink_Alcohol))) + 
  geom_bar(position = "fill", width = 0.5) + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Suicide_intervals") +
  labs(x = "Currently_Drink_Alcohol", y = "Percenatge") + 
  ggtitle("")

grid.arrange(p1, p2, ncol = 2)
p3 <- suicidal_behaviours %>%
  ggplot(aes(x = factor(Suicide_intervals), fill = factor(Really_Get_Drunk))) +
  geom_bar(width = 0.5) +
  xlab("Really_Get_Drunk") +
  ylab("Загальна кількість") +
  labs(fill = "Suicide_intervals")+
  ggtitle("")

p4 <- suicidal_behaviours %>%
  ggplot(aes(x = factor(Suicide_intervals), fill = factor(Really_Get_Drunk))) + 
  geom_bar(position = "fill", width = 0.5) + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Suicide_intervals") +
  labs(x = "Really_Get_Drunk", y = "Percenatge") + 
  ggtitle("")

grid.arrange(p3, p4, ncol = 2)

p5 <- suicidal_behaviours %>%
  ggplot(aes(x = factor(Suicide_intervals), fill = factor(Sex))) +
  geom_bar(width = 0.5) +
  xlab("%TryToSuicide ") +
  ylab("Загальна кількість") +
  labs(fill = "Sex")+
  ggtitle("")

p6 <- suicidal_behaviours %>%
  ggplot(aes(x = factor(Suicide_intervals), fill = factor(Sex))) + 
  geom_bar(position = "fill", width = 0.5) + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Sex") +
  labs(x = "%TryToSuicide ", y = "Percenatge") + 
  ggtitle("")

grid.arrange(p5, p6, ncol = 2)



p7 <- suicidal_behaviours %>%
  ggplot(aes(x = factor(Suicide_intervals), fill = factor(`Age Group`))) +
  geom_bar(width = 0.5) +
  xlab("%TryToSuicide ") +
  ylab("Загальна кількість") +
  labs(fill = "Age group")+
  ggtitle("")

p8 <- suicidal_behaviours %>%
  ggplot(aes(x = factor(Suicide_intervals), fill = factor(`Age Group`))) + 
  geom_bar(position = "fill", width = 0.5) + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Age group") +
  labs(x = "%TryToSuicide ", y = "Percenatge") + 
  ggtitle("")




p9 <- suicidal_behaviours %>%
  ggplot(aes(x = factor(Attempted_suicide), fill =(No_close_friends))) + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Age group") +
  labs(x = "%TryToSuicide ", y = "Percenatge") + 
  ggtitle("")
grid.arrange(p9, ncol = 1)

simpe_ggplot_function <- function(x,y,x_title,y_title){
  suicidal_behaviours %>%
  ggplot(aes(x =x, fill = y)) +
  geom_bar(width = 0.3) +
  xlab(x_title) +
  ylab(y_title)
}  

simpe_ggplot_function(suicidal_behaviours$No_close_friends,suicidal_behaviours$Attempted_suicide,"Currently_Drink_Alcohol","Attempted_suicide")
simpe_ggplot_function(suicidal_behaviours$Overwieght,suicidal_behaviours$Attempted_suicide,"Overwieght","Attempted_suicide")
simpe_ggplot_function(suicidal_behaviours$Missed_classes_without_permssion,suicidal_behaviours$Attempted_suicide,"Missed_classes_without_permssion","Attempted_suicide")
# Більшість змінних в нас наперервіні тому візуалізація на даному етапі є не зовсім коректна.

# 3 Для пропущених значень запропонуйте та виконайте дозаповнення даних (якщо їх багато, то не менше ніж для 3-х)

is.na(suicidal_behaviours)

missing_vars <- function(x) {
  var <- 0
  missing <- 0
  missing_prop <- 0
  for (i in 1:length(names(x))) {
    var[i] <- names(x)[i]
    missing[i] <- sum(is.na(x[, i])|x[, i] =="" )
    missing_prop[i] <- missing[i] / nrow(x)
  }
  # order   
  missing_data <- data.frame(var = var, missing = missing, missing_prop = missing_prop) %>% 
    arrange(desc(missing_prop))
  # print out
  missing_data
}

missing_vars(suicidal_behaviours)


# Спробуємо заповнити пропущені значення відповідною медіаною, взятою з усього датасету

summary.data.frame(suicidal_behaviours)

# 27.55 - медіана для Bullied
# 12.60 - медіана для Smoke_cig_currently


suicidal_behaviours$Bullied <- ifelse((is.na(suicidal_behaviours$Bullied) == TRUE), 27.55,suicidal_behaviours$Bullied)
suicidal_behaviours$Smoke_cig_currently <- ifelse((is.na(suicidal_behaviours$Smoke_cig_currently) == TRUE), 12.60,suicidal_behaviours$Smoke_cig_currently)

missing_vars(suicidal_behaviours)


write.table(suicidal_behaviours, file = "suicidal_behaviours2.csv",
            sep = "\t", row.names = F)


#5 Виконайте для даних які потребують групування або перетворення відповідні заміни


group_by_age <-suicidal_behaviours  %>% group_by(`Age Group`)
group_by_age_with_mean <- group_by_age %>% summarise(mean(Attempted_suicide)) 
                                                     
group_by_sex <-suicidal_behaviours  %>% group_by(Sex)
group_by_sex_with_mean <- group_by_sex %>% summarise(mean(Attempted_suicide))



p5 <- suicidal_behaviours %>%
  ggplot(aes(x = factor(Suicide_intervals), fill = factor(Sex))) +
  geom_bar(width = 0.5) +
  xlab("%TryToSuicide ") +
  ylab("Загальна кількість") +
  labs(fill = "Sex")+
  ggtitle("")

p6 <- suicidal_behaviours %>%
  ggplot(aes(x = factor(Suicide_intervals), fill = factor(Sex))) + 
  geom_bar(position = "fill", width = 0.5) + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Sex") +
  labs(x = "%TryToSuicide ", y = "Percenatge") + 
  ggtitle("")

grid.arrange(p5, p6, ncol = 2)



p7 <- suicidal_behaviours %>%
  ggplot(aes(x = factor(Suicide_intervals), fill = factor(`Age Group`))) +
  geom_bar(width = 0.5) +
  xlab("%TryToSuicide ") +
  ylab("Загальна кількість") +
  labs(fill = "Age group")+
  ggtitle("")

p8 <- suicidal_behaviours %>%
  ggplot(aes(x = factor(Suicide_intervals), fill = factor(`Age Group`))) + 
  geom_bar(position = "fill", width = 0.5) + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Age group") +
  labs(x = "%TryToSuicide ", y = "Percenatge") + 
  ggtitle("")


grid.arrange(p7, p8, ncol = 2)





