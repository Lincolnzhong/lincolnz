setwd("C:/Data for 31002")
data <- read.csv("hw4_data.csv")
#15
num_participants <- nrow(data)
num_employment_group <- sum(data$employment_group)
num_cash_group <- sum(data$cash_group)
num_control_group <- sum(data$control_group)
#16
#age
mean(data$age[data$employment_group == 1])
sd(data$age[data$employment_group == 1])

mean(data$age[data$cash_group == 1])
sd(data$age[data$cash_group == 1])

mean(data$age[data$control_group == 1])
sd(data$age[data$control_group == 1])

#Being married
mean(data$marry_dum[data$employment_group == 1])
sd(data$marry_dum[data$employment_group == 1])

mean(data$marry_dum[data$cash_group == 1])
sd(data$marry_dum[data$cash_group == 1])

mean(data$marry_dum[data$control_group == 1])
sd(data$marry_dum[data$control_group == 1])
#17
t.test(data$age[data$employment_group == 1], data$age[data$control_group == 1])
t.test(data$marry_dum[data$employment_group == 1], data$marry_dum[data$control_group == 1])
#18
t.test(data$b_mental_health_index[data$employment_group == 1], data$b_mental_health_index[data$control_group == 1])
t.test(data$b_mental_health_index[data$cash_group == 1], data$b_mental_health_index[data$control_group == 1])
t.test(data$b_mental_health_index[data$employment_group == 1], data$b_mental_health_index[data$cash_group == 1])
#19
reg1 <- lm(data$e_mental_health_index ~ data$employment_group+data$cash_group+data$b_mental_health_index)
summary(reg1)