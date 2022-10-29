#importing .csv file to R
employee <- read.csv("BAN_602_Case_4.csv")
View(employee)

#Question1
#Summary statistics for Experience

tapply(employee$Salary, employee$Experience, summary) 

emp_exp <- employee %>%      #calculating variance and standard deviation                     
  group_by(Experience) %>%
  summarise_at(vars(Salary),
               list(sd = sd,var=var)) %>% 
  as.data.frame()
emp_exp

#summary statistics for sales position

tapply(employee$Salary, employee$Position, summary) 

emp_pos <- employee %>%      #calculating variance and standard deviation                     
  group_by(Position) %>%
  summarise_at(vars(Salary),
               list(sd = sd,var=var)) %>% 
  as.data.frame()
emp_pos 

#Question2

#95% confidence interval using t statistics
t.test(employee$Salary,conf.level = 0.95) 

#Question3
#anova to test significant diff in annual salary due to experience ignoring effect of position
anova3 <- aov(employee$Salary~employee$Experience, data=employee)
summary(anova3) 

#Question4
#anova to test diff in annual salary due to position ignoring effect of experience
anova4 <- aov(employee$Salary~employee$Position, data=employee)
summary(anova4)


#Question5
#anova for significant diff in annual salary due to position, experience and interaction
anova5 <- aov(employee$Salary~employee$Position*employee$Experience, data=employee)
summary(anova5)



