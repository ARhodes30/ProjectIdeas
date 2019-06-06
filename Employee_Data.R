
library(stringr)
library(tidyr)
library(tibble)
library(ggplot2)
library(dplyr)

Employee <- read_csv("Springboard/Projects Ideas/employee_data.csv")

View(Employee)

#Create dummy variables variables
Employee <- Employee %>%
  mutate(department_admin = ifelse(department == "admin", 1, 0)) %>%
  mutate(department_engineering = ifelse(department == "engineering", 1, 0)) %>%
  mutate(department_sales = ifelse(department == "sales", 1, 0)) %>%
  mutate(department_support = ifelse(department == "support", 1, 0))%>%
  mutate(department_finance = ifelse(department == "finance",1, 0))%>%
  mutate(department_IT = ifelse(department == "IT", 1, 0))%>%
  mutate(department_management = ifelse(department == "management", 1, 0))%>%
  mutate(department_marketing = ifelse(department =="marketing", 1, 0))%>%
  mutate(department_procurement = ifelse(department == "procurement", 1, 0))%>%
  mutate(department_product = ifelse(department =="product", 1, 0))%>%
  mutate(department_temp = ifelse(department == "temp", 1, 0))%>%
  mutate(department_information_technology = ifelse(department == "information_technology", 1, 0))


#Ssee how many missing values exist in filed complaint column
summary(Employee$filed_complaint)

#Replace NA's with 0 in filed_complaint column

Employee$filed_complaint[is.na(Employee$filed_complaint)] <- 0
Employee$recently_promoted[is.na(Employee$recently_promoted)] <- 0

#After finding the mean of last_evaluation. Replace NA with mean in last_evaluation column
Employee %>% 
  summarise(avg_evaluation = mean(last_evaluation, na.rm = TRUE))

Employee <- Employee %>% 
  replace_na(list(last_evaluation = .718))



# After finding mean of satisfaction. Replace NA with mean.
Employee %>% 
  summarise(avg_satisfaction = mean(satisfaction, na.rm = TRUE))


Employee <- Employee %>% 
  replace_na(list(satisfaction = .621))

#Create dummy variables for salary
Employee <- Employee %>%
  mutate(salary_low = ifelse(salary == "low", 1, 0))%>%
  mutate(salary_medium = ifelse(salary == "medium", 1, 0))%>%
  mutate(salary_high = ifelse(salary == "high", 1,0))

#Convert variables to factor variables
Employee$department <- as.factor(Employee$department)
Employee$filed_complaint <- as.factor(Employee$filed_complaint)
Employee$department <- as.factor(Employee$department)
class(Employee$department)
Employee$recently_promoted <- as.factor(Employee$recently_promoted)
Employee$salary <- as.factor(Employee$salary)
Employee$status <- as.factor(Employee$status)
Employee$tenure <- as.factor(Employee$tenure)
Employee$department_admin <- as.factor(Employee$department_admin)
Employee$department_engineering <- as.factor(Employee$department_engineering)
Employee$department_sales <- as.factor(Employee$department_sales)
Employee$department_admin <- as.factor(Employee$department_admin)
Employee$department_support <- as.factor(Employee$department_support)
Employee$department_finance <- as.factor(Employee$department_finance)
Employee$department_IT <- as.factor(Employee$department_IT)
Employee$department_management <- as.factor(Employee$department_management)
Employee$department_marketing <- as.factor(Employee$department_marketing)
Employee$department_procurement <- as.factor(Employee$department_procurement)
Employee$department_product <- as.factor(Employee$department_product)
Employee$department_temp <- as.factor(Employee$department_temp)
Employee$department_information_technology <- as.factor(Employee$department_information_technology)
Employee$salary_low <- as.factor(Employee$salary_low)
Employee$salary_medium <- as.factor(Employee$salary_medium)
Employee$salary_high <- as.factor(Employee$salary_high)



