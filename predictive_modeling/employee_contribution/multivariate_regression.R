# Based on background attributes, what employees attain the highest yearly salary hike?
# What are the key predictors as to whether an employee will provide a valuable contribution? 

library(tidyverse)
# load data set
data <- read.csv("ibm_emps.csv", header=TRUE, sep=",", na.strings="?")
# delete records that contain na
data <- na.omit(data)

# feature engineering #
# convert monthly income into yrly income (thousands)
data <- data %>%
  mutate(yrly_income = (MonthlyIncome * 12) / 1000)
# create previous years of exp col
data <- data %>%
  mutate(prev_yrs_exp = TotalWorkingYears - YearsAtCompany)

# create initial age when joining the company
data <- data %>%
  mutate(firstday_age = Age - YearsAtCompany)

# create a # of previous companies col
data <- data %>%
  mutate(num_previous_companies = ifelse(NumCompaniesWorked == 0, 0, NumCompaniesWorked - 1))

# create a yearly % salary hike col
# need to adjust base index for years at company from 0 to 1
data <- data %>%
  mutate(years_at_company = YearsAtCompany + 1)
data <- data %>%
  mutate(yrly_salary_hike = PercentSalaryHike / years_at_company)

# convert education level 5 to 4
xtabs(~ PerformanceRating + Education, data=data)
data <- data %>%
  mutate(education_level = ifelse(Education == 1, 1,
                                  ifelse(Education == 2, 2,
                                         ifelse(Education == 3, 3,
                                                ifelse(Education == 4, 4,
                                                       ifelse(Education == 5, 4, NA))))))


# select relevant cols
model_data <- data %>%
  select(PercentSalaryHike, PerformanceRating, StockOptionLevel, yrly_income, yrly_salary_hike, 
         firstday_age, prev_yrs_exp, num_previous_companies,
        education_level, EducationField, Gender,
         JobLevel, JobRole, Department)

#get a feel for the data
attach(model_data)
summary(yrly_income)
summary(PercentSalaryHike)
summary(yrly_salary_hike)
summary(PerformanceRating) # only contains 3 or 4
xtabs(~ PerformanceRating + PercentSalaryHike) # correlation between PercentSalaryHike & Performance rating
xtabs(~ PerformanceRating + StockOptionLevel) # fairly evenly spread
xtabs(~ StockOptionLevel + PercentSalaryHike)
xtabs(~ StockOptionLevel + yrly_salary_hike)
# tag factor variables
xtabs(~ PerformanceRating + Gender) # factor
xtabs(~ PerformanceRating + EducationField) # factor
xtabs(~ PerformanceRating + education_level) # factor
xtabs(~ PerformanceRating + JobLevel) # factor
xtabs(~ PerformanceRating + JobRole) # factor
xtabs(~ PerformanceRating + Department) # factor

# check variable declarations
str(model_data)
# declare factors
model_data$Gender <- as.factor(model_data$Gender)
model_data$EducationField <- as.factor(model_data$EducationField)
model_data$education_level <- as.factor(model_data$education_level)
model_data$JobLevel <- as.factor(model_data$JobLevel)
model_data$JobRole <- as.factor(model_data$JobRole)
model_data$Department <- as.factor(model_data$Department)
str(model_data)

# correlation matrix
cor(model_data[, 4:8]) # first

# look at individual feature relationships with target variable
age_reg <- lm(yrly_salary_hike ~ firstday_age)
summary(age_reg) # entrance age plays a big role in increasing yearly salary hike

exp_reg <- lm(yrly_salary_hike ~ prev_yrs_exp)
summary(exp_reg) # very significant, positive relationship

prev_companies_reg <- lm(yrly_salary_hike ~ num_previous_companies)
summary(prev_companies_reg) # significant positive relationship


# throw all features in the model
ks_reg <- lm(yrly_salary_hike ~ Gender + firstday_age + EducationField + 
               education_level + prev_yrs_exp + num_previous_companies)
summary(ks_reg) # intersting Variables => Gender, firstday_age, education_level, prev_yrs_exp

firstcut_reg <- lm(yrly_salary_hike ~ Gender + firstday_age + education_level +
                   prev_yrs_exp + num_previous_companies)
summary(firstcut_reg)

secondcut_reg <- lm(yrly_salary_hike ~ Gender + firstday_age  + education_level)
summary(secondcut_reg)

thirdcut_reg <- lm(yrly_salary_hike ~ firstday_age  + education_level)
summary(thirdcut_reg)

# Is there an interaction between first day age and education level?
inter_reg <- lm(yrly_salary_hike ~ Gender + firstday_age  * education_level)
summary(inter_reg)

# is there a curivlinear relationship for first day age?
firstday_age_sqd <- firstday_age^2
curvil_reg <- lm(yrly_salary_hike ~ Gender + firstday_age  + firstday_age_sqd + education_level)
summary(curvil_reg)

# Anovas
gender_anov <- aov(yrly_salary_hike ~ Gender)
anova(gender_anov)

edu_level_anov <- aov(yrly_salary_hike ~ education_level)
anova(edu_level_anov)

# plot relationship of response variable to our predictor variables
# test assumptions of selected model
secondcut_reg.stres <- rstandard(secondcut_reg) 
plot(yrly_salary_hike, secondcut_reg.stres, pch = 16, main = "Standardized Residual Plot", 
     xlab = "% avg yearly salary hike", ylab = "Standardized Residuals") 
abline(0,0, lty=2, col="red") # severe heteroscedasticity

h <- hist(secondcut_reg.stres)
x <- secondcut_reg.stres
xfit <- seq(min(x), max(x), length = 40) 
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue")

qqnorm(secondcut_reg.stres, main = "Normal Probability Plot", xlab = "Normal Scores", 
       ylab = "Standardized Residuals") 
qqline(secondcut_reg.stres, col = "red")

shapiro.test(secondcut_reg.stres) # evidence that the data is not normally distributed


plot(firstday_age, yrly_salary_hike, pch = 16, main = "Salary Hike by Age", xlab = "Age When Hired", 
     ylab = "Avg Yearly Salary Hike") 
abline(lm(yrly_salary_hike ~ firstday_age), lty=2, col="red")
