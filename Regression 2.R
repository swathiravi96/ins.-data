#####Regression Methods -------------------

#### Linear Regression -------------------



##  Predicting Medical Expenses ----
##  Exploring and preparing the data ----

#The file includes 1,338 examples of beneficiaries currently enrolled
#in the insurance plan, with features indicating characteristics of the patient as well as
#the total medical expenses charged to the plan for the calendar year.

insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)

# summarize the charges variable

#Our model's dependent variable is expenses, which measures the medical costs
#each person charged to the insurance plan for the year.

summary(insurance$expenses)

# histogram of insurance charges
#Because the mean value is greater than the median, this implies that the distribution of
#insurance expenses is right-skewed.
#the figure shows a right-skewed distribution.
#It also shows that the majority of people in our data
#have yearly medical expenses between zero and $15,000

hist(insurance$expenses)

# table of region

#Regression models require that every feature is numeric, 
#yet we have three factor-type features in our data frame. 
#For instance, the sex variable is divided into male and female levels, while
#smoker is divided into yes and no. From the summary() output, we know that the
#region variable has four levels,
table(insurance$region)

# exploring relationships among features: correlation matrix
#Before fitting a regression model to data, 
#it can be useful to determine how the
#independent variables are related to the dependent variable and each other.
#A correlation matrix provides a quick overview of these relationships.

#age and bmi appear to have a weak positive
#correlation, meaning that as someone ages, 
#their body mass tends to increase. 
#There is also a moderate positive correlation between age and expenses,
#bmi and expenses, and children and expenses. 
#These associations imply that as age, body mass, and
#number of children increase, the expected cost of insurance goes up.

cor(insurance[c("age", "bmi", "children", "expenses")])

# visualing relationships among features: scatterplot matrix

#scatterplot matrix ,
#which is simply a collection of scatterplots arranged in a grid. 
#It is used to detect patterns among three or more variables.
# We create a scatterplot matrix for the four numeric features: 
#age, bmi, children, and expenses.

pairs(insurance[c("age", "bmi", "children", "expenses")])

# more informative scatterplot matrix

library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])

##Training a model on the data ----

#The following command fits a linear regression
#model relating the six independent variables to the total medical expenses.

ins_model <- lm(expenses ~ age + children + bmi + sex + smoker + region,
                data = insurance)


# see the estimated beta coefficients
#The intercept is the predicted value of expenses when the independent 
#variables are equal to zero.
#For example, since no person exists with age zero and BMI zero, 
#the intercept has no real-world interpretation.

ins_model

## Step 4: Evaluating model performance ----
# see more detail about the estimated beta coefficients
summary(ins_model)

## Step 5: Improving model performance ----
#the output provides three key ways to evaluate the performance, 
#or fit, of our model.


# add a higher-order "age" term

#To add the non-linear age to the model,
#we simply need to create a new variable
#when we produce our improved model, we'll add both age and age2 to the
#lm() formula using the expenses ~ age + age2 form. This will allow the model
#to separate the linear and non-linear impact of age on medical expenses.

insurance$age2 <- insurance$age^2

# add an indicator for BMI >= 30
#We can then include the bmi30 variable in our improved model, either replacing the
#original bmi variable or in addition, depending on whether or not we think the effect
#of obesity occurs in addition to a separate linear BMI effect. Without good reason to
#do otherwise, we'll include both in our final model.

insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

# create final model
#The * operator is shorthand that instructs R to model expenses ~ bmi30 +
#smokeryes + bmi30:smokeryes. The : (colon) operator in the expanded form
#indicates that bmi30:smokeryes is the interaction between the two variables. Note
#that the expanded form also automatically included the bmi30 and smoker variables
#as well as the interaction.

ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance)

summary(ins_model2)

