#Check and set working directories
getwd()

#Clear sorted  variables and load libraries
rm(list=ls())
library(pacman)
p_load(data.table, ggplot2, broom, DBI)

wpull <- function(name){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  dt <- data.table(DBI::dbReadTable(con,name))
  print(DBI::dbReadTable(con,paste(name,'labels',sep='_')))
  DBI::dbDisconnect(con)
  return(dt)
}

#Question 1 Load wage1 data 
wage1 <- wpull('wage1')

#1
summary(wage1$educ)
#Education min is 0 while max is 18. Mean for educ is 12.56

#2
average_wage <- mean(wage1$wage); round(average_wage, 3)
#Wage average is $5.909 which seems low compared to the average minimum hourly wage of $7.25. However compared to 1976 where the average minimum was $2.3 this seems relatively high

#3
CPI_1976 <- 56.9
CPI_2010 <- 218.056

#4
average_wage_2010 <-average_wage*(CPI_2010/CPI_1976)
average_wage_2010

# After having performed this calculation the hourly wage = $22.64 seems reasonable compared to the wage of $7.25 

#5
table(wage1$female)
#The sample has 274 males and 252 females 


#Question 2
meap01 <- wpull('meap01')

#1 Range of math4
summary(meap01$math4)
#The min of this sample is 0 and the max for this is 100, the range hence becomes 100. 
#This stat does not make sense to refer to as this is a test and the likelyhood that students have either a 0 or a 100 is very low.

#2 Schools with perfect pass rate and percentage
x <- meap01[math4 == 100, .N]
x
x <- x/nrow(meap01)
x * 100
# 38 Students out of 1823 which equates to 2.084% of the sample

#3 Schools with 50% pass rate and percentage
half_pass <- meap01[math4 == 50, .N]
half_pass
per_half_pass <- half_pass/nrow(meap01)
per_half_pass*100

#There are 17 schools that have a 50% pass rate which equates to 0.93% of the sample

#4 Comparison of Math and Reading Test Scores
mean(meap01$math4)
mean(meap01$read4)

#The mean for math tests is 71.909% and the mean for reading tests is 60.06%. Based on this reading tests are harder to pass

#5. Correlation between Math and Reading Tests
cor(meap01$math4,meap01$read4)

# reading tests and math tests have a positive correlation of 0.84 which suggests that it is a relatively strong and positively correlated, suggesting that people with higher math scores also have higher scores in the reading tests

#6. mean and sd of exppp
mean(meap01$exppp)
sd(meap01$exppp)

#On average expenditure per pupil is $5194.865 while the standard deviation is at $1091.89. Compared to the average I would conclude that there is a wide variation in spending

#7. Percentage Change and Comparison with Difference in Natural Logs
School_A <- 6000
School_B <- 5500

per_diff <- (School_A/School_B) - 1
per_diff <- per_diff * 100
#The difference between School A and School B is 9.09% 

#Log Difference

log_diff <- 100*(log(6000)-log(5500))
log_diff

diff <- per_diff - log_diff
diff

#Compared to the percent difference, the log difference underestimates the spending by 0.39%

#Question 3
Retirement_data <- wpull('401k')

#1. Average prate and average mrate
mean(Retirement_data$prate)
mean(Retirement_data$mrate)

#As we can  see the average prate from the sample is 87.363 and mean mrate is 0.732

#2. Estimating simple linear regression

model <- lm(prate~mrate,Retirement_data)
model
coef(model)
summary(model)

#The estimate regression that we come up with is prate_hat = mrate * 5.861 + 83.075
#The mrate variable is a significant variable with significance at 0% significance level
#The sample size is 1534 (df-residual+number of parameters estimated in the model/regression equation) 1532+2=1534 while the R-squared is 0.074 or 7.4%. 
#We can view any other details using summary(model)

#3. Interpreting Coefficients
#Coefficient of mrate
#For every $1 of match rate the prate changes by 5.86%

#Intercept
#From the equation we can note that at mrate = 0 the prate would be equal to 83.07%. By this we can assume that when the firm does not contribute anything to meet the employee contrbution we have an average prate of 83.07%

#4. When mrate is equal to 3.5
predict(model,data.table(mrate=3.5))

#According to our prediction when mrate equals to 3.5 then prate equals to 103.589
#This is prediction does not seem reasonable as it  is higher than 100% while the percentage cannot exceed 100%
#The predicted values are beyond acceptable because the model is fitting a linear line without taking into accounts the bounds.
#A better approach would be to use a different technique such as beta regression which is used to limit observations in an upper and lower (0,1). 
#We can also choose to model it with some non-linear relationship as the scatterplot shows values near the center are more varying than compared with the upper and lower bound.

ggplot(Retirement_data, aes(mrate, prate)) + geom_point() + geom_line(aes(y=predict(model)))

#5. Percentage of prate Variation Explained by mrate
#The coefficients of determination (R-squared) tells us how much of the independent variable variation is explained by the dependent variable. In this case 7.4% variation of the dependent variable is explained by the independent variable. 

#Question 4

#Load the "ceosal2" data 
ceosal2 <- wpull('ceosal2')

#1. Average salary and average tenure in the sample
mean(ceosal2$salary)
mean(ceosal2$ceoten)

#The average salary of a CEO is 865.86 thousand while the average tenure of a CEO is 7.95 years

#2. CEOs with Tenure = 0
ceosal2[ceoten==0,.N]
ceosal2[,max(ceoten)]
#There 5 individuals with ceoten at 0 and the longest ceoten equates to 37

#3. Simple regression

model_2 <- lm(log(salary)~ceoten,data=ceosal2)
model_2
coef(model_2)
summary(model_2)
predict(model_2, data.table(ceoten=1))

#Based on the results we can assume that our equation is ln(salary_hat) = 6.51 + 0.00972*ceoten
#We can interpret that for a one year change in ceoten the salary increases by 0.972% and for a ceoten of 0 the salary is $6.51 thousand
#When we equate ceoten = 1 the salary would equal to $6.52 thousand

#We can visualize this relationship with a plot
ggplot(ceosal2, aes(ceoten, log(salary))) + geom_point() + geom_line(aes(y=predict(model_2)))

#Question 5

#load wage2 into R
wage2 <- wpull('wage2')

#1. Average wage and average and standard deviation of IQ
mean(wage2$wage)
mean(wage2$IQ)
sd(wage2$IQ, na.rm = TRUE)

#Average wage in the sample set is 957.95 while average IQ is 101.28
#Standard Deviation of IQ is 15.05

#2. Simple regression model with the equation is wage=beta_0 + beta_1 * IQ + u

model_3 <- lm(wage~IQ, data = wage2)
summary(model_3)
predict(model_3,data.table(IQ=15))

#Based on the result we can assume that wage_hat = 116.992 + IQ * 8.303
#Using this equation we can see that for increase in IQ by 15 points wage equals to 241.54 i.e for a change of 15 in IQ wage changes by $8.303

#Variation Explanation by Model
#The R-Squared value of the model infers that 9.6% variation of the dependent variable is defined by the independent variable

#3. Log-Level Linear Model

model_4 <- lm(log(wage)~IQ,data=wage2)
summary(model_4)
predict(model_4,data.table(IQ=5))

#The estimated log-linear model is log(wage_hat) = 5.887 + IQ * 0.0088
#For a change of IQ = 15, wage is 5.93103 i.e for a change of 15 in IQ wage changes by 13.2%

# Plotting Linear Model
ggplot(wage2, aes(IQ, wage)) + geom_point() + geom_line(aes(y=predict(model_3))) + ggtitle("Plot of Linear Model") + labs(y= "Wage")
#Plotting Log-Linear Model
ggplot(wage2, aes(IQ, log(wage))) + geom_point() + geom_line(aes(y=predict(model_4))) + ggtitle("Plot of Log_Linear Model") + labs(y= "Wage")
#Question 6

#Load the "meap93" data table

meap93 <- wpull('meap93')

#1. Effect of additional dollar on passrate
cor(meap93$expend,meap93$math10)
#Based on correlation we can assume that the two variables have a positive relationship that is not very strong. The effect of every dollar spent on a student will have a diminishing effect and will be stagnant after a time. Owing to the fact that while an initial spending would help improve the pass rate but after a certain time pass rate will reach a limit which would ideally be a 100%

#2. Beta_1/10 as the percentage change in math10 for a 10% increase in expend
#The math10 variable is a percentage variable so the percentage change in expenditure will translate as the percentage point change, because dependent variable is in percentages,
#As, we know that this is a level-log linear model and thus a change in y for any given x% increase in spending per student (expend) in a level-log model is defined by (Beta-1/100)*(%Delta-x) where Delta-x=x2-x1/x1. 
#The %Delta_x is given as 100*(Delta-x/x) and that percentage change in x is 10% or 0.1. The proportional change in x of 0.1 equals Delta-x/x1. We substitute this value in the Beta-1/100*(%Delta-x) formula. So, Beta-1/100*(%Delta-x)=(Beta-1/100)*(100*0.1)=(Beta-1/100)*(100*(10/100))=(Beta-1/100)*(10)=Beta-1/10 or B/10. 
#So, it shows that B/10 is the percentage point increase in math10 for 10% increase in expend.

#3. Estimate the Model: math10=beta_0 + beta_1*log(expend)+u

model_5 <- lm(math10~log(expend),data = meap93)
model_5
summary(model_5)

#The estimated simple regression model is math10_hat = log(expend) * 11.16 - 69.34
#The sample size is 408 and the R-squared is 0.02966 or 2.966%

#4. Estimated spending effect and increase for 10% Increase in spending
#The estimated spending effect is 0.112% which is very minor for an increase in expend of 1% increase 
#The estimated spending effect is 1.12% for a spend of 10%


#5. Fitted Values Greater than 100
#The fitted values for math10 will result in being  100 because the maximum value of expend in the sample is 7419 and the fitted value against it is 30.15%.
#A case can be made that it as math10 is a log function of spending per student and this function can go beyond 100. 
#While it is true that the log function is increasing, however a log function after a specific point increase at a diminishing rate so it will be highly unlikely that the fitted values will go beyond 100.

#Question 7 

#Load data hprice1
hprice1 <- wpull('hprice1')

#1. The linear equation estimation: price=beta_0 + beta_1 * sqrft + beta_2 * bdrms + u

model_6 <- lm(price~sqrft+bdrms,data = hprice1)
model_6
summary(model_6)

#The estimated linear regression equation is: price_hat= -19.315 + 0.1284 * sqrft + 15.198 * bdrms
#The sample size is 88 and the R squared 0.6233 or 62.33%

#2. Estimated Increase in House Price with One more Bedroom and keeping sqrft fixed/constant (ceteris paribus)
#The price of the house increases $15,198 with more more bedroom while keeping sqrft constant because the beta_2_hat coefficient gives the change in house price for a one unit increase in bedrooms while keeping all other factors constant

#3. Estimated Increase in House Price of a House with an Additional Bedroom and 140 sqrft. size
#We calculate the value of this house first with one bedroom, 140 sqrft. size and then with two bedrooms and 140 sqrft. size

price_a <- predict(model_6,data.table(bdrms=1,sqrft=140))

price_b <- predict(model_6,data.table(bdrms=2,sqrft=140))

price_diff <- price_b - price_a
price_diff

#The price increase calculated in this part, $15,198 is equal to the increase in price in the previous part as we repeat the same calculation we simply compare by increasing bdrms from 1 to 2 instead of 0 to 1 and we keep sqrft. constant

#4. Variation in Price explained by independent variables
#The  R-squared multiple is 0.6319 which  means that 63.2% variation is explained by the independent variables

#5. Predicted price of house with bdrms = 4 and sqrft = 2438 
house_price <- predict(model_6,data.table(sqrft=2438,bdrms=4))
house_price

#The predicted selling for the first house is 354,605.2

#6. Residual of the Selling Price of First House and Buyer's Transaction Success
residual <- hprice1[1,2] - house_price
residual

#The residual of the selling for the first house is -54,605.25. This suggests that buyer underpaid for the house by $54,605.25 based on the prediction which is almost 15% less than its worth. 

#Question 8

#The data set is already loaded

head(ceosal2)

#1. Estimation Model: Annual Salary to Firm Sales and Market Value (Constant Elasticity)

model_7 <- lm(log(salary)~log(sales)+log(mktval),ceosal2)
summary(model_7)

#The estimation model: ln(annual salary)_hat= 4.62 +  log(sales)_hat * 0.162 + log(mktval)_hat * 0.1067
#The sample size is 177 and the R-squared is 0.299 or 29.9%

#2 Adding the profits variable in log form to previous model, constraint to adding in log form 
model_7_a <- lm(log(salary)~log(sales)+log(mktval)+profits,ceosal2)
summary(model_7_a)

#constraint to add in log form
summary(ceosal2$profits)
#The profit variable cannot be added because this sample contains negative values and the domain of the log function is x > 0. 

#Variation Explanation
#Based on the R.squared only 29.9%  of the variation of the dependent variable is explained by the independent variable. 

#Adding ceoten variable to the  model
#We will not add log of ceoten because ceoten contains 0 value
model_7_b <- lm(log(salary)~log(sales)+log(mktval)+profits+ceoten,ceosal2)
summary(model_7_b)

coef(model_7_b)
#The annual salary of a CEO increases by 1.168% for another year as CEO holding all other factors constant

#Correlation between ln(mktval) and profits OLS Estimator
ceosal2$ln_mktval <- log(ceosal2$mktval)
cor(ceosal2$ln_mktval,ceosal2$profits)

#There is a strong positive correlation between the two variables it suggests that the mdoel might be suffering from a problem of  multicolinearity 
#(futher testing needs to be done or we should check measures such as VIF). 
#High correlations among variables such as in this case can lead to large variances in OLS slope estimators and this can lead to statistical significance of independent variable (increase in Type-II error rate) which lead  to unreliable Hypothesis testing

#Question 9

#Load dataset attend 
attend <- wpull('attend')
attend
attend$atndrte <- (attend$attend / 32) * 100

#1. Minimum, Maximum, and Mean
summary(attend$priGPA)
summary(attend$ACT)
summary(attend$atndrte)

#Minimum values for priGPA, ACT and atndrte respectively are 0.857, 13 and 0.0625
#Maximum values for priGPA, ACT and atndrte respectively are 3.930, 32 and 1
#Mean values for priGPA, ACT and atndrte respectively are 2.587, 22.51 and 0.8171

#2. Estimation Model: atndrte = Beta_0 + beta_1 * GPA + beta_2 * ACT + u
model_8 <- lm(atndrte~priGPA+ACT, data = attend)
summary(model_8)

#Assumption
#pricGPA is used because it is assumed to be a cummulation of previous semester GPAs and ACT

#The estimation model: atndrte_hat = 75.7 + 17.261 * priGPA -1.717 * ACT
#The sample size is 680 with a R.squared of 0.29606 or 29.606%

#3. Discussing Slope Coefficients

#GPA
#If  the cumulative GPA till the previous term of a student improves by 1 grade point then attendance rate increases by 17.261% holding all other factors constant
#The slope coefficient of GPA is as per expectations with regards to the direction of the relationship between GPA and attendance rate. It supports the assumption that students who attend more classes are likely to improve their GPA moving forward.

#ACT
#If the ACT score increases by 1 point then attendance rate falls by -1.717% 
#This is a surprising relationship because of the direction of the coefficient. There is a negative relationship which is counterintuitive given that students with a higher ACT score are more likely to attend classes and maximum learning in order to improve GPA

#4. Predict value of atndrte given: priGPA = 3.65 & ACT = 20
predict(model_8,data.table(priGPA=3.65,ACT=20))

#Students with given value of explanatory variable
attend[ACT==20,priGPA==3.65,.N]

#Average attendance of a student with these explanatory variable is 104.37%
#There is one student with the explanatory variable
#The predicted attendance rate of such students does not seem because the attendance rate cannot be greater than 100%. 
#Furthermore the actual attendance for such a student is 87.5%.
#The model has over predicted the mathpass rate, which might be due to a weak and negative correlation between atndrte and ACT variable. 
#If we regress atndrte and priGPA alone then for a maximum value for GPA = 4 our atndrte is 99.6671%. 
#However is we add ACT to the model then the prediction exceeds 100. 
#In order to resolve this issue we should either eliminate ACT from the model because it  estimated coefficients are counter to our expected relation between ACT and atndrte
#Another concern might be the quality of data (maybe data recorded is flawed).

#5. Predict Difference in Attendance Rates
predict_1 <- predict(model_8,data.table(priGPA=3.1,ACT=21))
predict_2 <- predict(model_8,data.table(priGPA=2.1,ACT=26))
predict_diff <- predict_1 - predict_2
predict_diff

#The predicted difference between student A and student b is 25.84%

#Question 10

htv <- wpull('htv')

#1. range of educ, men with education equal to  12th grade, men or their parents have higher level of education

range(htv$educ)

#Education range is from 0 i.e no education at all to 20 years of education(maybe enrolled in higher educ or PhD)

#Percentage of men who completed 12th grade education
perc_12th <- (htv[educ==12,.N] / nrow(htv)) * 100
perc_12th

#The percenntage of people with education upto the 12th grade is 41.63%

#Men education and parent education
summary(htv$educ)
summary(htv$fatheduc)
summary(htv$motheduc)

#Men's education is at mean 13.04 which on an average is higher than both parents (with father's average at 12.45 and the mother's average at 12.18)

#2. Estimating the model: educ=beta_0 + beta_1 * motheduc + beta_2 * fatheduc + u

model_9 <- lm(educ~motheduc+fatheduc,data = htv)
summary(model_9)

#Estimation model: educ_hat = 6.9644 + 0.3042 * motheduc_hat + 0.1903 * fatheduc_hat
#The sample size for this dataset is 1230 and R.squared is 24.93%

#Variation Explained
#In this estimation model 24.93% of the variation in the dependent variable is explained by the independent variable

#Interpretation of motheduc coefficient
#If the mother highest education level increases by one more grade completed then men's highest level of education improves by 0.3 grades completed holding all else constant

#3. Adding abil variable to the previously estimated regression
model_9_a <- lm(educ~motheduc+fatheduc+abil,data = htv)
summary(model_9_a)

#The estimated model equation is: educ_hat=8.44+0.18*motheduc_hat+0.11*fatheduc_hat+0.502*abil_hat
#The sample size is 1,230 and the R-Squared is 0.4275 or 42.75%.

#Predictive Contribution of Ability
#The ability variable helps in explaining the variation in education because it is a highly statistically significant variable even at approximately 0% significance level and further, the R-Squared (adjusted-R-Squared as well) also increased from 24.93% to 42.75%. The coefficient of ability also gives useful information to help undestand how increased cognitive ability improves the highest education level.

#4. Estimating the Model with Quadratic Form of Abil and Local Minimum of Education through Calculus
model_9_b <- lm(educ~motheduc+fatheduc+abil+I(abil^2),htv)
summary(model_9_b)

#The estimation model equation is: educ_hat=8.24+0.19*motheduc_hat+0.108*fatheduc_hat+0.40*abil_hat+0.051*abil^2
#The sample size is 1,230 and the R-Squared is 0.4444 or 44.44%.

#Using Calculus we Fin the Minimum Value of Education with Respect to abil 
#First we will take the first partial derivative of educ with respect to abil variable

#d(educ)/d(abil)=beta-3+2*beta-4*abil

#Set the first derivative equals to zero and solve for abil's value
#d(educ)/d(abil)=beta-3+2*beta-4*abil=0

#Rearrnaging the equation above we get

#abil=-beta-3/2*(beta-4)               #equation 1

#Subsitute the values of estimated beta-3 and beta-4 coefficients in equation 1
#abil=-.4/2*0.051
#abil=-3.92

#No we will find the second partial derivative of education with respect to abil variable to confirm that we found the minimum point
#d2(educ)/d(abil)2=2*beta-4           #equation 2

#We know that beta-4 is a positive value so, the equation 2 will never have a negative value. Since, the second partial derivative of education is positive so, abil=-3.92 is the value for where education is minimized.

#5. Argument: Only a Small Fraction of men in the Sample have Ability less than local minimum value
fraction_less <- htv[abil< -3.921569,.N]/(nrow(htv))
fraction_less

#The fraction of men in the sample having ability value less than the minimum value of -3.921569 is only 1.12%. 

#It is important because if have more values less than the minimum value then we will have a u-shaped parabolic fitted curve which is symmetric around the axis of symmetry.
#In that case, we will have a scenario where the students with very low ability scores will have comparable (or same we go compare symmetrical values) education levels with males with high ability scores. 
#The u-shaped curve will also imply that as the ability score starts to increase from negative to positive, it results in decline of the education levels and after the minimum value the education levels start increasing again. 
#Further, the graph, given below in code, of ability and education (actual outcomes) shows that they are positively correlated and at best a curvilinear relationship could be used to demonstrate slight curve in the relationship. 
#We used quadratic term of ability to model that curvilinear relationship however, a u-shaped relationship will not be appropriate to model our expected linear relationship between these variables.

#Graph between Education Levels (Actual Outcomes) and Ability
ggplot(htv, aes(x=abil, y=educ))+geom_point()+labs(x="Ability", y="Education", title="Relationship between Predicted Education and Ability")

#6. Plotting the Relationship between Predicted Education and Ability Variable with Parents' Education fixed at average values in the sample

#Generating Predicted Education Variable with Given Specification
htv$predicted_education <- predict(model_9_b, data.table(motheduc=12.18, fatheduc=12.45, abil=htv$abil))

#Plotting the Relationship
ggplot(htv, aes(x=abil, y=predicted_education))+geom_point()+labs(x="Ability", y="Predicted Education", title="Relationship between Predicted Education and Ability")+geom_vline(xintercept=-3.921569)

#The graph confirms that the minimum predicted education value is also at abil=-3.92.
#It also shows how these variables are non-linearly related (quadratic relationship).