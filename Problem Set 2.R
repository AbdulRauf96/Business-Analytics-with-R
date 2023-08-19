#Check and set working directories
getwd()
rm(list=ls())

library(pacman)
p_load(tidyverse, data.table, DBI, broom, zoo, lmtest, sandwich, estimatr)


wpull <- function(name){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  dt <- data.table(DBI::dbReadTable(con,name))
  print(DBI::dbReadTable(con,paste(name,'labels',sep='_')))
  DBI::dbDisconnect(con)
  return(dt)
}

#Data for Question 1
Vote1 <- wpull('Vote1')
summary(Vote1)
model_1 <- lm(voteA ~ log(expendA) + log(expendB) + prtystrA, data = Vote1)
summary(model_1)

#1. Interpretation of B_1
#A 1% increase in the expenditure of candidate A the percentage of voters change B_1 / 100 holding all else constant

#2. Null Hypothesis: 1% increase in A's expenditures is offset by a 1% increase in B's expenditures
#H0 b-1=-b-2 or b-1+b-2=0
#3. Estimation of Model: voteA=b-0+b-1*ln(expendA)+b-2*ln(expendB)+b3*prystrA+u
#Generate the Ln Transformed IVs
Vote1$log_expendA <- log(Vote1$expendA)
Vote1$log_expendB <- log(Vote1$expendB)

#Model Estimation

VoteA <- lm(voteA ~ log_expendA + log_expendB+prtystrA, Vote1)
summary(VoteA)
tidy(VoteA,conf.int = TRUE)
lmtest::coeftest(VoteA,vcov=vcovHC)

#The estimated multiple linear regression model is: VoteA = 45.08788 + 6.08136 * ln(expendA) - 6.61563 * ln(expendB) + 0.15201 * prtystrA
#The sample size 173 and R squared of the model is 79.25% or 0.7925

#Effect of the expenditure of A and B on the Votes as a Percentage of A 
#The coefficient of expendA and expendB are highly statistically significant at approx 0.1% significance level which means that they do effect the vote percentage of A

#Hypothesis Testing based on the coefficients of ln expend A and B
#We can use the result to perform a hypothesis test in part 2, but we need to calculate the covariance of the ln(expendA) and ln(expendB) variabes using vcov() and vcovHC() functions and the variances of B_1 and B_2 in order to calculate standard of error for B_1 + B_2; 
#So it is not entirely possible to perform hypothesis test based on the available coefficient estimate although this is the required information for hypothesis tests.

#4. Estimate the Model for the T-statistic for Hypothesis testing in part 2

#We know that Null hypothesis equates to B_1 + B_2 = 0 and we assume theta = 0 hence we get B_1 + B_2 = Theta or B_2 = Theta - B_1 and substitute it in the model
#VoteA=b_0+b_1*ln(expendA)+(theta-b_1)*ln(expendB)+b_3*prystrA+u
#VoteA=b_0+b_1*ln(expendA)+theta*ln(expendB)-b_1*ln(expendB)+b_3*prystrA+u
#VoteA=b_0+b_1*(ln(expendA)-ln(expendB))+theta*ln(expendB)+b_3*prystrA+u 
#VoteA=b_0+b_1*ln(expendA/expendB)+theta*ln(expendB)+b_3*prystrA+u          #using log A-log B=log(A/B) ---------> 1

#In order to run the model we first calculate ln(expendA/expendB)
Vote1$log_ratioAB <- log((Vote1$expendA/Vote1$expendB))

#Estimating the revised Model
hyp_model <- lm(voteA ~ log_ratioAB + log_expendB + prtystrA, Vote1)
tidy(hyp_model)
lmtest::coeftest(hyp_model,vcov=vcovHC)

#Conclusion to Hypothesis Test
#We observe the t-statistic and corresponding p-value of the ln(expendB) term i.e theta or B_1 + B_2 = 0. 
# As the two sided p-value of 0.399197 which is greater than significance levels of 0.01 (even 0.1 significance level) hence we accept the null hypothesis. 
# We can conclude that 1% increase in the expenditure of A is nullified by a 1% increase in B's expenditure

#Question 2

#loading the "lawsch85" dataset
lawsch85 <- wpull('lawsch85')

#Estimation Model: ln(salary) = b_0 + B_1 * LSAT + B_2 * GPA + B_3 * ln(libvol) + B_4 * ln(cost) + B_5 * rank + u and interpret the beta coefficient of rank

#Create ln variables for model estimation
lawsch85$log_salary <- log(lawsch85$salary)
lawsch85$log_libvol <- log(lawsch85$libvol)
lawsch85$log_cost <- log(lawsch85$cost)

#Model Estimation

model_2 <- lm(log_salary ~ LSAT + GPA + log_libvol + log_cost + rank, data = lawsch85)
summary(model_2)
tidy(model_2,conf.int = TRUE,conf.level = 0.95)
#Robust standard of errors of the coefficient of estimates
lmtest::coeftest(model_2,vcov. = vcovHC)

#Hypothesis for Rank Variables Effect on Median Salary 
#H0: B_5=0
#H1: B_5!=0

#Significance Level is 5%

#The confidence interval of B_5 is -0.00401 < B_5 < -0.00264. 
#The confidence interval does not contain the  hypothesized value of 0 so, we reject the null hypothesis that the rank of law schools has no cetris paribus effect on median starting salary.
#If calculate the robust standard of errors, the p-value, 0.0000022 for the B_5 coefficient is less than the 0.001 significance level, 
#hence we reject the null hypothesis at a 0.001 significance level and conclude that the rank of law school cetris paribus has effect on salary.

#2. Individual and Joint significance of LSAT and GPA variables

#Individual Significance of LSAT variable
#We test the null hypothesis that B_1 = 0 against the alternative hypothesis that B_1 != 0
#If we use robust standard errors the p-value, 0.358420, is greater than the significance level 0.1. Hence, we fail to reject the null hypothesis and conclude that the LSAT variable is individually statistically insignificant in explaining salary

#Individual Significance of GPA variable 
#We test null hypothesis that B_2 = 0 against the alternative hypothesis that B_2 != 0
#If we use the robust standard errors the p-value, 0.013413 for the B_2 coefficients is greater than 0.01 significance level. So, we reject the null hypothesis at 1% significance level and conclude that the GPA variable is individually significant in explaining salary.

#Joint Significance of LSAT and GPA in explaining Salary
#We test the null hypothesis that B_1=B_2=0 or atleast one of the betas is not equal to 0 against the alternative hypothesis b-1-b-2!=0.
#We run the restricted model where we do not include the LSAT and GPA variables and run the ANOVA to calculate the F-statistic.

Model_2_restr <- lm(log_salary ~ log_libvol + log_cost + rank,data=subset(lawsch85,LSAT!="NA" & GPA !="NA"))
anova(Model_2_restr,model_2)

#As the p-value corresponding to the F-statistic is ~0, we reject the null hypothesis (restriction B-1=B_2=0) and conclude that both GPA and LSAT are jointly statistically significant for explaining salary

#The Joint addition of size of entering class variable and size of faculty variables in the equation
#First we estimate the original model by removing missing values from the clsize and faculty variable 
model_2_alpha <- lm(log_salary ~ LSAT + GPA + log_libvol + log_cost + rank, subset(lawsch85,clsize != "NA" & faculty != "NA"))

#The null hypothesis is that clsize and faculty variable are jointly statistically significant B_6=B_7=0 and the alternative  hypothesis is they can be jointly added to this equation

#We will add these variables in model and estimate the following model
#log(salary) = B_0 + B_1 * LSAT + B_2 * GPA + B_3 * log(libvol) + B_4 * log(cost) + B_5 * rank + B_6 * clsize + B_7 * faculty + u
model_2_ext <- lm(log_salary ~ LSAT + GPA + log_libvol + log_cost + rank + clsize + faculty, lawsch85)
anova(model_2_alpha, model_2_ext)

#As the p-value corresponding to the F-statistic, 0.3902, is greater than even the 10% significance level, we do not reject  the null hypothesis (B_6=B_7=0) and conclude that clsize and faculty need not be added to the model

#Other factors effecting median starting salary 
#The age of the law school can be added to salary regression because a college which older would have a wider network of alumni and more credibility to it's name resulting in a positive influence on median salary 
#The location of law school can also be a contributing factor because the economic conditions of the region can also effect median salaries for graduates

#Question 3

#Load the "hprice1" data table

hprice1 <- wpull('hprice1')

#Generate the ln(price) variable

hprice1$log_price <- log(hprice1$price)

#Estimate the Model: ln(price) = B_0 + B_1 * sqrft + B_2 * bdrms + u
model_3 <- lm(log_price ~ sqrft + bdrms, hprice1)
model_3
tidy(model_3)

#We know that theta_1 = 150 * B_1 + B_2. We estimate theta_1 by substituting the values of B_1_Hat and B_2_Hat in the equation as follows:
#Estimation of theta_1: Theta_1_hat=150*0.0003794 + 0.0288845 = 0.08579
#The estimation of theta_1 is 0.08579 in decimal form

#2. B_2 in terms of theta_1 and B_1 and substitute that into ln(price) equation
#As Theta_1=150*B_1+B_2, B_2=-150*B_1+Theta_1. We now Substitute this into our ln(price) equation

#ln(price)=B_0 + B_1 * sqrft + (Theta_1 - 150 *B_1) * bdrms + u
#ln(price)=B_0 + B_1 * sqrft + (Theta_1 * bdrms) - (150* B_1 * bdrms) + u
#ln(price)=B_0 + B_1(sqrft - 150 *bdrms) + (Theta_1 * bdrms) + u

#3. Standard Error of Theta_1_hat and the 95% confidence Interval
#Estimate the Model in Equation 2
model_3_theta <- lm(log_price ~ I(sqrft - 150*bdrms)+bdrms, hprice1)
summary(model_3_theta)
tidy(model_3_theta)

#The standard error of theta_1_hat is 0.0268.

#To construct the 95% confidence interval, we calculate the t-statictic ciritical value as follows

t_cric <- qt(0.975,85)

#UCL
0.08580+t_cric*0.0268

#LCL
0.08580-t_cric*0.0268

#The predicted percentage change in price when a 150 sqrft bedroom is added to a house will be between 3.26% to 13.9% with 95% confidence interval

#Question 4

#Load the "wage2" data table 

wage2 <- wpull('wage2')

#1. Null Hypothesis for same effect of the general workforce experience and tenure on ln(wage)

#The null hypothesis that another year of general workforce experience has the same effect on ln(wage) as another year of tenure with current employer is as follows
#H0: B_2=B_3

#Test of Null hypothesis in part 1 at 5% significance level

#We know that B_2=B_3=Theta as Theta=0 so the null hypothesis become B_2-B_3=Theta. 
#Rearranging to solve  for B_2 we get, B_2=Theta+B_3. We substitute B_2 into our model equation
#ln(wage) = B_0 + B_1 * educ + (Theta + B_3) * exper + B_3 * tenure + u
#ln(wage) = B_0 + B_1 * educ + Theta * exper + B_3 * exper + B_3 * tenure + u
#ln(wage) = B_0 + B_1 * educ + Theta * exper + B_3 * (exper + tenure) + u


#Generate the ln(wage) variable
wage2$log_wage <- log(wage2$wage)

#Estimate the revised Model in Equation 3
Model_4 <- lm(log_wage ~ educ + I(exper+tenure)+exper, wage2)
Model_4
confint(Model_4, level = 0.95)

#Testing the NUll HYpothesis
#The 95% confidence interval of the theta variable is -0.007355 < Theta < 0.011263. 
#Since, the confidence Interval include hypothesized value of Theta=0 so we do not reject the null hypothesis and conclude that another year of general workforce has the same effect on ln(wage) as another year of tenure with the current employer.

#Question 5

#Load the "401ksubs" data table 

Four01ksubs <- wpull('401ksubs')

#1. Number of single person households in dataset
nrow(Four01ksubs[fsize==1])

#There are 2017 single person households in the data-set

#2. Estimation Model: netttfa = B_0 + B_1 * inc + B_2 * age + u for a single person household and Interpretation of slope coefficients

Model_5 <- lm(nettfa ~ inc + age, Four01ksubs[fsize==1])
summary(Model_5)

#The Estimation Model equation is nettfa_hat = -43.03981+0.79932*inc+0.84266*age.
#The sample size is 2017 and the R-Squared is 0.1193 or 11.93%.

#Interpretation of Income Coefficient
#For one unit increase ($1000) in annual income of the single-person household, the net financial wealth of the individual living in a single-person household increases by $799.32, cetris parabis

#Interpretation of age Coefficient
#For one unit increase in the age  variable, the net financial wealth of the individual increases by $842.66, ceteris paribus.

#Surprises in Slope the Coefficients
#The slope coefficients of income and age do not hold any surprises as we can safely assume as a income would increase their net financial value would also increase, a similar relationship can also be assumed between age and net financial wealth

#3. Significance of Intercept COefficent 

#The intercept coefficcent does not have any significant meaning given that age can never be zero of a survey respondent 

#4. Test the Hypothesis that One Year Increase in Age Translates into less than $1000 increase in net financial wealth
#H0: B_2 = 1
#H1: B_2 < 1

#Heteroskedasticity Consistent Estimators
tidy(lmtest::coeftest(Model_5, vcov=vcovHC), conf.int = TRUE)

#We now calculate the test statistic for B_2_hat coefficient  
#We use z-statistic because  we have sufficiently large sample size to assume normal distribution

Z_stat <- (0.84266-1)/(.120)

#Now calculate p-value correspondingto the z-stat
pnorm(Z_stat)

#Conclusion
#As the  p-value of 0.094 is greater than 0.01 we do not reject the null hypothesis and conclude B_1=1. This means that for a 1 year increase in Age the net financial wealth increases by $1000 certis paribus

#Simple regression of nettfa with inc: nettfa = B_0 + B_1 * inc + u
Model_5_simp <- lm(nettfa ~ inc, Four01ksubs[fsize==1])
summary(Model_5_simp)

# The estimation Model equation is nettfa_hat = -10.5709+0.8207*inc
# The sample size is 2017 and the R-Squared is 0.08267 or 8.267%.

# The estimated coefficient on inc in the simple linear regression is 0.8207 which is not much different from the estimate in part 2, 0.79932
# The reason for the minimal difference between the two coefficients is the weak correlation with the age, with a correalation coefficent of 0.105

# Correlation of inc and age
cor(Four01ksubs$inc,Four01ksubs$age)

#Question 6
#Load the "kielmc" data table 

kielmc <- wpull('kielmc')

# 1. Estimating the Model: ln(price)=B_0 + B_1 * ln(dist) + u, Expected  behaviour and Results interpretation 

# Expected B_1 sign
# We expect that for a one percent increase in distance from the house to the local garbage incinerator there will be a B_1% increase in the house price holding all else constant

#Model Estimation
Model_6 <- lm(lprice ~ ldist,kielmc[y81==1])
summary(Model_6)

# The estimation model equation is ln(price)_hat = 8.0472+0.3649*ln(dist)
#The sample size is 142 and the R-Squared is 0.1803 or 18.03%

#Interpretation of B_1 Coefficient 
# As per our the sign of our B_1 coefficent is positive 
# For a one percent increase in the distance from the house to the local garbage incinerator, the house price increases by 0.3649%, cetris paribus

# 2. Adding variables and Estimating the Model:ln(price)=B_0 + B_1 * ln(dist) + B_2 *ln(intst)+ B_3 *ln(area)+ B_4 *ln(land)+ B_5 *rooms+ B_6 *bath+ B_7 *age+u

Model_6_extend <- lm(lprice~ldist+lintst+larea+lland+rooms+baths+age,kielmc[y81==1])
summary(Model_6_extend)

#The estimated model equation is ln(price)-hat=7.5923+0.0554*ln(dist)-0.0390*ln(intst)+0.3192*ln(area)+0.0768*ln(land)+0.0425*rooms+0.1669*baths-0.0036*age
#The sample size is 142 and the R-Squared is 0.7475 or 74.75%.

#Conclusion on Effects of Incinerator
#For a 1% increase in the distance from the house to the local garbage incinerator, the house price will increase by 0.0554%, cetris paribus

#Comparison of B_1 in 1 and 2
# The B_1 estimator in 1 and 2  behave as we expected with regards to the sign of B_1 coefficients, however, the magnitude of the effect of distance of garbage incinerator has decreased in 2 compared to 1 (holding all else constant)
# Moreover, the B_1 estimator is statistically insignificant in 2. These changes could be because we added other confounding variables especially adding lintst with ldist due to their high correlation with a coefficient of 0.7808. There may be an effect of multicollinearlity might have an effect on the estimates of standard errors of ldist and lintst variable which would have resulted in significance and magnitude of B_1 estimator in 2. 

#Independent and Dependent variables
dt <- data.table(kielmc[y81==1, c("lprice", "ldist", "lintst", "larea", "lland", "rooms", "baths", "age")])
cor(dt)

# 3. Adding ln(intst)^2 to the  Model from part 2

Model_6_2 <- lm(lprice~ldist+lintst+lintstsq+larea+lland+rooms+baths+age,kielmc[y81==1])
summary(Model_6_2)

#The Importance of func form ln(intst)^2
#The ln(intst)^2 is a statistically significant addition in the model. The In(intst) and ln(intst)^2 terms in the func form model. The Quadratic relationship between ln(intst) and the  ln(price) variables. For a 1% percent increase in interstate distance from home, the cetris paribus  effect can be approximated by d(ln(price))/d(intst)=1/intst*(2.0728-2*0.1193*ln(intst))
#All the independent variables in the functional form are individually significant, expect rooms variable, the p-value of the F-statistic for overall significance of the model is also approx 0. so the is overall statistically significant. 

# 4. Adding square of ln(dist) to the model in part 3

kielmc$ldistsq <- (kielmc$ldist)^2

Model_6_3 <- lm(lprice~ldist+ldistsq+lintst+lintstsq+larea+lland+rooms+baths+age,kielmc[y81==1])
summary(Model_6_3)

# Significance of ln(ldist)^2 variable
# The ln(dist)^2 variable is individually statistically insignificant because the p-value corresponding to the t-stat is 0.7409, which is way greater than the 0.1 significance level

#Question 7

# Load "wage1" data table

wage1 <- wpull('wage1')

#1. Estimate the Model: ln(wage) = B_0 + B_1 * educ + B_2 * exper + B_3 * exper ^ 2 + U

wage1$log_wage <- log(wage1$wage)
Model_7 <- lm(log_wage~educ+exper+I(exper^2), wage1)
summary(Model_7)

#The estimated model equation is ln(wage)_hat=0.1263+0.0906*educ+0.0409*exper-0.0007*exper^2
#The sample size is 526 and the R-Squared is 0.3004 or 30.03%.

# The statistical significance of exper^2 at 1% significance level
#The exper^2 is statistically significant at 1% because p-value of B_3 estimate, 0.00000000163 is approx.0 and less than the 0.01 significance level (we reject the null hypothesis that b-3=0 in favor of the alternative that b-3!=0).

#3. Approximate Return to Years of Experience using dln(wage)/d(exper)~b-2-hat+2*b-3-hat*exper (partial derivaitve of ln(wage) w.r.t exper)

#Approximate Return to Fifth Year of Experience
#To calculate the approximate return to the fifth year of experience, we substitute the B_2 and B_3 estimates for model above and also exper = 4

0.0409+2*(-0.0007121)*4

#The approx return of the 5th year of experience is 0.0352 or 3.52% increase in the average hourly earnings

#Approximate Return to Twentieth Year of Experience
#To calculate the approximate return to the fifth year of experience, we substitute the B_2 and B_3 estimates for model above also exper=19

0.0409+2*(-0.0007121)*19

#The approximate return of the twentieth year of experience is 0.0138 or 1.38% increase in the average hourly earnings.

#4. Value of Experience at which Additional Experience Lower Predicted ln(wage)
#We set the partial derivative of ln(wage) w.r.t exper equal to 0 to calculate the maximum value on the curve

#dln(wage)/d(exper)~b-2-hat+2*b-3-hat*exper=0
#-b-2-hat/(2*b-3-hat)            
#-(0.0409731)/(2*(-0.0007121))                #Substitute Value of b-2-hat and b-3-hat from the model
#28.7692

#To confirm that it is the maximum value on the curve we take second partial derivate of ln(wage) w.r.t exper
#dln(wage)/d(exper)~b-2-hat+2*b-3-hat*exper
#d2ln(wage)/d(exper)2~+2*b-3-hat<0            #Take Second Partial Derivative of ln(wage) w.r.t exper

#Since, b-3-hat is negative so the second partial derivative is negative which means that exper=28.7692 is the maximum point on the curve.

#At exper=28.7692, every additional year of experience lowers the predicted ln(wage).

#People with More Experience than the Maximum Value of exper=28.7692
nrow(wage1[exper>28.7692])

#There are 121 people who have more experience than the 28.7692 in this sample (approx. 23% of the sample).

#Question 8

#1. Return to Additional Year of Education is b-1+b-3*exper

#We take the Partial Derivative of ln(wage=b-0+b-1*educ+b-2*exper+b-3*educ*exper+u w.r.t educ
#d(ln(wage))/d(educ)=b-1+b-3*educ

#2. Null Hypothesis that Return to Education does not Depend on Level of Experience
#We know that the return to additional year of education is b-1+b-3*exper. 
#If we want to show that return to addition year of education is not dependent on level of experience then partial derivate of ln(wage) w.r.t educ should be d(ln(wage))/d(educ)=b-1 (i.e., b-3=0 (effect of interaction term is 0)).
#The null and alternative hypotheses that the return to additional year of education does not depend on level of experience are as follows.

#H0: b-3=0
#Ha: b-3!=0

#3. Test the Null Hypothesis in part 2. 

#Estimate the Model: ln(wage=b-0+b-1*educ+b-2*exper+b-3*educ*exper+u

Model_8 <- lm(log_wage~educ+exper+I(educ*exper), wage2)
lmtest::coeftest(Model_8, vcov=vcovHC)

#Conclusion of Hypothesis Testing
#The p-value of educ*exper coefficient estimate term (B_3_hat) is 0.03270 which is less than  0.05 significance level. So, we reject the null hypothesis that a return to additional year experience is not dependent on level of experience in favor of the alternative hypothesis at 5% significance level.

#4. Estimate and Confidence Interval of theta_1_hat

#We know that theta=B_1+10 * B_3 when exper=10
#To estimate theta_1 when exper=10, we substitute the value of B_1_hat and B_3_hat from the model in part 3

theta_1_hat <- 0.0440498+10*0.0032030
theta_1_hat

#The estimate of theta-1-hat (in decimal form) is 0.07607 which is the return to additional year of education when exper=10.

#Estimation of Confidence Interval of theta

#To estimate the confidence interval of theta, we replace the value of b-1: b1=theta-1-10*b-3 in the ln(wage) equation.
#ln(wage)=b-0+(theta-1-10*b-3)*educ+b-2*exper+b-3*educ*exper
#ln(wage)=b-0+theta*educ-10*b-3*educ+b-2*exper+b-3*educ*exper
#ln(wage)=b-0+theta*educ+b-2*exper+b-3*educ*(exper-10)              #Combining the Common Terms

Model_8_1 <- lm(log_wage~educ+exper+I(educ*(exper-10)), wage2)
summary(Model_8_1)
confint(Model_8_1, level = .95)

#The estimated value for theta is theta-1 is 0.076080 (the same as we calculated earlier).
#The 95% confidence interval for theta, return to education when exper=10, is: 0.0631 < theta < 0.0891.

#Question 9

# Load the "gpa2" data table

gpa2 <- wpull('gpa2')

#1. Estimation of the Model: SAT = B_0 + B_1 * hsize + B_2 * hsize^2 + u

Model_9 <- lm(sat~hsize+I(hsize^2), gpa2)
summary(Model_9)

#The estimated model equation is sat_hat = 997.981+19.8145*hize-2.1306*hsize^2.
#The sample size is 4137 and the R-Squared is 0.00765 or 0.765%.

#Statistical Significance of Quadratic Term
lmtest::coeftest(sat, vcov=vcovHC)                      #Heteroskedasticity Consistent Estimators

#The quadratic term (hsize^2) is highly statistically significant because the p-value is approx. 0 (9.75*10^(-5)) and less than the 0.1% significance level.

#2. Optimal High School Size
#We first take the derivative of sat-hat equation w.r.t hsize
#d(sat-hat)/d(hsize)=b-1-hat+b-2-hat*hsize  

#Now, to find the optimal hsize which maximizes the sat score, we substitute the d(sat-hat)/d(hsize) equals to 0.
#d(sat-hat)/d(hsize)=b-1-hat+b-2-hat*hsize=0
#hsize=-b-1-hat/(2*b-2-hat)

hsize <- -(19.8145)/(2*(-2.1306))              #Substitute the b-1-hat and b-2-hat from the estimated model equation in part 1
hsize

#The optimal hsize (size of graduating class) that maximizes the SAT score is 464.99.

#This is the optimal hsize (maximum point on curve) because the second derivative of sat-hat w.r.t hsize is d2(sat-hat)/d(hsize)2=2*b-2-hat. As, the b-2-hat is negative so second derivative is negative which shows that hsize=4.64 is the optimal high school size.

#3. Representatives Results for All High School Seniors?
#The sample is not representative of all the high school seniors because it only caters to the students who have taken SAT test and uses as a representation of their academic performance. Secondly, we do not know if the students are sampled from a given geography or a specific class of high schools. So, in order for a representative sample, a random sample of students who have taken the standardized test that is taken by all high school seniors students needs to be drawn alongwith ensuring that the students from different types of high schools is collected to capture variation in behavior of students due to high school location. 

#4. Optimal High School Size Using ln(sat) as the Dependent Variable

#Estimation of the Model: ln(sat)=b-0+b-1*hsize+b-2*hsize^2+u
Model_9_1 <- lm(log(sat)~hsize+I(hsize^2), gpa2)
summary(Model_9_1)

#Optimal hsize when ln(sat) is the Dependent Variable
#We first take the derivative of sat_hat equation w.r.t hsize
#d(ln(sat_hat))/d(hsize)=b_1-hat+b_2_hat*hsize 


#Now, to find the optimal hsize which maximizes the sat score, we substitute the d(ln(sat-hat)/d(hsize) equals to 0.
#d(ln(sat-hat))/d(hsize)=b-1-hat+b-2-hat*hsize=0
#hsize=-b-1-hat/(2*b-2-hat)

ln_hsize <- -(0.0196029)/(2*(-0.0020872))              #Substitute the b-1-hat and b-2-hat from the earlier estimated model equation in part 4
ln_hsize

#The optimal hsize (size of graduating class) that maximizes the percentage increase in SAT score is 469.59.
#The optimal hsize when ln(sat) is dependent variable is slightly higher than when SAT was the dependent variable. However, the difference is not too large and it is due to approximations using ln() function.

#This is the optimal hsize (maximum point on curve) because the second derivative of ln(sat-hat) w.r.t hsize is d2(ln(sat-hat)/d(hsize)2=2*b-2-hat. As, the b-2-hat is negative so second derivative is negative which shows that hsize=4.695 is the optimal high school size.

#Question 10

#1. Estimation of the Model: ln(price)=b-0+b-1*ln(lotsize)+b-2*ln(sqrft)+b-3*bdrms+u

Model_10 <- lm(log_price~log(lotsize)+log(sqrft)+bdrms, hprice1)
summary(Model_10)

#The estimated model equation is ln(price)_hat=-1.2970+0.1679*ln(lotsize)+0.7002*ln(sqrft)+0.0369*bdrms
#The sample size is 88 and the R-Squared is 0.643 or 64.3%. 

#2. Predicted Value of price; when lotsize=20,000, sqrft=2,500, and bdrms=4
predict(Model_10, data.table(lotsize=20000,sqrft=2500,bdrms=4))

#To get the predicted values of price based on log(price) prediction, we use the equation: price-hat=alpha-hat*exp(logprice-hat).      From Wooldridge
#To estimate alpha-hat, we regress the price on exponent function of fitted values from log(price) model i.e., exp(log(price)) as follows.
hprice1$price_fitted <- predict(Model_10, 
                                data.table(lotsize=hprice1$lotsize,
                                           sqrft=hprice1$sqrft,bdrms=hprice1$bdrms))    

RTO_alpha <- lm(price~0+I(exp(price_fitted)), hprice1)                   
summary(RTO_alpha)                                                       #alpha-hat=1.0229

#Now, we predict the log(price) value when lotsize=20,000, sqrft=2,500, and bdrms=4
desire_ln_price <- predict(Model_10, 
                           data.table(lotsize=20000,
                                      sqrft=2500,bdrms=4)) 

desire_ln_price                   #ln(price)-hat=5.993

#We take the exponent of the predicted log(price)
desire_ln_price_exp <- exp(desire_ln_price)
desire_ln_price_exp               #exp(ln(price)-hat)=400.574

#Predicting Price using price-hat=alpha-hat*exp(logprice-hat) 
desire_price <- 1.0229*desire_ln_price_exp
desire_price                     #price-hat=409.7471

#The predicted price when lotsize=20,000, sqrft=2,500, and bdrms=4 is approx. $ 409,747.1.

#3. Comparisons of Models for Explaining Variation in Price 

#Estimate the Model: price=b-0+b-1*lotsize+b-2*sqrft+b-3*bdrms+u
price_level_level <- lm(price~lotsize+sqrft+bdrms, hprice1)
summary(price_level_level)

#The estimated model equation is price-hat=-21.77+.002068*lotsize+0.1228*sqrft+13.85*bdrms
#The sample size is 88 and the R-Squared is 0.6724 or 67.24%. 

#The model in part 3 has R-Squared of .6724 whereas the R-Squared model of model in part 1 is not comparable because the dependent variable is log transformed in that model.
#For a comparable goodness-of-fit measure, we use the square of the correlation between the price and exponent of predicted log(price) values i.e, exp(log(price).
hprice1$exp_price_fitted <- exp(hprice1$price_fitted)
comp_r_2_log <- (cor(hprice1$price, hprice1$exp_price_fitted))^2           #From Wooldridge
comp_r_2_log                                                               #0.7377

#The comparable R-Squared for log(price) model is 0.7377. 
#So, we conclude that the log-log model in part 1 is better at explaining the variation in price compared to level-level model in part 3 because comparable R-Squared, 0.7377, for log-log model is greater than level-level model's R-Squared, 0.6724.
