# importing .csv file to R
mdr <- read.csv("BAN_602_Case_3.csv")

#Question1

#providing descriptive statistics for AZ100 and T2005
summary(mdr)

var_AZ100 = var(mdr$AZ100)
round(var_AZ100,3)
sd1 = sd(mdr$AZ100)
round(sd1,3)
round(range(mdr$AZ100),3)

var_T2005 = var(mdr$T2005)
round(var_T2005,3)
sd2 = sd(mdr$T2005)
round(sd2,3)
round(range(mdr$T2005),3)

#Deviation from mean
round(mean(mdr$AZ100) - 3,3) 
round(mean(mdr$T2005) - 3,3) #Mean of T2005 is closer to 3 and hence more accurate.

#Question2

#performing t-test since population variance is unknown
t.test(mdr$AZ100,mdr$T2005,var.equal = FALSE) 

#p-value approach : since pvalue is greater than 0.05, we fail to reject null hypothesis

#Alternative we have also calculated the above using the formula for critical value approach

mean_AZ100 = round(mean(mdr$AZ100),3)
mean_T2005 = round(mean(mdr$T2005),3)
diff_mean = mean_AZ100 - mean_T2005

n1 = 31
n2 = 31
num_dof = (((var_AZ100/n1)+(var_T2005/n2))^2)
den_dof = (1/(n1-1))*((var_AZ100/n1)^2) + (1/(n2-1))*((var_T2005/n2)^2)

#calculating degree of freedom
dof = floor(num_dof/den_dof)
dof


#calculating t statistics
num = diff_mean
den = sqrt((var_AZ100/n1) + (var_T2005/n2))
t_val = round(num/den,3)
round(t_val,3)

#calculating talpha/2 value
t_alpha = qt(p=.05/2, df=dof, lower.tail=FALSE) 
round(t_alpha,3)

#critical value approach :comparing t_val and t_alpha. Since t_val < t_aplha- Fail to reject null hypothesis


#Question3
round(var(mdr$AZ100),3)
round(var(mdr$T2005),3) #T2005 has less variance and hence more precise

#Question4
#comparing two variances using F test
var.test(mdr$AZ100, mdr$T2005, alternative = "two.sided")

#p-value test: since p value is less than 0.05, reject null hypothesis

#critical value approach
#calculating falpha/2 value
f_alpha = round(qf(p=.975, df1= 30, df2 = 30),3)
f_alpha #Since F> f_alpha, we reject null hypothesis


#Question5

round(mean(mdr$AZ100),3)
round(var(mdr$AZ100),3)

round(mean(mdr$T2005),3) 
round(var(mdr$T2005),3) 

#mean ofT2005 is more close to 3 and var is less than AZ100, hence T2005 is more accurate and precise.
