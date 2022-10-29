#Importing .csv file to R

Heavenly_chocolates <- read.csv(file = "BAN_602_Case_1.csv", header = TRUE) #Changing .csv file name to Heavenly_chocolates

#Question 1

summary(Heavenly_chocolates) #Provides summary (Mean, Median, Min, Max)

round(var(Heavenly_chocolates$Time..min.),2) #Variance, SD and Range for Time min upto 2 decimal points
round(sd(Heavenly_chocolates$Time..min.),2)
round(range(Heavenly_chocolates$Time..min.),2)

round(var(Heavenly_chocolates$Pages.Viewed),2)  #Variance, SD and Range for Pages Viewed upto 2 decimal points
round(sd(Heavenly_chocolates$Pages.Viewed),2)
round(range(Heavenly_chocolates$Pages.Viewed),2)

round(var(Heavenly_chocolates$Amount.Spent....),2) #Variance, SD and Range for Amount Spent upto 2 decimal points
round(sd(Heavenly_chocolates$Amount.Spent....),2)
round(range(Heavenly_chocolates$Amount.Spent....),2)

#Question 2

#Numerical Measure of Relationship between Pages Viewed & Amount Spent
round(cov(Heavenly_chocolates$Pages.Viewed, Heavenly_chocolates$Amount.Spent....),3) #Covariance between Pages Viewed & Amount Spent 
round(cor(Heavenly_chocolates$Pages.Viewed, Heavenly_chocolates$Amount.Spent....),3) #Correlation between Pages Viewed & Amount Spent

#Scatter plot showing relationship between Pages Viewed & Amount Spent
ggplot(data = Heavenly_chocolates,aes(x=Pages.Viewed,y=Amount.Spent....)) + geom_point() + geom_smooth(formula = y~x, method = lm) + ggtitle("Relationship between Amount spent & Pages Viewed") + labs(x="Pages Viewed", y = "Amount Spent") 

#Question 3

#Histogram showing Time spent distribution
ggplot(data = Heavenly_chocolates, aes(x=Time..min.)) + geom_histogram(fill = "grey", col="black", bins = 10) + ggtitle("Histogram of Minimum Time") + labs(x="Minimum Time Spent", y ="Frequency")

#Density curve of above histogram

ggplot(data = Heavenly_chocolates, aes(x=Time..min.)) + geom_histogram(aes(y=..density..),bins=10,fill="grey",col= "black") +geom_density()

#Question 4

table(Heavenly_chocolates$Day,Heavenly_chocolates$Browser) #Cross tabulation of Categorical variables

x <- table(Heavenly_chocolates$Day,Heavenly_chocolates$Browser)  # storing cross tabulation in x
print(x)

#Calculating Row Percentage
rowpercent <- round((prop.table(x,1) * 100),3) #Calculating Row Percentage 
print(rowpercent)                              #Printing Row Percentages

#Calculating Col Percentage
colpercent <- round((prop.table(x,2) * 100), 3) #Calculating Column Percentage
print(colpercent)                              #Printing Column Percentages

#Question 5

#Boxplot for browserType
ggplot(data = Heavenly_chocolates,aes(x=Browser, y=Amount.Spent....)) + geom_boxplot(fill= "grey") + labs(x = "Browser", y="Amount Spent") + ggtitle("Amount Spent by Customers by Browser Type") 

Heavenly_chocolates %>% filter(Browser == "Chrome") -> chrome_user    #filtering Chrome users
round(quantile(chrome_user$Amount.Spent....),3)                       #Calculating Quartile for Chrome users

Heavenly_chocolates %>% filter(Browser == "Firefox") -> firefox_user #Filtering Firefox users
round(quantile(firefox_user$Amount.Spent....),3)                     #Calculating Quartile for Firefox users

Heavenly_chocolates %>% filter(Browser == "Other") -> other_user    #Filtering other browser users
round(quantile(other_user$Amount.Spent....),3)                      #Calculating Quartile for Other browser users
