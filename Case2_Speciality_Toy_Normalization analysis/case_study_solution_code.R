#Question1

mean_value = 20000
x <- 30000

#Probability of area under the curve to the left of z score adds upto 0.975
#Calculating the z-score of 0.975 
z_score = qnorm(0.975)
round(z_score,3)

#Calculating standard deviation
standard_deviation <- (x - mean_value)/z_score
standard_deviation

#Plotting the demand distribution
s = seq(10000-standard_deviation ,30000+standard_deviation)
cal = dnorm(s,mean = mean_value, sd = standard_deviation)
plot(s,cal,type = "l", xlab = "Demand Quantity (X)", ylab = "Probability Density", main="Normal Probability Distribution with Mean = 20000 and SD = 5102")
abline(v=mean_value, col = "red") #Plotting the mean line in the normal distribution

#forecaster's prediction (0.95 probability between 10000 to 30000 order quantity)

s = seq(10000 ,30000, 300)
cal = dnorm(s,mean = mean_value, sd = standard_deviation)
options(scipen=999)
polygon(c(10000, s, 30000),c(0,cal,0),border = "black", col = "grey") #shadding 0.95 probability in normal distribution


#Question2

#Calculating probability of stockout when order quantity is 15000
round(1-pnorm(15000, mean_value, standard_deviation),3)

#Calculating probability of stockout when order quantity is 18000
round(1-pnorm(18000, mean_value, standard_deviation),3)

#Calculating probability of stockout when order quantity is 24000
round(1-pnorm(24000, mean_value, standard_deviation),3)

#Calculating probability of stockout when order quantity is 28000
round(1-pnorm(28000, mean_value, standard_deviation),3)

#Question 3
#calculating projected profit for order quantities suggested by management team using functions:

compute_profit<-function(orderquantity,sales){
  cp=orderquantity*16
  if (orderquantity>sales){
    sp=sales*24+(orderquantity-sales)*5
  }
  else
    sp=orderquantity*24
  return (sp-cp)
}
compute_costprice<- function(orderquantity,sales){
  cp=orderquantity*16
  return (cp)
}
compute_sellingprice<-function(orderquantity,sales){
  if (orderquantity>sales){
    sp=sales*24+(orderquantity-sales)*5
  }
  else
    sp=orderquantity*24
  return (sp)
}
x<-list(15000,18000,24000,28000)
x1<-list(10000,20000,30000)
for (i in x){
  for (j in x1){
    print(paste("The cost price with order quantity:",i,"  and sales: ",j,"  is ",compute_costprice(i,j)))
    print(paste("The selling price with order quantity:",i,"  and sales: ",j,"  is ",compute_sellingprice(i,j)))
    print(paste("The profit with order quantity:",i,"  and sales:",j,"  is ",compute_profit(i,j)))
  }
}

#Question4

#calculating order quantity for 70% profit 
i = round(qnorm(0.70, mean = mean_value, sd= standard_deviation),digits = 0)
i

#projected profit under three sales scenario for above calculate order quantity using functions created in Question3

x<-list(10000,20000,30000)
for (j in x){
  print(paste("The cost price with",i,"order quantity and ",j," sales is ",compute_costprice(i,j)))
  print(paste("The selling price with",i,"order quantity and ",j," sales is ",compute_sellingprice(i,j)))
  print(paste("The profit with",i,"order quantity and ",j," sales is ",compute_profit(i,j)))
}

#Question5

#Calculating profit/loss projection under three sale scenarios for different order quantities for recommendation
x<-list(15000,18000,19000,20000,21000, 22000,23000,24000,25000)
x1<-list(10000,20000,30000)
for (i in x){
  for (j in x1){
    print(paste("The cost price with order quantity:",i,"  and sales: ",j,"  is ",compute_costprice(i,j)))
    print(paste("The selling price with order quantity:",i,"  and sales: ",j,"  is ",compute_sellingprice(i,j)))
    print(paste("The profit with order quantity:",i,"  and sales:",j,"  is ",compute_profit(i,j)))
  }
}


