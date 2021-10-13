#Sushila Kaur
#101903274

#Assignment 8
#Ques 1
data_reg<-read.csv('regressionDataSet.csv')
str(data_reg)
train<-data_reg[1:13000,]
test<-data_reg[13001:16382,]

#lm is used to fit linear models
relation<-lm(formula=Area~Energy,data = train)
print(relation)
?lm

#predict the values
predicted<-predict(relation, test)

#plot the data for visualization of best fit line
plot(data_reg$Area,data_reg$Energy,col = "red",main = "Regression",
     abline(relation),cex = 1.3,pch = 16,xlab = "Independent",ylab = "Dependent")

#correlation between predicted and actual value of the tested data
cor(predicted, test$Area)

#calculating root mean square error
RMSE<-function(m, o)
{
  return(sqrt(mean((m-o)^2)))
}

Accuracy<-RMSE(predicted, test$Area)
Accuracy

#Ques 2
x<-rpois(100, 50)
y<-rpois(100, 100)
z<-rpois(100, 150)
df<-data.frame(x,y,z)

#for fitting the linear regression model of the form z= a + bx + cy
lm<-lm(z~x+y,data=df)
summary(lm)


#fit model of form y=a+bx
model1<-lm(z~x,df)
print(model1)

#fit model of form y=a+bx+cx^2
x2<-x^2
df<-cbind(df, x2)
model2<-lm(z~x+x2,df)
print(model2)

#fit model of form y=a.b^x
model3<-lm(log(z)~x,df)
print(model3)

#calculating coefficient of determination for different models
summary(model1)$r.squared
summary(model2)$r.squared
summary(model3)$r.squared