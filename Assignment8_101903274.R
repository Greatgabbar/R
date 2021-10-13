#Sushila Kaur
#101903274

#Assignment 8
#Ques 1
data<-read.csv('regressionDataSet.csv')
n<-length(data$RMSD)
index<-sample(1:n,n)
index
train_data_len=floor(n*0.8)
train_data=data.frame(data[index[1:train_data_len],])
test=data.frame(data[index[train_data_len+1:n],])
test

relationlm<-lm(formula=Area~Energy,data = train_data)
print(relationlm)
predictedData <- predict(relationlm,test)
test$Area
predictedData

plot(data$Area,data$Energy,col = "red",main = "Regression",
     abline(relationlm),cex = 1.3,pch = 16,xlab = "Independent",ylab = "Dependent")
length(predictedData)
cor(predictedData, test$Area,use = "complete.obs")

#calculating root mean square error
RMSE<-function(m, o)
{
  return(sqrt(sum((m-o)^2,na.rm = TRUE)/n))
}


Accuracy<-RMSE(predictedData, test$Area)
Accuracy/n


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