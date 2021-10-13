#SushilaKaur
#101903274

#Importing Inbuilt dataset mtcars
cars <- data(mtcars)
head(mtcars)
cars

#mean, median, sd
carmean <- mean(mtcars$mpg)
print(carmean)
carsd <- sd(mtcars$mpg)
print(carsd)
carmedian <- median(mtcars$mpg)
print(carmedian)

#Descriptive Analysis of dataset
hist(mtcars$mpg)
hist(mtcars$cyl)
hist(mtcars$disp)
hist(mtcars$hp)
hist(mtcars$drat)
hist(mtcars$wt)
hist(mtcars$qsec)
hist(mtcars$vs)
hist(mtcars$am)
hist(mtcars$gear)
hist(mtcars$carb)

#Finding Outliers in each Column
outlier<-function(data){
  q<-as.numeric(quantile(data))
  IQR=IQR(data)
  min=q[2]-1.5*IQR
  max=q[4]+1.5*IQR
  flag<-0
  for(val in data)
  {
    #print(val>max)
    if(val<min||val>max)
    {
      print("Outlier")
      print(val)
      flag<-1
    }
  }
  if(flag == 0)
    print("No outliers")
  return("Done")
}
res<-outlier(mtcars$mpg)
res<-outlier(mtcars$drat)
res<-outlier(mtcars$wt)
res<-outlier(mtcars$qsec)
res<-outlier(mtcars$vs)
res<-outlier(mtcars$am)
res<-outlier(mtcars$gear)
res<-outlier(mtcars$carb)
res<-outlier(mtcars$cyl)
res<-outlier(mtcars$disp)
res<-outlier(mtcars$hp)
