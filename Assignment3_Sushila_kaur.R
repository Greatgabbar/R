create<-function(med){
  data<-runif(750,0,100)
  #using the rnorm(no_of_observations, mean, standard_deviation) function
  #data<-rnorm(750,med)
  #data<-sample(data,750)
  #print(data)
  return(data)
}

subj1<-create(55)
subj2<-create(51)
subj3<-create(58)
subj4<-create(60)
subj5<-create(53)
subj6<-create(50)

#mean-------------------------------------------
subjmean <- function(data){
  datamean <- mean(data)
  #print(datamean)
  return(datamean)
}

subj1mean<- subjmean(subj1)
subj2mean<- subjmean(subj2)
subj3mean<- subjmean(subj3)
subj4mean<- subjmean(subj4)
subj5mean<- subjmean(subj5)
subj6mean<- subjmean(subj6)

#median---------------------------------------------
subjmedian <- function(data){
  datamedian <- median(data)
  #print(datamedian)
  return(datamedian)
}

subj1median<- subjmedian(subj1)
subj2median<- subjmedian(subj2)
subj3median<- subjmedian(subj3)
subj4median<- subjmedian(subj4)
subj5median<- subjmedian(subj5)
subj6median<- subjmedian(subj6)

#standard deviation-------------------------------------
subjsd <- function(data){
  datasd <- sd(data)
  #print(datasd)
  return(datasd)
}

subj1sd<- subjsd(subj1)
subj2sd<- subjsd(subj2)
subj3sd<- subjsd(subj3)
subj4sd<- subjsd(subj4)
subj5sd<- subjsd(subj5)
subj6sd<- subjsd(subj6)

#mode----------------------------------------------------
subjmode <- function(data,dmedian,dmean){
  #print(-table(data))
  #datamode <-( sort(-table(data))[1]
  datamode <- (3*dmedian)-(2*dmean)
  print(datamode)
}

subj1mode<- subjmode(subj1,subj1median,subj1mean)
subj2mode<- subjmode(subj2,subj2median,subj2mean)
subj3mode<- subjmode(subj3,subj3median,subj3mean)
subj4mode<- subjmode(subj4,subj4median,subj4mean)
subj5mode<- subjmode(subj5,subj5median,subj5mean)
subj6mode<- subjmode(subj6,subj6median,subj6mean)

#range value-----------------------------------------------
subjrange <- function(data){
  datarange <- range(data)
  #print(datarange)
  return(datarange)
}
subj1range<- subjrange(subj1)
subj2range<- subjrange(subj2)
subj3range<- subjrange(subj3)
subj4range<- subjrange(subj4)
subj5range<- subjrange(subj5)
subj6range<- subjrange(subj6)

#mean deviation------------------------------------------------
subjmeandev<-function(data)
{
  datameandev=sum(abs(data-mean(data)))/length(data)
  print(datameandev)
  return(datameandev)
}
subj1meandev<- subjmeandev(subj1)
subj2meandev<- subjmeandev(subj2)
subj3meandev<- subjmeandev(subj3)
subj4meandev<- subjmeandev(subj4)
subj5meandev<- subjmeandev(subj5)
subj6meandev<- subjmeandev(subj6)

#variance---------------------------------------------------
subjvar <- function(data){
  datavar <- var(data)
  #print(datavar)
  return(datavar)
}
subj1var<- subjvar(subj1)
subj2var<- subjvar(subj2)
subj3var<- subjvar(subj3)
subj4var<- subjvar(subj4)
subj5var<- subjvar(subj5)
subj6var<- subjvar(subj6)

#root mean square deviation----------------------------------
subjrms <- function(data){
  datamode <-subjmode(data)
  n = 0
  for(i in 1:750)
  {
    n = n + ((data[i]-datamode)^2)
  }
  n = n/750
  n = sqrt(n)
  return (n)
}

subj1rms<- subjrms(subj1)
subj2rms<- subjrms(subj2)
subj3rms<- subjrms(subj3)
subj4rms<- subjrms(subj4)
subj5rms<- subjrms(subj5)
subj6rms<- subjrms(subj6)

#gaussian plot for subject 1---------------------------------
library(moments)
guassplot <- function(data){
  dx <- dnorm(data, mean(data), sd(data))
  plot(data,dx, main = "Normal Distribution")
  
  px <- pnorm(data, mean(data), sd(data))
  plot(data,px, main = "P Distribution")
  
  p<- runif(750)
  qx <- qnorm(p, mean(data), sd(data))
  plot(p,qx, main = "Q Distribution")
  
  rx<- rnorm(750, mean(data), sd(data))
  plot(1:750, px, main="rnorm")
  
  #hist(data)
  print("Skewness :")
  print(skewness(data))
  print("Kurtosis :")
  kurtosis(data)
}

guassplot(subj1)
guassplot(subj2)
guassplot(subj3)
guassplot(subj4)
guassplot(subj5)
guassplot(subj6)
