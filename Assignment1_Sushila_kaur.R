#LCG equation
#x(i+1)=(a*x(i)+c)%%m

#generating random marks
lcg <- function(a,c,M,length,seed){
  x <- rep(0,length)
  x[1]<-seed
  for(i in 1:length){
    x[i+1]<-(a*x[i]+c)%%M
  }
  return(x)
}

#calling LCG function for 6 subjects
sub1<-lcg(2,9,101,750,7)
sub2<-lcg(44,3,101,750,2)
sub3<-lcg(3,5,101,750,5)
sub4<-lcg(5,6,101,750,4)
sub5<-lcg(8,2,101,750,5)
sub6<-lcg(12,8,101,750,6)

#mean, median and standard deviation inbuild function calling

#subj1
sub1mean<-mean(sub1)
print(sub1mean)
sub1median<-median(sub1)
print(sub1median)
sub1sd<-sd(sub1)
print(sub1sd)
hist(sub1)

#subj2
sub2mean<-mean(sub2)
print(sub2mean)
sub2median<-median(sub2)
print(sub2median)
sub2sd<-sd(sub2)
print(sub2sd)
hist(sub2)

#subj3
sub3mean<-mean(sub3)
print(sub3mean)
sub3median<-median(sub3)
print(sub3median)
sub3sd<-sd(sub3)
print(sub3sd)
hist(sub3)

#subj4
sub4mean<-mean(sub4)
print(sub4mean)
sub4median<-median(sub4)
print(sub4median)
sub4sd<-sd(sub4)
print(sub4sd)
hist(sub4)

#subj5
sub5mean<-mean(sub5)
print(sub5mean)
sub5median<-median(sub5)
print(sub5median)
sub5sd<-sd(sub5)
print(sub5sd)
hist(sub5)

#subj6
sub6mean<-mean(sub6)
print(sub6mean)
sub6median<-median(sub6)
print(sub6median)
sub6sd<-sd(sub6)
print(sub6sd)
hist(sub6)

sum<-sub1+sub2+sub3+sub4+sub5+sub6
result<-data.frame(sub1, sub2, sub3, sub4, sub5, sub6, sum)
print(result)
mean<- mean(sum)
median<- median(sum)
sdeviation<-sd(sum)
hist(sum)

outlier1<-boxplot(sub3)$out
outlier1

outlier<-function(data)
{
  q<-quantile(data)
  print(q[2])
  IQR=q[4]-q[2]
  min=q[2]-1.5*IQR
  max=q[4]+1.5*IQR
  print("min of range")
  print(as.numeric(min))
  print("max of range")
  print(as.numeric(max))
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

res<-outlier(sub1)
print(res)
res<-outlier(sub2)
res<-outlier(sub3)
res<-outlier(sub4)
res<-outlier(sub5)
res<-outlier(sub6)