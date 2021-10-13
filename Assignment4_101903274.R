#Sushila Kaur
#101903274

#Assignment 4 
#Ques 1
birthday <-function(n)
{
  pnot<-1
  for(i in 1:n-1)
    pnot<-pnot *((365-i)/365)
  p<-1-pnot;
  return (p)
}
flag<-0;
sum<-0;
prob<- c(1:365);
for(i in 1:365)
{
  prob[i]<-birthday(i);
  if(prob[i]>=0.5 && flag==0)
  {
    print(paste("minimum people for probablity to be 0.5 are ",i))
    flag<-1
  }
}
plot(prob)

#Ques 2

a <- vector(length=11)
x <- c(0:10)
payoff <- x^2 - (7 *x)
ph<- choose(10,x)*(0.6^x)*(0.4^(10-x))
print(ph)
print(payoff)
sum = 0
for (i in 1:11)
{
  sum= sum + (ph[i]*payoff[i])
  
}
print(sum)

if(sum > 0){
  print("It is a good bet")
  } else if( sum = 0){
  print("Neither good nor bad bet")
  } else{
  print("It is a bad bet")
  }

