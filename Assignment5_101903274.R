#Sushila Kaur
#101903274

#Assignment 5 
#Ques 1
#number of simulations
sim<-1e5
#number of samples picked
n<-250L
num_success<-0

for(i in 1:sim)
{
  num_success<-num_success + (max(rle(rbinom(n, 1, 0.5))$lengths)>=16)
}

#estimated probability
print(num_success/sim)

#Ques 2
nsim<-1e5
#number of samples picked
n<-8
success<-0
p<-0

for(i in 1:nsim)
{
  x<-rle(rbinom(n, 1, 0.5))
  success<-0
  for(i in x$length)
  {
    if(i<=1)
      success<-success+1
  }
  p<-p+success/n
}

#estimated probability
print(p/nsim)


