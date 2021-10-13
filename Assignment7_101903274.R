#Sushila Kaur
#101903274

#Assignment 7
#Ques 1

r1<-rnorm(1000, mean=1.7, sd=0.1)
#same parameters, sample size=10000
r2<-rnorm(10000, mean=1.7, sd=0.1)
#(a) plot the density of r1
plot(density(r1), col='green')
#(b) plot on the same graph the density of r2
lines(density(r2), col='red')
#(c) find the 90% interval of a population with mean=1.7 and sd=0.1 between 0.05 and 0.95
#done using qnorm
left<-qnorm(0.05, mean=1.7, sd=0.1)
right<-qnorm(0.95, mean=1.7, sd=0.1)

#(d) calculate the q-value corresponding to every percentile in standard normal distribution
percentiles<-seq(0.01, 0.99, 0.01)
qnorm(percentiles)

#(e) calculate the p-values corresponding to the z values ranging from 0 to 1 at an interval of 0.05
z<-seq(0, 1, 0.05)
pnorm(z)

#ques 2
data<-read.csv('auto.csv')
head(data)
#(a) calculate simple linear correlation between car price and its fuel economy
correlation<-cor(data$Price, data$MPG)
correlation

#(b) create a correlation matrix 
#(c) create a subset auto_num, calculate correlation matrix for it
auto_num<-data[,3:12]
#str function to display the internal structure of an R object
str(auto_num)
#data must be numeric only for correlation matrix
correlation_matrix<-cor(auto_num)
correlation_matrix
#symnum function  used to replace the degree of correlation in numbers by symbols
symnum(correlation_matrix)

#(d)corrgram function for auto_num
#to produce a graphical display of the correlation matrix
install.packages('corrgram')
library('corrgram')
corrgram(auto_num)

#(e)create auto_subset dataframe
auto_subset<-auto_num[, 1:4]
str(auto_subset)
corrgram(auto_subset,lower.panel = panel.cor,upper.panel = panel.pts)