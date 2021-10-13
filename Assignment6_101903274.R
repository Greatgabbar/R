#Sushila Kaur
#101903274

#Assignment 6
#Ques 1
# Number of simulation
n <- 5e5

animals <- 1:6
num_success <- 0

for (i in 1L:n) 
{
  num_success <- num_success + identical(sample(animals, size = 6), 1:6)
}

cat('Simulated Probability of getting all the animals in alphabetical order:', num_success/n)

cat('\nActual Probability:', 1/factorial(6))

#Ques 2
# Number of simulation
n <- 5e5

ani <- 1:6
success <- 0

for (i in 1:n) 
{
  success <- success + (sum(sample(ani, size = 3)) == 6)
}

# Estimated probability
cat('Simulated Probability of getting all the animals in alphabetical order:', success/n)
#Analytical Solution
cat('\nActual Probability:', factorial(3)^2/factorial(6))
