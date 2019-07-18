###
# Jimmy Hickey
# 2019-07-18
# Sampling Distribution of the Laplace Distribution.
###

set.seed(1978)

# Problem 1.

# d.

# We’ll generate N = 50 values of L and K for every value of n from 1 to 250.
# That is, consider n = 1. We want to generate N = 50 datasets with n = 1.
# For each data set we want to find L and K. Now, for n = 2 we want to
# generate 50 datasets, find L and K for each one. All the way until we get to
# n = 250.

max_sample_size = 250 
n = seq(1:max_sample_size)
N = 50

K_Matrix = matrix(data = NA, nrow = N, ncol = max_sample_size)
L_Matrix = matrix(data = NA, nrow = N, ncol = max_sample_size)

mu = 0
b = 5

# F inverse
# Plug in values from a Uniform(0, 1) into the y parameter
# to get a realization of the Laplace CDF
F_inv = function(mu, b, y){
  return(mu - b * sign(y - 0.5) * log(1 - 2 * abs(y - 0.5)))
}

# L inverse
# Sum the squares of an array of inputs and divide by the length
L_inv = function(ys){
  n = length(ys)
  return(1/n * sum(ys^2))
}


# K inverse
# Sum an array of inputs and divide by the length
K_inv = function(ys){
  n = length(ys)
  return(1/n * sum(ys))
} 


# Loop over sample sizes
for (j in 1:max_sample_size){
  
  # Loop through data sets
  for (i in 1:N){
    random = runif(n[j])
    K_Matrix[i, j] = K_inv(F_inv(mu, b, random))
    L_Matrix[i, j] = L_inv(F_inv(mu, b, random))
  }
}

# Plotting K and L

plot(n, K_Matrix[1,], main="n vs K", 
     xlab="n ", ylab="K_n", add = TRUE)

for (i in 2:50){
  points(n, K_Matrix[i,], col = i)
  
}

plot(n, K_Matrix[1,], main="n vs K", 
     xlab="n ", ylab="K_n", add = TRUE)

points(n, K_Matrix[25,], col = 2)

points(n, K_Matrix[50,], col = 3)

